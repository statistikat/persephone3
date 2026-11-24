# Item 27 (M7) product lifecycle acceptance: install -> load -> construct ->
# fit -> forecast -> diagnose -> plot -> dashboard -> serialize/reload/rerun
# for both methods WITHOUT Java or rjd3, on the deliberate native backend.
# Usage: Rscript tools/verify-lifecycle.R --library=/isolated/library
# --library isolates this R process only; it never edits the user's libraries.
# Launch environment (asserted below, never relaxed): R_LIBS_USER and
# R_LIBS_SITE name directories free of the six Java-related packages, so the
# callr dashboard child and the Shiny grandchild it serves also run Java-free;
# LD_LIBRARY_PATH provides the OpenBLAS provider for the rkalos core and
# JAVA_HOME deliberately points nowhere.
args <- commandArgs(trailingOnly = TRUE)
lib_arg <- args[startsWith(args, "--library=")]
stopifnot(length(lib_arg) == 1L)
lib <- normalizePath(sub("^--library=", "", lib_arg), mustWork = TRUE)
for (name in c(".Library", ".Library.site")) {
  locked <- bindingIsLocked(name, baseenv())
  if (locked) unlockBinding(name, baseenv())
  assign(name, if (name == ".Library") lib else character(), baseenv())
  if (locked) lockBinding(name, baseenv())
}
.libPaths(lib)
forbidden <- c("rJava", "rjd3toolkit", "rjd3x13", "rjd3tramoseats", "rjd3jars", "RProtoBuf")
nojava_ok <- function(cond, label) {
  if (!isTRUE(cond)) stop("JAVA-FREE GATE FAILED: ", label, call. = FALSE)
}
# The child/grandchild guarantee: inherited R_LIBS_USER/R_LIBS_SITE must be
# real directories without any forbidden package, so callr processes launched
# by this session (including the package's own dashboard launcher) cannot
# resolve Java packages from the user or site libraries.
for (var in c("R_LIBS_USER", "R_LIBS_SITE")) {
  dir <- Sys.getenv(var)
  nojava_ok(dir != "" && dir.exists(dir) &&
    !any(vapply(forbidden, function(p) dir.exists(file.path(dir, p)), logical(1))),
    paste0(var, " must name a package directory without forbidden packages"))
}
nojava_ok(identical(.libPaths(), lib),
          paste(".libPaths() must be exactly", lib))
nojava_ok(!any(vapply(forbidden, function(p) nzchar(system.file(package = p)), logical(1))),
          "a forbidden package is findable")
nojava_ok(!any(forbidden %in% loadedNamespaces()), "a forbidden package is loaded")
dir_entries <- list.dirs(lib, recursive = FALSE, full.names = FALSE)
cat(sprintf("ISOLATED LIBRARY %s entries=%d forbidden=%s\n", lib, length(dir_entries),
            paste(sprintf("%s=%s", forbidden, ifelse(forbidden %in% dir_entries,
                                                     "PRESENT", "absent")), collapse = " ")))
cat(sprintf("ISOLATED RKALOS version=%s core=%s\n",
            read.dcf(file.path(lib, "rkalos", "DESCRIPTION"), "Version"),
            rkalos::core_version()$commit))

steps <- character()
step <- function(lane, name, pass, evidence) {
  status <- if (isTRUE(pass)) "PASS" else if (is.character(pass)) pass else "FAIL"
  steps <<- c(steps, sprintf("STEP %s|%s|%s|%s", lane, name, status, evidence))
  cat(sprintf("%-14s %-22s %-4s %s\n", lane, name, status, evidence))
  invisible(pass)
}
fail <- function(lane, name, evidence) {
  step(lane, name, FALSE, evidence)
  stop(sprintf("HARD LIFECYCLE FAILURE in %s/%s: %s", lane, name, evidence),
       call. = FALSE)
}
attempt <- function(expr) tryCatch(expr, error = function(e)
  structure(conditionMessage(e), class = "lifecycle_error"))
gap <- function(lane, name, err) {
  step(lane, name, "GAP", paste0("cutover gap: ", as.character(err)))
}

library(persephone3)
nojava_ok(!any(forbidden %in% loadedNamespaces()),
          "library(persephone3) loaded a forbidden namespace")
step("package", "install-load", TRUE,
     paste("persephone3", as.character(utils::packageVersion("persephone3")),
           "loaded with zero Java packages findable or loaded"))
# The deliberate native backend: lane x13-rsa0 below constructs WITHOUT an
# explicit backend argument, so the option is the only native decision point.
options(persephone.backend = "native")
cap <- backend_capabilities("native")
nojava_ok(identical(cap$methods, c("x13", "tramoseats")),
          "native backend does not advertise both methods")

tramo_cap <- rkalos::capabilities()$tramo_seats_fit
lanes <- list(
  list(id = "x13-rsa0", construct = function() perX13(AirPassengers, "rsa0"),
       n_fc = 12L, se = "available", residuals = TRUE),
  list(id = "x13-rsa3",
       construct = function() perX13(AirPassengers, "rsa3", backend = "native",
                                     speclist = list(backcast = 0L, forecast = 6L)),
       n_fc = 6L, se = "unavailable", residuals = FALSE),
  list(id = "tramo-rsa0", construct = function() perTramo(AirPassengers, "rsa0",
                                                          backend = "native"),
       n_fc = 24L, se = "available", residuals = TRUE),
  list(id = "tramo-rsa1", construct = function() perTramo(AirPassengers, "rsa1",
                                                          backend = "native"),
       n_fc = 24L, se = "available", residuals = TRUE))
lanes <- Filter(function(lane) {
  if (lane$id != "tramo-rsa1") return(TRUE)
  keep <- isTRUE(tramo_cap$available) && "rsa1" %in% tolower(tramo_cap$templates)
  if (!keep) step("tramo-rsa1", "construct", "SKIP",
                  "pinned rkalos tramo_seats_fit capability omits RSA1")
  keep
}, lanes)

for (lane in lanes) {
  id <- lane$id
  # construct
  obj <- attempt(lane$construct())
  if (inherits(obj, "lifecycle_error")) fail(id, "construct", as.character(obj))
  if (!identical(obj$backend, "native")) fail(id, "construct",
                                              paste("backend is", obj$backend))
  step(id, "construct", TRUE,
       paste("class(obj$params)[[1]] =", class(obj$params)[[1]], "; obj$backend = native",
             if (id == "x13-rsa0") " (via the persephone.backend option alone)" else ""))
  # fit
  ran <- attempt(obj$run())
  if (inherits(ran, "lifecycle_error")) fail(id, "fit", as.character(ran))
  if (!identical(obj$result$status$code, "success") ||
      !identical(obj$result$backend, "native"))
    fail(id, "fit", paste("status:", obj$result$status$code,
                          "backend:", obj$result$backend))
  if (!identical(obj$adjusted, obj$output$finalSA))
    fail(id, "fit", "adjusted is not the final SA series")
  step(id, "fit", TRUE, paste("status=success backend=native adjusted n =",
                              length(obj$adjusted)))
  # forecast: points and interval uncertainty as honestly reported
  fc <- obj$forecasts
  if (!inherits(fc, "persephone_forecasts") ||
      !identical(length(fc$points$y), lane$n_fc) ||
      !identical(as.numeric(start(fc$points$y)), c(1961, 1)))
    fail(id, "forecast", "forecast points missing or on the wrong domain")
  se_avail <- fc$availability$standard_errors[1L]
  if (!identical(se_avail, lane$se))
    fail(id, "forecast", paste("standard_errors availability:", se_avail,
                               "expected:", lane$se))
  band_note <- if (identical(se_avail, "available")) {
    band <- {
      series <- obj$hchart()$x$hc_opts$series
      series[[match("confidence interval",
                    vapply(series, "[[", character(1), "name"))]]$data
    }
    ok_low <- identical(vapply(band, "[[", numeric(1), "low"),
                        as.numeric(fc$points$y - 1.96 * fc$standard_errors$y))
    ok_high <- identical(vapply(band, "[[", numeric(1), "high"),
                         as.numeric(fc$points$y + 1.96 * fc$standard_errors$y))
    if (!ok_low || !ok_high) fail(id, "forecast", "confidence band is not points +/- 1.96 SE")
    paste0("confidence band identical to points -/+ 1.96*SE (n SE = ",
           length(fc$standard_errors$y), ")")
  } else {
    series <- obj$hchart()$x$hc_opts$series
    if ("confidence interval" %in% vapply(series, "[[", character(1), "name"))
      fail(id, "forecast", "chart shows a band the fit does not claim")
    "standard errors honestly reported unavailable; no band drawn"
  }
  step(id, "forecast", TRUE,
       paste0("points n = ", lane$n_fc, " from 1961-1; ", band_note))
  # diagnose: diagnostics table accessor plus residual diagnostics
  tbl <- attempt(obj$diagnosticsTable())
  if (inherits(tbl, "lifecycle_error")) fail(id, "diagnose", as.character(tbl))
  need <- c("normality", "independence", "seasonality", "arimaMdl")
  if (!is.data.frame(tbl) || nrow(tbl) < 1L || !all(need %in% names(tbl)) ||
      !identical(diagnosticsTable(obj), tbl))
    fail(id, "diagnose", "diagnosticsTable incomplete or accessor pair disagrees")
  resid_note <- if (lane$residuals) {
    rs <- attempt(persephone3:::fitted_residuals(obj)$tsres)
    if (inherits(rs, "lifecycle_error")) fail(id, "diagnose", paste("residual series:", rs))
    if (is.null(rs) || length(rs) < 12L) fail(id, "diagnose", "residual series missing")
    acf_chart <- attempt(obj$plotResiduals(which = "acf", interactive = FALSE))
    if (inherits(acf_chart, "lifecycle_error"))
      fail(id, "diagnose", paste("residual acf chart:", as.character(acf_chart)))
    paste0("residuals n = ", length(rs),
           "; residual acf chart served; normality=", tbl$normality,
           " independence=", tbl$independence)
  } else {
    err <- attempt(obj$plotResiduals(which = "acf", interactive = FALSE))
    if (!inherits(err, "lifecycle_error"))
      fail(id, "diagnose", "residual charts unexpectedly available without residuals")
    gap(id, "diagnose-residuals", err)
    paste0("residual diagnostics refused natively; normality=", tbl$normality,
           " independence=", tbl$independence)
  }
  step(id, "diagnose", TRUE,
       paste0("obj$diagnosticsTable() 1x", ncol(tbl),
              " identical to diagnosticsTable(obj); ", resid_note))
  # plot: fit and forecast charts from both engines
  dy <- attempt(plot(obj))
  if (inherits(dy, "lifecycle_error") || !inherits(dy, "htmlwidget") ||
      !identical(dy$x$attrs$labels,
                 c("month", "Original", "Seasonally Adjusted", "Trend", "Forecasts")))
    fail(id, "plot-fit", if (inherits(dy, "lifecycle_error")) as.character(dy)
         else paste("dygraph labels:", paste(dy$x$attrs$labels, collapse = ",")))
  fc_col <- dy$x$data[[match("Forecasts", dy$x$attrs$labels)]]
  if (identical(se_avail, "available")) {
    # With standard errors the dygraph Forecasts column carries the rendered
    # interval triple (lowerci, y_f, upperci) on every row (plot_single.R).
    rows <- lapply(fc_col, function(z)
      if (length(z) == 3L) as.numeric(z) else rep(NA_real_, 3L))
    m <- do.call(rbind, rows)
    tail_rows <- nrow(m) - rev(seq_len(lane$n_fc) - 1L)
    if (!identical(m[tail_rows, 2L], as.numeric(fc$points$y)) ||
        !identical(m[tail_rows, 1L], as.numeric(fc$points$y - 1.96 * fc$standard_errors$y)) ||
        !identical(m[tail_rows, 3L], as.numeric(fc$points$y + 1.96 * fc$standard_errors$y)))
      fail(id, "plot-fit", "chart forecast interval differs from points -/+ 1.96*SE")
    fc_chart_note <- "Forecasts column is the (lowerci, y_f, upperci) triple, identical to points -/+ 1.96*SE"
  } else if (identical(as.numeric(utils::tail(fc_col, lane$n_fc)),
                       as.numeric(fc$points$y))) {
    fc_chart_note <- "chart forecast tail identical to points (no interval, SE unavailable)"
  } else {
    fail(id, "plot-fit", "chart forecast tail differs from forecast points")
  }
  step(id, "plot-fit", TRUE, paste(
    "plot() dygraph carries Original/Trend/Seasonally Adjusted/Forecasts;", fc_chart_note))
  dy0 <- attempt(plot(obj, forecasts = FALSE))
  if (inherits(dy0, "lifecycle_error") ||
      "Forecasts" %in% dy0$x$attrs$labels)
    fail(id, "plot-forecast-off", if (inherits(dy0, "lifecycle_error"))
         as.character(dy0) else "Forecasts still present")
  ap <- attempt(autoplot(obj))
  hc <- attempt(obj$hchart())
  if (inherits(ap, "lifecycle_error") || !inherits(ap, "ggplot") ||
      inherits(hc, "lifecycle_error") || !inherits(hc, "highchart"))
    fail(id, "plot-engines", paste("autoplot:",
        if (inherits(ap, "lifecycle_error")) as.character(ap) else class(ap)[[1]],
        " hchart:", if (inherits(hc, "lifecycle_error")) as.character(hc) else class(hc)[[1]]))
  step(id, "plot-engines", TRUE,
       "autoplot() ggplot and hchart() highchart valid; plot(forecasts = FALSE) drops Forecasts")
  # serialize -> reload -> rerun
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saved <- attempt(saveRDS(obj, file))
  if (inherits(saved, "lifecycle_error")) fail(id, "serialize", as.character(saved))
  re <- attempt(readRDS(file))
  if (inherits(re, "lifecycle_error")) fail(id, "serialize", as.character(re))
  # exact equality asserted after reload: bit-identical whole objects for the
  # adjusted series, the forecast contract, the full result and the raw output.
  eq <- c(adjusted = identical(re$adjusted, obj$adjusted),
          forecasts = identical(re$forecasts, obj$forecasts),
          result = identical(re$result, obj$result),
          output = identical(re$output, obj$output))
  if (!all(eq)) fail(id, "serialize", paste(names(eq)[!eq], collapse = ", "))
  requery <- attempt(identical(re$diagnosticsTable(), obj$diagnosticsTable()))
  if (!isTRUE(requery)) fail(id, "reload-requery",
      if (inherits(requery, "lifecycle_error")) as.character(requery)
      else "reloaded diagnosticsTable differs")
  rerun <- attempt({re$run()
    identical(re$adjusted, obj$adjusted) && identical(re$forecasts, obj$forecasts)})
  if (!isTRUE(rerun)) fail(id, "rerun",
      if (inherits(rerun, "lifecycle_error")) as.character(rerun)
      else "post-reload rerun is not bit-identical to the pre-serialize fit")
  step(id, "serialize", TRUE,
       paste("saveRDS/readRDS: identical() TRUE for adjusted, forecasts, result, output;",
             "requery identical(diagnosticsTable); rerun after reload bit-identical"))
  nojava_ok(!any(forbidden %in% loadedNamespaces()),
            paste("lane", id, "loaded a forbidden namespace"))
}

# Dashboard acceptance in a separate callr child process: the child re-isolates
# to the same Java-free library, builds the single-series dashboard through
# createDashboard() (dashboard_single HTML with embedded charts), then serves
# the batch dashboard through the package's own Shiny launcher (createDashboard
# -> callr::r_bg( shiny::runApp on an httpuv randomPort)) and fetches the root
# HTML over HTTP before shutting the server down. Java is physically absent in
# both the child and the Shiny grandchild.
dash <- callr::r(function(lib, forbidden, title) {
  for (name in c(".Library", ".Library.site")) {
    locked <- bindingIsLocked(name, baseenv())
    if (locked) unlockBinding(name, baseenv())
    assign(name, if (name == ".Library") lib else character(), baseenv())
    if (locked) lockBinding(name, baseenv())
  }
  .libPaths(lib)
  stopifnot(!any(vapply(forbidden, function(p) nzchar(system.file(package = p)),
                        logical(1))),
            !any(forbidden %in% loadedNamespaces()))
  suppressPackageStartupMessages(library(persephone3))
  options(persephone.backend = "native")
  obj <- perX13(AirPassengers, "rsa0", backend = "native")
  obj$run()
  file <- tempfile(fileext = ".html")
  createDashboard(obj, title = title, file = file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  single <- list(bytes = file.size(file), title = grepl(title, html, fixed = TRUE),
                 diagnostics = grepl("Normality", html, fixed = TRUE) &&
                   grepl("Independence", html, fixed = TRUE),
                 clean = !any(grepl(c("Plot error:", "Plot could not be created",
                                      "Plot unavailable", "Unsupported plot object"),
                                    html, fixed = TRUE)))
  group <- perBatch(first = AirPassengers, second = AirPassengers,
                    method = "x13", template = "rsa0", backend = "native")
  group$run()
  proc <- createDashboard(group, title = paste(title, "batch"), launch.browser = FALSE,
                          startup.timeout = 90)
  env_raw <- readBin(sprintf("/proc/%d/environ", proc$get_pid()), "raw", n = 1048576L)
  nul <- which(env_raw == 0L)
  starts <- c(1L, nul[-length(nul)] + 1L)
  entries <- vapply(seq_along(nul),
                    function(i) rawToChar(env_raw[starts[i]:(nul[i] - 1L)]),
                    character(1))
  getenv <- function(var)
    sub(paste0("^", var, "="), "", entries[startsWith(entries, paste0(var, "="))][1L])
  url <- attr(proc, "url")
  page_text <- tryCatch(paste(readLines(url, warn = FALSE), collapse = "\n"),
                        error = function(e) "")
  served <- list(url = url,
                 rlibs = identical(unname(getenv("R_LIBS")), lib),
                 user_clean = !any(vapply(forbidden, function(p)
                   dir.exists(file.path(unname(getenv("R_LIBS_USER")), p)), logical(1))),
                 site_clean = !any(vapply(forbidden, function(p)
                   dir.exists(file.path(unname(getenv("R_LIBS_SITE")), p)), logical(1))),
                 title = grepl(paste(title, "batch"), page_text, fixed = TRUE),
                 registry = grepl("Please select a series", page_text, fixed = TRUE),
                 html = grepl("<(!DOCTYPE|html)", page_text))
  proc$kill()
  proc$wait(timeout = 10000)
  served$alive_after_kill <- proc$is_alive()
  list(single = single, served = served,
       forbidden_loaded = intersect(forbidden, loadedNamespaces()),
       paths = .libPaths())
}, libpath = lib, args = list(lib = lib, forbidden = forbidden,
                              title = "M7 lifecycle dashboard"))
nojava_ok(length(dash$forbidden_loaded) == 0L,
          paste("dashboard child loaded:", paste(dash$forbidden_loaded, collapse = ",")))
step("dashboard", "child-isolation", TRUE,
     paste("callr child .libPaths =", paste(dash$paths, collapse = " + "),
           "; forbidden loaded = none"))
if (!(dash$single$bytes > 0 && all(unlist(dash$single[c("title", "diagnostics",
                                                         "clean")]))))
  fail("dashboard", "single", paste(capture.output(str(dash$single)), collapse = " "))
step("dashboard", "single", TRUE,
     paste("createDashboard() HTML", dash$single$bytes,
           "bytes: title, Normality/Independence table and all charts rendered, no plot errors"))
if (!all(unlist(dash$served[c("rlibs", "user_clean", "site_clean", "title",
                              "registry", "html")])))
  fail("dashboard", "shiny-launcher", paste(capture.output(str(dash$served)), collapse = " "))
step("dashboard", "shiny-launcher", TRUE,
     paste0("package launcher served ", dash$served$url,
            " (R_LIBS=", lib, ", grandchild user/site libs clean): root HTML contains title",
            " and series registry; process dead after kill = ",
            !dash$served$alive_after_kill))
nojava_ok(!dash$served$alive_after_kill, "dashboard server survived kill")
nojava_ok(!any(forbidden %in% loadedNamespaces()),
          "a forbidden namespace was loaded during the run")
gaps <- grep("|GAP|", steps, value = TRUE, fixed = TRUE)
cat(sprintf("LIFECYCLE STEPS=%d GAPS=%d\n", length(steps), length(gaps)))
for (g in gaps) cat(g, "\n")
cat("PASS: item 27 product lifecycle install -> load -> construct -> fit -> forecast",
    "-> diagnose -> plot -> dashboard -> serialize/reload/rerun without Java or rjd3\n")
