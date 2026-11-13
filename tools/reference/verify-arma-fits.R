# Pinned fixed-order qualification and matched timings; frozen references stay read-only.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling, "fit-timing.R"))
config <- fit_timing_options(commandArgs(trailingOnly = TRUE))
source(file.path(tooling, "common.R"))
source(file.path(tooling, "extended-fixtures.R"))
root <- reference_root(); base_dir <- reference_fixture_dir(root)
if (!is.null(config$output)) {
  config$output <- normalizePath(config$output, mustWork = TRUE)
  frozen <- normalizePath(base_dir, mustWork = TRUE)
  if (!dir.exists(config$output) || config$output == frozen || startsWith(config$output, paste0(frozen, "/"))) {
    stop("Timing output must be an existing directory outside the frozen reference corpus")
  }
  if (any(file.exists(file.path(config$output, c("raw.csv", "summary.csv", "by-model.csv", "metadata.R"))))) {
    stop("Timing output already exists; choose an empty directory")
  }
}
before <- extended_parent_hashes(base_dir)
native_preloaded <- "persephone3" %in% loadedNamespaces()
native_load <- fit_timing_once(function() {
  if (config$source) pkgload::load_all(root, quiet = TRUE, compile = FALSE)
  else if (!requireNamespace("persephone3", quietly = TRUE)) stop("Install persephone3 before comparing fits")
})
reference_preloaded <- any(c("rjd3toolkit", "rjd3x13", "rjd3tramoseats", "rJava") %in% loadedNamespaces())
reference_load <- fit_timing_once(function() {
  for (package in c("rjd3toolkit", "rjd3x13", "rjd3tramoseats")) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Missing reference package: ", package)
  }
  # Public runtime query ensures JVM initialization has occurred inside this interval.
  rJava::.jcall("java/lang/System", "S", "getProperty", "java.version")
})
pin <- fit_timing_once(function() extended_assert_oracle(base_dir))
source(file.path(root, "inst/tinytest/fixtures/native/arma-fit-cases.R"))
setup <- getFromNamespace("native_arma_objective_setup", "persephone3")
optimize_native <- getFromNamespace("native_arma_optimize", "persephone3")
control <- list(max_iterations = 200L, max_evaluations = 4000L)
execute_native <- function(context) optimize_native(context, control = control)
native_value <- function(result) {
  if (!isTRUE(result$converged)) stop("Timed native fit did not converge")
  result$final$value
}
reference_value <- function(result) -result$estimation$likelihood$adjustedll
source(file.path(tooling, "arma-fit-reference.R"))
native_preparer <- function(case) { force(case); function() do.call(setup, case$arguments) }
reference_preparer <- function(case, method) { force(case); force(method); function() prepare_reference(case, method) }
jobs <- list()
checks <- 0L; failures <- character(); direct_fits <- 0L
assert <- function(value, label) {
  checks <<- checks + 1L
  if (!isTRUE(value)) failures <<- c(failures, label)
}
# Predeclared bounded-matrix comparison budgets; do not edit corpus tolerances.
close <- function(a, b, tolerance, label) assert(length(a) == length(b) &&
  all(is.finite(a)) && all(is.finite(b)) && all(abs(a - b) <= tolerance * (1 + abs(b))), label)
maximum <- c(coefficient = 0, regression = 0, objective = 0, density = 0)
cases <- arma_fit_cases()
for (case in cases) {
  context <- do.call(setup, case$arguments)
  native <- execute_native(context)
  assert(native$converged, paste(case$id, "native", native$status))
  if (!native$final$available) next
  if (config$enabled) jobs[[length(jobs) + 1L]] <- fit_timing_job(case, "native", context,
    native_preparer(case), execute_native, native_value, native$final$value)
  expected <- arma_fit_density(case, native$final$parameters$coefficients)
  close(native$final$value, expected$value, 2e-8, paste(case$id, "independent density"))
  close(native$final$likelihood$regression_coefficients, expected$beta, 2e-8, paste(case$id, "independent GLS"))
  maximum["density"] <- max(maximum["density"], abs(native$final$value - expected$value))
  for (method in c("x13", "tramoseats")) {
    prepared <- prepare_reference(case, method)
    spec <- prepared$spec; model_context <- prepared$context; y <- prepared$series
    columns <- case$arguments$regressors
    direct_fits <- direct_fits + 1L
    execute <- reference_executor(method)
    fit <- execute(prepared)
    label <- paste(method, case$id)
    assert(is.list(fit) && !is.null(fit$estimation), paste(label, "reference fit"))
    if (!is.list(fit) || is.null(fit$estimation)) next
    if (config$enabled) jobs[[length(jobs) + 1L]] <- fit_timing_job(case,
      if (method == "x13") "jd_x13" else "jd_tramo", prepared,
      reference_preparer(case, method), execute, reference_value, reference_value(fit))
    blocks <- lapply(c("phi", "theta", "bphi", "btheta"), function(key) {
      x <- fit$description$arima[[key]]
      if (is.null(x)) numeric() else as.numeric(x["value", ])
    })
    names(blocks) <- names(case$specification)
    blocks$ar <- -blocks$ar; blocks$seasonal_ar <- -blocks$seasonal_ar
    actual <- unlist(blocks, use.names = FALSE)
    estimate <- unlist(native$final$parameters$coefficients, use.names = FALSE)
    close(estimate, actual, 1e-4, paste(label, "ARMA coefficients"))
    fixed <- !is.na(unlist(case$specification, use.names = FALSE))
    assert(identical(actual[fixed], unlist(case$specification, use.names = FALSE)[fixed]), paste(label, "fixed coefficients"))
    likelihood <- fit$estimation$likelihood
    close(native$final$value, -likelihood$adjustedll, 2e-8, paste(label, "fitted objective"))
    close(likelihood$adjustedll - likelihood$ll, expected$jacobian, 1e-10, paste(label, "Jacobian"))
    # The generated mean/drift precedes user variables in the reference description.
    beta <- vapply(fit$description$variables, function(v) v$coef$value, numeric(1))
    names_beta <- vapply(fit$description$variables, function(v) v$name, character(1))
    assert(identical(tail(names_beta, 2L), c("r.x", "r.fixed")), paste(label, "regressor order"))
    close(native$final$likelihood$regression_coefficients, beta[c(2, 3, 1)], 1e-5, paste(label, "regression coefficients"))
    close(likelihood$neffectiveobs, expected$finite, 0, paste(label, "effective observations"))
    close(likelihood$nparams, prepared$free_parameters + 3L, 0, paste(label, "parameter count"))
    # Evaluate the reference coefficients through an independent density too.
    direct_density <- arma_fit_density(case, blocks)
    close(-likelihood$ll, -direct_density$profile, 2e-8, paste(label, "reference density convention"))
    maximum <- pmax(maximum, c(coefficient = max(abs(estimate - actual)),
      regression = max(abs(native$final$likelihood$regression_coefficients - beta[c(2, 3, 1)])),
      objective = abs(native$final$value + likelihood$adjustedll),
      density = abs(likelihood$ll - direct_density$profile)))
    if (case$kind == "mixed_factors" && grepl("gaps", case$id)) {
      changed <- columns
      for (j in seq_along(changed)) {
        future <- seq.int(length(case$arguments$series) + 1L, length(changed[[j]]))
        changed[[j]][future] <- changed[[j]][future] + 1000
      }
      changed_context <- rjd3toolkit::modelling_context(variables = list(r = changed))
      direct_fits <- direct_fits + 1L
      other <- if (method == "x13") rjd3x13::regarima_fast(y, spec, context = changed_context) else
        rjd3tramoseats::tramo_fast(y, spec, context = changed_context)
      assert(identical(fit$description, other$description), paste(label, "future-context description"))
      assert(identical(fit$estimation, other$estimation), paste(label, "future-context estimation"))
    }
  }
  cat(case$id, native$status, "\n")
}
# Separate persistent-AR controls; both frequencies and both pinned engines.
set.seed(10101)
noise <- rnorm(240)
for (period in c(4L, 12L)) for (phi in c(.97, .995)) {
  y <- ts(as.numeric(filter(noise, phi, method = "recursive")), frequency = period)
  native <- optimize_native(setup(y, ar = NA_real_, include_mean = TRUE),
    control = list(max_iterations = 200L, max_evaluations = 4000L))
  assert(native$converged, paste("persistent", period, phi, "native convergence"))
  for (method in c("x13", "tramoseats")) {
    spec <- if (method == "x13") rjd3x13::regarima_spec("rg0") else rjd3tramoseats::tramo_spec("tr0")
    spec <- rjd3toolkit::set_arima(spec, mean = 0, mean.type = "Undefined", p = 1L, d = 0L,
      q = 0L, bp = 0L, bd = 0L, bq = 0L, coef = 0, coef.type = "Initial")
    spec <- rjd3toolkit::set_automodel(spec, enabled = FALSE)
    spec <- rjd3toolkit::set_transform(spec, fun = "None")
    spec <- rjd3toolkit::set_estimate(spec, tol = 1e-12, exact.ml = TRUE)
    direct_fits <- direct_fits + 1L
    fit <- if (method == "x13") rjd3x13::regarima_fast(y, spec) else rjd3tramoseats::tramo_fast(y, spec)
    label <- paste("persistent", period, phi, method)
    close(native$final$parameters$coefficients$ar, -as.numeric(fit$description$arima$phi["value", ]), 1e-4,
      paste(label, "AR coefficient"))
    close(native$final$value, -fit$estimation$likelihood$ll, 2e-8, paste(label, "likelihood"))
    close(native$final$likelihood$regression_coefficients, fit$estimation$b, 1e-5, paste(label, "mean"))
  }
}
assert(identical(before, extended_parent_hashes(base_dir)), "original reference artifacts unchanged")
cat("Cases", length(cases), "plus 4 persistent controls; direct fits", direct_fits, "checks", checks, "failures", length(failures), "\n")
print(maximum, digits = 9)
if (length(failures)) { print(failures); stop("Fixed-order fit qualification failed", call. = FALSE) }
cat("PASS: pinned fitted-model comparisons and independent densities\n")
if (config$enabled) {
  metadata <- list(schema_version = 1L, captured_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source_commit = system2("git", c("-C", shQuote(root), "rev-parse", "HEAD"), stdout = TRUE),
    native_version = as.character(utils::packageVersion("persephone3")),
    native_library_sha256 = reference_sha256(getLoadedDLLs()[["persephone3"]][["path"]]),
    timing_script_sha256 = reference_sha256(file.path(tooling, c("verify-arma-fits.R", "fit-timing.R", "arma-fit-reference.R"))),
    native_load_mode = if (config$source) "development source" else "installed package",
    native_preloaded = native_preloaded, reference_preloaded = reference_preloaded,
    native_load_seconds = native_load$seconds, reference_load_seconds = reference_load$seconds,
    reference_pin_seconds = pin$seconds, R = R.version.string, platform = R.version$platform,
    OS = Sys.info()[c("sysname", "release", "machine")], logical_cores = parallel::detectCores(),
    numerical_libraries = extSoftVersion(), java = pin$value$java,
    reference_packages = lapply(pin$value$packages, function(package) package$Version),
    native_parallel = getFromNamespace(".native_parallel_info", "persephone3")(),
    thread_environment = Sys.getenv(c("PERSEPHONE_NUM_THREADS", "OMP_NUM_THREADS", "OMP_DYNAMIC",
      "OPENBLAS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS", "MKL_NUM_THREADS"), unset = NA_character_),
    control = control, input_scope = "64 main-matrix cases; persistent and future-context controls excluded")
  fit_timing_run(jobs, config, metadata)
  stopifnot(identical(before, extended_parent_hashes(base_dir)))
}
