# Current native and live products are both fed the original dated observations.
helper <- new.env(parent = environment())
sys.source(system.file("tinytest/helpers/fixtures.R", package = "rkalos", mustWork = TRUE), helper)
directory <- system.file("tinytest/fixtures/native/tramo-seats-bridge", package = "rkalos", mustWork = TRUE)
panel <- function(key) helper$fixture_named_rsa0_cases(file.path(directory, paste0("rsafull", key, "-x86_64.txt")))
cases <- c(panel(""), panel("-late-calendar"))
calendars <- c(panel("-calendar-effect"), panel("-late-calendar-effect"))
errors <- c(panel("-forecast-errors"), panel("-late-forecast-errors"))
diagnostics <- c(panel("-residual-diagnostics"), panel("-late-residual-diagnostics"))
tokens <- function(x) strsplit(x, " ", fixed = TRUE)[[1L]]
row <- function(lines, key) tokens(lines[startsWith(lines, paste0(key, " "))])
domain <- function(x) c(start(x), end(x), frequency(x))
keys <- c(Series = "y", Trend = "t", SeasonallyAdjusted = "sa", Seasonal = "s", Irregular = "i")
reference <- requireNamespace("rjd3tramoseats", quietly = TRUE)
points <- innovations <- 0L
for (id in names(cases)) {
  lines <- cases[[id]]
  input <- row(lines, "input")
  series <- stats::ts(helper$fixture_hex_doubles(input[-seq_len(4L)]), start = as.integer(input[2:3]), frequency = 12)
  obj <- perTramo(series, "rsafull", backend = "native")
  obj$run()
  fit <- obj$result
  expect_identical(fit$status$code, "success", info = id)
  expect_identical(fit$provenance$template, "rsafull", info = id)
  for (line in lines[startsWith(lines, "merged ")]) {
    fields <- tokens(line)
    key <- keys[[fields[2L]]]
    x <- if (fields[3L] == "Value") fit$components[[key]] else fit$forecasts$points[[key]]
    expect_identical(as.integer(start(x)), as.integer(fields[4:5]), info = id)
    expect_identical(writeBin(as.numeric(x), raw()), writeBin(helper$fixture_hex_doubles(fields[-seq_len(6L)]), raw()), info = id)
    points <- points + length(x)
  }
  for (key in c("cal", "cal_f")) {
    fields <- row(calendars[[id]], key)
    x <- if (key == "cal") fit$components$cal else fit$forecasts$points$cal
    expect_identical(as.integer(start(x)), as.integer(fields[2:3]), info = id)
    expect_identical(writeBin(as.numeric(x), raw()), writeBin(helper$fixture_hex_doubles(fields[-seq_len(4L)]), raw()), info = id)
  }
  y_ef <- fit$forecasts$standard_errors$y
  expected <- helper$fixture_hex_doubles(row(errors[[id]], "y_ef")[-seq_len(4L)])
  expect_true(all(abs(as.numeric(y_ef) - expected) <= 2e-8 * (1 + abs(expected))), info = id)
  expect_identical(domain(y_ef), domain(fit$forecasts$points$y), info = id)
  innovations <- innovations + length(fit$diagnostics$residuals$tsres)
  report <- obj$generateQrTable()
  vars <- lapply(lines[startsWith(lines, "variable ")], tokens)
  labels <- vapply(vars, `[[`, character(1L), 2L)
  td <- startsWith(labels, "td")
  expect_equal(report$NbTD, sum(td), info = id)
  expect_identical(report$LeapYear, "lp" %in% labels, info = id)
  expect_identical(report$MovingHoliday, "easter" %in% labels, info = id)
  expect_identical(report$Noutliers, sum(labels %in% c("AO", "LS", "TC")), info = id)
  metrics <- persephone3:::diagnostic_metrics(obj)
  for (name in c("independence", "normality")) {
    key <- if (name == "independence") "lb" else "doornikhansen"
    expected <- helper$fixture_hex_doubles(row(diagnostics[[id]], paste0("residuals.", key))[2:3])
    expect_identical(c(metrics[[name]]$statistic, metrics[[name]]$pvalue), expected, info = id)
  }
  if (id == "fit_tr0_air") {
    expect_error(obj$fixModel(), "automatic_spec_pinned")
    expect_error(obj$fixOutlier(timespan = 0), "automatic_spec_pinned")
  }
  regs <- obj$getRegressors()
  selected <- td | labels %in% c("lp", "easter")
  if (any(selected)) {
    native <- regs[regs$type %in% c("TD", "LP", "EASTER"), ]
    row_labels <- unname(labels[selected])
    if (sum(td) == 6L) row_labels[startsWith(row_labels, "td")] <- c("mon", "tue", "wed", "thu", "fri", "sat")
    expect_identical(native$name, row_labels, info = id)
    expected <- vapply(vars[selected], function(r) tail(r, 1L), character(1L), USE.NAMES = FALSE)
    expect_identical(native$value, helper$fixture_hex_doubles(expected), info = id)
  }
  if (reference) {
    jd <- perTramo(series, "rsafull", backend = "rjd3", userdefined = c("cal_f(24)", "y_ef(24)"))
    jd$run()
    for (key in c(unname(keys), "cal")) {
      for (forecast in c(FALSE, TRUE)) {
        actual <- if (forecast) fit$forecasts$points[[key]] else fit$components[[key]]
        expected <- if (!forecast) jd$result$components[[key]] else if (key == "cal")
          jd$output$user_defined[["cal_f(24)"]] else jd$result$forecasts$points[[key]]
        expect_identical(domain(actual), domain(expected), info = paste(id, key))
        expect_true(all(abs(as.numeric(actual) - as.numeric(expected)) <= 2e-8 * (1 + abs(as.numeric(expected)))), info = paste(id, key))
      }
    }
    expected <- jd$output$user_defined[["y_ef(24)"]]
    expect_identical(domain(y_ef), domain(expected), info = id)
    expect_true(all(abs(as.numeric(y_ef) - as.numeric(expected)) <= 2e-8 * (1 + abs(as.numeric(expected)))), info = id)
    public <- jd$getRegressors()
    if (any(selected)) {
      public <- public[public$type %in% c("TD", "LP", "EASTER"), ]
      expect_identical(native$name, public$name, info = id)
      expect_identical(native$type, public$type, info = id)
      expect_true(all(abs(native$value - public$value) <= 2e-8 * (1 + abs(public$value))), info = id)
    }
  }
}
expect_identical(c(length(cases), innovations, points), c(29L, 4165L, 26160L))
