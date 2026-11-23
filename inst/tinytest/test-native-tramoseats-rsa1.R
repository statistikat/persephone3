helper <- new.env(parent = baseenv())
sys.source(system.file("tinytest/helpers/fixtures.R", package = "rkalos", mustWork = TRUE), helper)
path <- system.file("tinytest/fixtures/native/tramo-seats-bridge/rsa1-x86_64.txt", package = "rkalos", mustWork = TRUE)
cases <- helper$fixture_named_rsa0_cases(path)
additional_cases <- function(file, prefix) {
  x <- helper$fixture_named_rsa0_cases(file.path(dirname(path), file))
  stats::setNames(x, paste0(prefix, names(x)))
}
cases <- c(cases, additional_cases("rsa1-level-effects-x86_64.txt", "level_"),
           additional_cases("rsa1-level-tc-x86_64.txt", "level_tc_"),
           additional_cases("rsa1-log-tc-x86_64.txt", "log_tc_"))
keys <- c(Series = "y", Trend = "t", SeasonallyAdjusted = "sa", Seasonal = "s", Irregular = "i")
tokens <- function(line) strsplit(line, " ", fixed = TRUE)[[1L]]
domain <- function(x) c(start(x), end(x), frequency(x))
points <- rows <- 0L
for (id in names(cases)) {
  lines <- cases[[id]]
  input <- tokens(lines[2L])
  x <- stats::ts(helper$fixture_hex_doubles(input[-seq_len(4L)]), start = as.integer(input[2:3]), frequency = 12)
  obj <- perTramo(x, "rsa1", backend = "native")
  obj$run()
  fitted <- obj$result
  expect_identical(fitted$provenance$template, "rsa1", info = id)
  expect_identical(fitted$status$code, "success", info = id)
  log <- tokens(lines[startsWith(lines, "model ")])[3L] == "true"
  expect_identical(fitted$model$log, log, info = id)
  expect_identical(fitted$forecasts$mode, if (log) "Multiplicative" else "Additive", info = id)
  expect_true(all(fitted$components$cal == as.numeric(log)), info = id)
  expect_true(all(fitted$forecasts$points$cal == as.numeric(log)), info = id)
  expect_null(fitted$forecasts$standard_errors$y, info = id)
  expect_identical(obj$output$series_forecast_stdev_status, "not_qualified", info = id)
  expected_outliers <- lines[startsWith(lines, "variable ") & !startsWith(lines, "variable const ")]
  outliers <- obj$getOutliers()
  expect_identical(nrow(outliers), length(expected_outliers), info = id)
  expect_identical(obj$generateQrTable()$Noutliers, length(expected_outliers), info = id)
  if (length(expected_outliers)) {
    expect_true(all(outliers$autoout & outliers$coef.type == "ESTIMATED"), info = id)
    for (j in seq_along(expected_outliers)) {
      var <- tokens(expected_outliers[j])
      expect_identical(outliers$name[j], paste(var[2:3], collapse = " "), info = id)
      expect_identical(as.character(outliers$date[j]), gsub("[()]", "", var[3L]), info = id)
      expect_identical(outliers$coef.value[j], helper$fixture_hex_doubles(var[4L]), info = id)
    }
  }
  expected_residual <- tokens(lines[startsWith(lines, "residuals ")])
  tsres <- fitted$diagnostics$residuals$tsres
  expect_identical(as.integer(start(tsres)), as.integer(expected_residual[2:3]), info = id)
  expect_identical(as.numeric(tsres), helper$fixture_hex_doubles(expected_residual[-seq_len(4L)]), info = id)
  for (line in lines[startsWith(lines, "merged ")]) {
    row <- tokens(line)
    key <- unname(keys[row[2L]])
    value <- if (row[3L] == "Value") fitted$components[[key]] else fitted$forecasts$points[[key]]
    expect_identical(as.integer(start(value)), as.integer(row[4:5]), info = id)
    expect_identical(frequency(value), 12, info = id)
    expect_identical(writeBin(as.numeric(value), raw()), writeBin(helper$fixture_hex_doubles(row[-seq_len(6L)]), raw()), info = id)
    rows <- rows + 1L
    points <- points + length(value)
  }
  if (id == "fit_multi") {
    expect_error(obj$fixOutlier(timespan = 0), "automatic_spec_pinned")
    expect_identical(obj$fixModel(), obj$params)
  }
  if (requireNamespace("rjd3tramoseats", quietly = TRUE)) {
    jd <- perTramo(x, "rsa1", backend = "rjd3", userdefined = "cal_f(24)")
    jd$run()
    for (key in c("y", "t", "sa", "s", "i", "cal")) {
      for (part in c("observed", "forecast")) {
        actual <- if (part == "observed") fitted$components[[key]] else fitted$forecasts$points[[key]]
        reference <- if (part == "observed") jd$result$components[[key]] else if (key == "cal")
          jd$output$user_defined[["cal_f(24)"]] else jd$result$forecasts$points[[key]]
        expect_identical(domain(actual), domain(reference), info = paste(id, key, part))
        expect_true(all(abs(as.numeric(actual) - as.numeric(reference)) <= 2e-8 * (1 + abs(as.numeric(reference)))),
                    info = paste(id, key, part))
      }
    }
    native_si <- persephone3:::si_plot_data(obj)[["SI-Ratio"]]
    reference_si <- persephone3:::si_plot_data(jd)[["SI-Ratio"]]
    expect_true(all(abs(native_si - reference_si) <= 2e-8 * (1 + abs(reference_si))), info = id)
  }
  if (id == "fit_multi") {
    obj$updateParams(speclist = list(forecast = 6L, backcast = 3L))
    expect_true(obj$is_stale)
    obj$run()
    expect_identical(length(obj$forecasts$points$sa), 6L)
    expect_identical(length(obj$forecasts$points$cal), 6L)
    back <- obj$output$merged
    expect_identical(length(back$values[[which(back$component == "Trend" & back$information == "Backcast")]]), 3L)
    expect_true(all(obj$forecasts$points$cal == 1))
  }
}
expect_identical(c(length(cases), rows, points), c(12L, 120L, 7560L))
zero <- perTramo(AirPassengers, "rsa1", backend = "native", speclist = list(forecast = 0L))
zero$run()
expect_null(zero$forecasts)
expect_error(perTramo(UKgas, "rsa1", backend = "native"), "monthly_only")
