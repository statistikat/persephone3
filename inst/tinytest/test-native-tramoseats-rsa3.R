helper <- new.env(parent = baseenv())
sys.source(system.file("tinytest/helpers/fixtures.R", package = "rkalos", mustWork = TRUE), helper)
path <- system.file("tinytest/fixtures/native/tramo-seats-bridge/rsa3-x86_64.txt", package = "rkalos", mustWork = TRUE)
files <- c("rsa3-x86_64.txt", "rsa3-additional-x86_64.txt", "rsa3-mean-retry-x86_64.txt",
           "rsa3-ref-retry-x86_64.txt")
cases <- do.call(c, lapply(files, function(file) helper$fixture_named_rsa0_cases(file.path(dirname(path), file))))
diagnostics <- c(helper$fixture_named_rsa0_cases(file.path(dirname(path), "rsa3-residual-diagnostics-x86_64.txt")),
                 helper$fixture_named_rsa0_cases(file.path(dirname(path), "rsa3-mean-retry-diagnostics-x86_64.txt")),
                 helper$fixture_named_rsa0_cases(file.path(dirname(path), "rsa3-ref-retry-diagnostics-x86_64.txt")))
expect_identical(sort(names(diagnostics)), sort(names(cases)))
wk_cases <- helper$fixture_named_rsa0_cases(file.path(dirname(path), "rsa3-ref-retry-wk-x86_64.txt"))
names(wk_cases) <- sub(" .*", "", names(wk_cases))
tokens <- function(line) strsplit(line, " ", fixed = TRUE)[[1L]]
domain <- function(x) c(start(x), end(x), frequency(x))
forecast_errors <- c(helper$fixture_named_rsa0_cases(file.path(dirname(path), "rsa3-forecast-errors-x86_64.txt")),
                     helper$fixture_named_rsa0_cases(file.path(dirname(path), "rsa3-ref-retry-forecast-errors-x86_64.txt")))
expect_identical(sort(names(forecast_errors)), sort(names(cases)))
keys <- c(Series = "y", Trend = "t", SeasonallyAdjusted = "sa", Seasonal = "s", Irregular = "i")
points <- residuals <- 0L
for (id in names(cases)) {
  lines <- cases[[id]]
  field <- function(key) tokens(lines[startsWith(lines, paste0(key, " "))])
  input <- field("input")
  series <- stats::ts(helper$fixture_hex_doubles(input[-seq_len(4L)]), start = as.integer(input[2:3]), frequency = 12)
  obj <- perTramo(series, "rsa3", backend = "native")
  obj$run()
  fit <- obj$result
  expect_identical(fit$status$code, "success", info = id)
  expect_identical(fit$provenance$template, "rsa3", info = id)
  model <- field("model")
  expect_identical(unname(fit$model$orders), as.integer(model[7:12]), info = id)
  expect_identical(fit$model$log, model[3L] == "true", info = id)
  expect_identical(fit$model$mean_correction, model[5L] == "true", info = id)
  coefficients <- unlist(fit$model$coefficients[c("ar", "sar", "ma", "sma")], use.names = FALSE)
  expect_identical(coefficients, helper$fixture_hex_doubles(field("parameters")[-1L]), info = id)
  expect_identical(fit$forecasts$mode, if (fit$model$log) "Multiplicative" else "Additive", info = id)
  errors <- forecast_errors[[id]]
  expected <- tokens(errors[startsWith(errors, "y_ef ")])
  reference_errors <- helper$fixture_hex_doubles(expected[-seq_len(4L)])
  actual_errors <- fit$forecasts$standard_errors$y
  expect_identical(actual_errors, obj$output$series_forecast_stdev, info = id)
  expect_identical(obj$output$series_forecast_stdev_status, "available", info = id)
  expect_identical(fit$forecasts$availability$standard_errors[1L], "available", info = id)
  expect_identical(domain(actual_errors), domain(fit$forecasts$points$y), info = id)
  expect_true(all(abs(as.numeric(actual_errors) - reference_errors) <= 2e-8 * (1 + abs(reference_errors))), info = id)
  wk <- tokens(errors[startsWith(errors, "wk_series_stdev_forecast ")])
  expect_identical(as.numeric(fit$seats_forecast_standard_errors$y),
                   helper$fixture_hex_doubles(wk[-seq_len(4L)]), info = id)
  if (id %in% names(wk_cases)) {
    wk <- wk_cases[[id]]
    for (name in names(keys)) {
      key <- keys[[name]]
      line <- wk[startsWith(wk, paste("wk", name, "StdevForecast", ""))]
      values <- fit$seats_forecast_standard_errors[[key]]
      if (length(line)) {
        expected <- tokens(line)
        expect_identical(as.integer(start(values)), as.integer(expected[4:5]), info = id)
        expect_identical(as.numeric(values), helper$fixture_hex_doubles(expected[-seq_len(6L)]), info = paste(id, key))
      } else {
        expect_null(values, info = paste(id, key))
        expect_null(fit$forecasts$standard_errors[[key]], info = paste(id, key))
        availability <- fit$forecasts$availability
        expect_identical(availability$standard_errors[availability$component == key], "unavailable", info = id)
        expect_match(availability$uncertainty_reason[availability$component == key], "no stochastic seasonal component")
        expect_error(persephone3:::si_plot_data(obj), "no stochastic seasonal component")
      }
    }
  }
  chart <- obj$hchart()$x$hc_opts$series
  interval <- chart[[match("confidence interval", vapply(chart, "[[", character(1), "name"))]]$data
  expect_identical(vapply(interval, "[[", numeric(1), "low"),
                   as.numeric(fit$forecasts$points$y - 1.96 * actual_errors), info = id)
  expect_identical(vapply(interval, "[[", numeric(1), "high"),
                   as.numeric(fit$forecasts$points$y + 1.96 * actual_errors), info = id)
  reference_diagnostics <- diagnostics[[id]]
  expected_test <- function(key, positions) {
    row <- reference_diagnostics[startsWith(reference_diagnostics, paste0("residuals.", key, " "))]
    helper$fixture_hex_doubles(tokens(row)[positions])
  }
  metrics <- persephone3:::diagnostic_metrics(obj)
  residual_tests <- fit$diagnostics$residuals
  expect_identical(residual_tests$ser, expected_test("ser", 2L), info = id)
  expect_identical(residual_tests$lb$df, 24 - length(fit$model$parameters), info = id)
  table <- obj$diagnosticsTable()
  dashboard <- persephone3:::diagnostic_dashboard_values(obj)
  report <- obj$generateQrTable()
  for (name in c("normality", "independence")) {
    key <- if (name == "normality") "doornikhansen" else "lb"
    metric <- metrics[[name]]
    expect_identical(c(metric$statistic, metric$pvalue), expected_test(key, 2:3), info = paste(id, key))
    expect_identical(metric$source, paste0("residuals/", key), info = id)
    expect_identical(table[[name]], metric$status, info = id)
  }
  expect_identical(c(report$Ljung.Box.p.value, report$Doornik.Hansen.p.value),
                   c(metrics$independence$pvalue, metrics$normality$pvalue), info = id)
  expect_identical(c(dashboard$p_indep, dashboard$p_norm),
                   c(metrics$independence$pvalue, metrics$normality$pvalue), info = id)
  res <- field("residuals")
  tsres <- fit$diagnostics$residuals$tsres
  expect_identical(as.integer(start(tsres)), as.integer(res[2:3]), info = id)
  expect_identical(as.numeric(tsres), helper$fixture_hex_doubles(res[-seq_len(4L)]), info = id)
  residuals <- residuals + length(tsres)
  reference_outliers <- lines[startsWith(lines, "variable ") & !startsWith(lines, "variable const ")]
  outliers <- obj$getOutliers()
  expect_identical(nrow(outliers), length(reference_outliers), info = id)
  expect_identical(obj$generateQrTable()$Noutliers, length(reference_outliers), info = id)
  for (i in seq_along(reference_outliers)) {
    row <- tokens(reference_outliers[i])
    expect_identical(outliers$name[i], paste(row[2:3], collapse = " "), info = id)
    expect_identical(outliers$coef.value[i], helper$fixture_hex_doubles(row[4L]), info = id)
  }
  for (line in lines[startsWith(lines, "merged ")]) {
    row <- tokens(line)
    key <- keys[[row[2L]]]
    actual <- if (row[3L] == "Value") fit$components[[key]] else fit$forecasts$points[[key]]
    expect_identical(as.integer(start(actual)), as.integer(row[4:5]), info = id)
    expect_identical(frequency(actual), 12, info = id)
    expect_identical(writeBin(as.numeric(actual), raw()), writeBin(helper$fixture_hex_doubles(row[-seq_len(6L)]), raw()), info = id)
    points <- points + length(actual)
  }
  if (requireNamespace("rjd3tramoseats", quietly = TRUE)) {
    jd <- perTramo(series, "rsa3", backend = "rjd3", userdefined = c("cal_f(24)", "y_ef(24)"))
    jd$run()
    if (id %in% names(wk_cases) && !any(startsWith(wk_cases[[id]], "wk Seasonal Value ")))
      expect_error(persephone3:::si_plot_data(jd), "no stochastic seasonal component")
    jd_errors <- jd$output$user_defined[["y_ef(24)"]]
    expect_identical(domain(actual_errors), domain(jd_errors), info = id)
    expect_true(all(abs(actual_errors - jd_errors) <= 2e-8 * (1 + abs(jd_errors))), info = id)
    for (name in c("normality", "independence")) {
      key <- if (name == "normality") "doornikhansen" else "lb"
      metric <- metrics[[name]]
      ref <- jd$output$user_defined[[paste0("residuals.", key)]]
      expect_equal(c(metric$statistic, metric$pvalue), c(ref$value, ref$pvalue),
                   tolerance = 2e-8, info = paste(id, key))
      expect_identical(jd$diagnosticsTable()[[name]], table[[name]], info = id)
    }
    expect_identical(jd$diagnosticsTable()$arimaMdl, obj$diagnosticsTable()$arimaMdl, info = id)
    for (key in c("y", "t", "sa", "s", "i", "cal")) {
      for (part in c("observed", "forecast")) {
        actual <- if (part == "observed") fit$components[[key]] else fit$forecasts$points[[key]]
        reference <- if (part == "observed") jd$result$components[[key]] else if (key == "cal")
          jd$output$user_defined[["cal_f(24)"]] else jd$result$forecasts$points[[key]]
        expect_identical(domain(actual), domain(reference), info = paste(id, key, part))
        expect_true(all(abs(as.numeric(actual) - as.numeric(reference)) <= 2e-8 * (1 + abs(as.numeric(reference)))),
                    info = paste(id, key, part))
      }
    }
  }
  if (id == "fit_multi") {
    expect_error(obj$fixModel(), "automatic_spec_pinned")
    expect_error(obj$fixOutlier(timespan = 0), "automatic_spec_pinned")
    obj$updateParams(speclist = list(forecast = 6L, backcast = 3L))
    expect_true(obj$is_stale)
    obj$run()
    expect_identical(length(obj$forecasts$points$sa), 6L)
    expect_identical(length(obj$forecasts$standard_errors$y), 6L)
    expect_identical(domain(obj$forecasts$standard_errors$y), domain(obj$forecasts$points$y))
    expect_true(all(abs(as.numeric(obj$forecasts$standard_errors$y) - head(reference_errors, 6L)) <=
                    2e-8 * (1 + abs(head(reference_errors, 6L)))))
    expect_identical(length(obj$output$merged$values[[which(obj$output$merged$component == "Trend" & obj$output$merged$information == "Backcast")]]), 3L)
  }
}
expect_identical(c(length(cases), residuals, points), c(20L, 2105L, 13980L))
expect_error(perTramo(UKgas, "rsa3", backend = "native"), "monthly_only")
i <- seq_len(36L)
retry <- stats::ts(round(2000 + i^2 / 100 + 70 * sin(2 * pi * i / 12) + 10 * cos(i)), frequency = 12)
obj <- perTramo(retry, "rsa3", backend = "native")
expect_error(obj$run(), "approximation_required")
expect_null(obj$result)
short <- perTramo(stats::ts(as.numeric(AirPassengers)[1:37], frequency = 12), "rsa3", backend = "native")
short$run()
expect_identical(short$diagnosticsTable()$independence, "Undefined")
expect_identical(short$generateQrTable()$Ljung.Box.p.value, NA_real_)
expect_true(is.finite(short$generateQrTable()$Doornik.Hansen.p.value))
