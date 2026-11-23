helper <- new.env(parent = baseenv())
sys.source(system.file("tinytest/helpers/fixtures.R", package = "rkalos", mustWork = TRUE), helper)
path <- system.file("tinytest/fixtures/native/tramo-seats-bridge/rsa3-x86_64.txt", package = "rkalos", mustWork = TRUE)
files <- c("rsa3-x86_64.txt", "rsa3-additional-x86_64.txt", "rsa3-mean-retry-x86_64.txt")
cases <- do.call(c, lapply(files, function(file) helper$fixture_named_rsa0_cases(file.path(dirname(path), file))))
tokens <- function(line) strsplit(line, " ", fixed = TRUE)[[1L]]
domain <- function(x) c(start(x), end(x), frequency(x))
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
  expect_null(fit$forecasts$standard_errors$y, info = id)
  expect_identical(obj$output$series_forecast_stdev_status, "not_qualified", info = id)
  expect_identical(names(fit$diagnostics$residuals), "tsres", info = id)
  expect_identical(obj$diagnosticsTable()$independence, "Undefined", info = id)
  expect_identical(obj$diagnosticsTable()$normality, "Undefined", info = id)
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
    jd <- perTramo(series, "rsa3", backend = "rjd3", userdefined = "cal_f(24)")
    jd$run()
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
    expect_identical(length(obj$output$merged$values[[which(obj$output$merged$component == "Trend" & obj$output$merged$information == "Backcast")]]), 3L)
  }
}
expect_identical(c(length(cases), residuals, points), c(13L, 1196L, 8340L))
expect_error(perTramo(UKgas, "rsa3", backend = "native"), "monthly_only")
i <- seq_len(36L)
retry <- stats::ts(round(2000 + i^2 / 100 + 70 * sin(2 * pi * i / 12) + 10 * cos(i)), frequency = 12)
obj <- perTramo(retry, "rsa3", backend = "native")
expect_error(obj$run(), "requires another identification round")
expect_null(obj$result)
