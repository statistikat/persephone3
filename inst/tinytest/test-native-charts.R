# Exercise both public native routes. Read back the points that the widgets
# would serialize, including their calendar and the automatic outlier markers.
fixed <- perX13(AirPassengers, "restricted", backend = "native",
                speclist = list(seasonal = rep("s3x3", 12L), trend_length = 13L))
expect_identical(plot(fixed)$x$attrs$labels[-1], "Original")

observed <- AirPassengers
observed[70] <- observed[70] * 3
automatic <- perX13(observed, "rsa3", backend = "native")
for (obj in list(fixed, automatic)) {
  obj$run()
  expected <- list(Original = obj$ts, `Seasonally Adjusted` = obj$result$components$sa,
                   Trend = obj$result$components$t)
  dates <- zoo::as.Date(obj$ts)
  dy <- obj$plot()
  expect_identical(as.Date(dy$x$data[[1L]]), dates)
  for (name in names(expected)) {
    index <- match(name, dy$x$attrs$labels)
    expect_identical(as.numeric(dy$x$data[[index]]), as.numeric(expected[[name]]))
  }
  hc <- obj$hchart()$x$hc_opts
  series_names <- vapply(hc$series, "[[", character(1), "name")
  expect_identical(sort(series_names), sort(tolower(names(expected))))
  for (name in names(expected)) {
    points <- hc$series[[match(tolower(name), series_names)]]$data
    expect_equal(vapply(points, "[[", numeric(1), 1L),
                 as.numeric(dates) * 86400000, tolerance = 0)
    expect_identical(vapply(points, "[[", numeric(1), 2L), as.numeric(expected[[name]]))
  }
  effects <- plotSeasIrrCal(obj, plotEngine = "dygraph", annualComparison = 1L)
  expect_identical(effects$x$attrs$labels[-1L], "Irregular Component")
  expect_identical(as.Date(effects$x$data[[1L]]), dates)
  expect_identical(as.numeric(effects$x$data[[2L]]), as.numeric(obj$result$components$i))
  hc_effects <- obj$plotSeasIrrCal()$x$hc_opts$series
  expect_identical(vapply(hc_effects, "[[", character(1), "name"), "Irregular Component")
  expect_identical(vapply(hc_effects[[1L]]$data, "[[", numeric(1), 2L),
                   as.numeric(obj$result$components$i))
  for (type in c("original", "sa", "irregular")) {
    signal <- switch(type, original = diff(obj$ts), sa = diff(obj$result$components$sa),
                     irregular = obj$result$components$i)
    reference <- stats::spec.pgram(signal, plot = FALSE)
    static <- plotSpectrum(obj, "ggplot", type, "periodogram")$data
    interactive <- plotSpectrum(obj, "hc", type, "periodogram")$x$hc_opts$series[[1L]]$data
    expect_identical(static$spec, as.numeric(reference$spec))
    expect_identical(static$freq, as.numeric(reference$freq) / frequency(signal))
    expect_identical(vapply(interactive, "[[", numeric(1), "y"), static$spec)
    expect_identical(vapply(interactive, "[[", numeric(1), "x"), static$freq)
  }
  expect_error(plotSpectrum(obj, tsType = "residuals"), "does not supply")
  dashboard <- as.character(dashboard_single(obj))
  expect_true(grepl("Decomposition effects", dashboard, fixed = TRUE))
  expect_true(grepl("Residual diagnostics are unavailable", dashboard, fixed = TRUE))
  expect_false(grepl("Plot error:", dashboard, fixed = TRUE))
  if (identical(obj, automatic)) {
    outliers <- obj$getOutliers()
    expect_true(nrow(outliers) > 0L)
    expect_true(all(outliers$type %in% dy$x$attrs$labels))
    lines <- hc$xAxis$plotLines
    expect_equal(vapply(lines, "[[", numeric(1), "value"),
                 as.numeric(outliers$date) * 86400000, tolerance = 0)
  }
}
