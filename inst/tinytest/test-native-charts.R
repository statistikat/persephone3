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
  if (identical(obj, automatic)) {
    outliers <- obj$getOutliers()
    expect_true(nrow(outliers) > 0L)
    expect_true(all(outliers$type %in% dy$x$attrs$labels))
    lines <- hc$xAxis$plotLines
    expect_equal(vapply(lines, "[[", numeric(1), "value"),
                 as.numeric(outliers$date) * 86400000, tolerance = 0)
  }
}
