# Item 26 product boundary: preserve the public ts domains of the core's
# final forecast windows, including their original scale and calendar.
x <- perX13(AirPassengers, "rsa3", backend = "native",
            speclist = list(backcast = 3L, forecast = 6L))
x$run()
fc <- x$forecasts
expect_inherits(fc, "persephone_forecasts")
expect_identical(fc, x$result$forecasts)
raw <- x$output
expected <- list(y = raw$preadjustment$tables$a1a, sa = raw$finals$tables$d11a,
                 t = raw$finals$tables$d12a, s = raw$finals$tables$d16a,
                 cal = raw$finals$tables$d18a)
for (key in names(expected)) {
  expect_identical(fc$points[[key]], expected[[key]])
  expect_identical(as.numeric(start(fc$points[[key]])), c(1961, 1))
  expect_identical(length(fc$points[[key]]), 6L)
}
expect_identical(fc$points$i, NULL)
expect_true(all(fc$availability$standard_errors == "unavailable"))
expect_identical(fc$mode, "Multiplicative")
expect_identical(unserialize(serialize(fc, NULL)), fc)
expect_identical(as.numeric(start(raw$preadjustment$tables$a1b)), c(1948, 10))
expect_identical(tsp(x$result$components$t), tsp(AirPassengers))
old <- x$result
for (bad in list(NA_integer_, -2147483647, 0.9, TRUE, c(1L, 2L), "6", Inf)) {
  expect_error(x$updateParams(forecast = bad), "forecast|index range")
  expect_identical(x$result, old)
  expect_false(x$is_stale)
}
x$updateParams(backcast = 0L, forecast = -1L)
expect_true(x$is_stale)
expect_identical(x$forecasts, fc)
x$run()
expect_identical(length(x$forecasts$points$y), 12L)
expect_false(identical(old$provenance$request_fingerprint, x$result$provenance$request_fingerprint))

# Both chart engines show the points, with no currency or scale conversion.
points <- x$forecasts$points$y
hc <- x$hchart()$x$hc_opts$series
hc_names <- vapply(hc, "[[", character(1), "name")
expect_false("confidence interval" %in% hc_names)
expect_identical(vapply(hc[[match("forecast", hc_names)]]$data, "[[", numeric(1), 2L),
                 as.numeric(points))
dy <- x$plot()
index <- match("Forecasts", dy$x$attrs$labels)
expect_identical(as.numeric(tail(dy$x$data[[index]], length(points))), as.numeric(points))
expect_false("Forecasts" %in% x$plot(forecasts = FALSE)$x$attrs$labels)

# Two fitted components retain their selected forecasts in a hierarchy.
h <- perHts(a = x, b = x, method = "x13", template = "rsa3", iterate = FALSE,
            speclist = list(forecast = -1L), backend = "native")
h$run()
expect_equal(h$forecastsIndirect$points$y, 2 * points, tolerance = 0)
expect_identical(h$forecastsIndirect$standard_errors$y, NULL)
direct <- h$forecastsDirect$points$y
graph <- plot(h, showOutliers = FALSE)
index <- match("Forecasts", graph$x$attrs$labels)
expect_identical(as.numeric(tail(graph$x$data[[index]], length(direct))), as.numeric(direct))
expect_true(all(c("Direct SA", "Indirect SA") %in% graph$x$attrs$labels))

# The independent reference exposes cal_f and y_ef on its own horizon;
# compare matching calendar periods on each delivered point series.
if (requireNamespace("rjd3x13", quietly = TRUE)) {
  ref <- rjd3x13::x13_fast(AirPassengers,
    spec = rjd3x13::set_x11(rjd3x13::x13_spec("rsa3"), fcasts = 6, bcasts = 3),
    userdefined = paste0(names(expected), "_f"))
  for (key in names(expected)) {
    target <- window(ref$user_defined[[paste0(key, "_f")]], start = start(expected[[key]]),
                     end = end(expected[[key]]))
    expect_identical(tsp(fc$points[[key]]), tsp(target))
    # The existing application level-series tolerance for AirPassengers.
    expect_true(max(abs(fc$points[[key]] - target)) <= 1e-8)
  }
}
