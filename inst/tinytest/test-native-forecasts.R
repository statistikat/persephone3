# Item 26 product boundary: preserve the public ts domains of the core's
# final forecast windows, including their original scale and calendar.
# Capture domain: bridge (PORTABLE). The live-JVM block below compares the
# qualified native rsa3 products against live rjd3 fits; that comparison is
# qualified on a JVM with HotSpot _dexp/_dlog intrinsics disabled (the capture
# profile of kalos/docs/contracts/tramo-seats-bridge-contract.md; the ledger
# tmp_opencode/m6-c-verify/rebaseline-ledger-2026-11-24.md, scratch, shows the
# failures vanish under exactly those flags). On x86-64 it runs in the bridge
# pass (see R/helpers.R); the guard defers it with a documented message or
# refuses a mismatched single pass. On aarch64 the domains coincide.
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
# M13 P5 (rkalos 0.1.0.9033, core 69f83be): the automatic lane publishes the
# original-series forecast standard errors and the wrapper consumes the
# envelope pair by name (R/forecasts.R native_automatic_forecasts). Presence
# pins only -- the engine-vs-JVM claim is the core's replay (144 predictions
# at <= 1e-12 relative, worst 1.86e-14) plus the live bridge-domain parity
# table (worst 3.13e-13 abs, tmp_opencode/m13-p3consumer/parity-yef-table.csv);
# no SE numerics are hand-pinned here.
expect_identical(raw$series_forecast_stdev_status, "available")
expect_identical(fc$standard_errors$y, raw$series_forecast_stdev)
expect_identical(length(fc$standard_errors$y), 6L)
expect_identical(as.numeric(start(fc$standard_errors$y)), c(1961, 1))
expect_identical(fc$availability$standard_errors[fc$availability$component == "y"],
                 "available")
expect_identical(fc$availability$uncertainty_reason[fc$availability$component == "y"], "")
# The non-y components keep their honest unavailable arms unchanged.
expect_true(all(fc$availability$standard_errors[fc$availability$component != "y"] == "unavailable"))
expect_identical(fc$mode, "Multiplicative")
expect_identical(unserialize(serialize(fc, NULL)), fc)
expect_identical(as.numeric(start(raw$preadjustment$tables$a1b)), c(1948, 10))
expect_identical(tsp(x$result$components$t), tsp(AirPassengers))
old <- x$result
expect_identical(tsp(old$components$cal), tsp(old$components$y))
expect_identical(length(old$components$cal), length(AirPassengers))
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
# M13 P5 (2026-11-28): the SE-carrying automatic lane draws the band, exactly
# like the estimated route -- the band must equal points +/- 1.96 SE (the
# verify-lifecycle identity), and the dygraph range-series layout serializes
# every point as the (low, value, high) triple whose centre is the point.
points <- x$forecasts$points$y
hc <- x$hchart()$x$hc_opts$series
hc_names <- vapply(hc, "[[", character(1), "name")
expect_true("confidence interval" %in% hc_names)
band <- hc[[match("confidence interval", hc_names)]]
expect_identical(length(band$data), length(points))
expect_identical(vapply(band$data, "[[", numeric(1), "low"),
                 as.numeric(points - 1.96 * x$forecasts$standard_errors$y))
expect_identical(vapply(band$data, "[[", numeric(1), "high"),
                 as.numeric(points + 1.96 * x$forecasts$standard_errors$y))
expect_identical(vapply(hc[[match("forecast", hc_names)]]$data, "[[", numeric(1), 2L),
                 as.numeric(points))
dy <- x$plot()
index <- match("Forecasts", dy$x$attrs$labels)
expect_identical(vapply(tail(dy$x$data[[index]], length(points)),
                        function(v) if (!length(v)) NA_real_ else v[[2L]], numeric(1)),
                 as.numeric(points))
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
expect_identical(vapply(tail(graph$x$data[[index]], length(direct)),
                        function(v) if (!length(v)) NA_real_ else v[[2L]], numeric(1)),
                 as.numeric(direct))
expect_true(all(c("Direct SA", "Indirect SA") %in% graph$x$attrs$labels))

# The independent reference exposes cal_f and y_ef on its own horizon;
# compare matching calendar periods on each delivered point series.
if (requireNamespace("rjd3x13", quietly = TRUE) &&
    persephone3:::oracle_domain_allows("bridge", "test-native-forecasts.R")) {
  ref <- rjd3x13::x13_fast(AirPassengers,
    spec = rjd3x13::set_x11(rjd3x13::x13_spec("rsa3"), fcasts = 6, bcasts = 3),
    userdefined = c("y", "s", "cal", paste0(names(expected), "_f")))
  for (key in c("y", "s", "cal")) {
    observed <- old$components[[key]]
    reference <- ref$user_defined[[key]]
    expect_identical(c(start(observed), end(observed), frequency(observed)),
                     c(start(reference), end(reference), frequency(reference)))
    expect_true(max(abs(as.numeric(observed) - as.numeric(reference))) <= 1e-8)
  }
  for (key in names(expected)) {
    target <- window(ref$user_defined[[paste0(key, "_f")]], start = start(expected[[key]]),
                     end = end(expected[[key]]))
    expect_identical(tsp(fc$points[[key]]), tsp(target))
    # The existing application level-series tolerance for AirPassengers.
    expect_true(max(abs(fc$points[[key]] - target)) <= 1e-8)
  }
}
