# Test plot changes: smoothed lines, optional range selector, plot engines,
# overlaid spectra

data(AirPassengers, package = "datasets")

message("Test plot - highcharter engine (default)")

obj <- perX13(AirPassengers, "rsa1")

# pre-run and post-run plots work, the series are drawn with Highcharts
# splines
g_hc0 <- plot(obj)
expect_true(inherits(g_hc0, "highchart"))
expect_true(all(vapply(g_hc0$x$hc_opts$series, function(s) s$type, "") ==
  "spline"))

obj$run()

n <- length(obj$output$user_defined$y)

g_hc <- plot(obj)
expect_true(inherits(g_hc, "highchart"))
nm <- vapply(g_hc$x$hc_opts$series, function(s) s$name, "")
expect_true(all(c("Original", "Trend", "Seasonally Adjusted") %in% nm))
# the range selector at the bottom is off by default
expect_false(isTRUE(g_hc$x$hc_opts$rangeSelector$enabled))
# smooth = FALSE draws plain lines
g_hc_lin <- plot(obj, smooth = FALSE)
expect_true(all(vapply(g_hc_lin$x$hc_opts$series, function(s) s$type, "") ==
  "line"))
# rangeSelector = TRUE switches it on
g_hc_rs <- plot(obj, rangeSelector = TRUE)
expect_true(isTRUE(g_hc_rs$x$hc_opts$rangeSelector$enabled))

message("Test plot - dygraph engine")

# pre-run plot is smoothed by default (finer time grid than the observations)
g0 <- plot(obj, plotEngine = "dygraph")
expect_true(length(g0$x$data[[1]]) > length(AirPassengers))

# the range selector is off by default and can be switched on
g_no <- plot(obj, plotEngine = "dygraph")
expect_true(!isTRUE(g_no$x$attrs$showRangeSelector))
g_yes <- plot(obj, rangeSelector = TRUE, plotEngine = "dygraph")
expect_true(isTRUE(g_yes$x$attrs$showRangeSelector))

# smoothed plot: 8 segments per observation interval
g <- plot(obj, forecasts = FALSE, plotEngine = "dygraph")
expect_equal(length(g$x$data[[1]]), (n - 1) * 8 + 1)

# the spline passes through the observations
vals <- vapply(g$x$data[[2]], function(z) z[1], 0)
grid <- 1 + 8 * seq_len(n) - 8
expect_equal(as.numeric(vals[grid]),
             as.numeric(obj$output$user_defined$y),
             tolerance = 1e-6)

# the plotted series have distinct colours
cols <- g$x$attrs$colors
expect_equal(length(unique(cols)), length(cols))
expect_true(all(startsWith(cols, "#")))

# smooth = FALSE keeps the original observations
g2 <- plot(obj, smooth = FALSE, forecasts = FALSE, plotEngine = "dygraph")
expect_equal(length(g2$x$data[[1]]), n)
vals2 <- vapply(g2$x$data[[2]], function(z) z[1], 0)
expect_equal(as.numeric(vals2), as.numeric(obj$output$user_defined$y),
             tolerance = 1e-6)

# default plot with forecasts: the forecast columns extend the time index by
# the forecast horizon; the smoothed estimated series must end with the last
# observation and must not be extrapolated over the forecast horizon (the
# extrapolated spline blew up there and determined the y-axis scaling)
g3 <- plot(obj, plotEngine = "dygraph")
nf <- n + length(obj$output$user_defined$y_f)
expect_equal(length(g3$x$data[[1]]), (nf - 1) * 8 + 1)
last <- 8 * n - 7 # grid point of the last observation
for (j in 2:4) {
  vj <- vapply(g3$x$data[[j]],
               function(z) if (length(z)) as.numeric(z[1]) else NA_real_,
               NA_real_)
  expect_false(is.na(vj[last]))
  expect_true(all(is.na(vj[(last + 1):length(vj)])))
}

# plots with outliers and forecasts still work (unsmoothed overlay series);
# the outliers are marked with coloured dots only, without annotations or
# vertical lines; the legend stays visible for the outlier colours
obj$updateParams(addout.type = "AO", addout.date = "1950-01-01")
obj$run()
g4 <- plot(obj, plotEngine = "dygraph")
expect_true(!inherits(try(print(g4), silent = TRUE), "try-error"))
expect_true(length(g4$x$annotations) == 0)
expect_equal(length(g4$x$events), 0)
expect_equal(g4$x$attrs$legend, "always")

# the widget adapts to small containers instead of keeping the fixed 960px
expect_equal(g4$width, "100%")
expect_true(!is.null(g4$x$sizingPolicy))

# the smoothed series stay close to their observations: the interpolated
# values between two observations may exceed them only slightly (a cubic
# spline, like the smoothed lines of Highcharts, rounds the peaks a bit)
y <- as.numeric(obj$output$user_defined$y)
g5 <- plot(obj, forecasts = FALSE, plotEngine = "dygraph")
vals3 <- vapply(g5$x$data[[2]],
                function(z) if (length(z)) as.numeric(z[1]) else NA_real_,
                NA_real_)
tol <- 0.05 * diff(range(y))
expect_true(max(vals3, na.rm = TRUE) <= max(y) + tol)
expect_true(min(vals3, na.rm = TRUE) >= min(y) - tol)
seg_lo <- (seq_along(vals3) - 1) %/% 8 + 1L
seg_hi <- pmin(seg_lo + 1L, n)
ok <- is.na(vals3) |
  (vals3 >= pmin(y[seg_lo], y[seg_hi]) - tol &
   vals3 <= pmax(y[seg_lo], y[seg_hi]) + tol)
expect_true(all(ok))

message("Test plot - outliers in the highcharter engine")

# outliers are scatter series (coloured dots), not vertical plotLines; the
# forecast is a dashed spline with an area band
g6 <- plot(obj, plotEngine = "hc")
types <- vapply(g6$x$hc_opts$series, function(s) s$type, "")
expect_true("scatter" %in% types)
expect_true("areasplinerange" %in% types)
expect_equal(length(g6$x$hc_opts$xAxis$plotLines), 0)

message("Test plotSiRatios - marker size")

# highcharter engine: the SI-ratio dots are small (radius 2), so that the
# seasonal factor lines remain visible
hs <- plotSiRatios(obj, plotEngine = "hc")
mk <- Filter(function(s) is.list(s$marker) && !is.null(s$marker$radius),
             hs$x$hc_opts$series)
expect_true(length(mk) > 0)
expect_true(all(vapply(mk, function(s) s$marker$radius, 0) <= 2))

# ggplot engine: the points are drawn with size 1
pg <- plotSiRatios(obj, plotEngine = "ggplot")
bd <- ggplot2::ggplot_build(pg)
sizes <- unique(unlist(lapply(bd$data, function(d) d$size)))
expect_true(length(sizes) == 0 || max(sizes) <= 1)

message("Test plot - overlaid spectra")

x <- perX13(AirPassengers, "rsa5c")
x$run()

# one spectrum per engine, all plot types
for (pt in c("arSpec", "arSpecBars", "periodogram")) {
  expect_true(!inherits(try(plotSpectrum(x, plotType = pt), silent = TRUE),
                        "try-error"))
  expect_true(!inherits(
    try(plotSpectrum(x, plotEngine = "ggplot", plotType = pt), silent = TRUE),
    "try-error"
  ))
}

# overlaid spectra of the three series types, highcharter engine
g3 <- plotSpectrum(x, tsType = c("sa", "irregular", "residuals"))
expect_equal(length(g3$x$hc_opts$series), 3)
expect_equal(
  vapply(g3$x$hc_opts$series, function(s) s$name, ""),
  c("sa", "irregular", "residuals")
)

# overlaid spectra, ggplot engine (one coloured line per series type)
g4 <- plotSpectrum(x, plotEngine = "ggplot",
                   tsType = c("sa", "irregular", "residuals"))
d4 <- ggplot2::ggplot_build(g4)$data[[1]]
expect_equal(length(unique(d4$group)), 3)
expect_equal(length(unique(d4$colour)), 3)

# numeric codes work as well
g5 <- plotSpectrum(x, tsType = c(2, 3))
expect_equal(length(g5$x$hc_opts$series), 2)

# overlay is not available for the bar plot type
expect_error(plotSpectrum(x, tsType = c("sa", "irregular"),
                          plotType = "arSpecBars"))
expect_error(plotSpectrum(x, plotEngine = "ggplot", tsType = c("sa", "irregular"),
                          plotType = "arSpecBars"))

# invalid tsType
expect_error(plotSpectrum(x, tsType = "nonsense"))
