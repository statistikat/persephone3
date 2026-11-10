# Test plot changes: smoothed lines, optional range selector, plot engines,
# overlaid spectra

data(AirPassengers, package = "datasets")

message("Test plot - dygraph engine (default)")

obj <- perX13(AirPassengers, "rsa1")

# pre-run plot is smoothed by default (finer time grid than the observations)
g0 <- plot(obj)
expect_true(inherits(g0, "dygraphs"))
expect_true(length(g0$x$data[[1]]) > length(AirPassengers))

# the range selector is off by default and can be switched on
g_no <- plot(obj)
expect_true(!isTRUE(g_no$x$attrs$showRangeSelector))
g_yes <- plot(obj, rangeSelector = TRUE)
expect_true(isTRUE(g_yes$x$attrs$showRangeSelector))

obj$run()

n <- length(obj$output$user_defined$y)

# smoothed plot: 8 segments per observation interval
g <- plot(obj, forecasts = FALSE)
expect_equal(length(g$x$data[[1]]), (n - 1) * 8 + 1)

# the spline passes through the observations
vals <- vapply(g$x$data[[2]], function(z) z[1], 0)
grid <- 1 + 8 * seq_len(n) - 8
expect_equal(as.numeric(vals[grid]),
             as.numeric(obj$output$user_defined$y),
             tolerance = 1e-6)

# the plotted series have the Highcharts style colours
cols <- g$x$attrs$colors
expect_equal(length(unique(cols)), length(cols))
expect_true(all(startsWith(cols, "#") | startsWith(cols, "rgb")))
expect_true("#7cb5ec" %in% cols) # Original

# smooth = FALSE keeps the original observations
g2 <- plot(obj, smooth = FALSE, forecasts = FALSE)
expect_equal(length(g2$x$data[[1]]), n)
vals2 <- vapply(g2$x$data[[2]], function(z) z[1], 0)
expect_equal(as.numeric(vals2), as.numeric(obj$output$user_defined$y),
             tolerance = 1e-6)

# default plot with forecasts: the forecast columns extend the time index by
# the forecast horizon; the smoothed estimated series must end with the last
# observation and must not be extrapolated over the forecast horizon (the
# extrapolated spline blew up there and determined the y-axis scaling)
g3 <- plot(obj)
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

# the confidence band of the forecasts: two additional series with the
# 95% limits, drawn without a line; the band is filled by the plotter of the
# upper limit series
expect_true("lowerci" %in% names(g3$x$attrs$series))
expect_true("Confidence Band" %in% names(g3$x$attrs$series))
expect_equal(g3$x$attrs$series[["lowerci"]]$strokeWidth, 0)
expect_true(!is.null(g3$x$attrs$series[["Confidence Band"]]$plotter))

# plots with outliers and forecasts still work (unsmoothed overlay series);
# the outliers are marked with coloured dots only, without annotations or
# vertical lines; the legend stays visible for the outlier colours
obj$updateParams(addout.type = "AO", addout.date = "1950-01-01")
obj$run()
g4 <- plot(obj)
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
g5 <- plot(obj, forecasts = FALSE)
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

message("Test plot - plotly engine")

p1 <- plot(obj, plotEngine = "plotly")
expect_true(inherits(p1, "plotly"))
tr <- p1$x$data
nms <- vapply(tr, function(s) s$name, "")
expect_true(all(c("Original", "Trend", "Seasonally Adjusted") %in% nms))
expect_true("Forecasts" %in% nms)
# the outliers are marker traces with the outlier colours
expect_true(all(c("AO") %in% nms))
mk <- Filter(function(s) identical(s$mode, "markers"), tr)
expect_true(length(mk) >= 1)
# the confidence band: the lower limit is filled to the upper limit
fills <- Filter(function(s) identical(s$fill, "tonexty"), tr)
expect_equal(length(fills), 1)
# Highcharts colours
lineTr <- Filter(function(s) !is.null(s$line$color) &&
                   identical(s$mode, "lines"), tr)
expect_true(any(vapply(lineTr, function(s) s$line$color == "#7cb5ec", TRUE)))
# pre-run plot works
p2 <- plot(perX13(AirPassengers, "rsa1"), plotEngine = "plotly")
expect_true(inherits(p2, "plotly"))
# drawPoints adds markers to the line traces
p3 <- plot(obj, plotEngine = "plotly", smooth = FALSE, drawPoints = TRUE)
modes <- vapply(p3$x$data, function(s) s$mode, "")
expect_true(any(modes == "lines+markers"))

message("Test plotSiRatios")

# plotly engine (default): one legend entry per series, the SI-ratios as
# small dots, the seasonal factors and their mean as lines
sp <- plotly::plotly_build(plotSiRatios(obj))
expect_true(inherits(sp, "plotly"))
nms <- vapply(sp$x$data, function(s) s$name, "")
expect_true(all(c("SI-Ratio", "Replaced SI-Ratio", "Seasonal Factor",
                  "SF Mean") %in% nms))
legendEntries <- unique(vapply(sp$x$data, function(s) {
  if (isTRUE(s$showlegend)) s$name else NA_character_
}, ""))
expect_true(setequal(stats::na.omit(legendEntries),
                     c("SI-Ratio", "Replaced SI-Ratio", "Seasonal Factor",
                       "SF Mean")))
mk <- Filter(function(s) identical(s$mode, "markers"), sp$x$data)
expect_true(length(mk) >= 1)
expect_true(all(vapply(mk, function(s) s$marker$size <= 3, TRUE)))
ln <- Filter(function(s) identical(s$mode, "lines"), sp$x$data)
expect_true(length(ln) > 0)

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

# overlaid spectra of the three series types, plotly engine (default)
gs3 <- plotSpectrum(x, tsType = c("sa", "irregular", "residuals"))
expect_true(inherits(gs3, "plotly"))
expect_equal(length(gs3$x$data), 3)
expect_equal(
  vapply(gs3$x$data, function(s) s$name, ""),
  c("sa", "irregular", "residuals")
)

# overlaid spectra, ggplot engine (one coloured line per series type)
gs4 <- plotSpectrum(x, plotEngine = "ggplot",
                    tsType = c("sa", "irregular", "residuals"))
ds4 <- ggplot2::ggplot_build(gs4)$data[[1]]
expect_equal(length(unique(ds4$group)), 3)
expect_equal(length(unique(ds4$colour)), 3)

# numeric codes work as well
gs5 <- plotSpectrum(x, tsType = c(2, 3))
expect_equal(length(gs5$x$data), 2)

# overlay is not available for the bar plot type
expect_error(plotSpectrum(x, tsType = c("sa", "irregular"),
                          plotType = "arSpecBars"))
expect_error(plotSpectrum(x, plotEngine = "ggplot", tsType = c("sa", "irregular"),
                          plotType = "arSpecBars"))

# invalid tsType
expect_error(plotSpectrum(x, tsType = "nonsense"))
