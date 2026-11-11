# Test plot changes: smoothed lines, optional range selector, plot engines,
# spectra in panels, residual overview
# NOTE: plotly widgets keep their traces in x$data only after
# plotly::plotly_build(); inspect the built widget, not the returned one.

data(AirPassengers, package = "datasets")

# plotly keeps the layout of a subplot figure partly in x$layout and partly
# in x$layoutAttrs; traces may be missing a name
lay_of <- function(p) Reduce(function(a, b) modifyList(a, b),
                             c(list(p$x$layout), p$x$layoutAttrs))
tr_names <- function(d) vapply(d, function(s) s$name %||% NA_character_,
                               character(1))

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

# the dates of the finer time grid are strictly increasing: repeated dates
# (as produced by rounding the period index) would make dygraphs draw
# vertical lines at the period boundaries when zooming in
gd <- as.Date(substr(g$x$data[[1]], 1, 10))
expect_true(all(diff(gd) > 0))

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

# the smoothed series stay close to their observations: the default monotone
# interpolation never exceeds the two observations it interpolates between
y <- as.numeric(obj$output$user_defined$y)
g5 <- plot(obj, forecasts = FALSE)
vals3 <- vapply(g5$x$data[[2]],
                function(z) if (length(z)) as.numeric(z[1]) else NA_real_,
                NA_real_)
seg_lo <- (seq_along(vals3) - 1) %/% 8 + 1L
seg_hi <- pmin(seg_lo + 1L, n)
tol <- 1e-8 * diff(range(y))
expect_true(all(vals3 <= pmax(y[seg_lo], y[seg_hi]) + tol, na.rm = TRUE))
expect_true(all(vals3 >= pmin(y[seg_lo], y[seg_hi]) - tol, na.rm = TRUE))

# smooth = "spline" uses the not-a-knot cubic spline, which may overshoot
g6 <- plot(obj, forecasts = FALSE, smooth = "spline")
vals4 <- vapply(g6$x$data[[2]],
                function(z) if (length(z)) as.numeric(z[1]) else NA_real_,
                NA_real_)
expect_equal(length(vals4), length(vals3))
expect_true(max(vals4 - pmax(y[seg_lo], y[seg_hi]), na.rm = TRUE) > 0)

# invalid smooth value
expect_error(plot(obj, smooth = "cubic"))

message("Test plot - plotly engine")

# the plotly traces are created lazily: build the widget before inspecting
p1 <- plotly::plotly_build(plot(obj, plotEngine = "plotly"))
expect_true(inherits(p1, "plotly"))
tr <- p1$x$data
nms <- tr_names(tr)
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
p3 <- plotly::plotly_build(plot(obj, plotEngine = "plotly", smooth = FALSE,
                                drawPoints = TRUE))
modes <- vapply(p3$x$data, function(s) identical(s$mode, "lines+markers"),
                TRUE)
expect_true(any(modes))

message("Test plotSiRatios")

# plotly engine (default): one legend entry per series, the SI-ratios as
# small dots, the seasonal factors and their mean as lines
sp <- plotly::plotly_build(plotSiRatios(obj))
expect_true(inherits(sp, "plotly"))
nms <- vapply(sp$x$data, function(s) s$name, "")
expect_true(all(c("SI-Ratio", "Replaced SI-Ratio", "Seasonal Factor",
                  "SF Mean") %in% nms))
legendEntries <- unique(vapply(sp$x$data, function(s) {
  # showlegend NULL means the plotly default (shown) for marker traces
  if (!identical(s$showlegend, FALSE)) s$name else NA_character_
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

message("Test plot - spectra in panels")

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

# several series types: one spectrum per panel (default), plotly engine
gs3_raw <- plotSpectrum(x, tsType = c("original", "sa", "irregular"))
gs3 <- plotly::plotly_build(gs3_raw)
expect_true(inherits(gs3, "plotly"))
expect_equal(length(gs3$x$data), 3)
expect_equal(
  tr_names(gs3$x$data),
  c("original", "sa", "irregular")
)
# three panels with their own axes, panel titles as annotations (set on the
# unbuilt widget by subplotTitles())
lay3 <- lay_of(gs3_raw)
expect_equal(length(grep("^xaxis(\\d*)$", names(lay3))), 3)
expect_equal(
  unname(sort(vapply(lay3$annotations, function(a) a$text, ""))),
  c("irregular", "original", "sa")
)
# the shapes of the seasonal/trading day lines are renumbered to their panel
expect_true(all(vapply(lay3$shapes, function(s) s$xref, "") %in%
                  c("x", "x2", "x3")))

# overlaid spectra, plotly engine (layout = "overlay")
gso <- plotly::plotly_build(
  plotSpectrum(x, tsType = c("sa", "irregular", "residuals"),
               layout = "overlay")
)
expect_equal(length(gso$x$data), 3)
expect_equal(
  tr_names(gso$x$data),
  c("sa", "irregular", "residuals")
)

# panels, ggplot engine (one facet per series type)
gs4 <- plotSpectrum(x, plotEngine = "ggplot",
                    tsType = c("sa", "irregular", "residuals"))
ds4 <- ggplot2::ggplot_build(gs4)$data[[1]]
expect_equal(length(unique(ds4$PANEL)), 3)
expect_equal(length(unique(ds4$group)), 3)

# overlaid spectra, ggplot engine (one coloured line per series type)
gsg <- plotSpectrum(x, plotEngine = "ggplot",
                    tsType = c("sa", "irregular", "residuals"),
                    layout = "overlay")
dsg <- ggplot2::ggplot_build(gsg)$data[[1]]
expect_equal(length(unique(dsg$PANEL)), 1)
expect_equal(length(unique(dsg$group)), 3)
expect_equal(length(unique(dsg$colour)), 3)

# 6 panels: the plotTypes are the rows, the series types the columns
g6_raw <- plotSpectrum(x, tsType = c("original", "sa", "irregular"),
                       plotType = c("arSpec", "periodogram"))
g6 <- plotly::plotly_build(g6_raw)
expect_equal(length(g6$x$data), 6)
lay6 <- lay_of(g6_raw)
expect_equal(length(grep("^xaxis(\\d*)$", names(lay6))), 6)
expect_equal(
  unname(vapply(lay6$annotations, function(a) a$text, "")),
  c("original \u2014 AR spectrum", "sa \u2014 AR spectrum",
    "irregular \u2014 AR spectrum", "original \u2014 Periodogram",
    "sa \u2014 Periodogram", "irregular \u2014 Periodogram")
)
# two rows of panels (the y axes start at two different heights)
ys6 <- grep("^yaxis", names(lay6), value = TRUE)
expect_equal(
  length(unique(round(
    unlist(lapply(ys6, function(a) lay6[[a]]$domain[1])), 2
  ))),
  2
)
# the shapes are renumbered to the axes of their own panel
expect_true(all(vapply(lay6$shapes, function(s) s$xref, "") %in%
                  paste0("x", c("", 2:6))))

# 6 facets, ggplot engine
g6g <- plotSpectrum(x, plotEngine = "ggplot",
                    tsType = c("original", "sa", "irregular"),
                    plotType = c("arSpec", "periodogram"))
d6g <- ggplot2::ggplot_build(g6g)$data[[1]]
expect_equal(length(unique(d6g$PANEL)), 6)

# overlay is only available for a single plotType
expect_error(plotSpectrum(x, tsType = c("sa", "irregular"),
                          plotType = c("arSpec", "periodogram"),
                          layout = "overlay"))

# numeric codes work as well
gs5 <- plotly::plotly_build(plotSpectrum(x, tsType = c(2, 3)))
expect_equal(length(gs5$x$data), 2)
gs5n <- plotly::plotly_build(
  plotSpectrum(x, tsType = c(2, 3), plotType = c(1, 3))
)
expect_equal(length(gs5n$x$data), 4)

# the bar plot type is only available for a single series type
expect_error(plotSpectrum(x, tsType = c("sa", "irregular"),
                          plotType = "arSpecBars"))
expect_error(plotSpectrum(x, plotEngine = "ggplot", tsType = c("sa", "irregular"),
                          plotType = "arSpecBars"))
expect_error(plotSpectrum(x, plotType = c("arSpec", "arSpecBars")))

# invalid tsType / plotType
expect_error(plotSpectrum(x, tsType = "nonsense"))
expect_error(plotSpectrum(x, plotType = "nonsense"))

message("Test plotResiduals - overview")

# residuals over time with acf, pacf and the histogram of the standardized
# residuals below, in one figure (plotly): 4 panels with panel titles
ov_raw <- plotResiduals(obj, which = "overview")
ov <- plotly::plotly_build(ov_raw)
expect_true(inherits(ov, "plotly"))
ovl <- lay_of(ov_raw)
expect_equal(length(grep("^xaxis(\\d*)$", names(ovl))), 4)
# top panel over the full width, the three lower panels side by side
expect_equal(round(ovl$xaxis$domain, 2), c(0, 1))
expect_equal(
  unname(vapply(ovl$annotations, function(a) a$text, "")),
  c("Residuals", "ACF", "PACF", "Density")
)
# the histogram bars have the colour of the series lines
hbar <- Filter(function(t) identical(t$type, "bar"), ov$x$data)
expect_true(length(hbar) >= 3)
expect_true(any(vapply(hbar, function(t)
  identical(t$marker$color, "rgba(124,181,236,1)"), TRUE)))
# numeric codes
ov2 <- plotResiduals(obj, which = 7)
expect_true(inherits(ov2, "plotly"))
# the histogram is still available as a single panel
sh1 <- plotResiduals(obj, which = "sreshist")
expect_true(inherits(sh1, "plotly"))
