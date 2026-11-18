# Test plot changes: smoothed lines, optional range selector, plot engines,
# spectra, residual overview
# NOTE: plotly widgets keep their traces in x$data only after
# plotly::plotly_build(); inspect the built widget, not the returned one.

data(AirPassengers, package = "datasets")

# plotly keeps the layout of a subplot figure partly in x$layout and partly
# in x$layoutAttrs, and it merges the entries the way it merges nested lists
# (an axis given twice keeps the keys that are not mentioned); traces may be
# missing a name
merge_deep <- function(a, b) {
  for (nm in names(b)) {
    a[[nm]] <- if (is.list(a[[nm]]) && is.list(b[[nm]])) {
      merge_deep(a[[nm]], b[[nm]])
    } else {
      b[[nm]]
    }
  }
  a
}
lay_of <- function(p) Reduce(
  merge_deep,
  c(list(p$x$layout), p$x$layoutAttrs[vapply(p$x$layoutAttrs, is.list, TRUE)])
)
tr_names <- function(d) vapply(d, function(s) s$name %||% NA_character_,
                               character(1))
# the text of an axis title (plotly keeps it as a string or as a list)
ax_title <- function(ax) {
  t <- ax$title
  if (is.null(t)) {
    ""
  } else if (is.list(t)) {
    t$text %||% ""
  } else {
    as.character(t)
  }
}

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
expect_true("#1874CD" %in% cols) # Original (dodgerblue3)
expect_false("#7cb5ec" %in% cols) # the bright blue is gone

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
# the outliers are marked with coloured dots and a discreet dashed vertical
# line in a lighter shade of the same colour, without annotations; the
# legend stays visible for the outlier colours
obj$updateParams(addout.type = "AO", addout.date = "1950-01-01")
obj$run()
g4 <- plot(obj)
expect_true(!inherits(try(print(g4), silent = TRUE), "try-error"))
expect_true(length(g4$x$annotations) == 0)
expect_equal(length(g4$x$events), 1)
expect_equal(g4$x$events[[1]]$color, "rgba(255, 0, 0, 0.35)")
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
# ... and with a discreet dashed vertical line at their date
oShapes <- Filter(function(s) identical(s$line$dash, "dash"),
                  p1$x$layout$shapes %||% list())
expect_equal(length(oShapes), 1)
expect_equal(oShapes[[1]]$line$color, "rgba(255, 0, 0, 0.35)")
# the confidence band: the lower limit is filled to the upper limit
fills <- Filter(function(s) identical(s$fill, "tonexty"), tr)
expect_equal(length(fills), 1)
# Highcharts colours
lineTr <- Filter(function(s) !is.null(s$line$color) &&
                   identical(s$mode, "lines"), tr)
expect_true(any(vapply(lineTr, function(s) s$line$color == "#1874CD", TRUE)))
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

message("Test plotSeasIrrCal - plotly range slider")

# the plotly plot has no range slider below the plot by default (that strip
# hid part of the plot), the dygraph plot keeps its range selector; both are
# switchable
sly <- lay_of(plotSeasIrrCal(obj, plotEngine = "plotly"))
expect_false(isTRUE(sly$xaxis$rangeslider$visible))
sly_on <- lay_of(plotSeasIrrCal(obj, plotEngine = "plotly",
                                rangeSelector = TRUE))
expect_true(isTRUE(sly_on$xaxis$rangeslider$visible))
expect_true(isTRUE(plotSeasIrrCal(obj)$x$attrs$showRangeSelector))
expect_false(isTRUE(plotSeasIrrCal(obj, rangeSelector = FALSE)
                     $x$attrs$showRangeSelector))
# the other plotly time series plots hide the slider by default as before
expect_false(isTRUE(lay_of(plot(obj, plotEngine = "plotly"))
                    $xaxis$rangeslider$visible))

message("Test plot - spectra")

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

# several series types: all spectra overlaid in a single plot (plotly engine)
gs3_raw <- plotSpectrum(x, tsType = c("original", "sa", "irregular"))
gs3 <- plotly::plotly_build(gs3_raw)
expect_true(inherits(gs3, "plotly"))
expect_equal(length(gs3$x$data), 3)
expect_equal(
  tr_names(gs3$x$data),
  c("original", "sa", "irregular")
)
# a single panel: the seasonal/trading day shapes are on the base x axis
lay3 <- lay_of(gs3_raw)
expect_equal(length(grep("^xaxis(\\d*)$", names(lay3))), 1)
expect_true(all(vapply(lay3$shapes, function(s) s$xref, "") == "x"))

# overlaid spectra, ggplot engine (one coloured line per series type)
gsg <- plotSpectrum(x, plotEngine = "ggplot",
                    tsType = c("sa", "irregular", "residuals"))
dsg <- ggplot2::ggplot_build(gsg)$data[[1]]
expect_equal(length(unique(dsg$PANEL)), 1)
expect_equal(length(unique(dsg$group)), 3)
expect_equal(length(unique(dsg$colour)), 3)

# several plot types: a named list with one overlaid plot per plot type
gl <- plotSpectrum(x, tsType = c("original", "sa"),
                   plotType = c("arSpec", "periodogram"))
expect_true(is.list(gl) && !inherits(gl, "plotly"))
expect_equal(names(gl), c("AR spectrum", "Periodogram"))
gla <- plotly::plotly_build(gl[["AR spectrum"]])
expect_equal(tr_names(gla$x$data), c("original", "sa"))
glp <- plotly::plotly_build(gl[["Periodogram"]])
expect_equal(tr_names(glp$x$data), c("original", "sa"))
# a single plot type gives a single plot, not a list
expect_true(inherits(plotSpectrum(x, plotType = "arSpec"), "plotly"))

# several plot types, ggplot engine: a named list of ggplot objects
glg <- plotSpectrum(x, plotEngine = "ggplot",
                    plotType = c("arSpec", "periodogram"))
expect_true(is.list(glg) && length(glg) == 2)
expect_true(inherits(glg[["AR spectrum"]], "ggplot"))
expect_true(inherits(glg[["Periodogram"]], "ggplot"))

# numeric codes work as well
gs5 <- plotly::plotly_build(plotSpectrum(x, tsType = c(2, 3)))
expect_equal(length(gs5$x$data), 2)
gs5n <- plotSpectrum(x, tsType = c(2, 3), plotType = c(1, 3))
expect_true(is.list(gs5n) && length(gs5n) == 2)

# panels: one panel per series type, equally wide, framed, in a single
# figure with the panel titles as annotations
pnl <- plotSpectrum(x, tsType = c("original", "sa"), layout = "panels")
plb <- plotly::plotly_build(pnl)
lay_p <- lay_of(pnl)
expect_true(inherits(pnl, "plotly"))
expect_equal(length(grep("^xaxis(\\d*)$", names(lay_p))), 2)
expect_equal(tr_names(plb$x$data), c("original", "sa"))
# the panels share the width equally (50:50 for two)
wid <- vapply(grep("^xaxis(\\d*)$", names(lay_p), value = TRUE),
              function(a) diff(lay_p[[a]]$domain), 0)
expect_true(max(wid) - min(wid) < 0.01)
# the seasonal/trading day shapes travel with their panel
expect_true(all(c("x", "x2") %in% vapply(lay_p$shapes, function(s) s$xref, "")))
expect_equal(unname(vapply(plb$x$layout$annotations,
                           function(a) a$text, "")),
             c("original", "sa"))
# a clear frame around every panel
for (a in grep("^xaxis(\\d*)$|^yaxis(\\d*)$", names(lay_p), value = TRUE)) {
  expect_true(isTRUE(lay_p[[a]]$showline) && isTRUE(lay_p[[a]]$mirror),
              info = a)
}
# the figure only gets the height that keeps the height:width ratio of a
# single panel (the fitting code multiplies the pane height by the factor
# rows/cols of the panel grid, here 1/2)
expect_true(any(grepl("var factor = 0.5;",
                      vapply(pnl$jsHooks$render, function(h) h$code, ""),
                      fixed = TRUE)))

# three panels do not fit into one row: the grid has two columns and two
# rows, the panel of the third spectrum below the first one and the space
# beside it empty; the figure is square-ish (factor 2/2 = 1), so every
# panel keeps the height:width ratio it would have on its own
p3s <- plotSpectrum(x, tsType = c("original", "sa", "irregular"),
                    layout = "panels")
pnl3 <- plotly::plotly_build(p3s)
lay3 <- pnl3$x$layout
expect_equal(length(grep("^xaxis(\\d*)$", names(lay3))), 3)
# the third panel is below the first one (same x domain, lower y domain),
# and the cell beside it stays empty (no fourth panel)
expect_equal(lay3$xaxis3$domain, lay3$xaxis$domain, tolerance = 1e-6)
expect_true(mean(lay3$yaxis3$domain) < mean(lay3$yaxis$domain) - 0.1)
expect_null(lay3$xaxis4)
# the axis titles are at the edges of the grid: "Frequency" under the
# bottom row, "Spectrum" beside the left column
expect_equal(ax_title(lay3$xaxis3), "Frequency")
expect_equal(ax_title(lay3$yaxis), "Spectrum")
expect_equal(ax_title(lay3$yaxis3), "Spectrum")
# the panel titles sit above their panel, in the top row and in the second
expect_equal(unname(vapply(lay3$annotations, function(a) a$text, "")),
             c("original", "sa", "irregular"))
ann_y <- vapply(lay3$annotations, function(a) a$y, 0)
expect_true(all(ann_y[1:2] > 1) && ann_y[3] < 1)
expect_true(any(grepl("var factor = 1;",
                      vapply(p3s$jsHooks$render, function(h) h$code, ""),
                      fixed = TRUE)))

# panels over plot types and series types: one figure, one panel per
# combination (the named list stays the overplot behaviour)
ppl <- plotSpectrum(x, tsType = c("original", "sa"),
                    plotType = c("arSpec", "periodogram"),
                    layout = "panels")
expect_true(inherits(ppl, "plotly"))
plb4 <- plotly::plotly_build(ppl)
lay_pp <- lay_of(ppl)
expect_equal(length(grep("^xaxis(\\d*)$", names(lay_pp))), 4)
expect_equal(unname(vapply(plb4$x$layout$annotations,
                           function(a) a$text, "")),
             c("AR spectrum / original", "AR spectrum / sa",
               "Periodogram / original", "Periodogram / sa"))
# a single series type and plot type: one panel, frame and all
p1 <- plotly::plotly_build(plotSpectrum(x, layout = "panels"))
expect_equal(length(p1$x$data), 1)

# panels with the ggplot engine: one ggplot with one facet per combination,
# in the interactive variant a plotly figure with the same fitting code
pplg <- plotSpectrum(x, plotEngine = "ggplot", tsType = c("original", "sa"),
                     plotType = c("arSpec", "periodogram"),
                     layout = "panels")
expect_true(inherits(pplg, "ggplot"))
expect_equal(length(unique(ggplot2::ggplot_build(pplg)$data[[1]]$PANEL)), 4)
ppli <- plotSpectrum(x, plotEngine = "ggplot", tsType = c("original", "sa"),
                     layout = "panels", interactive = TRUE)
expect_true(inherits(ppli, "plotly"))
expect_true(any(grepl("var factor = 0.5;",
                      vapply(ppli$jsHooks$render, function(h) h$code, ""),
                      fixed = TRUE)))

# invalid layout
expect_error(plotSpectrum(x, layout = "side_by_side"))

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

# residuals over time with the Q-Q plot of the standardized residuals
# beside them, and the acf, the pacf and the histogram of the standardized
# residuals in a second row, in one figure (plotly): 5 panels identified by
# their y-axis labels
ov_raw <- plotResiduals(obj, which = "overview")
ov <- plotly::plotly_build(ov_raw)
expect_true(inherits(ov, "plotly"))
ovl <- lay_of(ov_raw)
expect_equal(length(grep("^xaxis(\\d*)$", names(ovl))), 5)
# no panel titles: the panels are labelled on their y axes instead
expect_true(length(ovl$annotations) == 0 ||
              all(vapply(ovl$annotations, function(a)
                is.null(a$text) || !nzchar(a$text), TRUE)))
# the y-axis labels in the order of the panels (rows top to bottom, inside
# a row left to right)
ys <- grep("^yaxis(\\d*)$", names(ovl), value = TRUE)
xKey <- function(ay) {
  an <- ovl[[ay]]$anchor %||% "x"
  if (identical(an, "")) "xaxis" else sub("^x", "xaxis", an)
}
ys <- ys[order(-vapply(ys, function(a) ovl[[a]]$domain[1], 0),
               vapply(ys, function(a) ovl[[xKey(a)]]$domain[1], 0))]
expect_equal(unname(vapply(ys, function(a) {
  ti <- ovl[[a]]$title
  if (is.list(ti)) ti$text else ti
}, "")),
c("Residuals", "Standardized Residuals", "ACF", "PACF", "Density"))
# the panels of the second row keep the columns of the first row
# (the y-axis titles are set in a small font to fit into the gap)
expect_true(all(vapply(grep("^yaxis(\\d*)$", names(ovl), value = TRUE),
                      function(a) {
                        fo <- ovl[[a]]$title$font
                        is.null(fo$size) || fo$size <= 11
                      }, TRUE)))
# the residuals are twice as wide as the Q-Q plot, which is aligned over
# the density panel; both rows are equally high
dom <- function(ax) round(ovl[[ax]]$domain, 2)
expect_equal(dom("xaxis"), c(0, 0.64))
expect_equal(dom("xaxis2"), dom("xaxis5"))
expect_true(abs((dom("xaxis")[2] - dom("xaxis")[1]) /
                  (dom("xaxis2")[2] - dom("xaxis2")[1]) - 2) < 0.15)
expect_equal(dom("yaxis"), c(0.52, 1))
# the Q-Q points have the colour of the series lines
qqtr <- Filter(function(t) identical(t$mode, "markers"), ov$x$data)
expect_true(any(vapply(qqtr, function(t)
  identical(t$marker$color, "rgba(24,116,205,1)"), TRUE)))
# the histogram bars have the colour of the series lines
hbar <- Filter(function(t) identical(t$type, "bar"), ov$x$data)
expect_true(length(hbar) >= 3)
expect_true(any(vapply(hbar, function(t)
  identical(t$marker$color, "rgba(24,116,205,1)"), TRUE)))
# the confidence lines of the acf, pacf and acf2 plots are red (as the
# other limit lines of the package), not blue
for (wch in c("acf", "acf2", "pacf")) {
  bdw <- ggplot2::ggplot_build(plotResiduals(obj, which = wch,
                                             interactive = FALSE))
  hl <- Filter(function(d) !is.null(d$yintercept), bdw$data)
  expect_equal(length(hl), 1)
  expect_equal(unique(hl[[1]]$colour), "red")
}
# numeric codes
ov2 <- plotResiduals(obj, which = 7)
expect_true(inherits(ov2, "plotly"))
# the histogram is still available as a single panel
sh1 <- plotResiduals(obj, which = "sreshist")
expect_true(inherits(sh1, "plotly"))

message("Test plot - plot pane sizing and hover line")

# the code that gives a plot the height of the plot pane travels with the
# widget: the panes of RStudio and Positron do not honour the sizing policy of
# htmlwidgets (Positron rebuilds the widget in a page of its own), so the
# height has to be measured in the page of the widget itself
hooks <- function(w) vapply(w$jsHooks$render, function(h) h$code, "")
pl <- plot(x, plotEngine = "plotly")
expect_equal(pl$height, "100%")
expect_true(isTRUE(pl$x$config$responsive))
expect_true(isTRUE(pl$sizingPolicy$viewer$fill))
expect_true(any(grepl("data-p3-height", hooks(pl), fixed = TRUE)))
expect_true(any(grepl("p3-plot-css", hooks(pl), fixed = TRUE)))
expect_true(any(grepl("minHeight = 60", hooks(pl), fixed = TRUE)))
# the series the cursor is on is drawn bold and the others fade; a click
# pins the highlighting until a click on empty space lifts it
expect_true(any(grepl("__p3hl", hooks(pl), fixed = TRUE)))
expect_true(any(grepl("plotly_click", hooks(pl), fixed = TRUE)))
expect_true(any(grepl("line.width", hooks(pl), fixed = TRUE)))
# the dygraph plots get the fitting code as well
expect_true(any(grepl("data-p3-height", hooks(plot(x)), fixed = TRUE)))
# the stylesheets of the package are added to the widget as usual, and nothing
# of them touches the page around the widget (that page belongs to the host)
sty <- function(p) {
  d <- Filter(function(d) identical(d$name, "persephone3-plotly-css"),
              p$dependencies)
  unlist(d[[1]]$stylesheet)
}
expect_true("p3-plotly-hover.css" %in% sty(pl))
expect_false(any(grepl("p3-plotly-fill", sty(pl))))
# the hooks are part of the json of the widget, so they arrive wherever the
# widget arrives, and the page of the widget has no fixed height left
page <- tempfile(fileext = ".html")
htmltools::save_html(pl, page)
html <- paste(readLines(page, warn = FALSE), collapse = " ")
expect_true(grepl('"jsHooks":{"render":[{"code"', html, fixed = TRUE))
expect_true(grepl("data-p3-height", html, fixed = TRUE))
expect_true(grepl("p3-plot-css", html, fixed = TRUE))
expect_true(grepl("p3-plotly-hover.css", html, fixed = TRUE))
expect_false(grepl("p3-plotly-fill", html, fixed = TRUE))
expect_true(grepl("height:100%", html, fixed = TRUE))
expect_false(grepl("height:400px", html, fixed = TRUE))

# every plotly plot does this, the list of spectra elementwise
for (p in list(
  plotSeasIrrCal(x, plotEngine = "plotly"),
  plotSiRatios(x),
  plotSpectrum(x),
  plotSpectrum(x, plotType = "arSpecBars"),
  plotResiduals(x, which = "overview"),
  plotResiduals(x, which = "acf")
)) {
  expect_equal(p$height, "100%")
  expect_true(isTRUE(p$x$config$responsive))
  expect_true(any(grepl("data-p3-height", hooks(p), fixed = TRUE)))
}
spx <- plotSpectrum(x, plotType = c("arSpec", "periodogram"))
expect_true(all(vapply(spx, function(p) identical(p$height, "100%"), TRUE)))
# the interactive ggplot engines as well, the static ones stay ggplots
expect_equal(plotSpectrum(x, plotEngine = "ggplot",
                          interactive = TRUE)$height, "100%")
expect_equal(plotSiRatios(x, plotEngine = "ggplot",
                          interactive = TRUE)$height, "100%")
expect_true(inherits(plotSpectrum(x, plotEngine = "ggplot"), "ggplot"))

# the option switches the filling off, the responsive config stays
options(persephone3.plotFill = FALSE)
off <- plot(x, plotEngine = "plotly")
offd <- plot(x)
options(persephone3.plotFill = NULL)
expect_true(is.null(off$height))
expect_true(isTRUE(off$x$config$responsive))
expect_equal(length(hooks(offd)), 0)
# the hover highlighting stays (it has nothing to do with the filling), the
# height fitting hooks are gone
expect_equal(length(hooks(off)), 1)
expect_true(any(grepl("__p3hl", hooks(off), fixed = TRUE)))
expect_false(any(grepl("data-p3-height", hooks(off), fixed = TRUE)))
# in a knitr document the plots keep their own size as well
options(knitr.in.progress = TRUE)
kn <- plot(x, plotEngine = "plotly")
options(knitr.in.progress = NULL)
expect_true(is.null(kn$height))
expect_true(isTRUE(kn$x$config$responsive))
# a plot with a size of its own is never fitted to the window
options(persephone3.plotHeight = 300)
fx <- plot(x, plotEngine = "plotly")
fxd <- plot(x)
options(persephone3.plotHeight = NULL)
expect_equal(fx$height, 300)
expect_equal(fxd$height, 300)
# no fitting code (the plot has a height of its own), but the hovered series
# is highlighted all the same
expect_equal(length(hooks(fx)), 1)
expect_true(any(grepl("__p3hl", hooks(fx), fixed = TRUE)))
options(persephone3.plotWidth = 600)
expect_equal(plot(x, plotEngine = "plotly")$width, 600)
expect_equal(plot(x)$width, 600)
options(persephone3.plotWidth = NULL)

# the mouse position is marked by a vertical line over the whole plot
lay <- lay_of(pl)
expect_true(isTRUE(lay$xaxis$showspikes))
expect_equal(lay$xaxis$spikemode, "across")
expect_equal(lay$xaxis$spikesnap, "cursor")
expect_equal(lay$xaxis$spikedash, "solid")
# the shared value box of the time series plots and the range slider stay
expect_equal(lay$hovermode, "x unified")
layr <- lay_of(plot(x, plotEngine = "plotly", rangeSelector = TRUE))
expect_true(isTRUE(layr$xaxis$rangeslider$visible))
# the SI-ratio plot keeps its own hover mode
expect_equal(lay_of(plotSiRatios(x))$hovermode, "closest")
# every panel of the residual overview gets the line
ovl2 <- lay_of(plotResiduals(x, which = "overview"))
ovx <- grep("^xaxis(\\d*)$", names(ovl2), value = TRUE)
expect_equal(length(ovx), 5)
expect_true(all(vapply(ovx, function(a) isTRUE(ovl2[[a]]$showspikes), TRUE)))
# the markers at the hovered points are hidden by the stylesheet
css <- readLines(system.file("css", "p3-plotly-hover.css",
                             package = "persephone3"), warn = FALSE)
expect_true(any(grepl(".hoverlayer .point", css, fixed = TRUE)))

# the dygraphs plots draw the hovered series bold and fade the others into
# a whitish background (as the plotly plots)
gd_hl <- plot(x)$x$options
expect_equal(gd_hl$highlightSeriesOpts$strokeWidth, 3)
expect_equal(gd_hl$highlightSeriesBackgroundAlpha, 0.15)

# the dygraph plots mark the position by their cursor line only
expect_equal(plot(x)$x$options$highlightCircleSize, 0)
expect_equal(plotSeasIrrCal(x)$x$options$highlightCircleSize, 0)
expect_equal(plotResiduals(x, which = "res")$x$options$highlightCircleSize, 0)
