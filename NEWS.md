# persephone3 (development version)
* `diagnosticsTable()` and the `$diagnosticsTable()` method gained a `view`
  argument: with `view = TRUE` the diagnostics are shown as an interactive
  table (DT) in the viewer pane, with readable column headers and
  explanatory tooltips and the quality classes in coloured fonts (green,
  orange, red) on a black page (`dark`, the default, = TRUE; `dark = FALSE`
  for a light page); the default (`view = FALSE`) still returns the plain
  data.frame.
* New function `perTd()`: seasonal adjustment with user-defined trading day
  regressors (from a `rjd3toolkit` national calendar or from precomputed
  regressors such as `genTd()[[3]]`), for both x13 and tramoseats.
* `perX13()`, `perTramo()` and `$updateParams()` gained a `td` argument that
  takes a calendar, precomputed trading day regressors or a `td_context()`
  list and wires them up like `perTd()`.
* The default of the `contrasts` argument of `perTd()`/`td_context()` is now
  `TRUE` (centered regressors, as in `rjd3toolkit::calendar_td()` and
  `genTd()`); the previous uncentered regressors were nearly collinear with
  the regression constant, which made x13 often estimate the model in logs
  with the trend and s.a. series returned on the log scale.
* Highcharter is no longer used by the package: `plot()`,
  `plotSeasIrrCal()`, `plotSiRatios()` and `plotSpectrum()` now draw their
  interactive plots with dygraphs and/or plotly (`plotEngine` argument,
  dygraphs is the default for the time series plots, plotly for the SI-ratio
  and spectral plots; `hchart()` and the `plotSeasIrrCalHc()`,
  `plotSiRatiosHc()` and `plotSpectrumHc()` functions are no longer
  available). The plots keep the style of the former highcharter plots:
  smoothed lines and the default Highcharts colours.
* Time series plots (`plot()`) are drawn with smoothed lines by default; the
  new arguments `smooth` and `rangeSelector` control the smoothing and the
  range selector. The range selector is now off by default. The smoothing
  uses a monotone piecewise cubic interpolation, which cannot overshoot the
  data or oscillate between the observations (the previous not-a-knot cubic
  spline produced small ripples, especially in wide plots; it is still
  available with `smooth = "spline"`). The smoothed series are no longer
  extrapolated beyond their observations, which previously blew up at the
  end of the plot when forecasts were shown and ruined the scaling of the
  y-axis. The widgets resize with their container instead of keeping the
  fixed 960px width.
* The series of the time series plots (`plot()`) are drawn in the default
  Highcharts colours (blue, dark grey, light green, grey dashed forecast)
  and the forecast is shown with its 95% confidence band, like in
  `plotSeasIrrCal()`; the time axis of the monthly and quarterly series
  shows the exact month/quarter starts.
* Outliers in the time series plots are marked with coloured dots only and
  the legend (explaining the outlier colours) stays visible, instead of
  dashed vertical lines and labels that overlapped the title in small
  windows.
* The SI-ratio dots of `plotSiRatios()` are drawn smaller (both engines) so
  that the underlying seasonal factor lines remain visible.
* `plotSpectrum()` (both engines) accepts several series types
  (`tsType = c("original", "sa", "irregular")`) and several plot types
  (`plotType = c("arSpec", "periodogram")`): with the new default
  `layout = "panels"` the spectra are shown in one panel per combination,
  the plotTypes as rows and the series types as columns (e.g. 6 panels with
  the AR spectra above the periodograms); with `layout = "overlay"` they are
  overlaid in a single panel as before (only for a single plotType).
* `plotResiduals()` gained `which = "overview"`: the residuals over time
  with the acf, the pacf and the histogram of the standardized residuals
  (with the normal curve, its bars now in the colour of the series lines)
  below, in one plotly figure.
* Repeated dates in the fine time grid of the smoothed plots (which made
  dygraphs and plotly draw vertical lines at the month/quarter boundaries
  when zooming in) are fixed.
* `getOutliers()` now handles regression variables with more than one
  coefficient (e.g. a trading day variable with one coefficient per group);
  previously `print()` failed on models with user-defined trading days.

# persephone3 0.1.0
* First working version with minimally functional dashboard
