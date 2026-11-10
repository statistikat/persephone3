# persephone3 (development version)
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
* `plot()` and the hierarchical plots now use the highcharter engine by
  default (`plotEngine = "hc"`), in the style of `plotSeasIrrCal()`:
  Highcharts stock chart, spline smoothing, default Highcharts colours and
  the forecast with confidence band; the dygraphs style is still available
  with `plotEngine = "dygraph"`.
* Time series plots (`plot()`) are drawn with smoothed lines by default; the
  new logical arguments `smooth` and `rangeSelector` control the smoothing
  and the range selector. The range selector is now off by default. The
  smoothing uses a cubic spline like the smoothed lines of Highcharts, and
  the smoothed series are no longer extrapolated beyond their observations,
  which previously blew up at the end of the plot when forecasts were shown
  and ruined the scaling of the y-axis. The widgets resize with their
  container instead of keeping the fixed 960px width.
* In the dygraphs style the series are drawn in fixed, distinguishable
  colours and the time axis of the monthly and quarterly series shows the
  exact month/quarter starts.
* Outliers in the time series plots are marked with coloured dots only and
  the legend (explaining the outlier colours) stays visible, instead of
  dashed vertical lines and labels that overlapped the title in small
  windows.
* The SI-ratio dots of `plotSiRatios()` are drawn smaller (both engines) so
  that the underlying seasonal factor lines remain visible.
* `plotSpectrum()` (both engines) can overlay the spectra of several series
  types (`tsType` now accepts more than one value) in a single plot.
* `getOutliers()` now handles regression variables with more than one
  coefficient (e.g. a trading day variable with one coefficient per group);
  previously `print()` failed on models with user-defined trading days.

# persephone3 0.1.0
* First working version with minimally functional dashboard
