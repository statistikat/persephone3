# persephone3 (development version)
* New function `perTd()`: seasonal adjustment with user-defined trading day
  regressors (from a `rjd3toolkit` national calendar or from precomputed
  regressors such as `genTd()[[3]]`), for both x13 and tramoseats.
* `perX13()`, `perTramo()` and `$updateParams()` gained a `td` argument that
  takes a calendar, precomputed trading day regressors or a `td_context()`
  list and wires them up like `perTd()`.
* Time series plots (`plot()`) are drawn with smoothed lines (cubic spline
  interpolation) by default; the new logical arguments `smooth` and
  `rangeSelector` control the smoothing and the range selector. The range
  selector is now off by default. The smoothed series are no longer
  extrapolated beyond their observations and no longer overshoot the
  observations at the seasonal peaks, which previously blew up at the end of
  the plot when forecasts were shown and ruined the scaling of the y-axis.
* The series of the time series plots (`plot()`) are drawn in fixed,
  distinguishable colours and the time axis of the monthly and quarterly
  series shows the exact month/quarter starts.
* Outliers in the time series plots are marked with coloured dots and a
  short label at the top of the plot instead of dashed vertical lines.
* The SI-ratio dots of `plotSiRatios()` are drawn smaller (both engines) so
  that the underlying seasonal factor lines remain visible.
* `plotSpectrum()` (both engines) can overlay the spectra of several series
  types (`tsType` now accepts more than one value) in a single plot.
* `getOutliers()` now handles regression variables with more than one
  coefficient (e.g. a trading day variable with one coefficient per group);
  previously `print()` failed on models with user-defined trading days.

# persephone3 0.1.0
* First working version with minimally functional dashboard
