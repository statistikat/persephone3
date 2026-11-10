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
  selector is now off by default.
* `plotSpectrum()` (both engines) can overlay the spectra of several series
  types (`tsType` now accepts more than one value) in a single plot.
* `getOutliers()` now handles regression variables with more than one
  coefficient (e.g. a trading day variable with one coefficient per group);
  previously `print()` failed on models with user-defined trading days.

# persephone3 0.1.0
* First working version with minimally functional dashboard
