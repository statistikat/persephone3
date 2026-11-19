# Generate diagnostics table for persephone objects (internal)

Generate diagnostics table for persephone objects (internal)

## Usage

``` r
generate_diagnostics_table(x)
```

## Arguments

- x:

  an object of class
  [`persephone`](https://statistikat.github.io/persephone3/reference/persephone.md)

## Value

A data.frame with one row containing diagnostic measures

## Details

The function classifies various diagnostic statistics into quality
categories:

- **Normality/Independence tests:** "Good" (p ≥ 0.1), "Uncertain" (0.01
  ≤ p \< 0.1), "Bad" (0.001 ≤ p \< 0.01), "Severe" (p \< 0.001)

- **Seasonality/TD tests:** "Good" (p ≥ 0.1), "Uncertain" (0.05 ≤ p \<
  0.1), "Bad" (0.01 ≤ p \< 0.05), "Severe" (p \< 0.01)

- **TD regression F-test (`TD_status`):** small p indicates a
  significant trading day effect and thus justified trading-day
  regressors, i.e. the reverse direction of the residual tests: "Good"
  (p \< 0.05), "Uncertain" (0.05 ≤ p \< 0.1), "Bad" (p ≥ 0.1)

- **Outliers:** "Good" (\< 3\\

## Returned Columns

- run:

  Logical indicating if the model has been run

- method:

  Seasonal adjustment method used (x13 or tramoseats)

- type:

  Type of series ("single" or "aggregate")

- seasonality:

  Result of combined seasonality test

- normality:

  Classification of Doornik-Hansen normality test

- independence:

  Classification of Ljung-Box test for independence

- QS_seas:

  Classification of QS test for residual seasonality

- F_seas:

  Classification of F-test for residual seasonality

- combSA_all:

  Combined test for calendar effects in SA (all observations)

- combSA_last:

  Combined test for calendar effects in SA (last year)

- combI_all:

  Combined test for calendar effects in irregular (all observations)

- combI_last:

  Combined test for calendar effects in irregular (last year)

- fTestSA_td:

  F-test for trading day effects in SA series

- TD_Ftest:

  P-value of regression F-test for trading days. `NA` if the final model
  contains no trading-day regressors — e.g. templates without calendar
  effects (`"rsa3"`) or when the automatic pre-test
  (`td.test = "Remove"`, as in `"rsa5c"`) dropped them; use
  `td.test = "None"` to force their inclusion

- TD_status:

  Classification of the regression F-test for trading days: "Good" if
  the trading day effect is significant (small p), "Bad" if the
  trading-day regressors are not justified by the data

- logTransform:

  Logical indicating if log transformation was applied

- arimaMdl:

  ARIMA model specification as string

- nOutliers_Q:

  Classification of outlier proportion

- nOutliers:

  Number of outliers in the model

- nAutoout:

  Number of automatically detected outliers (see the `autoout` column of
  [`getOutliers`](https://statistikat.github.io/persephone3/reference/getOutliers.md))

- nRecentout:

  Number of outliers falling into the last observations of the series
  (see the `recentout` column of
  [`getOutliers`](https://statistikat.github.io/persephone3/reference/getOutliers.md))

- nSigRecentout:

  Number of *significant* recent outliers: those recent outliers whose
  absolute t-statistic exceeds the model's critical value, i.e.
  `recentout` and `abs(t) > crit.value` in
  [`getOutliers`](https://statistikat.github.io/persephone3/reference/getOutliers.md).
  This is the same threshold JDemetra+ uses to decide whether an effect
  enters the model at all, so automatically detected recent outliers
  essentially always count. The value differs from `nRecentout` mainly
  for manually specified recent outliers that do not reach the
  threshold, and for outliers with a non-estimable t-statistic (e.g.
  `coef.type = "FIXED"`), which are never counted as significant.

- Q:

  Composite quality indicator built from the X-11 M-statistics (M1 to
  M11): a weighted mean of the individual statistics, each of which
  ranges from 0 to 3 and is accepted below 1; a value below 1 indicates
  a satisfactory quality

- Q2:

  The same composite computed without the M2 statistic (`Q-M2`)

- nMstatOutside:

  Number of X-11 M-statistics outside their acceptance limits (value
  above 1); statistics that could not be computed do not count, and the
  value is `NA` when no M-statistics are available at all (e.g.
  TRAMO-SEATS)

- TotalQ:

  Overall quality summary (JDemetra+ node `quality.summary`): an
  aggregate of all standard diagnostic tests (residual, forecasting,
  outlier, spectral, seasonality and trading-day tests, plus the
  M-statistics Q/Q2 for X-13 or the Wiener-Kolmogorov tests for SEATS),
  classified as `Good`, `Uncertain`, `Bad`, `Severe` or `Undefined`. See
  the [diagnostics
  vignette](https://statistikat.github.io/persephone3/articles/persephone-diagnostics.html)
  for details
