# Conditional endpoint prediction (20d.1)

```r
native_arma_forecasts(context, fitted_coordinates,
    forecast_horizon = context$prepared$inputs$forecast_horizon,
    backcast_horizon = context$prepared$inputs$backcast_horizon,
    forecast_error_horizon = forecast_horizon,
    backcast_error_horizon = backcast_horizon, scale = "ml")
```

This internal Java-free API predicts original-series endpoints conditional on
supplied admissible ARMA coefficients, including estimated regression effects.
It does not implement seasonal adjustment or change the package's transitional
backend default. `scale="residual_df"` is an explicit alternative to ML; neither
method propagates ARMA or innovation-scale estimation uncertainty.

## Estimation and prediction domains

The original selected estimation window alone supplies the GLS fit, Q, finite
innovation count `n`, free regression count `r` and free ARMA count `k`. The scale
is `Q/n` or `Q/(n-r-k)`. Extending the prediction domain neither introduces
pseudo-observations nor changes these counts. Responses outside an explicitly
selected estimation window are never consumed, even when their dates overlap
requested predictions. Unknown original endpoint observations remain unobserved.

The prepared context must cover every requested past, estimation and future
regressor row, including fixed effects and generated mean/drift. Smaller horizons
crop the already validated context. Larger horizons fail explicitly; regressors
are never extrapolated, padded or recycled. Predictions start immediately before
or after the selected window boundaries, including when its endpoints are
missing. Backcasts are returned in chronological order. Source positions can be
zero or negative when extending before the original input start; calendar
ordinals and frequency are the authoritative date mapping.

Each error horizon may be shorter than its point horizon. Forecast errors cover
the nearest future prefix; backcast errors cover the nearest past suffix, both
chronological. The full joint covariance always covers **all requested points**,
backcasts then forecasts, with `covariance_ordinals`/matrix labels identifying
that domain. `point_domains` and `error_domains` separately label the reported
time series. The `forecasts` member adapts original forecast points to item 10's
existing contract; a shorter error horizon remains explicit there. Empty horizons
produce empty covariance and no forecast contract.

## Gaussian calculation and uncertainty

A separately requested complete projection retains endpoint missing coordinates.
The default residual/interpolation projection continues to trim endpoints.
Whitening the full prediction span and integrating all missing coordinates gives
the conditional Gaussian endpoint means. The existing QR/Gram formulation
separates known-regression/process covariance from the estimated-regression
increment; the requested endpoint submatrix is extracted from the full missing
coordinate matrix and scaled by the original-window likelihood.

Full off-diagonal entries are returned, including past/future covariance from
shared regression estimation. For example, a random walk with fitted forward
drift predicts `first - h*drift` in the past and `last + h*drift` in the future.
Its drift-estimation covariance across these domains is negative. This is not
an error: the complete joint covariance is positive semidefinite, while an
individual cross covariance can be negative. A fixed mean/regression coefficient
contributes no estimation uncertainty. No arbitrary large finite prior replaces
identified diffuse initial coordinates.

Points and errors have separate status fields. Invalid/aliased fits withhold
points; unavailable df or covariance range retains valid points and withholds
errors. A nonzero unrepresentable covariance component is not silently set to
zero. Interruptions propagate through numerical failure handling. Independent
owned covariance/scaling entries can use OpenMP; callbacks and LAPACK remain on
the calling thread. The core header and algorithm contain no Rcpp types; linear
algebra still links through R's declared BLAS/LAPACK ABI.

## Transforms and reference scope

Points and covariance are on the transformed scale. For logs, original points
are `exp(m)` (inverse-point medians), without mean adjustment. Original-scale
log errors remain unavailable until separately implemented lognormal moments;
transformed errors are never reused as original errors. An inverse-point range
failure retains transformed points and covariance. The identity transform uses
the same original/transformed errors.

Independent dense intrinsic-Gaussian Schur calculations qualify both directions
with the prospective `2e-8*(1+abs(target))` budget. Closed AR(1), fitted white-noise
mean and random-walk/drift formulas check diagonal and cross covariance. Tests
include quarterly/monthly models, fitted/fixed ARMA/regression, higher
integration, near-unit roots, units, endpoint/internal gaps, held-out responses,
context perturbations, empty/partial horizons, range/df failures and actual
serial/OpenMP equality.

Only public JDemetra model and Forecasts accessors are used for comparisons.
`RawForecasts` omits fixed regression effects; `Forecasts` restores them and
applies the inverse point transform. Accurate future points match that output
in the matrix. Public backcast points and error scaling have separate reporting
discrepancies, recorded by `verify-forecasts.R`. They are not accepted as accurate
Gaussian targets or silently incorporated into native predictions. See
`tools/reference/forecast-notes.md` for public observations and timing scope.
