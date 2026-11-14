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
backend default. `scale="residual_df"` is an explicit alternative to ML for
conditional errors. Optional ARMA propagation is described below; innovation-scale
estimation uncertainty is not propagated.

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
remain `exp(m)` (inverse-point medians), without mean adjustment. Step 20d.2a
adds separately labelled original Gaussian/lognormal means, covariance and
errors, with individual range statuses. A full variance can be unrepresentable
while the standard error remains available. An inverse-point or original-moment
range failure retains the transformed distribution. The identity transform uses
the same original/transformed errors. See `prediction-moments-contract.md` for
moment semantics and the distinction between standard errors and quantile intervals.

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

## Optional profile-ML ARMA propagation (20d.2b)

`native_arma_forecasts(context, coordinates, method="delta_profile_ml",
control=list())` adds a first-order delta increment to the conditional ML
covariance. The default remains `method="conditional"`; `residual_df` is
supported only for conditional uncertainty. This new option is accurate and
Java-free, not a public-reporting emulation.

For the chronological past/future point vector `m(theta)`, with regression
coefficients refitted at each perturbation, the selected transformed covariance
is `Vconditional + G Vtheta G'`. Here `G=dm/dtheta` uses free physical ARMA
coefficients and `Vtheta` is exclusively the qualified, accurate observed
profile-ML covariance. Fixed ARMA and fixed regression effects stay fixed.
Regression estimation uncertainty already occurs in `Vconditional`; the point
sensitivity includes its response to theta, rather than adding another direct
regression covariance block. Scale uncertainty and higher-order parameter or
prediction corrections are not propagated.

The information calculation uses only the original estimation window. Endpoint
point samples retain the full supplied regressor context, refit free regression
effects, and integrate original internal/endpoint gaps. They remove the final
fitted regression effects, known mean and fixed homogeneous integrated levels
before differences, then divide by conditional standard errors. Three-scale
Richardson checks qualify the dimensionless derivatives before conversion back
to physical response units. A shared budget covers information and point
samples, excluding the already computed conditional diagnostic. These samples
and LAPACK calls remain on the main thread; owned derivative/covariance assembly
uses OpenMP where the work threshold is met.

The result adds `conditional_status`, `conditional_available`,
`conditional_covariance`, `conditional_standard_errors`, `arma_increment`,
`sensitivity`, `sensitivity_error`, derivative status/steps/rejections/counts,
`arma_information`, labelled `arma_parameter_covariance`, `arma_coefficients`,
and the combined `evaluations`/`evaluation_budget`. Square matrices use the full
point calendar, sensitivities use that calendar and free physical coefficient
labels, and both selected/conditional error series honor the requested shorter
error horizons. The conditional process/regression components and fitted center
are retained unchanged. Empty horizons need no information samples. All-fixed
ARMA copies the conditional covariance exactly, with a zero increment and an
`m x 0` sensitivity.

A failed stationarity/identification/curvature, derivative, budget or range gate
leaves conditional covariance/errors and accurate points available. The selected
covariance is absent and selected errors are missing; they are never replaced
silently by the conditional errors. Gaussian/lognormal original moments consume
the selected covariance. In particular, unavailable delta covariance leaves log
means and original errors unavailable while keeping inverse-point medians.

Independent qualification uses 64 endpoint cases with quarterly/monthly,
fixed/free factors and regression, selected windows, leading/internal/trailing
gaps and log/identity transformations. Dense covariance whitening and SVD projection with independent OLS/null-space
centering supply point derivatives through fixed h/h2 stencils; a separate
Schur calculation checks point levels and conditional covariance. The SVD path
avoids precision-Schur cancellation when differentiating twice-integrated
endpoints. An independent full Gaussian Hessian supplies the
parameter covariance target. Prospective budgets remain `2e-7*(1+abs(G))` for
sensitivities and `2e-5` relative error for covariance normalized by conditional
standard errors. Closed AR(1) derivatives, refitted means, response/regressor
units, homogeneous shifts, fixed/empty/exact-zero cases, rejected information,
exact budgets, cancellation recovery and 260-point serial/OpenMP checks cover
additional invariants. Public endpoint errors remain separate reporting evidence;
they are not targets for parameter-uncertainty propagation.
