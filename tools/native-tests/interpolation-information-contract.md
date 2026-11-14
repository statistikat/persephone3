# Conditional interpolation uncertainty (20c.2b.2a)

The internal Java-free API is:

```r
native_interpolation_information(context, fitted_coordinates, scale = "ml")
native_interpolation_information(context, fitted_coordinates, scale = "residual_df")
```

It supplies accurate Gaussian points and prediction-error covariance at internal
missing observations. It conditions on the supplied admissible ARMA coefficients
and accounts for estimated regression effects. It neither optimizes ARMA nor
propagates its estimation error. Thus it can evaluate a nonoptimal parameter
point; it makes no claim of a qualified information matrix there. Optional ARMA
propagation is the separate next step, 20c.2b.2b.

## Covariance components

Whiten the trimmed complete span at unit innovation variance. Let `M` contain
the whitened missing-coordinate indicators, and `X` the free regression columns.
The existing augmented GLS fit estimates coefficients on `[X M]`. Its upper-left
unit covariance block `V_beta` equals the GLS coefficient covariance after
integrating the missing coordinates. The accurate interpolation point is the
negative fitted missing-coordinate coefficient, with mean and fixed effects
restored.

```
U = inverse(M' M)
S = least_squares(M, X)
prediction_covariance_unit = U + S V_beta S'
```

`known_regression_covariance` is the scaled `U`, including process uncertainty
and the identified diffuse initial-condition uncertainty. `regression_covariance`
is the additional scaled `S V_beta S'`; `covariance` is their sum. All are square
matrices indexed by the missing source rows, including off-diagonal covariance.
Fixed coefficients contribute no estimation uncertainty. QR supplies `U` and
`S` without forming normal equations; a Cholesky factor of `V_beta` forms the
regression increment as a Gram matrix. No nearly equal covariance matrices are
subtracted.

Scale always uses the **original observed likelihood**, not the augmented
complete-data count:

```
ml:          sigma2 = Q / n
residual_df: sigma2 = Q / (n - r - k)
standard_errors = sqrt(diag(covariance))
```

`n` is the finite observed innovation count, `r` the free regression count and
`k` the free ARMA count. Missing coordinates and the innovation-scale parameter
are not subtracted again. These are plug-in Gaussian prediction errors, with
no propagation of innovation-scale estimation uncertainty or Student-t interval
interpretation. The df option matches the public JDemetra 3.9.0 transformed
missing-value standard-error convention.

## Domains, transformations and availability

Only internal missing rows receive points or errors. Leading/trailing gaps stay
unfilled, observed values stay unchanged, and both local and original source
indices are returned. `aligned_standard_errors` covers the selected original
calendar window with `NA` outside the imputed rows. No internal missing values
is a valid empty result with zero-dimensional covariance matrices.

Covariance and `standard_errors` are on the **transformed scale**. For an identity
transform, `standard_errors_original` is identical. For logs it is deliberately
`NA`, with `original_uncertainty_status="lognormal_moments_not_computed"`.
Original-scale points are `exp(m)` without mean adjustment; a transformed
standard error is not reused as an original-scale error. Lognormal mean/variance
outputs belong to the forecast/back-transformation step. If `exp(m)` is out of
range, the transformed point/covariance remain available and the original point
has a separate `original_scale_range` status.

The `available` flag describes transformed uncertainty; `point_available` and
`original_point_status` describe the two point domains separately. Nonpositive
df or a failed covariance certificate withholds errors and retains valid points.
An unrepresentable nonzero regression-variance component is likewise unavailable;
it is not rounded into an assertion of zero regression uncertainty.
Aliasing, unidentified diffuse states and invalid ARMA candidates remain explicit
point failures. Unavailable errors are never replaced by zero. Interruptions
propagate through all covariance/error-handling paths and allow subsequent reuse.

This function has no legacy reporting input or branch. The existing
`native_arma_residuals()` remains a residual/point diagnostic and does not compute
prediction errors; callers request this separate uncertainty result when needed.
The Rcpp-free core owns all arrays and uses R's declared BLAS/LAPACK ABI for
factorizations. OpenMP handles independent covariance/loading/scaling entries;
callbacks and LAPACK stay on the calling thread.

## Evidence

Mandatory tests check the 64 fitted models and 96 additional quarterly/monthly
reporting controls against an independent intrinsic-Gaussian precision/Schur
oracle. Closed AR(1), OLS shared-mean and two-point seasonal bridges verify
diagonal and off-diagonal entries. The suite also covers units, fixed effects,
log inputs, inverse-point overflow, absent df, aliases, invalid candidates,
windows, endpoint gaps, serialization and a 260-missing-value serial/OpenMP
comparison. The standalone sanitizer harness checks all 177 callback boundaries.

`tools/reference/verify-interpolation.R` uses only public model/estimation
accessors to compare 404 fits. The unchanged scaled budget is
`2e-8*(1+abs(target))`; missing dates and likelihood counts/df are exact.
Accurate points and full covariance are independently qualified; public
`MissingValueEstimation.getStandardError()` supplies the transformed-df target.
Legacy public points are not accepted as accurate targets. Versioned evidence
and fit-plus-output timing are in
`tools/reference/information/2026-11-14-interpolation-conditional/`.
