# Estimated-ARMA regression uncertainty (20c.2a)

The internal API is

```r
native_regression_information(context, fitted_coordinates,
  method = "conditional_residual_df", arma_method = "profile_ml")
```

The same result is available as `$regression_information` on
`native_arma_information()`, without another likelihood evaluation. The core
calculation and its types have no Rcpp dependency; R labels plain numeric
results and the boundary converts storage. Nothing invokes Java.

## Conditional compatibility covariance (20c.2a.1)

With whitened free-regressor matrix `Xw`, profiled residual quadratic form `Q`,
finite effective sample count `n`, free regression count `r` and free ARMA count
`k`, the reported matrix is

```
V_beta_given_theta = inverse(Xw'Xw) * Q/(n-r-k)
```

The unit matrix is obtained from the existing rank-aware GLS QR factorization,
not by forming normal equations in production. Fixed coefficients are omitted
from both counts and covariance directions; the innovation-scale parameter is
not subtracted again. Internal missing observations are already accounted for
in the effective sample count. When `k=0`, this restores the existing fixed-ARMA
regression residual-df covariance. The ML likelihood still uses `Q/n`.

`conditioned_on="supplied_ARMA_coefficients"` and
`propagates_ARMA_uncertainty=FALSE` are explicit. Reducing the residual denominator
by `k` does **not** propagate uncertainty in the fitted ARMA coefficients. Joint
profile-ML covariance and regression/ARMA cross covariance remain a separate
inference method in 20c.2a.2.

`arma_method` selects the final-fit qualification from `profile_ml`,
`conditional_residual_df`, or `tramo_residual`. It does not change the conditional
GLS covariance formula. Choosing the explicit legacy ARMA comparison mode also
does not change it at a given fitted point. An available final ARMA information
result is required conservatively before reporting regression inference.
Nonstationary, structurally redundant, numerically unresolved or budget-limited
fits withhold the covariance. Available coefficient estimates are retained;
standard errors are `NA`, not zero. Rank, nonpositive denominator and arithmetic
range failures have explicit statuses. Zero free regressors produce an empty
0-by-0 matrix only after qualification.

Outputs include named free coefficients, their original design-column indices,
standard errors and covariance; effective/free counts, residual df and variance;
ARMA coefficients/method/covariance mode; and the final-fit availability status.
Results serialize as plain R data. No additional model optimization occurs.

## Independent and reference validation

Mandatory tests use dense Gaussian Cholesky whitening, a joint missing-coordinate
and regression projection, and its independent free-regression covariance block.
They cover 64 quarterly/monthly fixed-order fits, windows, transformations,
internal/end gaps, mixed fixed/free ARMA coefficients, means/drifts, fixed/free
regressors, label/order alignment, unit changes, fixed-ARMA reductions, aliasing,
nonstationary points, budget failure and serialization. A closed OLS C++ test
checks the covariance entries independently, the estimated-ARMA denominator,
range handling and every covariance callback interruption.

`tools/reference/verify-regression-information.R` compares 128 fresh pinned
fits (64 cases, both X-13 and TRAMO), with a predeclared `2e-5` relative Frobenius
covariance budget and `1e-7` against independent dense GLS. It never rewrites the
frozen corpus. The public backends' parameter count includes innovation scale;
the covariance denominator does not subtract that parameter again.
