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
profile-ML covariance and regression/ARMA cross covariance are a separate
inference method delivered in 20c.2a.2 below.

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

## Joint profile-ML covariance (20c.2a.2)

```r
native_regression_information(context, fitted_coordinates,
  method = "joint_profile_ml")
```

This optional method profiles innovation scale, while jointly representing the
free regression coefficients `beta` and free physical ARMA coefficients `theta`.
It requires **accurate observed profile-ML** ARMA information; combining it with
`arma_method="tramo_residual"`, `"conditional_residual_df"`, legacy covariance or
a legacy state is an error. Conditional inference remains the default regression
method, and accurate numerical covariance remains the default ARMA mode.

At `beta_hat(theta)`, let `B = d beta_hat / d theta`,
`A = (Q/n) inverse(Xw'Xw)` and `Vtheta = inverse(H_profile)`. Differentiating the
GLS first-order equation gives `B = -H_beta_beta^-1 H_beta_theta`. Block inversion
of the full negative Gaussian profile log likelihood then gives

```
Vjoint = [ A + B Vtheta B'    B Vtheta ]
         [ Vtheta B'         Vtheta   ]
```

This is an inverse observed Hessian for `(beta, theta)`, with innovation scale
profiled out. It is a local asymptotic normal approximation, not a finite-sample
coverage guarantee or a covariance of the innovation-scale parameter. There is
**no residual-df multiplier**. The conditional ML block `A` is separate from the
compatibility matrix `Q/(n-r-k) inverse(Xw'Xw)`; neither may be silently substituted
for the selected joint regression block.

The C++ core differentiates fitted GLS corrections using the response
`y - X beta_final`. Fixed regression coefficients are set to zero for this
correction problem, while their original column states, the known mean, calendar
domain and missing mask are retained. The affine GLS identity avoids subtracting
two large nearly equal coefficient estimates. Corrections are divided by the
conditional ML coefficient standard deviations before differentiation. The new
`vector_jacobian` helper uses three central stencils, Richardson agreement and
componentwise numerical error estimates. Its zero-valued center is valid; it
does not assign a residual scale or a statistical score to coefficient vectors.
Reported sensitivities and errors are restored to physical coefficient units.

The ARMA information and correction derivatives share `control$max_evaluations`.
A qualified primary ARMA block cannot make budget-limited, infeasible or
unresolved correction derivatives available. Zero free ARMA or regression
coordinates skip the derivative calculation and return correctly shaped empty
cross/sensitivity matrices. With no free ARMA coefficients, the regression
covariance reduces to fixed-ARMA **ML** covariance, `Q/n`, not residual-df GLS.

Assembly occurs in normalized coefficient units, with checked restoration of
physical units. The spectrum of the joint **correlation** matrix gates rank and
weak joint directions using the declared rank/weak tolerances; this is separate
from the primary ARMA information spectrum. Range failure also withholds the
selected covariance. Unavailable results retain coefficient labels and, when a
center likelihood exists, free column indices and estimates, with `NA` standard
errors. The separate
`conditional_ml_covariance` and nested qualified ARMA result are diagnostic
components and do not constitute a fallback joint result.

Returned fields include:

- `covariance` and `standard_errors` for the free regression coefficients;
- `arma_covariance`, `cross_covariance` (regression rows, ARMA columns), and the
  full `joint_covariance`, ordered regression then ARMA with distinct prefixes;
- `sensitivity`, `sensitivity_error`, derivative status, steps, refinement count,
  rejected proposals and evaluation count;
- joint correlation eigenvalues/conditioning, ML scale and effective/free counts;
- the labelled primary `arma_information` result and total evaluation count.

All outputs serialize as plain R data. The owned C++ implementation and types
have no Rcpp dependency. Existing whitening channels may use OpenMP; derivative
candidates, callbacks and LAPACK remain sequential on the main thread.

### Joint validation and timing

The independent oracle constructs the complete Gaussian density with fixed
regression coefficients at every perturbation, then differentiates **all** free
regression and ARMA coordinates together. It inverts this full Hessian directly,
without production sensitivity derivatives or Schur assembly. All 64 fitted
cases require a `2e-5` relative covariance budget in standardized coordinates,
a `2e-5` inverse-identity defect and GLS derivative agreement within
`2e-7 * (1 + abs(B))`. Tests also cover response/regressor unit changes, a large
regression offset, fixed/free directions, known means, persistent AR(1), zero
coordinate blocks, budgets, aliases, structural redundancy, weak conditioning,
serialization and serial/OpenMP results. Closed AR(1) mean derivatives and the
full bivariate Gaussian Hessian are checked by the standalone sanitizer harness.

`tools/reference/verify-joint-information.R --timing` records the independent
matrix and elapsed times for prepared native fit plus conditional or joint
inference, alongside prepared pinned JDemetra X-13/TRAMO fits. JDemetra's reported
regression covariance is conditional; it is **not a target for joint covariance**.
Three warmed rounds use calibrated batches of at least 50 ms where possible,
with setup, package/JVM startup and explicit pre-batch garbage collection
excluded. Natural within-batch collection remains included. These are API
workload comparisons with different estimators and additional backend outputs,
not a claim that the backends perform identical operations. Historical timing
artifacts remain unchanged.
