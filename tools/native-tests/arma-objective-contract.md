# ARMA parameter and objective contract (20b.2a)

This internal interface prepares and evaluates **one** concentrated regression-
SARIMA likelihood candidate. It does not run an optimizer, select orders, compute
ARMA-parameter uncertainty or fit a seasonal-adjustment model. It builds on
[20b.1 regression likelihood](regression-likelihood-contract.md).

## Parameter maps

`native_arma_parameter_map(ar, ma, seasonal_ar, seasonal_ma, initial,
reflection_margin)` accepts coefficient specifications in increasing lag order.
Finite entries are fixed; `NA` entries are free. Empty factors are supported.
NaN, infinity, factors, arrays and more than 16 entries per factor are rejected.
Optional `initial` is a uniquely named list of finite coefficient vectors for
any of the four factors. Lengths must agree and supplied fixed values must match
exactly. Unspecified free starts are zero. If that initial combination violates
the constraints, supply a feasible start; the map does not silently replace it.

Each factor has one of four coordinate modes:

| Factor specification | Coordinates |
| --- | --- |
| Empty | None |
| All fixed | None; coefficients preserved exactly |
| All free | Unconstrained coordinates mapped to bounded reflection coefficients |
| Mixed fixed/free | Free polynomial coefficients directly; every proposal is checked |

Mixed blocks deliberately avoid a reflection transform: fixing a reflection
coefficient generally does not fix the corresponding polynomial coefficient.
Invalid direct proposals are rejected without clipping, root reflection, or
changing the fixed entries. Such a constrained region is not an unconstrained
optimizer domain. Coordinate names include the factor, mode and one-based index;
named proposals must match the map's exact names/order. Unnamed proposals are
positional. Maps contain ordinary serializable data, not external pointers.

The sign convention remains `(1-phi(B))(1-Phi(B^s)) y =
(1+theta(B))(1+Theta(B^s)) epsilon`. The Schur/Levinson recursion is applied to
`phi` for AR and `-theta` for MA. Writing the order-m reflection as `k_m`, the
forward and inverse equations are

```
phi_m,m = k_m
phi_m,j = phi_(m-1),j - k_m * phi_(m-1),(m-j)
phi_(m-1),j = (phi_m,j + k_m * phi_m,(m-j)) / (1-k_m^2).
```

These are independently implemented mathematical recursions, documented in
[Halliday and Boshnakov, section 2, equations 3–6](https://arxiv.org/pdf/2208.05055).
The strict reflection test implies roots outside the unit circle. It applies
separately to ordinary and seasonal factors; substituting `B^s` preserves that
property. No R/Java numerical implementation is copied or translated.

For fully free blocks, `k=(1-margin)*tanh(z)` and
`z=atanh(k/(1-margin))`. The default **reflection margin is `1e-8`**; it must be
representably positive and less than one. All blocks, including fixed blocks,
require `abs(k)<1-margin`. This is a declared numerical domain, **not** a minimum
polynomial root distance. The margin is never silently widened. Unit/explosive
factors, excluded near-boundary factors and nonfinite proposals are distinct
statuses. Saturated floating-point `tanh` results are rejected. An inverse/
forward coefficient reconstruction check uses `256*epsilon*order*max(1,|phi|)`;
numerically unresolved recursions fail explicitly.

This constrained estimation interface requires MA invertibility. Existing
fixed-parameter likelihood functions continue to accept noninvertible MA values;
their contract is unchanged. R's public `arima` documentation describes its
[different treatment of MA constraints and fixed coefficients](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/arima.html);
the new map does not claim to reproduce that optimizer's parameterization.

## Prepared objective and evaluation

`native_arma_objective_setup()` takes the same response, regressor coefficient,
window, identity/log, mean/drift and context inputs as the regression interface,
plus the ARMA specifications above. Input preparation is shared with 20b.1.
The existing bounds remain: period 4/12, `d,D` in 0..2, at most 16 stationary
states, 32 total states and 32 regression columns. Specified orders are retained
even for zero coefficients; orders are not reduced implicitly.

`native_arma_objective(context, coordinates)` decodes a candidate and calls the
C++ regression likelihood directly. The objective for minimization is

```
value = -(profiled_log_likelihood + observation_log_jacobian).
```

Regression coefficients and innovation scale are concentrated out using the
existing `Q/n_finite` ML convention. There is no REML determinant, no change in
diffuse-history measure, and **no parameter-transform Jacobian** in this ML
objective. The observation Jacobian is computed once from original selected
observations. Missing masks, fresh window initialization and generated drift
coordinates retain their existing meaning. Future/past regressor context is
validated and retained; it does not enter estimation rows or create forecasts.
When available, the parameter count is free ARMA + free regression + one scale.

The result reports decoded factors, constraint diagnostics, value, raw/adjusted
profile likelihood, scale, finite count and regression coefficients/rank. It
does not rerun the filter to construct full observation traces. Full regression
results remain obtainable through the 20b.1 interface at the decoded coefficients.

```r
y <- ts(sin(seq_len(60)), frequency = 4)
context <- persephone3:::native_arma_objective_setup(
  y, ar = c(NA_real_, .2), ma = NA_real_, include_mean = TRUE)
context$parameters$modes  # mixed AR uses coefficients; free MA uses reflections
candidate <- persephone3:::native_arma_objective(context)
stopifnot(candidate$available, is.finite(candidate$value))
```

## Failure contract

Only `status="available"` yields a finite usable objective. All other statuses
set `value` and adjusted objective to `NA`; a caller must inspect the status.
No arbitrary finite penalty or successful boundary optimum is manufactured.

| Status | Meaning |
| --- | --- |
| `inadmissible_parameters` | A factor fails strict stationarity/invertibility |
| `parameter_boundary` | Excluded reflection margin or saturated transform |
| `nonfinite_parameters` | Candidate coordinates contain NA/NaN/infinity |
| `numerical_failure` | Finite-precision recursion, initialization, likelihood, covariance or range guard failed; diagnostic message retained |
| `rank_deficient` | Regression coefficients are not uniquely identified |
| `unresolved_diffuse` | Initial diffuse state is not fully identified |
| `no_observations` / `no_finite_innovations` | No usable likelihood scale |
| `zero_scale_boundary` | Scale is zero; raw profile may be `+Inf`, but usable objective remains unavailable |

Malformed data, dimensions, order limits, control values and thread settings
remain **errors**, even for an inadmissible candidate. Static preflight runs
before candidate rejection. Numerical exceptions are caught only around the
validated likelihood call. Caller interruption—including callbacks throwing
standard C++ exceptions—propagates; allocation and internal logic failures are
not converted into ordinary bad candidates. Subsequent calls remain reusable.

The parameter module uses only the C++ standard library. Objective evaluation
uses the existing R BLAS/LAPACK ABI through the numerical core, with Rcpp confined
to conversion/registration. Factor recursions and candidate evaluations run on
the calling thread. Only existing independent whitening channels may use OpenMP;
R/Rcpp, LAPACK and time recursion are never placed in worker regions.

## Evidence and next step

`test-native-arma-objective.R` passes 1,911 assertions. Independent polynomial
construction and `polyroot` validate maps/root decisions, including real and
complex factors and exact fixed values. Dense observed Gaussian covariance/GLS
calculations validate 42 AR(1)/MA(1) profiles, their scalar minima and local
central differences. Eight monthly/quarterly integrated/log/window cases retain
20b.1 semantics. Failure statuses, serialization and input controls are tested.
These qualify the objective; the scalar minimizer in the test is an oracle,
not an implemented native optimizer.

Standalone `test_arma_parameters.cpp` needs no R headers or libraries and checks
the independent AR(2) stability triangle. `test_arma_objective.cpp` checks the
white-noise density, statuses and all 36 callback cancellation/reuse boundaries.
Both pass strict-warning C++17 and ASan/UBSan; prebuilt BLAS/LAPACK is not instrumented.

The bounded optimizer is now delivered in
[20b.2b.1](arma-optimizer-contract.md), with derivative and stopping diagnostics.
Next is 20b.2b.2: pinned fitted-model and identification qualification. Factor
admissibility does not establish joint ARMA identifiability (for example,
common AR/MA roots), convergence, parameter uncertainty, or full JD+ parity.
