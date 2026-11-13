# ARMA observed information and uncertainty (20c.1)

`native_arma_information(context, coordinates, control, method)` is an internal,
optional calculation after fixed-order fitting. It leaves the optimizer and its
convergence flag unchanged. All outputs are serializable ordinary R data. Core
types and calculations use no Rcpp; checked LAPACK calls stay on the main thread.
Only the existing independent whitening channels can use OpenMP.

## Two explicit inference conventions

Let `theta` contain only free physical polynomial coefficients, `bhat(theta)`
be GLS, `Q(theta,b)` its residual quadratic form, and `n` the finite innovation
count. Fixed regression and ARMA coefficients retain their exact supplied values.
There is no REML determinant, parameter-transform Jacobian or expected Fisher
information in this calculation.

| Method | Observed information `H` | Reported covariance |
| --- | --- | --- |
| `profile_ml` (default) | Hessian of negative log likelihood with regression and `sigma²=Q/n` profiled at every perturbation | `H^-1` |
| `conditional_residual_df` | Hessian with regression held at its fitted GLS values; scale is still profiled as `Q/n` at each perturbation | `n/(n-r-k) * H^-1` |

Here `r` is the number of estimated regression coefficients and `k` the number
of estimated ARMA coefficients. `n-r-k` excludes the scale parameter, whereas
the likelihood's estimated parameter count is `r+k+1`. A nonpositive residual
denominator withholds conditional uncertainty. Fixed coefficients are omitted
from covariance matrices and standard-error vectors, never inserted as estimated
zero-error directions. The conditional method first fits GLS once and counts
that evaluation against its budget.

The second convention was inferred from pinned public X-13/JD+ 3.9.0 outputs and
checked independently; see [the comparison notes](../reference/arma-information-notes.md).
It is a compatibility calculation and does not propagate uncertainty in the
fitted regression coefficients into ARMA covariance. The first method retains
that nuisance profiling. Neither method estimates an exact finite-sample sampling
covariance or promises nominal confidence-interval coverage.

Writing `eta=(b, log(sigma²))`, the profile Hessian at a regular stationary point
equals the full observed-information Schur complement
`H_theta,theta - H_theta,eta H_eta,eta^-1 H_eta,theta`. This identity is tested
against an independently evaluated full Gaussian likelihood. Joint regression
uncertainty is a separate step (20c.2), not the conditional GLS matrix renamed.

## Coordinates and differentiation

Derivatives use free **polynomial coefficients**, in AR, MA, seasonal AR,
seasonal MA order. Admissibility and the existing reflection margin are checked
for every physical proposal. The optimizer's working reflection coordinates
and BFGS matrix never enter the calculation. For a smooth reparameterization
`theta=g(z)`, stationary-point information transforms as `J' H_theta J` and
covariance as `J V_z J'`. Away from stationarity the Hessian also contains
gradient times second derivatives of `g`; direct physical differences avoid
omitting that term. Fully free AR(2) tests exercise a nonlinear map.

For each coordinate/pair, central stencils at `h`, `h/2`, `h/4` give two
Richardson estimates `(4*D(h/2)-D(h))/3`. The finer estimate is retained. Its
error estimate is the difference between Richardson estimates plus a roundoff
allowance using the largest absolute objective value in the stencils. Gradient
allowance is `64*epsilon*magnitude/(h/4)`; Hessian allowance is
`128*epsilon*magnitude/(h_i*h_j/16)`. These are practical numerical estimates,
not rigorous error bounds. Missing feasibility causes halving, never a one-sided
curvature estimate or clipping. Static input errors and cancellation propagate.

The constant observation log Jacobian is excluded from all differentiation to
avoid unnecessary subtraction error. The reported `center` is the fitted
unadjusted likelihood and regression result, including in conditional mode;
the original adjustment is retained separately.

Defaults: `difference_step=1e-3` times `max(1,abs(coefficient))`, eight possible
halvings, `max_evaluations=20000`, absolute/relative Hessian error tolerances
`1e-6`/`1e-4`, physical gradient tolerance `1e-4`, numerical rank ratio `1e-10`
and weak-information ratio `1e-6`. Hessian error is tested in the infinity norm;
gradient error must be at most one eighth of its tolerance. The returned steps
refer to the coarsest accepted stencil; the finest uses one quarter of them.
All rejected candidates and initial GLS work count toward the evaluation budget.

The independently authored finite-difference implementation uses elementary
central-difference/Taylor identities. [Baker (2021)](https://arxiv.org/abs/2105.04829)
provides context for validating numerical Hessians against analytic covariance
calculations; this implementation does not reproduce that paper's algorithm.
The symmetric eigenvalue adapter calls documented LAPACK `DSYEV`, without
copying a numerical library or upstream Java implementation.

## Availability and interpretation

Qualified curvature is scaled by the square roots of positive diagonal entries
before symmetric eigenanalysis. Nonpositive diagonals use a common matrix norm
for signed diagnostics. Rank is a numerical positive-eigenvalue classification;
the error estimate must also resolve the smallest eigenvalue as positive before
inversion. Weak information here means a poorly conditioned normalized observed
matrix, not a complete statistical identifiability test or a test of effect size.

| Status | Inferential output |
| --- | --- |
| `available` | Covariance and standard errors for free coefficients |
| `no_free_parameters` | Empty covariance/standard errors |
| `parameters_unavailable` / `center_unavailable` | No covariance; fixed constraints, likelihood, regression rank or scale failed |
| `evaluation_limit` / `derivatives_unresolved` | No covariance; derivative prerequisites failed |
| `not_stationary` | Curvature diagnostics retained, no covariance |
| `indefinite_information` / `rank_deficient_information` | Signed spectrum/rank retained, no covariance |
| `unresolved_curvature` / `weak_information` | Numerical error or conditioning prevents inference |
| `structural_redundancy` / `structural_near_redundancy` | Independent transfer screen vetoes inference, even if sample curvature is positive |
| `nonpositive_residual_df` | Conditional scaling is unavailable |

For unavailable directions, standard errors are `NA`, not zero; unavailable
covariance is `NULL`. `derivative_status`, `curvature_status`, structural
diagnostics, errors, counts and rejected proposals remain separately visible.
Numerical-library/range failures that throw also return no inferential result.
An available matrix establishes only the stated local approximation, not a
global optimum or population identification.

## Qualification

The mandatory Java-free tests cover all 64 established monthly/quarterly
fixed-order cases, missingness, log/window conventions, fixed/free ARMA,
generated means/drifts and conditional/profile GLS. Independent dense Gaussian
curvature, exact AR(1) derivatives through `.995`, two starting stencil sizes,
response scaling, a nuisance Schur complement and the full AR(2) map check the
formulas. Near-boundary, common-root, aliased-design, malformed-control and
budget cases withhold uncertainty.

Standalone `test_information.cpp` checks analytic Gaussian curvature,
coordinate transformations, signed/weak/rank-deficient matrices, overflow,
budgets and every cancellation callback. `test_arma_information.cpp` verifies
the analytic AR(1) inverse curvature, physical/fixed constraints, structural
rejection and every callback. Link the latter with the objective/optimizer
sources plus `information.cpp`, `arma_information.cpp` and
`arma_identification.cpp`. The generic information harness needs only
`numerics.cpp`, `linear_algebra.cpp`, `information.cpp` and LAPACK/BLAS.
