# Deterministic ARMA optimization (20b.2b.1)

`native_arma_optimize()` minimizes the prepared
[20b.2a objective](arma-objective-contract.md) with fixed orders, differencing,
transformation, regression design and coefficient constraints. It runs the
independently authored C++ solver through a thin Rcpp boundary. It is not
automatic model selection, a seasonal-adjustment fit, or full JD+ estimator
qualification. The legacy backend remains the public fitting default.

```r
context <- persephone3:::native_arma_objective_setup(
  ts(sin(seq_len(60)), frequency = 4), ar = NA_real_, include_mean = TRUE)
fit <- persephone3:::native_arma_optimize(context)
fit$status
fit$final$parameters$coefficients
fit$gradient
```

## Coordinates and starting point

There is one deterministic start: the prepared map's initial coordinates, or the
explicit `coordinates` argument in the same map convention. No random restart,
silent model simplification, root reflection or automatic alternate start is
performed. Named coordinates must match the map exactly. Input contexts and
fixed coefficient values remain unchanged.

For a fully free factor, the map uses `k=(1-margin)*tanh(z)`. The solver works in
`u=tanh(z)`, with the open constraint `abs(u)<1`; the existing factor checks still
apply after decoding. This avoids accepting a nearly saturated transform as a
small physical gradient. Mixed fixed/free factors use their free polynomial
coefficients directly. All fixed coefficients remain exact. An infeasible
proposal is rejected; coordinates are never clipped to a boundary.

`coordinates` and `initial_coordinates` in the result use the reusable map's
`z`/coefficient convention. `working_coordinates`, `gradient`, numerical
`gradient_error` and difference diagnostics use **unit-reflection/coefficient**
coordinates, named accordingly. These gradients are not derivatives with
respect to every original polynomial coefficient and are not uncertainty
estimates. Fully free multi-coefficient factors couple their polynomial entries.

## Solver and numerical derivatives

The solver starts with an identity inverse-Hessian approximation and proposes
`p=-H*g`, capped by `max_step` in infinity norm. A nonfinite or non-descent
direction resets `H` to identity. Armijo backtracking starts at one and halves
the step until the candidate is available and provides the required decrease.
Rejected likelihoods retain their statuses; no finite penalty substitutes for
an undefined, rank-deficient, zero-scale or numerically failed likelihood.

The standard inverse-BFGS update is applied only when the curvature is positive,
exceeds the estimated derivative-error contribution and passes the declared
relative curvature test. Nonfinite/nonpositive updates reset the matrix.
This is Armijo halving with guarded updates, not an Armijo–Wolfe implementation;
no global convergence theorem is claimed. Mathematical context for the update,
descent conditions and error sensitivity is
[Xie, Byrd and Nocedal, sections 2–3](https://arxiv.org/pdf/1901.09063).
No upstream optimizer implementation was copied or translated.

For coordinate `j`, the initial difference width is
`h=difference_step*max(1,abs(x[j]))`. Central differences at `h` and `h/2` are
preferred. If either side is unavailable, the code attempts a second-order
forward or backward stencil using `f0`, `f(h/2)`, `f(h)` and `f(2h)` on a feasible
side. It halves `h` when no sufficiently accurate stencil is available.

The diagnostic error estimate combines `abs(g_fine-g_coarse)/3` with
`16*epsilon*max(1,abs(stencil values))/h`. A coordinate is accepted when that
estimate is at most `gradient_tolerance/8 + 1e-3*abs(g_fine)`. Convergence requires
`max(abs(g)+gradient_error) <= gradient_tolerance`. The error and trace's
`gradient_bound` are finite-difference diagnostics, **not rigorous mathematical
bounds or statistical standard errors**. Scalar density derivatives provide
independent checks of those estimates in the mandatory test matrix.

The inverse-Hessian approximation is an optimization aid and is not returned
as coefficient covariance. Small parameter movement or small likelihood change
alone cannot establish convergence: if both stall while the gradient criterion
fails, the result is explicitly unsuccessful.

## Controls and budgets

Controls are a uniquely named list. Unknown names, duplicates, NULL entries,
nonfinite values and malformed scalar types fail before optimization.

| Control | Default | Meaning / bounds |
| --- | ---: | --- |
| `max_iterations` | 100 | Accepted steps; integer 0..10,000 |
| `max_evaluations` | 2,000 | All objective attempts, including derivative/rejected candidates; integer 1..1,000,000 |
| `max_line_search` | 30 | Halving attempts per step; integer 1..100 |
| `difference_refinements` | 10 | Additional halvings of derivative width; integer 0..20 |
| `gradient_tolerance` | `1e-5` | Positive absolute tolerance in working-coordinate objective units |
| `difference_step` | `1e-4` | Initial relative width; positive and at most 0.1 |
| `step_tolerance` | `1e-10` | Positive relative movement threshold for detecting a stall |
| `function_tolerance` | `1e-10` | Nonnegative relative objective threshold for detecting a stall |
| `armijo` | `1e-4` | Sufficient-decrease constant, strictly between 0 and 0.5 |
| `max_step` | 1 | Positive infinity-norm direction cap |

The numerical error criterion uses absolute objective units. Large offsets,
poor scaling, severe conditioning or requests for unrealistic precision may
produce `gradient_unavailable` rather than false convergence. No tolerance is
silently relaxed. Every run has bounded evaluations, steps, derivative attempts
and line searches, with at most 64 working coordinates. R's model/state limits
are validated before starting.

Initial and final likelihood payloads are retained with owned C++ memory. The
final fit requires **no uncounted reevaluation**. Trace storage is bounded by
accepted steps. Interrupts propagate from every callback; all candidate, derivative
and LAPACK work stays on the calling thread. Existing independent regression
whitening channels may still use OpenMP under their established rules.

## Result and failure states

`status` and `converged` describe the optimizer; `final$status` and
`final$available` describe the retained objective. A usable final candidate does
not turn a failed optimization into a success. Output includes starting/final
map coordinates, working coordinates, gradients/errors/stencils, evaluation and
gradient counts, accepted steps, matrix-reset count, rejected-status counts and
a monotone trace of accepted objectives. Everything serializes as plain R data.

| Optimizer status | Interpretation |
| --- | --- |
| `converged_gradient` | Qualified gradient meets the numerical first-order criterion |
| `no_free_parameters` | One valid conditional regression evaluation; success, but no ARMA optimization/convergence claimed |
| `initial_unavailable` | Starting objective unavailable; its original reason is retained |
| `evaluation_limit` / `iteration_limit` | Budget reached; last accepted candidate retained |
| `gradient_unavailable` | No feasible, sufficiently accurate derivative stencil |
| `line_search_failed` | No accepted feasible decrease within the halving limit |
| `stalled` | Small movement/change without a sufficiently small gradient |
| `step_resolution_limit` | A proposed move rounds back to the current point |
| `direction_failure` | No finite descent direction can be formed |

Only `converged_gradient` sets `converged=TRUE`. `success` additionally includes
the valid no-free-parameter case. Invalid static inputs, allocation failures,
internal logic errors and cancellation continue to throw. Runs leave no global
optimizer state and may be repeated after failures or interruptions.

`identification="not_assessed"` and `optimality="first_order_only"` are explicit.
A small gradient can occur on a ridge, saddle or nonidentified model. It does
not certify a local/global minimum, common-root exclusion, information-matrix
rank, inferential precision or equivalence to a JD+ fitting procedure.

## Validation and next work

The mandatory R suite has **1,196 assertions**, including 58 fits against
independent dense Gaussian minima: 48 AR(1)/MA(1) starts across two frequencies,
windows, missingness and identity/log inputs; three mixed AR(2) starts; three
joint ARMA(1,1) starts; and four seasonal AR/MA models with seasonal integration
and estimated drift. Scalar minimization and derivative-free two-dimensional
minimization use the independent densities. Checks cover original Jacobians,
fixed coefficients, gradients, budgets, statuses, serialization, deterministic
reuse and serial/OpenMP equality.

`test_optimizer.cpp` needs no R headers/libraries and checks analytic quadratic
and Rosenbrock optima, feasible one-sided differences, strict budgets, stalls,
precision failure and all 12 callbacks of its cancellation case.
`test_arma_optimizer.cpp` compares an independently minimized closed AR(1)
density and checks 967 actual main-thread callback boundaries plus reuse. Both
pass strict C++17 and ASan/UBSan; prebuilt numerical libraries are not instrumented.

Next, **20b.2b.2** qualifies fitted models against pinned JD+ APIs and addresses
near-unit/common-root/weak-identification cases and multi-platform agreement.
Broader fit acceptance, ARMA/regression uncertainty, forecasts and the native
seasonal-adjustment pipeline remain separate work. This slice changes no
reference fixtures, statistical tolerances, model-selection policy or defaults.
