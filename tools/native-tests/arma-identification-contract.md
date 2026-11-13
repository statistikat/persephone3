# ARMA transfer-sensitivity diagnostics (20b.2b.2)

`native_arma_identification(parameters, period, coordinates)` screens local
parameter redundancy in the supplied ARMA map. `native_arma_optimize()` includes
the same screen as `parameter_diagnostics` at its retained final point, even
when optimization does not converge. It performs no additional likelihood
evaluation and does not change optimizer stopping or accepted parameters.

The implementation uses owned C++ data and R-supported LAPACK `DGESVD` through
the existing linear-algebra boundary. Rcpp only converts arguments/results;
polynomial algebra, SVD and callbacks run on the calling thread. No upstream
numerical source is copied or translated.

## Mathematical quantity

For the expanded ordinary/seasonal polynomials `A(B)` and `M(B)`, the transfer
function is `H(B)=M(B)/A(B)`. For every **free physical polynomial coefficient**
`c_j`, form the numerator of its derivative:

```
dH/dc_j = J_j(B) / A(B)^2
J_j(B)  = A(B) * dM/dc_j - M(B) * dA/dc_j
```

The matrix `J` stores these polynomial coefficients by increasing lag. Each
derivative is computed algebraically by multiplying the other factors and
shifting by the coefficient's lag; it does not require finite differences or
polynomial root solving. Fixed coefficients contribute to the factors but have
no derivative columns. This respects mixed constraints, including cases where
fixing one coefficient removes a free cancellation direction.

Normalize each column by its Euclidean norm and compute singular values in
descending order. `rank` counts values strictly greater than
`rank_tolerance * largest`, and `singular_ratio` is smallest/largest. The defaults
are `rank_tolerance=1e-10` and `near_tolerance=1e-4`, with validated
`0 < rank_tolerance < near_tolerance < 1`. These are declared numerical screening
thresholds in the polynomial coefficient basis, **not statistical significance
levels or a calibrated measure of parameter uncertainty**.

The connection between redundant AR/MA factors and singular information is
discussed in [McLeod (1999)](https://arxiv.org/abs/1611.01387). The diagnostic here
is an independently derived, constraint-aware transfer Jacobian; it does not
compute the paper's Fisher information or extend its theorem to arbitrary
finite samples, regressors or missing observations. LAPACK's
[DGESVD interface](https://netlib.org/lapack/explore-html/d1/d7f/group__gesvd_gac6bd5d4e645049e49bb70691180abf07.html)
supplies singular values, with checked workspace, result and cancellation rules.

## Results and interpretation

| Status | Meaning |
| --- | --- |
| `full_rank_transfer` | No small singular direction at the supplied screening thresholds |
| `near_redundant_transfer` | Full numerical rank, but singular ratio at or below `near_tolerance` |
| `rank_deficient_transfer` | At least one numerically unresolved free transfer direction at `rank_tolerance` |
| `no_free_parameters` | No ARMA coefficients to assess; rank zero and no spectrum |
| `parameters_unavailable` | Inadmissible/boundary/nonfinite candidate; original parameter status retained and rank unavailable |

Results also include the unnormalized `numerator_jacobian`, column norms,
singular values, thresholds and free-coefficient names. The coordinates are
physical free polynomial coefficients, distinct from the optimizer's
unit-reflection working coordinates. Results serialize without external pointers.

`minimum_reflection_slack = min(1-abs(k))` reports proximity to a factor's Schur
reflection boundary across fixed and free coefficients. It is unavailable for
an empty model or an inadmissible candidate. For order one it equals the usual
coefficient slack; for higher orders it is **not a polynomial-root distance**.

The screen is independent of data. A full-rank transfer can still have poorly
estimated coefficients, an aliased regression design, insufficient observations,
a flat sampled likelihood or an optimizer failure. Conversely, a small gradient
can occur on a redundant ridge. Therefore `statistical_identification` and the
optimizer's existing `identification` remain **`not_assessed`**. No covariance,
standard error, local/global minimum certificate, automatic model reduction or
root cancellation is inferred.

## Qualification

Mandatory tests compare the algebraic Jacobian against direct ARMA(1,1)
identities and independent finite polynomial perturbations for seasonal and
mixed blocks. Base R SVD independently checks the spectrum. Exact/near common
factors, cross-factor cancellation, fixed constraints, near-unit coefficients,
bad inputs and serialization are covered. A constructed zero-score ARMA(1,1)
case converges while correctly retaining a rank-deficient screen; zero and
nearly duplicate regressors fail separately despite full transfer rank.

The standalone harness checks tall/wide/zero SVD cases, exact/near/fixed transfer
directions, validation and every callback boundary, followed by successful reuse.
Run it with `numerics.cpp`, `linear_algebra.cpp`, `arma_parameters.cpp`,
`arma_identification.cpp` and the existing R BLAS/LAPACK link flags. Prebuilt
numerical libraries are not sanitizer-instrumented.

The paired [fitted-model qualification](../reference/arma-fit-notes.md) adds
independent minima, deterministic alternate starts and pinned direct fits.
Finite-sample information and estimated-parameter uncertainty remain item 20c.
