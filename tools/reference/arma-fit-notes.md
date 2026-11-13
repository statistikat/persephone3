# Fixed-order ARMA fit qualification (20b.2b.2)

The read-only `verify-arma-fits.R` compares native estimates with both pinned
JD+ 3.9.0 preprocessing engines through public R APIs. It checks the existing
R/wrapper/JAR/JVM manifest before fitting and original artifact hashes afterward.
No stored fixture, reference tolerance, numerical upstream source or fitting
default is changed.

```sh
Rscript tools/reference/verify-arma-fits.R
# Use a previously compiled development tree:
Rscript tools/reference/verify-arma-fits.R --source
```

## Prospective matrix and comparison rules

The matrix is declared in `inst/tinytest/fixtures/native/arma-fit-cases.R`, shared
by mandatory Java-free tests and this live probe. Its 64 cases cross monthly and
quarterly data, complete and leading/internal/trailing gaps, identity and log
transformations, and eight fixed-order models:

| Model | Free ARMA coefficients / constraints |
| --- | --- |
| AR(1), MA(1), ARMA(1,1) | Ordinary free factors |
| AR(2) | First coefficient free, second fixed at -0.25 |
| Seasonal AR(1), seasonal MA(1) | Free seasonal factor with seasonal differencing |
| Airline | Free ordinary/seasonal MA with ordinary and seasonal differencing |
| Mixed factors | Free ordinary AR and seasonal MA; ordinary MA fixed at 0.2; seasonal differencing |

Each case includes an estimated regressor, an exactly fixed regressor and an
estimated mean or drift. Full and middle inclusive windows preserve source
indices and context domains. Data use a recorded R seed and explicit filtering,
integration and deterministic formulas; no native/reference estimates define
the observations. Independent tests restore the caller's RNG state.

JD+ receives the selected window directly, matching the delivered window
contract. Automatic modelling and transformation selection are disabled, all
free ARMA starts are zero, exact ML is requested, and its estimation tolerance
is `1e-12`. AR signs and factor ordering are converted explicitly. This does not
claim equivalence to every JD+ estimation-span selector or automatic policy.

Predeclared relative-plus-absolute budgets for this bounded matrix are:

| Comparison | Bound |
| --- | --- |
| Native/direct ARMA coefficients | `1e-4 * (1 + abs(reference))` |
| Native/direct free regression coefficients | `1e-5 * (1 + abs(reference))` |
| Fitted objectives and independent density values | `2e-8 * (1 + abs(reference))` |
| Calendar Jacobian | `1e-10 * (1 + abs(reference))` |
| Counts, fixed coefficients and future-context invariance | Exact |

Mandatory independent minimization additionally requires absolute coefficient
differences at most `1e-4` and objective differences at most `2e-7` from each of
two native starts. Scalar minimization and two deterministic Nelder–Mead starts
use independent densities. None of these fixture budgets changes the original
field-tolerance manifest or establishes a general production acceptance rule.

## Independent density and difficult cases

The dense oracle expands ordinary/seasonal polynomials independently, computes
impulse coefficients, solves the finite Yule–Walker equations and forms a
Toeplitz covariance. Cholesky whitening, exact differencing and QR integration
of missing original observations give the finite Gaussian density. Free
regression effects are concentrated by an independent QR. Neither native state
recursions nor reference-estimated coefficients are used to find these minima.

The main matrix has 128 direct fits. Eight additional direct fits perturb only
future regressors and require identical fitted descriptions and estimation
results. Four persistent AR controls, generated with coefficients 0.97/0.995
at both frequencies, add eight direct fits. Mandatory tests independently
minimize their closed AR(1) density and run three starts (-0.8, 0, 0.98), checking
coefficient agreement within `1e-6` and objective agreement within `2e-8`.

Exact and near shared factors, a converged redundant ridge, mixed fixed/free
constraints, near-unit coefficients and weak/aliased regression designs are
covered separately by the [transfer-sensitivity screen](../native-tests/arma-identification-contract.md).
Its full rank does not establish finite-sample identification.

## Numerical correction exposed by qualification

The initial matrix exposed two quarterly airline cases that reached an objective
within roughly `1e-11` of the reference but reported `line_search_failed` or
`stalled`. Their gradients had not met the unchanged threshold: these were
proper unsuccessful optimizer statuses, not successes to relabel. Large drift
levels were repeatedly propagated and subtracted by the regression whitening
recurrence, obscuring the final tiny likelihood improvements.

The regression kernel now obtains an ordinary least-squares centering vector
`b0` on observed rows, whitens `y-X*b0`, estimates the GLS correction and returns
`b0+b_correction`. For a linear whitening operator `L`, this is the identity
`L(y-X*b0)=Ly-LX*b0`. Original-response whitening outputs are reconstructed;
the whitened design still determines the actual rank and covariance. This
changes numerical conditioning, not the likelihood, coefficient constraints,
effective sample size or convergence tolerances. Centering does not silently
drop any requested coefficient. Existing regression/reference checks and an
additional large-drift translation check qualify the change.

Both cases now converge, including the alternate starts. The main live matrix's
largest discrepancies are below `5e-7` for ARMA coefficients, `1e-8` for regression
coefficients, and `1.2e-11` for fitted objectives and independent density values.
Exact local counts and completed build checks are recorded in
`IMPLEMENTATION_STATUS.md`; supported-platform workflows are configured but
remote execution remains unverified.

## Next acceptance gate

Item 20c must qualify local observed/expected information, weak finite-sample
directions and joint ARMA/regression uncertainty. A transfer screen, a converged
first-order optimizer and agreement on this bounded matrix do not provide a
valid Hessian covariance or universal fitted-model parity. Forecast/backcast
uncertainty, seasonal adjustment and the native default switch remain later
items. Extend prospective cases and obtain platform evidence without changing
these fixtures to hide discrepancies.
