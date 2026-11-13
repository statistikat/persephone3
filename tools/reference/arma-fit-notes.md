# Fixed-order ARMA fit qualification (20b.2b.2)

`verify-arma-fits.R` compares native estimates with both pinned
JD+ 3.9.0 preprocessing engines through public R APIs. It checks the existing
R/wrapper/JAR/JVM manifest before fitting and original artifact hashes afterward.
No stored fixture, reference tolerance, numerical upstream source or fitting
default is changed. Timing reports may be saved separately from the frozen corpus.

```sh
Rscript tools/reference/verify-arma-fits.R
# Use a previously compiled development tree:
Rscript tools/reference/verify-arma-fits.R --source
# Numerical qualification only, without the repeated timing phase:
Rscript tools/reference/verify-arma-fits.R --no-timing
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

## Runtime comparison

After numerical qualification passes, the default command times the **same 64
main-matrix cases** for native, JD+ X-13 preprocessing and JD+ TRAMO preprocessing.
The four persistent controls and future-context perturbation fits are excluded
from timing totals so every engine has the same workload. Two scopes are reported:

- `fit_only`: native optimization on a prepared objective, or a direct JD+
  preprocessing call on prepared specifications, context and selected series.
- `with_setup`: input/specification/context preparation plus the same fit call.

These are R API measurements, including conversion and result construction.
JD+ preprocessing returns additional outputs, and the native prepared objective
already contains transformed data. The second scope includes that native
preparation cost. Neither scope times a complete native seasonal-adjustment
pipeline, which is still under implementation.

Every case, engine and scope receives a warm-up before **three measured rounds**.
Short calls are batched toward at least 50 ms per measurement (at most 1,000
calls), with the actual count and elapsed duration retained. Engine order rotates
across cases/rounds; scope order alternates. The clock is R's elapsed
`proc.time()`. R garbage collection occurs before each batch outside the clock;
natural R/JVM collection inside a batch remains included. No JVM collection is
forced. Each batch's final fit must retain a finite qualified objective, and
native convergence is required.

For each round, sum the 64 measured per-fit averages; report the median and
minimum/maximum of those round totals. `mean_ms_per_fit` divides the median total
by 64. `native_over_engine` is native time divided by that engine's time:
**values above one mean native is slower**. Ratios are observational and are not
performance acceptance thresholds.

Native package loading, reference-wrapper/JVM initialization, pin verification
and warm-up are recorded separately. These startup phases run sequentially in
one process and share R dependencies; they are not independent cold-start
benchmarks. Installed-package timing is preferred; `--source` records development
loading explicitly. Metadata includes versions, library/script hashes, OS,
numerical libraries, thread settings and the native OpenMP runtime probe.

To retain raw rounds, aggregate and per-model tables, and metadata, use an existing
empty output directory outside the frozen corpus:

```sh
mkdir -p /tmp/persephone-fit-times
Rscript tools/reference/verify-arma-fits.R \
  --timing-repeats=3 --timing-min-seconds=0.05 \
  --timing-output=/tmp/persephone-fit-times
```

The files are `raw.csv`, `summary.csv`, `by-model.csv` and `metadata.R`. Existing
reports are never overwritten. Machine load, warm-up/JIT behavior and output
scope affect results; compare the recorded spread and configuration alongside
any ratio.

### Recorded local run — 2026-11-13

The installed native implementation at `9cd42b3` was measured on macOS arm64,
R 4.5.2 and pinned JD+ 3.9.0 / Java 27.0.1.0.2. Native OpenMP requested and
probed two workers; Accelerate/BLAS thread limits were set to one. Three rounds
produced 1,152 measured batches / 14,092 fit calls. All batches met the 50 ms
duration target. The original 144 direct fits / 1,517 numerical checks also pass.

| API / scope | Median total for 64 fits | Round min–max | Mean per fit |
| --- | ---: | ---: | ---: |
| Native, prepared fit | 9.006 s | 9.005–9.015 s | 140.71 ms |
| JD+ X-13, prepared fit | 0.195 s | 0.193–0.197 s | 3.04 ms |
| JD+ TRAMO, prepared fit | 0.181 s | 0.181–0.185 s | 2.83 ms |
| Native, setup + fit | 9.051 s | 9.050–9.067 s | 141.43 ms |
| JD+ X-13, setup + fit | 0.284 s | 0.282–0.290 s | 4.43 ms |
| JD+ TRAMO, setup + fit | 0.258 s | 0.255–0.262 s | 4.03 ms |

**Native currently takes 31.9× X-13 time and 35.1× TRAMO time including setup**
on this matrix (46.3× and 49.8× for prepared fit calls). The native monthly half
takes 7.920 s versus 1.130 s for its quarterly half. The largest native model
means are monthly airline (948.25 ms), mixed factors (576.75 ms), seasonal AR
(223.00 ms) and seasonal MA (207.75 ms). These measurements identify workloads
to profile; they do not identify which internal routine causes the gap.

Observed startup phases were 0.495 s for loading Persephone and 0.334 s for
loading reference wrappers/initializing the JVM, followed by 0.048 s for pin
verification. Warm-up calls took 19.284 s and discarded calibration retries
18.144 s. All are excluded from the table above.

Raw measurements and complete configuration are retained in
[raw.csv](timings/2026-11-13-arma-fit/raw.csv),
[summary.csv](timings/2026-11-13-arma-fit/summary.csv),
[by-model.csv](timings/2026-11-13-arma-fit/by-model.csv) and
[metadata.R](timings/2026-11-13-arma-fit/metadata.R).

Reproduce the thread configuration with an installed package and a fresh output
directory:

```sh
mkdir -p /tmp/persephone-fit-times
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 OMP_DYNAMIC=FALSE \
VECLIB_MAXIMUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
Rscript tools/reference/verify-arma-fits.R \
  --timing-output=/tmp/persephone-fit-times
```

## Next acceptance gate

Item 20c must qualify local observed/expected information, weak finite-sample
directions and joint ARMA/regression uncertainty. A transfer screen, a converged
first-order optimizer and agreement on this bounded matrix do not provide a
valid Hessian covariance or universal fitted-model parity. Forecast/backcast
uncertainty, seasonal adjustment and the native default switch remain later
items. Extend prospective cases and obtain platform evidence without changing
these fixtures to hide discrepancies.
