# Native numerical foundation checks

The C++ core is independently authored and contains no translated JDemetra+
source. It currently supplies time indexing, differencing polynomials,
convolution, finite filters, transformations, LAPACK adapters and a stationary
fixed-parameter innovations likelihood, with a separate exact diffuse kernel
for integrated SARIMA models, plus concentrated regression likelihood with
fixed ARMA parameters. A bounded fixed-order ARMA optimizer builds on these
kernels. It does **not** fit seasonal-adjustment models.

Run from the repository root with any supported C++17 compiler:

```sh
c++ -std=c++17 -Wall -Wextra -pedantic -Isrc src/numerics.cpp tools/native-tests/test_numerics.cpp -o /tmp/persephone-numerics-test
/tmp/persephone-numerics-test
```

On a toolchain supporting AddressSanitizer and UndefinedBehaviorSanitizer:

```sh
c++ -std=c++17 -fsanitize=address,undefined -fno-omit-frame-pointer -g -Isrc src/numerics.cpp tools/native-tests/test_numerics.cpp -o /tmp/persephone-numerics-sanitized
/tmp/persephone-numerics-sanitized
```

The installed-package tests in `inst/tinytest/test-native-primitives.R` exercise
the Rcpp boundary and compare filters with independent R operations. Missing
input and out-of-range active filter taps produce an explicit mask and `NA`
outputs; zero-weight taps do not propagate missingness. No endpoint extension
is performed. A nonfinite coefficient or arithmetic overflow is an error.

## Linear algebra (19b)

`src/core/linear_algebra.hpp` owns column-major memory without Rcpp types.
The implementation uses R's declared Fortran interfaces to DPOTRF/DPOTRS,
DPOCON, DGELSY and DGEMM, linked by the package's portable Makevars flags.
No LAPACK implementation is copied. Cholesky reports a 1-norm reciprocal
condition **estimate** and accepts symmetry differences only within 64 machine
epsilons of the largest matrix entry, averaging accepted pairs.

Pivoted QR uses the caller's positive rank threshold (default `1e-10`) and
returns minimum-norm solutions for rank-deficient or underdetermined systems.
Coefficients retain original column order; pivots are diagnostic. Residuals
are recomputed as `B - A %*% coefficients`. Rank is an estimated numerical
rank, not a promise of coefficient identifiability; no covariance or QR
condition estimate is fabricated. Zero matrix dimensions, missing/nonfinite
values, integer-range overflow, and non-SPD Cholesky inputs are explicit errors.

`inst/tinytest/test-native-linear-algebra.R` compares with independent analytic,
R solve and SVD cases. `test_linear_algebra.cpp` adds storage/factor corruption,
bounds and interruption/reuse checks outside Rcpp. Sanitizing these wrappers
does not instrument the prebuilt R BLAS/LAPACK libraries themselves.

For a Unix R installation, the standalone linear-algebra harness can be run
with its installed LAPACK/BLAS (override `CXX` for your C++17 toolchain):

```sh
${CXX:-c++} -std=c++17 -Wall -Wextra -Wconversion -pedantic \
  -fsanitize=address,undefined -fno-omit-frame-pointer -g -Isrc \
  $(R CMD config --cppflags) src/linear_algebra.cpp \
  tools/native-tests/test_linear_algebra.cpp \
  $(R CMD config LAPACK_LIBS) $(R CMD config BLAS_LIBS) \
  $(R CMD config FLIBS) $(R CMD config --ldflags) -o /tmp/persephone-linear-algebra
/tmp/persephone-linear-algebra
```

## Transforms and regressor domains (19c)

The elementary `none`/`log` transformations preserve explicit missing masks.
Log requires positive observed values and returns each observation's log
Jacobian (`-log(y)`), so a future likelihood can select the correct estimation
span before summation. Inverse log is pointwise exp; it does not apply a
lognormal forecast-mean adjustment. Nonfinite input or exponential overflow/
underflow is rejected. Internal Rcpp transformation defaults select `none`.

`native_prepare_inputs()` validates a monthly/quarterly univariate `ts` and
a uniquely named list of regressor series or an `mts`. It aligns by integer
period, requires finite regressors throughout the entire requested observed,
backcast and forecast domain, and crops only unused extra supplied periods.
Missing responses are allowed; missing regressors, padding, recycling and
implicit extrapolation are not. Output is a serializable plain-data contract.
Estimation spans and coefficient constraints are connected by 20a.3/20b.1 below;
context-object conversion and automatic transformation selection remain later work.

The standalone transform harness uses no R headers or libraries:

```sh
c++ -std=c++17 -Wall -Wextra -Wconversion -pedantic \
  -fsanitize=address,undefined -fno-omit-frame-pointer -g -Isrc \
  src/numerics.cpp src/transforms.cpp tools/native-tests/test_transforms.cpp \
  -o /tmp/persephone-transforms
/tmp/persephone-transforms
```

## Stationary likelihood (20a.1)

The fixed-parameter stationary state-space/ARMA pilot, mathematical conventions,
scale normalization, missing-data behavior and explicit numerical limits are
documented in [state-space-contract.md](state-space-contract.md).
The standalone `test_state_space.cpp` harness links `src/numerics.cpp`,
`src/linear_algebra.cpp` and `src/state_space.cpp` with the same R LAPACK/BLAS
flags shown above. It exercises callback cancellation at every boundary and
subsequent successful reuse. Installed R tests compare closed forms, an
independent multivariate Gaussian density, and R's public Kalman/ARIMA APIs.
These establish the stated kernel contract, not JD+ likelihood conventions or
full SARIMA/seasonal-adjustment parity.

## Exact diffuse likelihood (20a.2)

The internal `.native_diffuse_likelihood()` accepts finite and diffuse initial
loadings explicitly. `.native_sarima_likelihood()` constructs a monthly/quarterly
fixed-parameter SARIMA model with initial observation history treated diffusely.
The core keeps finite and diffuse uncertainty separate throughout filtering; no
arbitrary large initial variance is used. Its initial-state coordinate measure,
effective sample count and numerical rejection rules are documented in
[diffuse-state-space-contract.md](diffuse-state-space-contract.md).

At most 32 total states are supported, including at most 16 stationary ARMA
states. Ordinary and seasonal differencing orders range from zero to two,
subject to that total bound. The common monthly airline model uses 27 states.
The scalar mean for integrated models stays zero; 20b.1 below supplies drift as
an explicit regressor. Unresolved diffuse states produce
an explicit unavailable likelihood, not a successful objective for optimization.

`test-native-diffuse.R` uses closed forms, differencing and an independent joint
Gaussian calculation integrated over the initial-state coordinates. Optional
external oracle scripts exercise public KFAS and pinned JD+ APIs; neither
package is required by the native kernel or its regular test suite. The
standalone sanitizer harness is `test_diffuse_state_space.cpp`; compile it with
`numerics.cpp`, `linear_algebra.cpp`, `state_space.cpp`, `sparse_history.cpp`,
`finite_state_factor.cpp`,
`diffuse_state_space.cpp` and the R LAPACK/BLAS flags above.

## Fixed-model windows and transformations (20a.3)

`native_fixed_likelihood()` selects an explicit inclusive estimation window,
prepares identity/log inputs and evaluates the fixed SARIMA model with a fresh
prior at that window's start. It preserves domains, missing positions and row
maps, and reports transformed and adjusted objectives separately. The log
adjustment follows the named pinned JD+ direct-window calendar convention;
it is not inferred from diffuse pivot positions. See
[fixed-likelihood-contract.md](fixed-likelihood-contract.md) and the independent
identities in `inst/tinytest/test-native-fixed-likelihood.R`. Regression and drift
are added through the separate 20b.1 contract below; ARMA estimation and native
seasonal adjustment remain unavailable.

## Optional OpenMP (19d)

Independent filter, convolution and transformation outputs may use bounded
OpenMP regions when the C++17 compile/link/runtime probe succeeds. Serial
fallback is automatic. `PERSEPHONE_NUM_THREADS` requests 1..64 threads (default
two), capped by the OpenMP runtime. See [openmp-contract.md](openmp-contract.md)
for thread safety, deterministic arithmetic, cancellation and standalone checks.

## Fixed-SARIMA regression (20b.1)

`native_regression_likelihood()` connects named fixed/free regressors, estimated
stationary mean or integrated drift, shared-gain whitening, rank-aware GLS and
explicit ML/residual-df covariance scales. It retains window/log/context semantics
and reports nonidentifiable coefficients without dropping columns. The exact
contract and numerical limits are in
[regression-likelihood-contract.md](regression-likelihood-contract.md).

Compile `test_regression_likelihood.cpp` with `numerics.cpp`,
`linear_algebra.cpp`, `state_space.cpp`, `sparse_history.cpp`,
`finite_state_factor.cpp`, `diffuse_state_space.cpp`, `regression_likelihood.cpp`
and the R LAPACK/BLAS flags above. Run both serial and `-fopenmp` builds where
supported; `PERSEPHONE_REQUIRE_OPENMP=1 OMP_NUM_THREADS=4 OMP_DYNAMIC=FALSE`
requires actual workers in the latter. The harness checks identical coefficients,
whitening, covariance and likelihood, plus main-thread cancellation/reuse.

## ARMA parameter/objective contract (20b.2a)

The internal parameter map uses bounded reflections for fully free factors and
checked coefficient coordinates for mixed fixed/free factors. A reusable plain
R objective contract evaluates the existing concentrated regression likelihood
at one admissible proposal, with explicit unavailable/failure statuses.
See [arma-objective-contract.md](arma-objective-contract.md) for signs,
constraints, numerical margins and the optimizer boundary.

`test_arma_parameters.cpp` compiles with `src/arma_parameters.cpp` alone, without
R headers/libraries. `test_arma_objective.cpp` adds `src/arma_parameters.cpp` and
`src/arma_objective.cpp` to the regression harness's source/link list above.
Use the same strict-warning and ASan/UBSan options. Mandatory R tests compare
independent polynomial roots and dense Gaussian AR/MA objective profiles.

## Deterministic ARMA optimization (20b.2b.1)

`native_arma_optimize()` accepts a prepared objective and runs bounded C++ BFGS
with feasible backtracking and checked numerical derivatives. It retains fixed
parameters, explicit failure/convergence statuses and the final likelihood
without uncounted reevaluation. The contract, working-coordinate convention,
controls and remaining identification/reference gates are in
[arma-optimizer-contract.md](arma-optimizer-contract.md).

Compile `test_optimizer.cpp` with `src/optimizer.cpp` alone; neither needs R.
For `test_arma_optimizer.cpp`, add `src/optimizer.cpp` and
`src/arma_optimizer.cpp` to the objective harness source/link list above.
Both support the same strict-warning/ASan/UBSan workflow. The R suite uses
independent dense Gaussian minima for scalar, joint, mixed and seasonal models.

## Fitted-model qualification and transfer sensitivity (20b.2b.2)

The [fit qualification notes](../reference/arma-fit-notes.md) describe 64
monthly/quarterly cases, persistent-AR controls, independent density minima,
alternate starts and pinned direct comparisons. These exposed a precision loss
with large drift terms; affine regression centering before whitening resolves
it without changing the model or optimizer tolerances.

`native_arma_identification()` and the optimizer's `parameter_diagnostics`
screen exact/near parameter redundancy using an algebraic transfer Jacobian and
column-normalized SVD. The [diagnostic contract](arma-identification-contract.md)
distinguishes this structural screen from finite-sample identification and
uncertainty. Compile `test_arma_identification.cpp` with `numerics.cpp`,
`linear_algebra.cpp`, `arma_parameters.cpp`, `arma_identification.cpp` and the
R LAPACK/BLAS flags above; strict warnings and ASan/UBSan cover all callbacks.

## Observed information and uncertainty

`native_arma_information()` computes physical-coefficient observed curvature
separately from optimization, with labelled full-profile ML and fixed-regression
residual-df covariance. The [information contract](arma-information-contract.md)
defines accuracy, structural, stationarity and curvature gates. Independent
Gaussian tests exercise all 64 fitted cases and nuisance Schur complements.
Standalone `test_information.cpp` and `test_arma_information.cpp` cover analytic
identities, signed/weak/rank-deficient information and exhaustive cancellation.
The [pinned comparison](../reference/arma-information-notes.md) qualifies the
X-13 convention; the separate TRAMO covariance contract is described below.

### TRAMO residual information and legacy replay

`test-native-tramo-information.R` and `test-native-tramo-legacy.R` exercise the
accurate default and explicit numeric-state replay without Java. The independent
dense oracle and additive recorded states are installed under `fixtures/native`.
`test_residual_information.cpp` adds analytic residual-information, rank/domain,
budget and interruption tests; `test_arma_information.cpp` also covers both TRAMO
modes. Include `src/residual_information.cpp` in the existing ARMA harness link.
See [the complete contract](tramo-information-contract.md).

### Estimated-ARMA regression uncertainty

`test-native-regression-information.R` checks conditional GLS inference against
independent dense covariance blocks across the 64 fitted cases and boundary/unit
changes. `test_regression_information.cpp` independently checks closed OLS
covariance, residual df, range handling and cancellation. Link the same source
set as `test_arma_information.cpp`. The
[contract](regression-information-contract.md) distinguishes this conditional
covariance from the joint profile-ML estimator.

`test-native-joint-information.R` now covers the optional joint profile-ML
estimator against full independent Gaussian Hessians. The standalone
`test_vector_jacobian.cpp` checks analytic vector derivatives, and
`test_joint_regression_information.cpp` checks closed AR(1) GLS derivatives,
full joint curvature, budgets, conditioning and exhaustive cancellation. Add
`src/vector_jacobian.cpp` and `src/joint_regression_information.cpp` to the ARMA
harness source list. See the same regression-information contract for the
Schur identity, estimator distinction and versioned timing procedure.

## Initial additive X-11 tables

The standalone `test_x11_initial.cpp` links the C++ numerical/filter sources
listed in [x11-initial-contract.md](x11-initial-contract.md), without R/Rcpp,
BLAS or LAPACK. Serial and owned-OpenMP sanitizer runs exercise closed additive
controls, masks/ranges, caller-only interruption and reuse. Mandatory R tests
add public intermediate-table comparisons, independent complete operators,
calendars, missing-support audits and serialization. These are initial B tables;
they do not expose a full native seasonal-adjustment fit.
