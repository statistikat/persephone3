# Numerical implementation provenance

Evidence checked on 2026-11-13. This extends the original oracle inventory;
it does not rewrite that historical manifest or assert legal clearance.

## Identified published release

The installed reference contains 16 JD+ JARs with Maven coordinates
`eu.europa.ec.joinup.sat:jdplus-*:3.9.0`. Every one of their recorded SHA256
digests matches the corresponding checksum published by Maven Central. Exact
artifact names, hashes and primary-source URLs are recorded in
[provenance-jdplus-3.9.0.csv](provenance-jdplus-3.9.0.csv); they can be compared
offline with `inst/tinytest/fixtures/backend/oracle-manifest.R`.

The official [3.9.0 release](https://github.com/jdemetra/jdplus-main/releases/tag/v3.9.0)
was published on 2026-09-14. Its
[tag ref](https://api.github.com/repos/jdemetra/jdplus-main/git/ref/tags/v3.9.0)
points to annotated tag object `81c2358345aa0ff7d793a817f5b7cc9a3b6056b7`.
That [tag object](https://api.github.com/repos/jdemetra/jdplus-main/git/tags/81c2358345aa0ff7d793a817f5b7cc9a3b6056b7)
resolves to release commit **`cd50b84773c2bdbf7a067a497954bb6d46cdb81b`**.
GitHub reports the tag as unsigned. The previously inspected
`15f1821fa71a949f842a5891e7cd68c83187f76d` is a later development snapshot and
must not be used as the 3.9.0 reference revision.

The [Maven Central parent POM](https://repo.maven.apache.org/maven2/eu/europa/ec/joinup/sat/jdplus-main/3.9.0/jdplus-main-3.9.0.pom)
is byte-identical to [`pom.xml` at the release commit](https://github.com/jdemetra/jdplus-main/blob/cd50b84773c2bdbf7a067a497954bb6d46cdb81b/pom.xml):

| Evidence | Digest |
| --- | --- |
| Parent POM SHA256 | `6b8db0d16b9adb61a5d3409f97ac6d7602e45660754476e2874977ad1b5f8d88` |
| Parent POM Git blob SHA1, computed over `blob <byte-length>\0<contents>` | `ca8b5cc47430a2701fd229b15c3ea68fbee631c3` |

The POM declares version 3.9.0, Java 21 and build output timestamp
`2026-09-14T10:43:30Z`. This establishes the published Maven artifacts and
the associated upstream release source revision using checksums and release
metadata, beyond matching version strings alone.

It does **not** prove that rebuilding that source produces those exact JAR
bytes. No reproducible Java build, signed provenance attestation or signature
verification was performed. The original manifest remains the runtime artifact
pin. The four non-JD+ JARs in its 20-JAR inventory are outside this 16-artifact
mapping. Installed rjd3 wrappers have no `RemoteSha`/`RemoteRef` metadata, so
their exact source-build revisions remain unresolved; current upstream
development branches are not substituted for their installed versions.

## Licensing evidence and implementation policy

The release [LICENSE](https://github.com/jdemetra/jdplus-main/blob/cd50b84773c2bdbf7a067a497954bb6d46cdb81b/LICENSE)
identifies EUPL 1.2, and the release POM declares EUPL. Installed
`rjd3toolkit`, `rjd3x13`, `rjd3tramoseats` and `rjd3jars` likewise declare
`License: EUPL` in the captured package metadata. Their current official
[toolkit](https://github.com/rjdverse/rjd3toolkit/blob/develop/DESCRIPTION),
[X-13](https://github.com/rjdverse/rjd3x13/blob/develop/DESCRIPTION),
[TRAMO/SEATS](https://github.com/rjdverse/rjd3tramoseats/blob/develop/DESCRIPTION)
and [JAR package](https://github.com/rjdverse/rjd3jars/blob/develop/DESCRIPTION)
metadata also declare EUPL; these moving pages corroborate the declaration,
not the installed wrappers' exact source revisions.

Persephone currently declares MIT. The preferred approach is independently
authored numerical code based on identified mathematical references and
independent identities, with pinned JD+ outputs used for differential tests.
No Java implementation or JAR is copied into this repository by this work.
Inspecting upstream public API/metadata does not authorize translating its
implementation and assigning the translation a new license.

The installed Rcpp 1.1.2 metadata declares `GPL (>= 2)`, also stated by
[Rcpp's official DESCRIPTION](https://github.com/RcppCore/Rcpp/blob/master/DESCRIPTION).
Independent numerical authorship does not remove the need to review
Rcpp integration, any future numerical-library dependency, bundled headers,
notices, and the resulting distribution terms. This document records evidence
and the implementation approach; it does not decide legal compatibility or
replace maintainer/legal review.

For each new numerical component, record the mathematical source, conventions,
author and independent tests beside the implementation. If upstream source is
copied or translated later, separately record the exact files/revision,
copyright notices, applicable license and approved distribution approach.
Before release, resolve that distribution review and any remaining source-build
mapping needed for the implementation actually shipped. Release identification
and the present artifact checksums do not complete those gates.

## Authorship record by numerical module

These modules were authored for Persephone in the current implementation series;
the corresponding repository commits identify the changes and reviewers.

| Module | Authored approach and references | Validation boundary |
| --- | --- | --- |
| `src/linear_algebra.cpp`, `src/core/linear_algebra.hpp` and the Rcpp boundary | Thin checked calls to R-distributed LAPACK/BLAS through R's declared interfaces, including `DPOTRF`, `DPOTRS`, `DPOCON`, `DGELSY` and `DGEMM`. Factorizations were not translated or reimplemented. Interface guidance comes from [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html) and the official [Netlib DGELSY documentation](https://netlib.org/lapack/lapack-3.1.1/html/dgelsy.f.html). | Independent solve/residual/rank checks and R numerical comparisons exercise the adapter and error handling; they do not establish a new factorization implementation or a seasonal-adjustment engine. |
| `src/transforms.cpp`, `src/core/transforms.hpp` and the Rcpp boundary | Independently authored elementary `log`/`exp` transformations. The per-observation log-Jacobian is derived directly from `d log(y)/dy = 1/y`, giving `-log(y)` for a positive observed value. Missing observations have a separate mask. | Algebraic inverse/Jacobian checks, observed-positive-domain validation and overflow/underflow tests. Transformation selection and final estimator likelihood conventions remain separate work. |
| `src/state_space.cpp`, `src/core/state_space.hpp` and the Rcpp boundary | Independently authored stationary Kalman innovations and discrete Lyapunov equations. Initial covariance follows the stationary unconditional-covariance equations, with [Casals and Sotoca's primary paper](https://www.ucm.es/data/cont/docs/562-2013-11-11-fvinitEcoLet.pdf) as initialization context. State timing and AR/MA conventions are compared with the public [R Kalman interface](https://www.stat.ethz.ch/R-manual/R-devel/library/stats/help/KalmanForecast.html) and [R ARIMA documentation](https://www.stat.ethz.ch/R-manual/R-devel/library/stats/html/arima.html). | Tests call public R `makeARIMA`/`KalmanLike`/`arima` and independently assembled Gaussian covariance densities. Public R function bodies were also inspected to confirm conventions; no R or Java implementation was copied or translated into this kernel. The current pilot evaluates fixed stationary parameters, not diffuse initialization, parameter estimation, automatic selection or seasonal adjustment. |
| `src/diffuse_state_space.cpp`, `src/core/diffuse_state_space.hpp` and the Rcpp boundary | Independently authored scalar exact diffuse filtering from the finite/infinite covariance expansion; Joseph-form covariance updates and a checked LAPACK orthogonal projection of the diffuse loading. The mathematical reference is [Helske (2017), sections 2.1 and Appendix A](https://www.jstatsoft.org/article/view/v078i10). Integrated SARIMA is constructed by adjoining initial observation-history states to the verified stationary ARMA model. | Random-walk/differencing identities and a separately assembled Gaussian density integrated over initial-state coordinates check the normalization. Public KFAS and pinned JD+ APIs provide additional independent comparisons. No upstream numerical implementation is copied or translated; covariance scaling and effective sample conventions are explicit in `tools/native-tests/diffuse-state-space-contract.md`. |
| `src/core/exact_history.hpp` | Independently derived rank certificate for the homogeneous solutions of `(1-B)^d(1-B^s)^D`, for `d,D <= 2`: seasonal polynomial columns plus at most two global monomials. Divided differences reduce the latter to a constant and the sum of their time knots. Mathematical identities: [NIST DLMF 3.3.35](https://dlmf.nist.gov/3.3.E35) and [3.3.38](https://dlmf.nist.gov/3.3.E38). Checked integer counts/sums certify dependence without altering the filter's diffuse coordinates or determinant. | Independent homogeneous-design SVD ranks, integrated Gaussian densities, registered gap fixtures, pinned JD+ calls and checked-overflow tests. This certifies algebraic rank for the stated differencing family; floating covariance propagation and weak-observability guards remain separately enforced. |
| `src/sparse_history.cpp`, `src/core/sparse_history.hpp` | Independently derived factored homogeneous functions spanning the observed-null history subspace. Thin QR normalizes their values at the original history times, so current functions times the inverse triangular factor preserve the original Euclidean diffuse measure. The interpolation identities above and [Netlib DGEQRF's documented factorization](https://netlib.org/lapack/explore-html/d0/da1/group__geqrf_gade26961283814bb4e62183d9133d8bf5.html) supply the mathematical/API references; LAPACK is called, not reimplemented. | Independent SVD ranks, canonical integer-history determinants and differenced Gaussian integration test sparse prefixes and later identification. Initial-frame orthonormality, conditioning and unchanged projection guards remain enforced. |
| `src/finite_state_factor.cpp`, `src/core/finite_state_factor.hpp` | Independently authored factor form of the Joseph and prediction identities: concatenate transformed/noise loadings and compress through QR. Initial positive-semidefinite covariance is factored with R's LAPACK [DSYEV interface](https://netlib.org/lapack/explore-html/d8/d1c/group__heev_ga8995c47a7578fef733189df3490258ff.html), preserving all positive eigenvalues and checking reconstruction. No numerical library implementation was copied. | Analytic covariance identities, singular-factor cases, interruption/reuse tests, independent Gaussian likelihood and conditional-moment checks, and pinned fixed preprocessing references. Finite-covariance roundoff handling is separate from exact diffuse-rank certification. |
| `R/native_likelihood.R` | Independently authored inclusive period selection, fresh-prior input assembly and explicit raw/adjusted fixed-model result contract. The log adjustment selects existing `-log(y)` terms using a calendar-window rule derived from direct pinned JD+ public API probes, including individual-observation perturbations. No upstream numerical implementation was copied. | Proper log-Gaussian and log-random-walk identities, exact domain/missing-mask checks, serialization and malformed-input tests. Both pinned preprocessors validate the bounded no-regression direct-window convention; their estimation selectors can differ and are not presented as equivalent. See `fixed-likelihood-notes.md` and `tools/native-tests/fixed-likelihood-contract.md`. |
| `src/regression_likelihood.cpp`, `src/core/regression_likelihood.hpp`, the Rcpp boundary and `R/native_regression.R` | Independently derived linear propagation through the existing exact-diffuse filter's gains, followed by concentrated Gaussian GLS. Checked calls to LAPACK `DGELSY` and `DGEQRF` perform factorizations; unit covariance is derived as `R^-1 R^-T`. ML scale is `Q/n`, separate from residual-df covariance. Mean/drift columns follow the polynomial differencing identity stated in the contract. No Java/R algorithm implementation was copied or translated. | Independent OLS and dense Gaussian integration over missing coordinates, seasonal-MA identities, explicit rank/scale/range boundaries, serial/OpenMP equality and pinned public preprocessing APIs. See `regression-notes.md` and `tools/native-tests/regression-likelihood-contract.md`. ARMA parameters remain fixed; this does not qualify estimated-ARMA uncertainty, forecasts or seasonal adjustment. |
| `src/core/parallel.hpp`, `configure` and the primitive/regression worker loops | Independently authored optional OpenMP execution of disjoint output coordinates and whitening channels. Compilation and runtime probing use R's declared OpenMP flags; no numerical formula or summation order is changed. Workers operate on owned C++ data and rejoin before exceptions or R callbacks. | Real-worker/serial equality, deterministic worker-error propagation, main-thread interruption/reuse, nested suppression, serial fallback and ASan/UBSan. See `tools/native-tests/openmp-contract.md`. Installed BLAS/LAPACK threading is independent of this helper; remote platform CI remains unverified. |
| `src/arma_parameters.cpp`, `src/arma_objective.cpp`, their core headers and R/Rcpp boundaries | Independently authored Levinson forward/inverse polynomial recursions, with [Halliday and Boshnakov (2022), section 2, equations 3–6](https://arxiv.org/pdf/2208.05055) as the mathematical source. MA signs are negated to use the same polynomial test; seasonal factors are checked separately. The objective reuses the existing concentrated regression likelihood without a parameter-transform Jacobian. | Independent `polyroot`, the AR(2) stability triangle and dense Gaussian AR/MA profile calculations, exact fixed-state checks, failure statuses and cancellation/reuse. No upstream numerical source was copied or translated. The strict-invertibility/mixed-block contract differs from R's documented optimizer policy and is not a JD+ optimization-parity claim. See `tools/native-tests/arma-objective-contract.md`. |
| `src/optimizer.cpp`, `src/arma_optimizer.cpp`, their core headers and R/Rcpp boundary | Independently authored inverse-BFGS algebra, feasible Armijo halving and two-scale central/one-sided differences. [Xie, Byrd and Nocedal, sections 2–3](https://arxiv.org/pdf/1901.09063) supplies mathematical context for the standard update, descent conditions and error sensitivity; this implementation uses guarded Armijo updates and does not claim that paper's convergence theorem. No optimizer source is copied or translated. | Analytic quadratic/Rosenbrock minima, independent dense Gaussian scalar/joint/mixed/seasonal minima, estimated derivative errors, budgets, failure semantics, exact fixed coefficients and exhaustive callback/reuse tests. The bounded pinned-fit matrix is recorded in `arma-fit-notes.md`; finite-sample identification and parameter uncertainty remain separate gates. |
| `src/arma_identification.cpp`, its core header and R/Rcpp boundary; `singular_values()` in `src/linear_algebra.cpp` | Independently derived derivative numerators for `H=M/A`, with exact polynomial products and free-coefficient columns. Checked calls to LAPACK DGESVD compute a column-normalized singular spectrum; no SVD implementation is reproduced. [McLeod (1999)](https://arxiv.org/abs/1611.01387) supplies context for common factors and singular ARMA information, not an implementation or a theorem about arbitrary sampled regression models. | Closed ARMA(1,1) identities, independent polynomial perturbations and R SVD, seasonal/cross-factor/fixed-free screens, near-unit/weak-regression examples and exhaustive cancellation/reuse. See `tools/native-tests/arma-identification-contract.md`. Full transfer rank does not certify finite-sample identification or uncertainty. |

Slice 20c.1a adds independently authored three-scale central differences and
Richardson/error checks in `information.cpp`, observed-information eigenanalysis
through LAPACK `DSYEV`, and physical-coefficient conditional/profile covariance
in `arma_information.cpp`. [Baker (2021)](https://arxiv.org/abs/2105.04829)
provides context for analytic validation of numerical-Hessian covariance; its
algorithm was not copied. Gaussian Taylor/Schur-complement identities and dense
likelihoods independently test the implementation. Public pinned X-13 outputs
identify the conditional-regression residual-df convention. TRAMO outputs are
explicitly recorded as a separate unresolved convention. No upstream numerical
implementation was inspected, copied or translated for this slice. See
[the information contract](../native-tests/arma-information-contract.md) and
[reference notes](arma-information-notes.md).

Slice 20c.1b.1 adds a prospective dense Gaussian residual-information oracle in
`arma-residual-oracle.R`. Cholesky whitening, GLS, missing-coordinate projection,
determinant normalization and Gauss-Newton covariance are independently derived
from the existing Gaussian model. Closed AR(1) whitening/GLS derivatives and
unit-invariance identities provide independent analytic checks. Pinned public
wrapper outputs and fixed-parameter residual calls supply differential evidence;
no numerical implementation is copied. This is validation tooling, not native
production code or an accepted TRAMO covariance parity contract. The explicit
compatibility decision is recorded in `tramo-information-decision.md`.

Slice 20b.2b.2 also uses the elementary linear identity
`L(y-X*b0)=L(y)-L(X)*b0` to center the regression response before whitening,
with an ordinary least-squares `b0` computed by the existing DGELSY adapter.
This independently authored conditioning step retains the original GLS contract;
its numerical motivation and validation are in `arma-fit-notes.md`.

The detailed state-space equations, sign conventions, initialization, numerical
limits and oracle interpretation are recorded in
[state-space-contract.md](../native-tests/state-space-contract.md). This is an
authorship and reference record, not a claim of a formally isolated clean-room
process. The dependency/distribution review above still applies.

## Rechecking the evidence

1. Compare CSV `installed_sha256` values with the original manifest's matching
   package-relative paths; no original artifact should change.
2. Read each CSV `checksum_url` and compare the complete SHA256 string.
3. Resolve the GitHub tag ref and annotated tag object above; compare the full
   commit SHA, not a branch head.
4. Hash the published parent POM and compare its Git blob ID with the pinned
   source-file metadata. Keep byte identity separate from a reproducible-build
   claim.

The new `extended-v1` fixtures use the existing runtime oracle and synthetic
formulas. Their own manifest records the parent artifact hashes and generation
scripts. They neither update the original 12-case corpus nor qualify a native
algorithm merely by adding reference examples.

Slice 20c.1b.2 independently implements complete residual projection, QR missing
coordinate volume, three-scale residual Jacobians and Gauss-Newton covariance.
The existing dense Gaussian/closed AR(1) identities supply its mathematical
validation. Java-free legacy covariance replay independently assembles
`Qz/(n-r-k) * inverse(Jw'Jw + mu I)` from explicit numeric optimizer state.
Public `RegSarimaComputer`, `SsqFunctionMinimizer` and derivative interfaces were
inspected with `javap -public` and observed through a small delegating test
class. The observer preserves the original residual-square summation, captures
the retained matrix before downstream rescaling and reports the residual defect
in its isotropic-damping reconstruction. No upstream numerical implementation
was read, copied or translated for these modules. Exploratory runtime field
inspection identified damping/scale constants; production code does not embed
those constants or emulate the optimizer's control flow. Recorded state is not
a covariance lookup. See [the TRAMO contract](../native-tests/tramo-information-contract.md)
and [the comparison notes](tramo-information-notes.md).

Slice 20c.2a.1 independently assembles conditional GLS coefficient covariance
from the existing QR unit covariance and `Q/(n-r-k)` in `arma_information.cpp`.
It reuses the qualified final-fit record and explicitly does not propagate ARMA
parameter uncertainty. Independent dense Gaussian joint projection and closed
OLS formulas validate the algebra; 64 direct fits per public preprocessing
engine qualify the denominator and free/fixed directions. No new upstream
implementation was inspected or copied. See the
[regression information contract](../native-tests/regression-information-contract.md).

Slice 20c.2a.2 derives joint profile-ML covariance from the Gaussian GLS
first-order condition and block-Hessian inversion. `vector_jacobian.cpp` and
`joint_regression_information.cpp` are independently authored; no upstream
numerical implementation was consulted or translated. Correction-response GLS
and dimensionless Richardson derivatives produce the sensitivity block. An
independent full Gaussian Hessian and closed AR(1) formulas check the joint
matrix. Public X-13/TRAMO calls supply timing comparisons, not joint-covariance
targets; their conditional covariance is qualified separately in 20c.2a.1.

Slices 20c.2b.1a–b independently derive accurate chronological innovations,
complete-span Gaussian interpolation, and optional legacy reporting conventions.
Only public model/filter APIs and public signatures informed the reporting
comparisons; no upstream numerical source or bytecode was copied, translated or
decompiled. Legacy filling, differencing parity, unpivoted Householder projection
and scalar AR(1) fast-innovation rounding are independently authored. The
reference generator records 404 public fits; its 30 AR(1) numeric-output fixtures
also run without Java. Accurate calculations use independent dense Gaussian and
intrinsic-conditioning oracles. See `residual-notes.md` and
`tools/native-tests/arma-residuals-contract.md` for the distinction between
accurate results and comparison-only reported outputs.

Slice 20c.2b.2a derives missing-coordinate prediction covariance by Gaussian block
conditioning: QR solves and a Gram matrix separate known-regression uncertainty
from the fitted-regression increment. The native algorithm is independently
authored. An independent intrinsic precision/Schur-complement oracle and closed
AR(1), OLS and seasonal-bridge formulas check the full covariance. Only public
missing-value standard-error accessors supply differential evidence. No legacy
point, upstream numerical source or bytecode enters the calculation. See
`interpolation-notes.md` and `tools/native-tests/interpolation-information-contract.md`.

Slice 20c.2b.2b independently propagates accurate profile-ML ARMA covariance
through the derivative of refitted conditional Gaussian points. Centered
correction responses, homogeneous diffuse levels, dimensionless Richardson
derivatives and a Gram-matrix increment implement the elementary first-order
delta formula. An independent dense Gaussian Hessian and Schur-complement point
derivative, together with closed AR(1) formulas including fitted-mean effects,
qualify the result. Public JDemetra calls provide fitting/timing comparisons,
not propagated-covariance targets. No upstream numerical implementation or
bytecode was inspected, copied or translated. See `interpolation-notes.md` and
`tools/native-tests/interpolation-information-contract.md`.

Slice 20d.1 independently derives conditional endpoint points and joint
backcast/forecast covariance by extending Gaussian missing-coordinate projection
to explicitly requested endpoint domains. Original-window GLS/scale remains
separate. Independent dense intrinsic precision/Schur calculations and closed
AR(1), white-noise mean and random-walk drift formulas qualify the result. Only
public Forecasts signatures/accessors inform reporting comparisons; their
backcast/scale discrepancies are retained separately. No upstream numerical
source or bytecode was inspected, copied or translated. See `forecast-notes.md`
and `tools/native-tests/forecast-information-contract.md`.

Slice 20d.2a independently implements the elementary Gaussian/lognormal moment
identities in logarithmic form, with signed cross covariance, expm1, separate
range statuses and independently evaluated standard deviations. Closed formulas
and extreme cancellation cases qualify the arithmetic; native model matrices
qualify domains and retained transformed outputs. Public Forecasts accessors
show that reported log errors omit fixed regression effects; raw-location and
fully restored-location comparisons are recorded separately. No upstream
numerical source or bytecode was consulted or translated. See
`tools/native-tests/prediction-moments-contract.md` and `forecast-notes.md`.

### Endpoint ARMA uncertainty: item 20d.2b

Optional forecast/backcast uncertainty propagates the independently authored
accurate observed profile-ML ARMA covariance through refitted conditional
Gaussian points. Its shared delta assembly was factored from the already
qualified interpolation implementation without changing the arithmetic.
Independent dense covariance/SVD point stencils with OLS/null-space centering
and a full Gaussian Hessian qualify
the result; closed AR(1) formulas check both known and refitted means. No upstream
numerical source, bytecode or optimizer internals were used. Public X-13/TRAMO
endpoint accessors supply separately labelled timing workloads only, never
native runtime inputs or delta-covariance targets. The core remains Rcpp-free;
its existing R BLAS/LAPACK ABI linkage is unchanged.

### Symmetric X-11 filters: item 22a.1

`x11_filters.cpp` independently constructs centered seasonal-period trend
averages, seasonal uniform-average convolutions and the closed Henderson cubic
smoother. Mathematical sources and conventions are in
`tools/native-tests/x11-symmetric-contract.md`: ABS/Census documentation for
seasonal averaging and Alexandrov et al. (2008), section 4.2, for Henderson's
minimum-third-difference/cubic-reproduction criterion. An independent constrained
roughness solve, polynomial/seasonal identities and direct tap sums qualify the
implementation. Only public JDemetra filter factory, coefficient and application
APIs/signatures were used; no numerical source or bytecode was consulted.
The 114-case public fixture is additive and does not modify the original corpus.
Its raw inputs are deterministic formulas independent of backend output.
Both filter core files and their standalone harness compile and link without
R/Rcpp, BLAS or LAPACK. The package boundary still uses Rcpp. Asymmetric endpoints,
automatic selection, decomposition passes and full native SA remain separate.

### Explicit Musgrave Henderson endpoints (22a.2a)

`src/core/x11_musgrave.hpp` and `src/x11_musgrave.cpp` independently implement
constant-constrained minimum squared revision under a linear mean plus independent
irregular noise, using the mathematical formula in Findley et al. (1998), Appendix
A.2 (linked in `tools/native-tests/x11-musgrave-contract.md`). The I/C ratio is
explicit; reciprocal scaling protects large finite values, and the zero-ratio
limit has independent slope/least-distance qualification. Mirrored endpoint
application preserves central Henderson arithmetic, masks and the input calendar.
The algorithms require no R/Rcpp, BLAS or LAPACK linkage; the Rcpp binding owns
copies and performs R-specific validation. Existing backend linear algebra
linkage is unaffected.

Reference inspection was limited to public signatures, class inventory, factory
execution, coefficient/bound accessors, DataBlock filtering and the public bulk
`AsymmetricEndPoints` processor. No numerical source or bytecode was translated.
628 additive frozen endpoint definitions and an independent orthonormal-basis
least-squares oracle qualify the implementation. Public zero-ratio NaNs remain a
recorded discrepancy, not an accurate-mode target. The old symmetric/reference
corpora are immutable. Automatic ratio selection, short-series fallback and
seasonal/initial-trend policies remain separate work.

### Centered-trend mean endpoints (22a.2b.1)

`src/core/x11_mean_endpoints.hpp` and `src/x11_mean_endpoints.cpp` independently
compose arithmetic means of the first/last half-width observations, constant
virtual padding and the existing centered trend convolution. Inputs require at
least one half-width; overlapping boundary windows are explicit. The means use
normalized summation to avoid an overflowing unscaled sum. Missingness remains
mask-based and no original observation is replaced. The core links without
R/Rcpp, BLAS or LAPACK; only the adapter handles R input and metadata.

Public `FilteredMeanEndPoints` behavior was characterized by full-series and
basis-vector API calls, with signature/class-inventory inspection only. No
numerical source, bytecode or implementation constants were translated.
An independent dense extension/convolution operator and rational quarterly
control qualify 22 frozen public cases and their short-series behavior. Old
corpora/reports are unchanged. This explicit processor does not yet establish
its placement in B/C/D passes or any automatic policy.

### Explicit seasonal endpoints (22a.2b.2a)

`src/core/x11_seasonal_endpoints.hpp` and `src/x11_seasonal_endpoints.cpp`
independently implement the fixed seasonal filter definitions in published
Census/ABS documentation. Census Technical Paper 15, Appendix B, supplies the
method-defining 3×1/3×9 weights. The ABS 2001 introductory time-series course
supplies virtual-value constructions for 3×3/3×5; integer convolution derives
their rational coefficients. Exact references and the definitions are in
`tools/native-tests/x11-seasonal-contract.md`. A separate R oracle pads actual
annual values and convolves them for 3×3/3×5. No full-precision Musgrave formula
is substituted for the published fixed seasonal definitions.

Reference inspection used only public enum/factory/processor signatures and
execution. No numerical Java source, bytecode or implementation constants were
read or translated. 120 additive public cases pin endpoint operators and full
points; four short S3X1 controls record a separate stable-selection behavior.
Core code needs no R/Rcpp, BLAS or LAPACK linkage. Calendar/mask metadata belongs
to the adapter; native runtime never reads fixtures or calls the reference
backend. Existing corpora/reports are unchanged. Remaining 3×15 endpoints,
automatic selection and decomposition integration are separate work.

### Explicit 3×15 seasonal endpoints (22a.2b.2b)

The seasonal core now independently constructs 3×15 endpoint coefficients by
collapsing a symmetric convolution onto actual observations after extension
with the nearest five-observation mean. Integer arithmetic applies the
five-decimal/constant-preserving closure convention characterized by public
factory/processor basis probes. No coefficient table is copied into runtime.
An independently composed dense R extension/convolution matrix with decimal
closure qualifies all thirty public cases. The unrounded operator is retained
as a separately reported mathematical control, not substituted for the declared
filter convention. See `tools/native-tests/x11-seasonal15-contract.md`.

No numerical Java source, bytecode or implementation constants were consulted.
Thirty additive operator/point captures and twenty-four separate short-series
selection controls use only public calls. All prior frozen artifacts remain
unchanged. The native core stays Java/R/Rcpp/BLAS/LAPACK-free; owned OpenMP and
caller-only interruption semantics are unchanged. Automatic selection and
decomposition integration are separate qualification steps.

### Fixed seasonal processing and short-series policy (22a.2b.3a)

`x11_seasonal_processing.cpp` independently composes the qualified seasonal
definitions with stable phase means and explicit length-based selection.
Public enum/factory/processor execution established complete/partial-year gates,
available endpoint rows and phase-mean middle rows. A separate dense R oracle
using published weights/virtual constructions qualified all 756 prospective
cases before native implementation. No numerical Java source or bytecode was
consulted, copied or translated. Full native basis applications, public basis
responses and full mixed-phase points provide independent/differential evidence.

Mean calculations use normalized sums, with independent phase accumulators in
bounded OpenMP chunks that preserve serial order. All worker outputs are owned;
callbacks and the R adapter remain on the caller. This core/header links without
R/Rcpp, BLAS or LAPACK and reads no runtime fixtures. Metadata states requested
and selected filters, reasons and each row's operation. The low-level processor
policy is not a qualification of top-level `sfshort`, automatic MSR or B/C/D
integration; those remain separate. Existing strict explicit APIs and frozen
corpora are unchanged. See `tools/native-tests/x11-seasonal-processing-contract.md`.

### Henderson selection and I/C conventions (22a.2b.3b)

`x11_trend_selection.cpp` independently composes the previously qualified
Henderson pilot with absolute/relative component changes, explicit extension
trimming, quarterly scaling and threshold/default policies. Published Census
definitions and public API execution establish the conventions; no numerical
Java source, bytecode or implementation constants were read or translated.
An independent minimum-roughness/convolution R oracle qualified 288 regular
inputs before native implementation. The complete prospective corpus has 376
ratio cases, 20 threshold cases and 102 endpoint-default cases with public
coefficient probes. Native runtime reads no targets and requires no Java.

Core/header code remains independent of R/Rcpp, BLAS and LAPACK. Owned OpenMP
only covers independent rows; reductions and callbacks remain on the caller.
Undefined/missing/unsupported relative-component ratios retain explicit statuses
instead of selecting through NaN or signed-denominator behavior. Captures preserve
those public differences as controls. B/C/D placement remains unqualified; the
low-level final selector is not substituted for every intermediate stage.
See `tools/native-tests/x11-trend-selection-contract.md` and
`tools/reference/x11-trend-selection-notes.md` for the exact supported scope.

### Staged seasonal request policy (22a.2b.3c.1)

`x11_seasonal_plan.cpp` is an independently authored bounded policy for fixed,
X11Default and MSR requests. The documented stage sequence and public validated
spec/context getters establish initial/final mappings and global MSR eligibility.
336 frozen contexts cover request forms, calendars, spans and modes; no numerical
Java source, bytecode or implementation constants were consulted. Runtime does
not read reference targets or invoke Java.

The all-MSR final stage deliberately stays unresolved until data-driven MSR
selection is implemented; provisional 3×5 mappings are exposed separately from
resolved filters. Mixed requests preserve the public context's disabled-global-MSR
semantics. This policy has at most twelve requests and needs no OpenMP or
callback. Core/header code links without R/Rcpp, BLAS or LAPACK. Numerical
passes and their stage placement remain separate qualification work.
