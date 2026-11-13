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
| `src/optimizer.cpp`, `src/arma_optimizer.cpp`, their core headers and R/Rcpp boundary | Independently authored inverse-BFGS algebra, feasible Armijo halving and two-scale central/one-sided differences. [Xie, Byrd and Nocedal, sections 2–3](https://arxiv.org/pdf/1901.09063) supplies mathematical context for the standard update, descent conditions and error sensitivity; this implementation uses guarded Armijo updates and does not claim that paper's convergence theorem. No optimizer source is copied or translated. | Analytic quadratic/Rosenbrock minima, independent dense Gaussian scalar/joint/mixed/seasonal minima, estimated derivative errors, budgets, failure semantics, exact fixed coefficients and exhaustive callback/reuse tests. See `tools/native-tests/arma-optimizer-contract.md`. Pinned fitted-model parity, identification and parameter uncertainty remain separate gates. |

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
