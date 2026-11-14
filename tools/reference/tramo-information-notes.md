# TRAMO covariance qualification

The maintainer selected accurate final-fit covariance by default and a
Java-free legacy comparison option on 2026-11-14. The historical study and
three explicit differences remain in [the decision record](tramo-information-decision.md).
No frozen fixture or general tolerance was changed.

`native_arma_information(..., method="tramo_residual")` computes the accurate
complete-residual Gauss-Newton covariance. Set `covariance="legacy"` and supply
`legacy_state` to replay a retained working Jacobian and damping in C++.
The [contract](../native-tests/tramo-information-contract.md) defines the six
plain numeric state fields and validation. Both runtime modes are Java-free.
The existing BFGS optimizer does not generate LM history; replay requires that
history explicitly and does not claim to reproduce a full legacy fit.

The independent accurate matrix covers 64 fixed-order quarterly/monthly cases,
complete/internal/endpoint missingness, log/identity transformations, windows,
fixed/free ARMA coefficients, free mean/drift and fixed/free regressors. Its
64-case dense oracle agrees within the declared `1e-6` covariance budget.
Closed AR(1), response-unit invariance and multiple step sizes provide separate
checks.

## Reproducing legacy comparisons

Run:

```sh
Rscript tools/reference/verify-tramo-information.R --source
Rscript tools/reference/verify-tramo-information.R --source --timing --output=/path/to/empty/directory
```

This optional reference tool requires the pinned rjd3/JD+ runtime and a JDK.
It compiles only `LegacyCovarianceProbe.java` into a temporary directory. The
observer delegates public optimizer and derivative calls; it implements no
optimizer. It captures the working matrix before the public processor rescales
it into likelihood information. The difference from the recorded Jacobian Gram
identifies retained isotropic damping; any remaining reconstruction defect is
reported. Negative roundoff-scale reconstructed damping is projected to zero
and counted in that defect, never silently ignored.

The public exported model contains interpolated observations, so the probe
restores the original internal missing mask before refitting. An observed refit
is a new optimizer history. Its covariance can differ from the original TRAMO
preprocessor even at effectively identical final coefficients. Both differences
and the replay error against the **observed refit** are reported. This prevents
misrepresenting a refit's working matrix as the original call's hidden state.

The additive `inst/tinytest/fixtures/native/tramo-legacy-states.R` contains 64
numeric states and their public refit covariance targets, used by mandatory
Java-free tests. These inputs contain no Java objects and the native function
accepts no covariance target. Tests also change the damping and verify that the
computed result changes.

The current public trace resolves the historical exceptions: the two complete
AR(1) cases retain approximately `0.01429259` and the quarterly complete log
MA(1) case `0.0008627486` of damping in determinant-normalized residual units.
The default removes these working-matrix terms and evaluates the final-fit
Jacobian afresh. Unit sensitivity therefore belongs to a retained numerical
working state, not to the intended Gauss-Newton covariance formula.

Prospective legacy replay tolerance remains `2e-5` relative Frobenius norm.
The 2026-11-14 report records source hashes, runtime, reconstruction defects,
refit-versus-original differences and measured timings. General score parity,
near-unit coverage and supported-platform qualification remain open.

Timing compares prepared native fitting **plus accurate ARMA covariance** with
prepared JDemetra TRAMO fitting and its legacy covariance, using three warmed
rounds and calibrated batches. It excludes setup, JVM startup and observer/state
capture. JDemetra also prepares other preprocessing outputs, so these are API
workloads, not isolated covariance kernels. The earlier 2026-11-13 optimizer-only
benchmark remains separate and has not been overwritten.

## Local results, 2026-11-14

The recorded run passes **1,217 checks**, with 64 original preprocessing calls
and 64 observed public processor refits. Maximum accurate-versus-dense covariance
error is **2.625e-9** relative; maximum Java-free legacy replay error is
**1.158e-6** against the observed refit's covariance. The largest retained-matrix
reconstruction defect is **4.304e-7**. None of these changes the `2e-5` budget.

For prepared fitting plus covariance, median totals across the 64 cases are
**52.1032 s native** and **0.19218 s JDemetra TRAMO** (about **271.1 times** as long).
Three warmed rounds and calibrated batches measured 4,935 calls including
calibration retries, with two native OpenMP workers. The accurate native path
adds repeated full residual projections; a performance profile is needed before
selecting optimizations. This is a correctness milestone, not a speed advantage.

The records are in [`information/2026-11-14-tramo/`](information/2026-11-14-tramo/).
`native-source-hashes.csv` identifies the uncommitted numerical sources used for
this run on parent `5b05da1`; the implementation commit subsequently records those
same sources. Timing includes no observer/state-capture work. The original
optimizer-only benchmark remains unchanged.
