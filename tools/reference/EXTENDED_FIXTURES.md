# Extended reference suite v1

This additive suite is stored in
`inst/tinytest/fixtures/backend/extended-v1/`. The original 12 fixtures,
manifests, checksums and generator remain unchanged. The new suite pins those
parent files by SHA256 and refuses an oracle change.

| Cases, each for X-13 and TRAMO/SEATS | Input and specification | Independent checks |
| --- | --- | --- |
| `12-mixed-ls-tc` | Original monthly synthetic formula plus a level shift of 20 from 2006-01 and a temporary change of `30 * 0.7^k` from 2010-01; fixed LS coefficient 20 and freely estimated TC | Fixed/free metadata and fixed coefficient value, covariance dimension covering estimated coefficients only, original values and additive component identity |
| `4-missing-internal` | Original quarterly synthetic formula with observations 20, 21 and 44 set to NA; fixed-order `rsa0` | Exact input mask, preserved observed values, complete finite SA domain and component identity at observed positions |
| `12-external-context` | Monthly synthetic formula plus two deterministic regressors; coefficient 3 fixed and the other freely estimated; effects assigned to trend | Complete context formulas and future coverage, coefficient states, estimated covariance dimensions |
| `12-external-future-shift` | Identical observations/specification; increase the fixed regressor by 2 only after the observed domain | Unchanged fitted preprocessing and exactly `3 * 2 = 6` increase in original-series forecasts, within elementary floating-point roundoff |

There are eight cases and 90 invariant checks. Each RDS stores its complete
input, ordinary-R context, specification, requested output names, unmodified
direct-backend result and captured fit warnings. Generation calls the rjd3
constructors/setters and fast engines directly, without loading Persephone or
using its accessors/adapter. The formulas are in `extended-fixtures.R`; no RNG,
external dataset, or copied Java implementation is involved.

The missing-data case records an actual method distinction: X-13 returns an
interpolated `user_defined$y`; TRAMO/SEATS retains the original NA positions in
that field. Both return finite seasonally adjusted values. The input missing
mask remains in the fixture independently of either representation. The
additive identity is checked against known observed values, without treating
an imputed original as observed data.

The external context includes one prior year and **two future years**. In this
oracle, X-13's original-series forecasts cover 12 months and TRAMO's cover 24.
An exploratory undersupplied-context probe showed that TRAMO can return the
second forecast year without the fixed regressor's intended future change.
That probe is not a supported-input target: fixtures supply every required
future value and check coverage explicitly. Native input validation must reject
undersupplied contexts instead of reproducing a silent loss of effects.

## Generation and verification

```sh
# First capture with the original pinned packages, JARs, R and JVM.
Rscript tools/reference/generate-extended.R

# Recompute an existing extended suite with that same pinned oracle.
Rscript tools/reference/generate-extended.R --regenerate

# Checksums, original-corpus pin, formulas, invariants and deliberate corruption.
# Does not load Java, rjd3 or Persephone.
Rscript tools/reference/verify-extended.R

# Recompute all eight direct fits and require exact identical raw results.
Rscript tools/reference/verify-extended.R --live
```

Generation writes to a staging directory, validates every invariant and checks
that parent hashes are unchanged before replacing the extended directory. An
existing suite requires explicit `--regenerate`. There is no oracle-refresh
switch: changing the oracle requires a separately reviewed baseline/version.
The manifest records parent artifact hashes, runtime pins, generator hashes and
the number of checked invariants. The original `verify.R` continues to validate
the original 12-case corpus independently.

## Numerical acceptance limits

No new statistical tolerances are added or widened. `tolerance-coverage.csv`
records 229 numeric paths covered by existing **provisional** parent rules and
one path without a reviewed native acceptance rule:
`/preprocessing/description/variables/[3]/coef/value`.
The inherited `compare_reference` function continues to fail closed for that
unregistered path. This is a review requirement, not a zero-filled omission.

Pinned live replay uses exact `identical()` equality for the complete raw
results, so it requires no fresh numeric threshold. That is a reproducibility
check on the recorded oracle, not a demand that a different native algorithm
be bit-identical. Algebraic invariants use a documented machine-roundoff budget
(`64 * .Machine$double.eps * max(1, abs(scale))`) only for exact identities;
estimated coefficients, likelihoods and other numerical outputs receive no new
acceptance standard from those invariants.

The suite does not yet cover arbitrary missingness, leading/trailing gaps,
fixed-zero coefficients, all outlier combinations, user calendar contexts,
transform/selection boundaries, benchmarking or difficult SEATS models. It
extends reference coverage without claiming full item 18 acceptance or native
seasonal-adjustment equivalence. Source and licensing evidence are recorded in
[NATIVE_PROVENANCE.md](NATIVE_PROVENANCE.md).
