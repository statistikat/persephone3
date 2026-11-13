# Fixed integrated likelihood reference probes

Checked against the pinned JD+ 3.9.0 oracle on 2026-11-13. These checks support
item 20a.2, the fixed-parameter exact diffuse kernel. They do not implement or
qualify parameter optimization, regression likelihoods, transformation selection,
SEATS, X-11 or a public native seasonal-adjustment backend.

## Reproduction and pinning

Run `Rscript tools/reference/verify-diffuse.R` with the reference dependencies
on `.libPaths()`; for the development library used locally:

```sh
R_LIBS=/tmp/persephone3-status-lib Rscript tools/reference/verify-diffuse.R
```

The script first verifies the original corpus checksums and the exact recorded
R version, rjd3 package versions, 20 JAR SHA256 values, JVM/vendor and architecture
from `inst/tinytest/fixtures/backend/oracle-manifest.R`. It makes no writes. A
different oracle fails before evaluation; the script neither regenerates that
manifest nor modifies the original or extended fixtures or their tolerances.

Add `--native` after installing the development package and selecting its library
through `R_LIBS`, or `--native --source` to use `pkgload` on an already compiled
development tree without recompiling. This requires the internal `.native_sarima_likelihood` boundary
and compares 14 untransformed input/model pairs with the direct JD+ results.
It does not substitute an adapter or estimate parameters if the boundary is
unavailable. The two complete log-transform examples remain separate evidence
for item 20a.3's transform/regression integration.

The 16 JD+ artifacts match published Maven Central 3.9.0 checksums, whose release
tag resolves to commit `cd50b84773c2bdbf7a067a497954bb6d46cdb81b`.
[NATIVE_PROVENANCE.md](NATIVE_PROVENANCE.md) records this source/release mapping
and its limits: published artifact identity is established, a reproducible source
build and exact installed R-wrapper source revisions are not.

## Direct fixed specifications

The script invokes `rjd3x13::regarima_fast()` and
`rjd3tramoseats::tramo_fast()` directly using `rg0`/`tr0`, automatic modeling
disabled, no regression variables, no automatic outlier search, and explicit
integration orders. TRAMO explicitly selects `exact.ml = TRUE`. Except for the
two log examples, transformation is explicitly `None`. The only MA coefficient,
when present, is fixed at `+0.4` in the public SARIMA sign convention.

Every returned fit must contain no estimated ARIMA coefficients, no regression
coefficients or variables, the requested integration orders, and either no MA
coefficient or the exact `FIXED` coefficient. The reported parameter count is
one: innovation variance is profiled, although all ARIMA/regression parameters
are fixed. Calling an estimator with an initial value is not a fixed-parameter
comparison.

Two installed-wrapper pitfalls are guarded explicitly:

- `set_arima(mean = FALSE)` creates an undefined mean parameter in this version;
  it does not remove drift. `mean = NULL` plus the returned empty-variable check
  provides the intended no-drift specification.
- Updating `coef`/`coef.type` without the orders can leave previous parameters
  unchanged. Orders, values and types are supplied in the same setter call, then
  the returned coefficient state is checked.

The raw likelihood record has ten fields: `nobs`, `neffectiveobs`, `nparams`,
`ll`, `adjustedll`, `aic`, `aicc`, `bic`, `bicc`, and `ssq`. It does not expose
the finite and diffuse determinants separately. `sarima_estimate()` is not used:
it estimates coefficients and does not provide a fixed-coefficient argument.

## Public diffuse API and normalization

Additional probes construct the fixed model with `rjd3toolkit::sarima_model()`
and its `.r2jd_sarima()` bridge, then invoke public Java APIs:

1. `SsfArima.ssf(IArimaModel)` constructs the state-space form.
2. `new SsfData(double[])` supplies observations, including explicit missingness.
3. `DkToolkit.likelihood(ISsf, ISsfData, boolean, boolean)` returns a
   `DiffuseLikelihood`.

For this pinned runtime, the boolean pairs `(TRUE, FALSE)` and `(FALSE, FALSE)`
produce the variance-profiled and unit-variance likelihoods respectively, as
checked against independent probability identities. This is an observed API
convention, not a guessed interpretation of unnamed `javap` arguments. The
probe reads `logLikelihood()`, `ssq()`, `dim()`, `getD()`, `logDeterminant()`,
`getDiffuseCorrection()` and `isLegacy()`. The latter is false in every case.

The corresponding public API locations at the identified release commit are
[SsfArima](https://github.com/jdemetra/jdplus-main/blob/cd50b84773c2bdbf7a067a497954bb6d46cdb81b/jdplus-main-base/jdplus-toolkit-base-parent/jdplus-toolkit-base-core/src/main/java/jdplus/toolkit/base/core/ssf/arima/SsfArima.java),
[DkToolkit](https://github.com/jdemetra/jdplus-main/blob/cd50b84773c2bdbf7a067a497954bb6d46cdb81b/jdplus-main-base/jdplus-toolkit-base-parent/jdplus-toolkit-base-core/src/main/java/jdplus/toolkit/base/core/ssf/dk/DkToolkit.java),
and [DiffuseLikelihood](https://github.com/jdemetra/jdplus-main/blob/cd50b84773c2bdbf7a067a497954bb6d46cdb81b/jdplus-main-base/jdplus-toolkit-base-parent/jdplus-toolkit-base-core/src/main/java/jdplus/toolkit/base/core/ssf/likelihood/DiffuseLikelihood.java).
Only release-tree metadata and installed `javap -public` signatures were used
for this API inspection. No Java numerical method body was read, copied or
translated for these probes.

Let `m = dim() - getD()`, `Q = ssq()`, `L = logDeterminant()` and
`C = getDiffuseCorrection()`. With unit innovation variance, the checked
convention is

```
fixed log likelihood    = -0.5 * (m * log(2*pi) + Q + L + C)
profiled log likelihood = -0.5 * (m * (log(2*pi) + 1 + log(Q/m)) + L + C)
profiled variance       = Q / m
```

Every case in the original 16-model suite has canonical diffuse correction
`C = 0`. That suite alone therefore does not qualify arbitrary diffuse-coordinate
changes with nonzero correction. Such normalization is tested independently in the native tests by
integrating a Gaussian observation density over fixed initial-state coordinates.
The native contract uses initial covariance `kappa * B B' + sigma^2 * P_star`
and the limit `(2*pi*kappa)^(rank(B)/2) * L_kappa`; a coordinate change in `B`
must not silently be discarded.

These constants use the finite innovation count, not the full input length.
For complete data here, it is `n - d - period*D`; with missingness, it is the
number of observed values minus exhausted diffuse directions. `nparams = 1`
is not subtracted again. Public preprocessing and public DK agree for all the
tested gaps, although their internal missing-data processing need not be the
same algorithm.

## Independent inputs and identities

Every input is synthetic and generated in the script, with 120 observations
starting in 2000 and frequency 4 or 12. Define
`z[t] = sin(t*sqrt(2)) + 0.5*cos(t*sqrt(3))`.

| Case | Input and fixed model | Independent check |
| --- | --- | --- |
| Random walk | `y = 100 + cumsum(z)`; `d=1` | Successive observed increments, with variance equal to elapsed periods |
| Seasonal walk | First `period` values are `100 + 1:period`; then `y[t] = y[t-period] + z[t]`; `D=1` | One independent random-walk chain per seasonal phase |
| Combined differences | Cumulative sum of the seasonal walk; `d=D=1` | White-noise density of `diff(diff(y, lag=period))` |
| IMA(1) | `100 + cumsum(z + 0.4*c(0,head(z,-1)))`; `d=1`, fixed MA `+0.4` | Dense Gaussian density of `diff(y)` with tridiagonal covariance diagonal `1+0.4^2`, adjacent entries `0.4` |
| Internal gaps | Random and seasonal walks with observations 20, 21 and 44 missing | Observed increments divided by their known gap variances; determinant includes each gap |
| Edge gaps | Random walk with 1, 2, 119 and 120 missing | The first observed value consumes the diffuse direction; trailing missingness adds no likelihood term |
| Log random walk | `exp(2+cumsum(z)/50)`; fixed `Log`, `d=1` | Random-walk density after taking logs, plus a separately checked adjustment |

For the monthly random walk, `m=119`, `Q=74.01271158326` and
`ll=-140.5979305792`. Its internal-gap version has `m=116`, `L=log(6)` and
`ll=-138.8882096213`. The monthly seasonal walk has `m=108`; with those gaps
it has `m=105` and `L=log(8)`. Leading/trailing random-walk gaps give `m=115`.
These values are illustrative; executable expectations come from the formulas,
not pasted oracle outputs.

The pinned reference run passes 32 direct fits and 577 checks. Maximum absolute
profiled-log-likelihood discrepancy from the independent identities is
`8.53e-14`; public DK's unit-variance discrepancy is `1.17e-11` (large-level,
combined-difference case). The script declares absolute-plus-relative bounds
of `1e-10` for the JD+ identity checks and `2e-8` for the native comparisons
before comparing native output. These are bounded roundoff checks, not newly
approved production parity tolerances.

With the compiled 20a.2 development tree, `--native --source` passes 675 checks,
including all 14 native comparisons. Maximum absolute native/public-DK
differences are `2.43e-11` for the profiled likelihood and `1.57e-11` for the
unit-variance likelihood. The separate sum of squares, finite determinant,
diffuse determinant, observation counts and exhausted diffuse rank also pass;
there is no fitted offset or correction to force agreement.

## Additional second-order differential coverage

`--native --source --second-order` evaluates 414 cases without changing the
original 32 preprocessing fits or their independent identities. All `d,D` in
`0:2` are included at periods 4/12, with white innovations, MA `+0.4`, ARMA
`(+0.3,+0.2)`, and airline MA `+0.2`/seasonal MA `-0.4`. Coefficients use the
native/R convention: the public JD+ bridge negates regular/seasonal AR
coefficients and preserves MA signs. Public autocovariance identities guard that
conversion.

Inputs are the bounded deterministic formula
`sin(t*sqrt(2)) + 0.5*cos(t*sqrt(3)) + t/40`, `t=1:120`. Six missingness masks
are used: complete; internal 20/21/44; edges 1/2/119/120; early
1/3/5/6/16/21/23/86; seasonal-phase gaps
`1,1+s,1+2*s,2+s,2+2*s`; and a consecutive block `(2*s+1):(3*s+3)`.
Eighteen model/mask configurations are excluded prospectively by the declared
32-state dimension bound. No failed native evaluation is converted to a pass.

All 414 native evaluations now succeed, including the formerly rejected
quarterly `d=2,D=1` early-gap case. This expanded block passes 4,064 checks and
includes 81 nonzero diffuse corrections. Counts, sum of squares, profiled/fixed
likelihoods and diffuse determinants pass the unchanged numerical bounds.
Nine public-DK **finite determinant intermediates** have a separate identified
oracle-precision limit; those cases require the alternative checks below.

| Monthly `d=2,D=2` model | DK finite-determinant limits |
| --- | --- |
| White | Complete, edges |
| MA `+0.4` | Complete, internal, edges |
| ARMA `(+0.3,+0.2)` | Complete, internal, edges, early |

For example, white complete data have exact finite log determinant zero; native
returns `4.41e-13`, whereas public DK returns `-5.46e-8`. The native profiled
likelihood agrees with the exact twice-ordinary/twice-seasonal differenced
Gaussian density to `2e-13`; DK differs by `1.39e-7`. Across the expanded block,
the largest native/DK profile, fixed and finite-determinant discrepancies are
`1.01e-6`, `1.05e-5` and `1.21e-6`. The likelihood comparisons pass their
predeclared scaled bounds, while the nine near-zero/small determinant
comparisons do not. No tolerance was widened and no fixture was regenerated.

The script explicitly reports those nine oracle limits. Uniformly across all
18 monthly `d=2,D=2` cases, it additionally checks native likelihood, sum of
squares and total determinant against direct fixed X-13 **and** TRAMO results.
The total determinant is recovered from their exposed raw likelihood, finite
count and sum of squares using the stated likelihood equation. Complete and
edge-only cases also require an independent dense Gaussian covariance check
after exact differencing: for ARMA(1,1),
`gamma0=(1+theta^2+2*phi*theta)/(1-phi^2)`,
`gamma1=phi*gamma0+theta`, and `gamma[h]=phi^(h-1)*gamma1`.
R's public dense linear algebra evaluates that Toeplitz covariance. This
validates the native result without approving the inaccurate DK intermediate.

The structural zero certificates were reviewed independently: an observed update
annihilates `Z B_post`, so a transition row exactly equal to `Z` must propagate
a zero loading row; an exact scalar-identity transition preserves that nullspace
through missing observations. The core uses exact structural matches, with a
residual check for the scalar-identity certificate. It does not treat approximate
row equality or merely small observability as proof of a mathematical zero.
The canonical SARIMA path additionally uses an exact integer homogeneous-history
rank certificate, independent of the stationary ARMA coefficients, to classify
dependent observations while retaining the original floating diffuse coordinates.

This finite matrix is not a general sparse-history qualification. A separate
known long sparse propagation limit remains: quarterly `d=2,D=1`, length 90,
observed positions 1/21/41/61/81 (also with the tail 87:90 observed), can trigger
the explicit projection-residual guard. That is a native numerical limit for
item 20a.2c, distinct from the nine JD+ intermediate-precision limits above.

## Log adjustments and remaining work

For the complete log random walk, raw `ll=324.93280706678`, while
`adjustedll=86.14477588500`. Their difference is
`-sum(log(y[-1])) = -238.78803118178`, not the all-observation sum
`-sum(log(y)) = -240.80618093532`. Both preprocessing engines agree. Thus the
native transform primitive's per-observation Jacobians must be retained, with
their likelihood selection determined by the estimator pipeline; summing every
observed term at the primitive boundary would miss this pinned convention.

This original example alone covers complete, no-regression `d=1` data. The
fixed-likelihood window contract and broader Jacobian selections are the next
20a.3 integration slice. Regression effects, estimated models and preadjustments
remain separate integration gates.

Fixed model agreement also does not establish the correct objective for every
future optimizer. Profile, diffuse and marginal likelihoods can differ when the
data transformation depends on estimated parameters; this is discussed by
[Koopman, Shephard and de Vos (2010)](https://doi.org/10.1111/j.1467-9892.2010.00673.x).
Initial-value assumptions and likelihood factorization also matter for
nonstationary and incomplete observations, as developed in
[McElroy and Monsell (2012), section 2](https://www.census.gov/content/dam/Census/library/working-papers/2012/adrm/rrs2012-09.pdf).
The exact diffuse equations used independently in the native core are referenced
to [Helske (2017), section 2.1 and Appendix A](https://www.jstatsoft.org/article/view/v078i10).
These mathematical references do not imply copying their package implementations.
See [the native diffuse contract](../native-tests/diffuse-state-space-contract.md)
for its explicit supported domain and numerical rejection rules.
