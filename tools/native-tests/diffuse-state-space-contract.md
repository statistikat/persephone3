# Exact diffuse likelihood pilot (20a.2 and 20a.2b)

This independently authored implementation evaluates fixed Gaussian state-space
parameters. It adds a bounded exact diffuse initialization and an explicit
fixed-order SARIMA construction. It does not estimate ARIMA coefficients, fit
regressions or drift, select models, smooth states, or perform seasonal adjustment.
The native seasonal-adjustment backend remains unsupported.

## Model and initial-coordinate measure

The generic internal boundary `.native_diffuse_likelihood` accepts

\[
y_t=\mu+Za_t+e_t,\qquad a_{t+1}=Ta_t+Lu_t,
\]

where the independent disturbances have covariances
\(\operatorname{Var}(u_t)=\sigma^2I\) and
\(\operatorname{Var}(e_t)=\sigma^2h\). The initial state has known mean \(a_1\),
finite covariance \(\sigma^2CC'\), and diffuse covariance \(\kappa BB'\), with
\(\kappa\to\infty\). The caller supplies the loading matrices \(C\) and \(B\),
so proper covariance is positive semidefinite by construction. Columns of
\(B\) must be numerically independent. Its column count \(r\) is the initial
diffuse rank. A matrix with zero columns represents no diffuse coordinates.
Finite and diffuse initial variation may overlap.

The first observation uses this initial state directly. Missing values skip
the observation update and its density contribution, but time advances through
leading, internal, consecutive and trailing gaps. Returned final state and
covariances refer to the posterior at the final supplied time.

The normalization is fixed to the supplied initial coordinates:

\[
L_D=\lim_{\kappa\to\infty}(2\pi\kappa)^{r/2}L_\kappa.
\]

It is equivalently the Gaussian likelihood integrated against Lebesgue measure
for the coefficients in \(a_1+B\beta\). Changing \(B\) to \(BA\), for a
nonsingular square \(A\), changes log likelihood by \(-\log|\det A|\).
Orthogonal changes preserve it. Rescaling the finite innovation variance does
not rescale the diffuse-coordinate measure.

This is the **diffuse likelihood**, with no additional marginal-likelihood
correction. Different measures need not give comparable optimization objectives
when their diffuse design depends on unknown parameters. The distinction is
described by [Francke, Koopman and de Vos](https://papers.tinbergen.nl/08040.pdf)
and in the [KFAS likelihood documentation](https://www.stat.ethz.ch/CRAN/web/packages/KFAS/refman/KFAS.html#logLik.SSModel).

## Updates and likelihood constants

At an observation, write

\[
v=y-\mu-Za,\quad F_*=ZP_*Z'+h,\quad F_\infty=ZP_\infty Z'.
\]

When the diffuse projection is nonzero, the gain is
\(K=P_\infty Z'/F_\infty\), one initial coordinate is identified, and the
log-likelihood contribution is \(-\tfrac12\log F_\infty\). There is no
quadratic residual, Gaussian constant, or \(\sigma^2\) factor in this contribution.
The finite covariance is updated in the equivalent Joseph form
\((I-KZ)P_*(I-KZ)'+hKK'\). An orthogonal LAPACK reflector removes one column
from the diffuse loading basis; the implementation does not subtract two large
diffuse covariance matrices.

When the projection is exactly zero, a positive \(F_*\) gives an ordinary
Gaussian update and contribution, even if another diffuse direction remains
unidentified. Standardized innovations \(v/\sqrt{F_*}\) are available only for
these finite updates. They are missing at missing observations and diffuse
pivots. Raw innovations are returned for every observed value.

The scalar exact diffuse equations and likelihood convention are documented
in [Helske's KFAS paper, section 2.1 and Appendix A](https://helske.r-universe.dev/articles/KFAS/KFAS.pdf).
The native implementation was authored independently from these mathematical
equations; no KFAS, JDemetra+ or R filtering implementation was copied.

After every initial diffuse coordinate is identified, let \(n_f\) be the number
of finite updates, \(S=\sum v_t^2/F_{*,t}\),
\(D_f=\sum\log F_{*,t}\), and \(D_\infty=\sum\log F_{\infty,t}\) over their
respective update types. The complete fixed-scale value is

\[
\ell_D(\sigma^2)=-\tfrac12\{n_f\log(2\pi\sigma^2)+D_f+D_\infty+S/\sigma^2\}.
\]

For \(n_f>0\), the profiled variance is \(S/n_f\). Its likelihood includes
all these constants. The finite and diffuse variance arrays and final
covariances are returned before finite scale multiplication.

| Situation | Fixed objective | Profile result |
|---|---|---|
| All initial coordinates identified; finite updates present | Available | Available when \(S>0\) |
| All coordinates identified; no finite updates | Diffuse determinant term only | Unavailable; `no_finite_innovations` |
| Finite updates with exactly zero \(S\) | Available at positive fixed scale | Zero scale and positive infinity; `zero_scale_boundary` |
| Initial coordinates remain unidentified | `NA`; `unresolved_diffuse` | `NA`; `unresolved_diffuse` |
| No observations and no initial diffuse coordinates | Zero; `no_observations` | Unavailable |

Numerical underflow of a nonzero residual is an error, not an exact zero-scale
boundary. A deterministic finite observation with zero innovation variance is
unsupported. Losing a diffuse direction under a singular transition is an
error: disappearance from the current state is not identification by data.

## SARIMA construction

`.native_sarima_likelihood` accepts fixed AR/MA and seasonal AR/MA coefficients,
period 4 or 12, and regular/seasonal differencing orders in `0..2`.
With \(\Delta(B)=(1-B)^d(1-B^s)^D\), the differenced series follows the
stationary ARMA convention from [the stationary pilot](state-space-contract.md).

The state contains the stationary ARMA state and the \(k=\deg\Delta\) preceding
undifferenced observations. The ARMA block receives its stationary finite
covariance; the history block has a flat initial prior with identity loading.
These initial history coordinates fix the diffuse measure. Complete-data
likelihoods therefore agree with stationary likelihoods of exact differences.
For gaps, the undifferenced state model is filtered; adjacent missing differences
are not discarded as a substitute.

The generic pilot permits at most 32 states and 32 loading columns. The stationary
ARMA block retains its 16-state bound, and the combined state must fit within 32.
For example, monthly airline MA order 13 needs 14 stationary states and 13
history states, or 27 total. Nonzero mean is rejected for integrated models;
drift and deterministic regression semantics are deferred. The stationary
zero-difference case retains the existing unconditional-mean convention.

## Numerical support boundaries

The diffuse basis is checked with pivoted QR at initialization and after each
transition, with rank tolerance `1e-12`. Column removal uses LAPACK `DLARFG`.
QR certifies rank but never renormalizes the supplied basis or its density
measure; the column-removing reflector is orthogonal.
The projection tolerance is `512 * machine epsilon`, scaled by state dimension
and the observation/basis norms.

Two exact structural identities preserve zero projections without a numerical
cutoff: a transition row exactly equal to the observation row stores a known
diffuse-free history coordinate after an observed update, and a transition
\(T=cI\) preserves the observation-null subspace after that update. These
certificates support the tested second-order seasonal gap patterns and mixed
unobservable directions with scalar-identity transitions, including intervening
gaps. Approximate row matches are not certificates; residual checks still apply.

The canonical SARIMA construction additionally certifies observation dependence
using the exact rank of its homogeneous history design. This covers the formerly
rejected quarterly `d=2,D=1` and monthly `d=1,D=2` or `d=2,D=2` gap fixtures.
The certificate establishes only whether a projection is zero. It does not
replace, normalize, or transform the numerical diffuse loading or its
determinant contribution. A certified zero must still pass the scaled residual
check. A certified nonzero pivot which is numerically zero or too small remains
an explicit error. The generic state-space boundary has no SARIMA certificate.

An exactly zero or structurally certified observation projection gives a finite
update. An uncertified nonzero projection below the roundoff threshold produces
an explicit `numerically ambiguous diffuse innovation` error. Arbitrary generic
models, poorly conditioned diffuse coordinates, and extreme time spans can
therefore still be rejected. The pilot does not silently identify an extra
coordinate or relax numerical thresholds. Unidentified directions produce
`unresolved_diffuse`, rather than a finite objective.

A remaining propagation limit is explicitly qualified. For a quarterly
`d=2,D=1` series of length 90 observed only at positions `1,21,41,61,81`,
the propagated numerical basis fails its certified-null residual check, even
though exact rank correctly leaves three initial coordinates unidentified.
Adding observations at `87:90` would identify all six coordinates, but the
same earlier residual failure still prevents an objective. Both fixtures must
raise `certified diffuse observation-null identity failed its residual check`.
Thus a full-rank final design does not ensure numerical support through a long
sparse prefix. A more stable propagated basis is deferred to item 20a.2c;
20a.2b does not change the existing residual tolerance.

### Exact SARIMA history rank

For the supported `d,D` in `0..2`, a basis of the homogeneous solutions of
\(\Delta(B)y_t=0\) consists of

\[
\{\mathbf{1}_{t\bmod s=j}t^q: j=0,\ldots,s-1,\ q=0,\ldots,D-1\}
\quad\text{and}\quad
\{t^D,\ldots,t^{D+d-1}\}.
\]

Empty groups are omitted when `D=0` or `d=0`. Each sequence is a homogeneous
solution. The groups are independent: a nonzero polynomial of degree at least
\(D\) cannot agree on an entire seasonal arithmetic progression with one of
degree below \(D\), and the remaining seasonal groups have disjoint support.
There are \(k=sD+d\) columns. Since the recurrence has nonzero leading and
trailing coefficients, these coordinates are invertibly related to the
\(k\) initial history values. Consequently their observed row rank is exactly
the number of initial history directions identified by those observations.

Let \(c_j\) be the number of observed positions in seasonal phase \(j\).
The seasonal columns contribute \(\sum_j\min(c_j,D)\) pivots. After eliminating
those columns, the \(D\)-th divided difference sends \(t^D\) to 1 and
\(t^{D+1}\) to the sum of its \(D+1\) time knots. These identities follow
from [the divided-difference formula](https://dlmf.nist.gov/3.3.E35) and
[Newton interpolation](https://dlmf.nist.gov/3.3.E38); this bounded-order rank
rule was independently derived from them.

For `d=1`, one extra pivot exists iff a phase has at least \(D+1\)
observations. For `d=2`, that same condition supplies one extra pivot, and a
second exists iff a phase has at least \(D+2\) observations or the first
\(D+1\) observed time sums differ between two qualifying phases. Distinct
ordered knots ensure that a phase's next divided difference has a different
sum. This includes `D=0`: its divided difference is direct evaluation of
\([1,t]\).

The implementation stores at most 12 phase counts, capped at \(D+2\), and
the checked unsigned 64-bit sums of each phase's first \(D+1\) observed
indices. No polynomial powers, growing elimination matrix, or floating rank
threshold enter this certificate. A prospective observation is diffuse exactly
when adding its row raises the certified rank. A missing position is classified
for its returned variance but is never added to the observed rank. Accumulation
stops once all \(k\) coordinates are identified. Length and time-index checks
reject potential unsigned overflow; each stored sum contains at most three
indices. The standalone tests exercise these range failures directly without
allocating an extreme-length series.

Numerical and certified remaining ranks must agree throughout filtering.
The `numerics` metadata identifies use of the exact history certificate and
counts its observed pivots and observed dependent rows during the diffuse
phase. Neither the finite-update count after that phase nor missing rows inflate
the dependent-row count.

Inputs must be real or integer arrays of the documented shapes. Complex,
logical, factor and character data are rejected before conversion. Model inputs
must be finite; observations may contain missing values. Callbacks run only on
R's main thread. No persistent external pointers are returned.

## Independent qualification

The mandatory tests in `inst/tinytest/test-native-diffuse.R` do not need Java or
KFAS. Their principal oracle independently unrolls the model into

\[
y=c+X\beta+\epsilon,\qquad \epsilon\sim N(0,\sigma^2\Sigma).
\]

Rows of \(X\) are \(ZT^{t-1}B\). The proper covariance \(\Sigma\) comes from
initial finite covariance and process/measurement disturbances, without any
filtering updates. Missing observations select rows of \(X\) and matching rows
and columns of \(\Sigma\). For positive definite observed \(\Sigma\) and full
column rank observed \(X\), direct Gaussian integration gives

\[
\ell_D=-\tfrac12\{(n_o-r)\log(2\pi\sigma^2)
+\log|\Sigma|+\log|X'\Sigma^{-1}X|+S/\sigma^2\}.
\]

The oracle whitens using R's Cholesky factorization and obtains residuals and the
design determinant through QR. It does not use native model output to build the
reference. Conditional Gaussian formulas independently check predictions and
finite variances. Analytic random-walk gap densities, seasonal walks, I(2)
determinants, coordinate changes, bounded random systems, overlapping finite and
diffuse loadings, and R stationary ARMA likelihoods provide further checks.
Large-variance asymptotics are not the primary oracle.

The exact history certificate is independently tested by constructing the
homogeneous basis above and computing observed-prefix ranks using base R's SVD,
with a required clear singular-value gap. Quarterly and monthly models cover
all nine `d,D` combinations under complete, registered, internal, edge, periodic,
block, sparse, all-missing, and seeded random masks. Of these 162 cases, 161
must succeed; the sparse quarterly propagation limit above must raise its
specific residual error. A separate late-identification fixture qualifies the
same limit when the final design has full rank. Special equal-knot-sum cases
require a finite observation while one diffuse coordinate remains, followed by
its later identification. Direct Gaussian integration checks the resulting
likelihood constants; the test oracle does not reuse the production count/sum
formula. A second Gaussian oracle forms the differencing operator with base R,
then integrates missing observations using a QR factorization of its missing
columns. This checks all identified stress-case likelihoods without state
propagation, a large covariance matrix, or the production rank formula.
Pinned direct JD+ comparisons are recorded separately in
[the reference notes](../reference/diffuse-notes.md).

The optional script `tools/native-tests/verify-diffuse-kfas.R` uses only public
KFAS model/filter/likelihood APIs. It checks fixed and profiled likelihoods,
scale, counts, innovations, predictions and final state. It requires a separately
installed KFAS; it neither installs packages nor changes package dependencies:

```sh
PERSEPHONE_KFAS_LIB=/tmp/persephone-diffuse-oracle-lib \
  Rscript tools/native-tests/verify-diffuse-kfas.R --source
```

`--source` assumes the current package sources are already compiled. Omit it to
check an installed package, selected through ordinary R library configuration.
KFAS 1.6.0 was used during development. Its `SSMcustom` constructor clears diffuse
flags where finite initial diagonal entries are nonzero, so the optional fixtures
explicitly verify their retained diffuse matrices and use disjoint initial
coordinates. The independent Gaussian oracle also covers overlapping loadings.

The development run against KFAS 1.6.0 passed 110 comparisons, with maximum
absolute fixed/profile log-likelihood difference below `5e-14`. The independent
Gaussian integration oracle also matched KFAS on nine initial qualification
cases to `3.6e-15`. These results qualify the tested fixed-parameter models and
normalization convention, not arbitrary diffuse models or seasonal adjustment.
