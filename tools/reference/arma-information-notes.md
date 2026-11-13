# Fixed-order ARMA information comparison (20c.1a)

Run `Rscript tools/reference/verify-arma-information.R` against an installed
package, or add `--source` for the compiled development tree. The script requires
the existing pinned R 4.5.2, rjd3/JD+ 3.9.0 wrappers, JVM and all JAR hashes.
Frozen original and extended fixtures are never regenerated. It shares public
wrapper preparation with the fitted-model comparison through
`arma-fit-reference.R`.

## Qualified observed-information contract

Two independently authored calculations are available. `profile_ml` profiles
GLS and ML scale at each physical ARMA perturbation. `conditional_residual_df`
holds fitted GLS coefficients fixed, profiles scale, inverts the resulting
observed information and multiplies by `n/(n-r-k)`. Both omit fixed directions;
`r` and `k` count free regression and ARMA coefficients. Likelihood parameter
counts additionally include the scale. Neither covariance comes from BFGS.
The full derivative, coordinate and availability rules are in the
[native contract](../native-tests/arma-information-contract.md).

The **X-13** public preprocessing outputs support the conditional convention.
This conclusion comes from mathematical alternatives tested against public
outputs, not inspection or translation of upstream numerical implementation.
Profiling regression, applying only a df multiplier to that profile covariance,
or replacing observed information by a residual-Jacobian product give different
answers. These alternatives are not silently treated as equivalent estimators.

The comparison uses the 64 monthly/quarterly fixed-order cases, both pinned
engines, and four persistent-AR controls: **136 direct fits**. Conditional
observed covariance has a prospective relative Frobenius-error budget `2e-5`
against X-13. Physical log-likelihood score has absolute budget `1e-4` (the
tool's scalar comparison includes `1+abs(reference)`, immaterial at these small
scores). A second initial stencil checks derivative stability independently of
the pinned output. AR signs are reversed and factor order is explicitly mapped:
native AR/MA/seasonal AR/seasonal MA versus JD+ AR/seasonal AR/MA/seasonal MA.

On 2026-11-13, all **2,317 checks** passed. Maximum X-13 covariance relative
error was **7.4473284e-6**, and maximum score error **8.59282481e-5**. At the
native optima all 68 conditional information calls were available. Some pinned
persistent-reference points fail the native physical stationarity gate. The
API withholds their covariance and returns `NA` standard errors; the comparison
uses their qualified Hessians only as algebraic diagnostics. It does not widen
the stationarity tolerance or convert those reference points into native
inferential successes.

Independent Java-free tests qualify the formulas through dense Gaussian
integration, closed AR(1) derivatives, fixed/free projections, response scaling,
nuisance Schur complements, two stencil sizes, all 64 fitted cases and explicit
failure controls. The full-profile covariance differs from X-13 by up to
**3.15328029%** in relative Frobenius norm in this matrix; this is a different
inference convention, not a failed covariance inverse.

## TRAMO remains a separate gate

**The X-13 covariance convention does not reproduce TRAMO.** Across the same
136 comparisons the maximum conditional-observed versus TRAMO difference is
**51.663949%** in relative Frobenius norm; the full-profile difference reaches
**48.88627332%**. TRAMO scores also differ (maximum absolute difference
**8.93985505e-4**), so they are not accepted as the same score contract.

The subsequent [20c.1b.1 study](tramo-information-decision.md) derives the
determinant-normalized complete residual Jacobian, including missing-coordinate
projection. It matches 61/64 covariances; three numerical differences remain.
Analytic and unit-invariance controls demonstrate a pinned AR(1) covariance's
unit sensitivity. The study records a maintainer decision on accurate final-fit
covariance versus reproduction of those legacy values. It does not widen the
X-13 budgets, alter frozen fixtures or advertise TRAMO covariance parity.

Next, obtain that compatibility decision and implement the chosen path, then extend regression uncertainty
and residuals in 20c.2. Default backend and native seasonal-adjustment capability
remain unchanged. The previously recorded matched fit timings still describe
the optimizer API: information is an explicit additional call and is excluded
from those fit-only/setup-plus-fit totals.
