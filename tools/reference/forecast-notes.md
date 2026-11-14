# Forecast/backcast public observations and independent qualification

`verify-forecasts.R` uses only public `RegSarimaModel.forecasts(int)` /
`backcasts(int)` and `Forecasts` accessors. `javap -public` supplied their
signatures. No upstream numerical source or bytecode was inspected, translated
or copied. The native endpoint distribution is independently derived by Gaussian
conditioning with flat diffuse initial coordinates and GLS regression.

The 64-model matrix has quarterly/monthly stationary, integrated and seasonal
models; fixed/free coefficients, transforms, estimation windows and missing
endpoints. There are 128 matrix fits and eight additional closed reporting controls. At each public physical-coefficient point,
an independent intrinsic-Gaussian precision/Schur calculation checks native
endpoint means, process covariance, regression increment and total covariance.
The prospective scaled budget is `2e-8*(1+abs(target))`, including near-zero
matrix entries. Native scale always comes from the original observed window.
Public output parity is recorded separately and never changes an independent
acceptance threshold.

`RawForecasts` excludes fixed regression effects. `Forecasts` restores them;
for logs it returns the inverse-point median `exp(m)` without mean adjustment.
Consequently transformed point comparisons use the original accessor directly
for identity models and its logarithm for log models. Public horizons are
extended by the trimmed original endpoint gaps, then matched by exact calendar
ordinals to the requested native domain. Missing dates are never compared by
blind vector position.

The matrix matches all 128 public **future points**. Public **backcast points**
and transformed standard errors differ in all 128 matrix fits. A simple closed
control shows why these cannot be universal accurate targets. With quarterly
observations

```
1, 3, 2, 4, 5, 2, 3, 5, 4, 2, 3, 6, 2, 4, 5, 3
```

and a random walk with fitted drift, the forward drift is `(3-1)/15=2/15`.
The nearest past conditional point is `1-2/15=13/15`; both public engines report
`1+2/15=17/15`. Their future point is the correct `3+2/15`. Native predictions
use the closed Gaussian identity in both directions. More complicated public
backcast differences remain reporting discrepancies, not an emulation contract.

A white-noise model with fitted mean on the same data gives `mean=3.375` and
`Q=29.75`. The ML prediction variance is `(Q/16)*(1+1/16)`; the explicit native
residual-df result is `(Q/15)*(1+1/16)`. The public variance is instead
`Q/14 + Q/(15*16) = 2.24895833333333`, whose square root is about `1.4996527376`.
This distinguishes process scaling from fitted-mean covariance scaling. It is
a closed public observation (also checked for fitted random-walk drift), not a
claim that one formula reproduces every
public forecast error convention. Native accurately labelled scales are retained.

Optional timing measures prepared fitting plus native full joint endpoint
covariance against public processing, serialization and both directions'
points/marginal errors. These workloads differ. Three warmed alternating rounds
use calibrated batches of at least 50 ms where possible. Setup/loading/startup
and explicit pre-batch GC are excluded; natural in-batch GC is included. Source
hashes, closed controls and raw matrices/timings are in
`information/2026-11-14-forecasts/`; earlier residual/interpolation evidence is
unchanged.

```sh
mkdir /tmp/persephone-forecast-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-forecasts.R \
  --timing --output=/tmp/persephone-forecast-report
```

## Original moments (20d.2a)

The current qualifier additionally checks native original Gaussian/lognormal
means and covariance against independent moment formulas, retaining explicit
range status when the full covariance cannot be represented. Original standard
errors are evaluated independently and can remain finite in those cases. The
transformed point/covariance budgets remain unchanged.

A separate public-input check distinguishes the inverse-transform convention
from the already recorded process scaling. Public original log errors match the
lognormal formula evaluated at `RawForecasts`, which excludes fixed regression
effects. The public original points instead restore those effects. Applying the
formula to their fully restored locations therefore differs. Both comparisons
remain explicit in the report. Native moments use the full accurate Gaussian
location consistently; this reporting difference is not incorporated into the
accurate default. Only public accessor execution informed this observation.

The report in `information/2026-11-14-prediction-moments/` also times prepared
fitting plus both transformed and original moments. Its native workload includes
full original joint covariance when representable; public outputs remain marginal
errors and inverse points. Earlier 20d.1 matrices/timings are preserved as
historical evidence.

## Optional ARMA propagation (20d.2b)

`verify-forecast-arma.R` qualifies the optional `delta_profile_ml` result against
independent dense Gaussian calculations over 64 nonempty endpoint domains.
Stationary covariance is obtained from finite Yule-Walker equations, then dense
whitening and an SVD least-squares projection supply missing/endpoint points.
Independent OLS centering removes regression effects and a null-space basis of
the differencing matrix before fixed h/h2 Richardson differences. This avoids
precision-Schur cancellation on twice-integrated endpoint derivatives. A separate
Schur calculation checks point levels and conditional covariance, and a full
Gaussian Hessian supplies parameter covariance. Calendar alignment of prepended
backcast regressors is explicitly checked against the original likelihood Q.
The budgets are unchanged: `2e-7*(1+abs(G))` for sensitivities and `2e-5` relative
error for covariance normalized by conditional errors. Closed AR(1) formulas
also qualify the SVD derivative oracle and production implementation.

Public X-13/TRAMO forecast/backcast accessors remain conditional reporting
comparisons and are not targets for the delta increment. The benchmark reports
four separate workloads: prepared native fit plus conditional endpoints, native
fit plus delta endpoints, and the two public processing/serialization/accessor
paths. Native workloads include full joint covariance and original moments;
public workloads return points and marginal errors. Startup, preparation and
explicit pre-batch GC are excluded; natural in-batch GC is included. The protocol
uses three warmed alternating rounds with calibrated batches of at least 50 ms
where possible. Source hashes, qualification matrix and timing rows are stored in
`information/2026-11-14-forecast-arma/`. The earlier public reporting differences
and historical artifacts are preserved.

```sh
mkdir /tmp/persephone-endpoint-delta-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-forecast-arma.R \
  --timing --output=/tmp/persephone-endpoint-delta-report
```

The captured report passes 2,305 checks across 64 independent delta covariances
and 128 public fits. Maximum normalized covariance relative error is 3.688e-8
and scaled sensitivity error is 1.563e-9. Across 7,458 timing calls, the median
summed time for 64 prepared fits/outputs is 9.460556 s for native conditional,
16.104000 s for native delta, 0.287880 s for JDemetra X-13 and 0.269339 s for
JDemetra TRAMO. The optional native propagation adds about 70.2% in this matrix;
these different output workloads are not an end-to-end performance parity claim.
