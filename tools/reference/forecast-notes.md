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
