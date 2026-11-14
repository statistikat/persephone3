# Conditional missing-value prediction errors

`verify-interpolation.R` qualifies the internal native interpolation-information
API at the same physical coefficients as public X-13 and TRAMO fits. It uses
`GeneralLinearModel.Estimation.getMissing()` and the public position/value/error
accessors only. Numerical source, bytecode and upstream algorithm implementations
are not inspected or translated.

Closed stationary probes distinguish the denominator: public errors use
`Q/(n-r-k)`, including free mean/regression and ARMA coefficients in the correction.
For example, one missing white-noise value with a fitted mean has unit prediction
variance `1+1/n_observed`, whereas a known mean gives unit variance one. Both
public engines exhibit these identities. For logs, the public standard error is
on the transformed scale. This is separate from the legacy reflected point
reporting described in `residual-notes.md`.

The native default explicitly uses `Q/n`; `scale="residual_df"` requests the
public convention. QR and Gaussian block conditioning supply the known-regression
and estimated-regression components and their full off-diagonal covariance.
An independent dense intrinsic precision/Schur-complement oracle supplies the
ML covariance and accurate point targets. There is no ARMA or innovation-scale
uncertainty propagation in this step.

The prospective matrix has 404 public fits: 64 fitted models, 96 focused models,
30 AR(1) controls extending to ±0.99, and 12 fixed-regression/known-mean/free-AR
controls per engine. Covariance, points and errors retain the scaled `2e-8` budget;
dates/counts/df are exact. The earlier frozen reference corpus and residual
reports remain unchanged.

Optional timing measures prepared native fitting plus conditional interpolation
covariance against public processing, serialization and the missing-value
accessor. These API workloads differ: native returns complete covariance and its
components, while the public accessor returns standard errors. Three warmed
rounds alternate engine order and calibrate batches to at least 50 ms where
possible. Setup/loading/startup and explicit pre-batch GC are excluded; natural
in-batch GC is included. Source/runtime hashes, matrices and timings are under
`information/2026-11-14-interpolation-conditional/`.

```sh
mkdir /tmp/persephone-interpolation-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-interpolation.R \
  --timing --output=/tmp/persephone-interpolation-report
```

## Optional ARMA prediction uncertainty

`verify-interpolation-arma.R` qualifies `method="delta_profile_ml"` against an
independent dense Gaussian joint Hessian and independently differentiated
Schur-complement predictions. The 64 fitted cases include 32 nonempty prediction
covariances and 32 empty-domain controls. The prospective normalized covariance
relative budget is `2e-5`; the point-sensitivity budget is
`2e-7*(1+abs(target))`. Neither budget uses the production derivative error
estimate as its acceptance threshold. Fixed/free directions, refitted regression
effects, transformations, units and missing domains remain explicit.

Only accurate observed profile-ML ARMA covariance enters `G V_theta G'`. The
conditional covariance is retained and the additional covariance is unavailable
when stationarity, information, point sensitivity or range checks fail. No
upstream numerical code or bytecode was consulted for this derivation.

Public X-13/TRAMO calls supply 128 comparison fits and timing baselines. Their
missing-value standard errors remain conditional and are **not** targets for
the delta-method result. The timing protocol measures four prepared fit/output
workloads: native conditional, native delta, public X-13 and public TRAMO.
Native returns full covariance and its components, so the workloads differ.
Three warmed rounds alternate engine order and calibrate batches to at least
50 ms; setup/loading/startup and explicit pre-batch GC are excluded, while
natural in-batch GC is included. Versioned source hashes, qualification rows
and timings are under `information/2026-11-14-interpolation-arma/`.

```sh
mkdir /tmp/persephone-interpolation-arma-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-interpolation-arma.R \
  --timing --output=/tmp/persephone-interpolation-arma-report
```
