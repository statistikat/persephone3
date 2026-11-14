# TRAMO uncertainty: compatibility decision (20c.1b.1)

**Decision accepted, 2026-11-14:** implement both modes. Accurate final-fit
covariance is the default; legacy comparisons must also run without Java.
The earlier evidence below is retained as the historical basis of that decision.

The native implementation uses the complete residual-Jacobian convention in
both modes. Accurate mode recomputes qualified derivatives at the final fit.
Legacy mode replays a recorded optimizer working point, signed forward steps
and retained LM damping from plain numeric state. Runtime public-API probes
now explain the three discrepancies: the working matrix can include damping
that an accurate final-fit covariance removes. A final coefficient vector
alone does not determine the retained state. The comparison parameter therefore
requires that state; it never calls Java or silently invents a damping value.

## Reproducible evidence

Run `Rscript tools/reference/study-tramo-information.R`. Optional
`--output=<existing empty directory>` stores a prospective report outside the
frozen corpus. The committed record is
[`information/2026-11-13-tramo/`](information/2026-11-13-tramo/), containing
`matrix.csv`, `scale.csv`, `raw.R` and `metadata.R`. Source hashes, the parent
commit and pinned runtime identify the calculation. No original fixture or
tolerance is changed.

The study passes **443 independent checks / 90 public preprocessing calls**.
A dense Gaussian calculation independently whitens the differenced full span,
profiles internal missing coordinates as dummy columns together with regression,
and accounts for the missing-coordinate determinant. Its likelihood, GLS,
effective count and residual quadratic form match the pinned outputs.

With `e(theta)` the resulting complete projected residual vector and `D(theta)`
the effective covariance determinant, define

```
z(theta) = e(theta) * exp(log(D(theta)) / (2*n))
J = d z / d theta
V_GN = (z'z)/(n-r-k) * (J'J)^-1
```

`n` is the effective finite count after missing-coordinate integration, `r=2`
in this fixture family, and `k` counts free ARMA coefficients. The innovation
scale is not subtracted again. This is a Gauss-Newton approximation built from
profiled, determinant-normalized residuals; it is neither observed Hessian nor
exact expected Fisher information. The oracle is deliberately dense R code for
independent validation, not the proposed production implementation.

Two central stencil sizes plus Richardson extrapolation give a maximum
covariance change of **2.196e-9** relative. For complete AR(1), closed-form
whitening/GLS derivatives independently verify the result. Derivatives of the
public fixed-parameter residual API also agree: maximum covariance discrepancy
**7.271e-11** relative in those AR(1) cases. This is evidence from public APIs
and independent mathematics; no upstream numerical source was translated.

The covariance candidate meets the prospective `2e-5` relative Frobenius budget
in **61 of 64 cases**. The exceptions are:

| Case | Candidate versus reported covariance |
| --- | ---: |
| Quarterly AR(1), complete, identity transform | 1.4457366% relative difference |
| Monthly AR(1), complete, identity transform | 1.4457366% relative difference |
| Quarterly MA(1), complete, log transform | 0.1091426% relative difference |

### Changing units exposes the numerical difference

The following transforms the response and the fixed-regressor units together.
The ARMA parameter and its covariance should be invariant; estimated regression
coefficients rescale. Each row is a fresh pinned TRAMO fit with the same model.

| Response scale | Reported AR coefficient variance | Independent residual-information variance |
| ---: | ---: | ---: |
| 1 | 0.005642977238 | 0.005724559823 |
| 7 | 0.005723820676 | 0.005724559824 |
| 100 | 0.005724507802 | 0.005724559822 |
| 0.001 | 0.005724560212 | 0.005724559822 |

The fitted AR coefficient changes by at most **1.85e-8**, and the likelihood
obeys the exact unit-change identity. Independent covariance changes by at most
**2.71e-10** relative. Reported covariance changes by **1.4457435%**. At original
units, the independent standard error is **0.7202743%** larger than the reported
one. Three other model/scale controls remain stable, so this is not a generic
mistake in the scale transformation.

These observations establish numerical unit sensitivity in the pinned output.
They do **not** identify its internal implementation cause. Starting-value probes
did not change the returned values, so solver-history dependence is not asserted.
Full covariance parity and score parity are still unqualified. The residual-score
comparison has maximum absolute difference `1.50413e-4` in this matrix.

### Missing residuals need their own contract

Observed-only filtering and complete-data projection can produce equal
likelihoods but parameter-dependent residual rotations; that distinction changes
Gauss-Newton information. The full projected vector resolves the earlier
missing-case covariance discrepancies in this matrix.

The public `estimation$res` field also has values at missing positions and is
not universally the same vector as the covariance calculation. Summing it
unmasked does not always reproduce the likelihood's `ssq`. The study checks the
likelihood quadratic form and explicitly restricts its public residual-derivative
identity to complete AR(1). Standardized/interpolated residual domains must still
be qualified in 20c.2; this report does not authorize treating synthetic entries
as observed innovations.

## Historical alternatives (resolved by the decision above)

| Choice | Implementation and acceptance |
| --- | --- |
| **Accurate final-fit covariance (recommended)** | Implement the independently validated complete-residual projection and qualified Jacobian in C++. Preserve X-13 observed-information and TRAMO Gauss-Newton conventions as separate methods. Register the three exceptions with unit-invariance/analytic evidence and retain them as regressions. Explain changed standard errors in compatibility metadata; do not widen the general tolerance. |
| **Reproduce legacy numerical values** | Keep TRAMO covariance unavailable in native mode and investigate how to reproduce the pinned values, including their unit sensitivity. Do not adopt the candidate as compatible or silently substitute X-13 covariance. This postpones the shared uncertainty contract. |

Either choice retains structural, stationarity, derivative-accuracy and
curvature/rank gates, excludes fixed directions, and preserves unavailable
standard errors as `NA`. Neither authorizes changing the default backend or
claiming full native seasonal adjustment.

## Follow-up implementation steps

1. **20c.1b.2:** implement the chosen TRAMO covariance path in the C++ core,
   retaining the dense oracle and all 64 cases; include missing-coordinate rank,
   range, budget, interruption and serial/OpenMP checks. Label the estimator,
   coordinates, normalization and denominator. Keep score and covariance parity
   as separate assertions.
2. **20c.2a:** add regression uncertainty. Retain the compatibility conditional
   covariance with `Q/(n-r-k)`. For optional joint observed inference, differentiate
   `bhat(theta)` and use `V_b = V_b|theta + B V_theta B'` and
   `Cov(b,theta)=B V_theta`, testing against a full Gaussian Schur complement.
   Never relabel conditional GLS covariance as joint uncertainty.
3. **20c.2b:** qualify standardized innovations, missing/interpolated rows,
   estimation domains and restored fixed/free effects against independent
   Gaussian conditioning and pinned public outputs.
4. **20d:** add original-domain forecasts/backcasts and errors with complete
   future regressors, explicit variance/mean conventions and the existing
   separate point/error horizons; then proceed to restricted X-11.

The C++ observed-information implementation remains commit `4f00d22`, with
14,107 package assertions, 12,778 Java-free native assertions and 12,780 serial
assertions qualified. This follow-up changes comparison tooling and documentation
only. Matched fit timings remain the separately recorded 64-case benchmark;
the dense oracle's runtime is not a native-backend performance claim.
