# Fixed-SARIMA regression likelihood (20b.1)

`native_regression_likelihood()` is an internal, serializable R contract around
the independently authored C++ regression kernel. ARMA parameters remain fixed.
This evaluates and estimates deterministic regression effects; it does not yet
optimize ARMA coefficients, select a model or perform seasonal adjustment.

## Inputs and coordinates

The response is a monthly/quarterly univariate `ts`. Regressors are a uniquely
named list of finite `ts` columns or a named `mts`. Inclusive estimation windows,
fresh initialization at the window's start, response masks and original source
rows follow [fixed-likelihood-contract.md](fixed-likelihood-contract.md).
Regressors must cover every selected observation and requested backcast/forecast
context period, including periods where the response is missing. Extra unused
periods are cropped; incomplete context, recycling and missing regressors fail.

`coefficients` is a named subset of design columns. Finite values, including
zero, are fixed; `NA` and omitted names are free. Names are matched, not ordered
positionally. NaN, infinity, duplicate/unknown names and malformed dimensions
fail. Up to 32 total design columns are supported. Zero-column and all-fixed
models are valid. The existing SARIMA limits remain: `d,D` in 0..2, period 4/12,
at most 32 total states and at most 16 stationary states.

The response is transformed before fixed effects are subtracted. Regressors and
coefficients therefore act on the transformed response scale. The log-Jacobian
still uses original positive response observations under the named
`jdplus_3.9.0_calendar_window` policy; regression rank and effects never change
its cutoff or values.

`include_mean=TRUE` adds a stationary level column and requires fixed `mean=0`.
`include_drift=TRUE` requires integration and generates

```
x[t] = t^(d+D) / ((d+D)! * s^D),       (1-B)^d (1-B^s)^D x[t] = 1.
```

Here `t` is the **one-based source index**, with the first original response
observation at 1, retained across selected windows and past/future context.
Changing the origin adds a homogeneous term absorbed by the initial-history
diffuse measure. Generated names (`mean`, `drift`) cannot collide with supplied
columns. Their coefficients may also be fixed. An integrated model's scalar
`mean` argument remains zero: drift enters explicitly through the design.

## Shared whitening and concentrated ML

The covariance filter runs once on a zero response with the actual observation
mask and records its gains. The adjusted response and every free design column
are propagated through the same gains, state transition and mask. Only finite
innovation rows, divided by their unit-scale standard deviations, enter the
regression. Diffuse rows contribute the existing initial-coordinate determinant
but do not become regression observations.

Rank-aware LAPACK `DGELSY` solves the whitened design without forming normal
equations. For full column rank, a separate thin QR yields `R^-1 R^-T`, the
unit-scale covariance. Let `n` be the finite innovation count, `p` the free rank,
`Q` the whitened residual sum of squares and `L` the finite-plus-diffuse log
determinant. The reported objectives are

```
fixed_log_likelihood    = -0.5 * (n*log(2*pi*variance) + L + Q/variance)
profiled_variance       = Q/n
profiled_log_likelihood = -0.5 * (n*(log(2*pi) + 1 + log(Q/n)) + L)
```

This is concentrated ML. There is no regression determinant and no subtraction
of `p` from the ML variance denominator. Fixed coefficients are excluded from
the free covariance and parameter count. With identified coefficients the
profiled parameter count is `p+1`, including scale.

Four covariance forms are explicit: unit scale, supplied fixed variance,
ML scale `Q/n`, and residual-df scale `Q/(n-p)`. The last is unavailable for
nonpositive residual degrees of freedom. Pinned JD+ `bvar` uses that last scale;
it must not be used to infer the likelihood's variance convention. Covariance
is conditional on the fixed ARMA parameters. Unrepresentable covariance or
arithmetic range is an error, not a zero uncertainty claim.

## Identification and boundaries

`rank_tolerance` is a finite number strictly between zero and one, default
`1e-10`. In integrated models, columns whose finite-innovation norm is tiny
relative to their observed input norm and maximum whitening scale are explicitly
marked numerically zero before the QR rank decision. The norm ratios, threshold
and flagged columns are returned. This guards homogeneous columns against
roundoff masquerading as identification; it is a conservative numerical limit,
not a universal symbolic alias certificate or a guarantee for arbitrarily weak
regressors.

Zero, duplicate, collinear or numerically extinguished free columns produce
`rank_deficient`: free coefficients, covariances and objectives are unavailable.
Fixed coefficients remain known. The implementation does not silently drop a
requested free column or report a particular minimum-norm solution as uniquely
identified. Unresolved initial diffuse rank and no finite observations have
separate unavailable statuses. All-fixed effects and residual series remain
available even when the stochastic initial state is unresolved.

A full-rank saturated design (`n=p>0`) has `Q=0`, profiled variance zero and a
`zero_scale_boundary` profile with positive-infinite likelihood; residual-df
covariance is unavailable. An exactly zero residual sum in other designs has
the same scale boundary. These are not successful interior optimization points.

## Output, parallelism and qualification

The result retains domains, row maps, coefficient states, rank diagnostics,
whitened response/design, all covariance scales, fixed/fitted deterministic
effects, residual series and separate raw/adjusted objectives. When coefficients
are identified, a final filter replay supplies observation-scale innovations
and predictions, with fitted effects added back to predictions. Those trace
variances are explicitly `conditional_on_coefficients`; they do not include
estimated-coefficient uncertainty. Deterministic effects over the requested
context are retained, but are not stochastic forecasts or backcasts.

Whitening channels use optional OpenMP through the same bounded helper as the
numerical primitives. Time propagation within each channel stays sequential.
Workers touch only preallocated, disjoint owned C++ memory; they call neither R,
Rcpp, BLAS/LAPACK nor R interrupt handlers. Main-thread callbacks occur between
blocks of at most 64 calendar rows. Covariance filtering, QR and the final replay
remain serial. See [openmp-contract.md](openmp-contract.md) for configuration,
thread limits, nested calls and exception rules.

`test-native-regression.R` contains 1,170 mandatory assertions using OLS,
independent dense ARMA and seasonal-MA Gaussian integration, mean/drift
identities, fixed/free/alias/scale boundaries, window/log/context behavior,
serialization and serial/parallel identity. `test_regression_likelihood.cpp`
adds standalone strict-warning, ASan/UBSan, callback/reuse, actual-worker equality
and extreme numeric range checks. Prebuilt BLAS/LAPACK is not instrumented.
The read-only [reference probes](../reference/regression-notes.md) qualify the
bounded pinned-JD+ conventions without changing stored fixtures or tolerances.

ARMA optimization is the next slice (20b.2). Estimated-ARMA uncertainty,
coefficient-aware forecasts, automatic transformation/model selection,
outlier search and native X-13/TRAMO-SEATS remain separate work.
