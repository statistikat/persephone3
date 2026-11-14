# Original prediction moments (20d.2a)

`native_prediction_moments(location, covariance, transformation="none"|"log")`
returns moments of the supplied plug-in Gaussian predictive distribution or its
lognormal inverse transform. The C++ core is Rcpp-free. This is a mathematical
back-transform; it does not refit the model, estimate scale, add ARMA uncertainty
or convert standard errors into quantile intervals.

For transformed Gaussian location `m` and covariance `V`:

```
median_i = exp(m_i)
mean_i = exp(m_i + V_ii/2)
covariance_ij = mean_i * mean_j * expm1(V_ij)
standard_error_i = mean_i * sqrt(expm1(V_ii))
```

The identity transform returns the original location and covariance. Input
covariance must be finite, symmetric and positive definite. A scaled correlation
Cholesky supplies the certificate; matrices are not repaired or clipped. Empty
R inputs produce a valid empty result without constructing a zero-sized core
matrix. Invalid standalone inputs remain errors. Model identity outputs copy
the already qualified kernel distribution, avoiding a redundant factorization
that could discard a tiny process component rounded below a much larger shared
regression component.

## Numerical range and availability

Lognormal covariance is evaluated through its signed logarithmic magnitude,
without forming `exp(V_ij)` or multiplying two potentially enormous means.
`expm1` preserves small nonzero covariance. Large positive cross variance is
split between the two log means before addition, preserving cancellation in
extreme but representable cases. Negative cross covariance keeps its sign;
exact zero covariance stays zero. Nonzero unrepresentable values are never
silently replaced by zero.

Medians, means, standard errors and full covariance have **separate statuses**.
Standard errors are evaluated independently in logarithmic units: a variance
can overflow or underflow while its square root remains representable. A failed
full covariance does not erase usable means/errors. For example, `m=500,V=1`
has a finite mean and standard error but an unrepresentable variance. Conversely,
`m=-1000,V=1000` has an unrepresentable inverse-point median, finite mean
`exp(-500)` and variance approximately one. The multivariate implementation
preserves the corresponding finite cross covariance too.

`available` on the moment object requires mean, standard error and covariance
availability; callers needing a single quantity use its individual status.
Numeric range failures return unavailable fields and retain other certified
quantities. Input certification failures throw in the standalone API. Model
wrappers retain their transformed distribution and record original-moment
failure explicitly. Interrupts propagate through every failure-handling path;
independent output entries use owned OpenMP arrays and LAPACK stays on the
calling thread.

## Model outputs and forecast contract

`native_interpolation_information()` and `native_arma_forecasts()` now expose
`original_moments`, `means_original`, `covariance_original`,
`standard_errors_original`, `original_mean_status` and
`original_covariance_status`. `original_uncertainty_status` describes the
reported **standard errors**, so it can be available when full covariance is
out of range. The original root `available` still describes the transformed
uncertainty. Selected delta-method interpolation covariance, when requested,
is the Gaussian approximation supplied to the back-transform; an unavailable
propagation result is never silently replaced by the conditional covariance.

`points_original` remains the inverse-point median. Means are separate fields;
`mean_adjustment="none"` continues to describe these unchanged point outputs.
Forecast errors respect their separately requested horizons, while full original
covariance retains the joint point domain. The item 10 forecast member reports
original errors and labels `uncertainty_distribution` / `interval_convention`.
A central lognormal interval is obtained by exponentiating Gaussian quantiles,
not by adding/subtracting original standard errors around a median. No new native
seasonal-adjustment constructor or interval-rendering guarantee is implied.

## Evidence and public differences

Independent closed formulas, small signed covariance, extreme exponent
cancellation, separate range statuses, invalid certificates, empty inputs,
serialization, model domains and 260-point serial/OpenMP equality are mandatory.
The existing fitted interpolation/forecast matrices additionally check original
means, errors and full covariance/range status. A standalone sanitizer harness
exhausts interruption boundaries and verifies reuse.

The pinned public forecast comparison keeps its earlier backcast/process-scale
discrepancies. It also shows that public original log errors use `RawForecasts`
locations, excluding fixed regression effects, while original point forecasts
restore those effects. At those raw locations the native lognormal error formula
matches public errors. At fully restored locations it differs, as expected.
Native moments consistently use the full accurate Gaussian location. The report
records both comparisons separately; no public discrepancy is rebaselined into
an accurate target.
