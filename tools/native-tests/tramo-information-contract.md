# TRAMO covariance modes (20c.1b.2)

The internal call is

```r
native_arma_information(context, fitted_coordinates,
  method = "tramo_residual", covariance = "accurate")
```

`covariance="accurate"` is the default for every information method. The existing
`profile_ml` and `conditional_residual_df` estimators retain their conventions.
Choosing a TRAMO numerical mode does not substitute either observed-Hessian
estimator for TRAMO's residual-Jacobian approximation. Native seasonal adjustment
and the default preprocessing backend are still unchanged.

## Accurate final-fit calculation

Trim missing endpoints, retain the original regressor coordinates and fill
internal missing response slots with zero. Append a free indicator column for
each internal missing coordinate. Whiten this complete span with the existing
exact diffuse filter and solve the joint GLS projection by rank-aware QR. The
extra columns integrate missing coordinates; they are not counted as estimated
regression coefficients. The public 32-regressor limit remains; internal missing
columns use checked matrix dimensions rather than consuming that allowance.

With complete projected residuals `e`, full covariance log determinant `log D0`,
and whitened missing indicators `M`, use

```
log D = log D0 + log det(M'M)
n = full finite count - number of internal missing coordinates
z(theta) = e(theta) * exp(log D(theta)/(2*n))
J = dz/dtheta
V = (z'z)/(n-r-k) * inverse(J'J)
```

The missing determinant is computed from thin QR diagonals, without forming
`M'M`. `r` counts free original regressors and `k` free ARMA coefficients. The
innovation-scale parameter is not subtracted again. All derivatives use free
physical polynomial coefficients, preserving fixed coefficients exactly.

For numerical range and unit invariance the implementation divides every
residual evaluation by the **same** `sqrt(z_final'z_final)`. The resulting
center norm is one; this changes neither score nor covariance. Three central
stencils, Richardson extrapolation, truncation/roundoff estimates, normalized
matrix eigenvalues, stationarity and structural identification qualify the
result. This is Gauss-Newton information, not an observed Hessian or exact Fisher
information. Its output is named `normal_information`; `hessian` is `NULL`.
The score is `n J'z/(z'z)`, the negative concentrated log-likelihood gradient.

The API includes the complete residual basis, its calendar/source rows, missing
rows and interpolated coordinates. This vector is for covariance calculation;
it is not labelled as standardized observed innovations. Those public residual
and interpolation-error contracts remain item 20c.2b.

## Java-free legacy replay

```r
native_arma_information(context, recorded_final_coordinates,
  method = "tramo_residual", covariance = "legacy", legacy_state = state)
```

`state` is a serializable named list with exactly six fields:

| Field | Meaning |
| --- | --- |
| `final_coefficients` | Free physical coefficients identifying the final fit |
| `working_coefficients` | Free physical coefficients where the retained Jacobian was evaluated |
| `difference_steps` | Nonzero signed forward steps in those same coordinates |
| `damping` | Nonnegative retained isotropic LM damping, in unscaled `z` units |
| `normalized_sum_squares` | Final `z'z`, for validating the response scale |
| `effective_count` | Final effective finite count |

The C++ core recomputes the complete residuals and forward Jacobian `Jw` at the
recorded working point, then calculates

```
V_legacy = (z_final'z_final)/(n-r-k) * inverse(Jw'Jw + damping*I)
```

It accepts no supplied covariance matrix and contains no case lookup or Java
call. The final point must match to `1e-10*(1+abs(theta))`, the normalized scale
must agree within `1e-8` in log units, and the effective count must match exactly.
State is rejected when missing, malformed, out of range or inconsistent. Both
modes omit fixed directions. Legacy mode first qualifies the accurate final fit;
damping cannot rescue structural redundancy, weak information, nonstationarity
or a nonpositive residual denominator. All residual evaluations share one budget.

The selected covariance and standard errors use the legacy matrix only when
explicitly requested. `accurate_covariance` remains available separately;
`comparison_only=TRUE`, the working Jacobian, retained information, normalized
damping and recorded state identify what was used. Accuracy diagnostics and the
stationarity gate describe the accurate final fit, **not** an error bound for the
legacy forward Jacobian. No general reference parity is advertised.

### History is an input, not a fitted-point property

The existing native optimizer is BFGS and does not produce an LM working history.
An exact legacy covariance cannot be reconstructed from its final coefficients
alone: public probes show that roundoff-sized changes in the fit can change the
retained damping. Legacy comparisons must supply recorded numeric state. This
implements covariance replay; it does not implement JDemetra's initialization,
LM optimizer, or its platform-dependent accept/reject history. Automatic
production of compatible state by a future native LM optimizer is separate work.
A standalone Java-free application can consume the same numeric records without
R or Java object references (the current linear algebra adapter still links
R's BLAS/LAPACK ABI).

## Validation and execution

Mandatory tests use independent dense Gaussian whitening/missing integration,
closed AR(1) derivatives, multiple stencil sizes, response-unit changes,
serialization, rank/domain/budget failures, and more than 32 missing indicators.
The 64 additive legacy records are public optimizer observations, not a changed
frozen oracle corpus. Changing their damping in tests changes the computed
covariance, demonstrating calculation rather than covariance lookup.

`tools/reference/verify-tramo-information.R` compiles an independently authored
public-API observer in a temporary directory. It captures the retained matrix,
working Jacobian and steps, then reconstructs nonnegative isotropic damping.
Any remaining reconstruction defect is reported and checked separately. It
restores missing masks when refitting the public exported model. The observed
refit can have a different LM history from the original preprocessing call;
those covariance differences are reported rather than treated as equal histories.

The predeclared replay budget is `2e-5` relative Frobenius norm. The accurate
path uses `1e-6` against the independent dense oracle. Fixed-point score parity,
universal inference accuracy and remote-platform qualification remain separate.

Candidate evaluations, QR/LAPACK, and callbacks stay on the calling thread.
Complete residual whitening reuses optional OpenMP across owned channels.
Serial and threaded results must agree. Strict C++/ASan/UBSan harnesses cover
closed-form information and exhaustive cancellation/reuse; the build source
list adds `residual_information.cpp` to the existing ARMA information sources.
