# Residual and interpolation points (20c.2b.1a–b)

The internal, Java-free API is:

```r
native_arma_residuals(context, fitted_coordinates, scale = "ml")
native_arma_residuals(context, fitted_coordinates, scale = "residual_df")
native_arma_residuals(context, fitted_coordinates, reporting = "legacy")
```

This evaluates a supplied admissible ARMA point and profiles the regression
coefficients over the selected estimation window. It does not optimize ARMA
parameters or assert that the point is a minimum with identifiable curvature.
The C++ core and types have no Rcpp dependency; the existing linear algebra
implementation links R's BLAS/LAPACK ABI. Rcpp converts owned storage, and R
adds names, calendar domains and plain serializable results.

## Two residual bases

`observed` contains chronological finite innovations from the original missing
observation likelihood, after subtracting the GLS coefficients fitted to the
whole window. A unit innovation is `v_t / sqrt(F_t)`, where `F_t` uses innovation
variance one. It is not a raw prediction error, a recursive coefficient estimate
or a leverage-studentized residual. Diffuse and missing rows have `NA` outputs;
`row_kind` distinguishes finite, diffuse, missing and unavailable rows. Local
rows and original source rows accompany the full-window `ts` values.

The selected scale is explicit:

```
ml:          sigma2 = Q / n
residual_df: sigma2 = Q / (n - r - k)
standardized innovation = unit innovation / sqrt(sigma2)
```

`n` counts finite observed innovations, `r` counts free regression coefficients
and `k` counts free physical ARMA coefficients. Innovation scale is not subtracted
again. The same scale is used for both residual bases. The df correction does
not propagate coefficient uncertainty.

`complete` contains the separate complete-span projection introduced for accurate
TRAMO information. It trims missing endpoints, appends free dummy columns for
internal missing coordinates, whitens the complete span and jointly solves GLS.
Its residual vector can include imputed rows and has a different length and
orientation from observed innovations. Both vectors have squared norm `Q` at
the same parameters. Calendar mappings and `rows_were_missing` are explicit;
missing rows never become observed innovations.

## Interpolation points and failures

The negative fitted missing-coordinate coefficients recover conditional Gaussian
points with the known mean and all fixed/estimated regression effects restored.
Only internal missing observations are filled. Observed values are preserved
exactly and missing endpoints remain unavailable. Original and transformed
points and full-window series are both returned. For log inputs, the original
point is `exp(transformed_point)` with `mean_adjustment="none"`; it is not a
lognormal conditional mean.

This residual/point API leaves prediction errors as `NA` with
`uncertainty_status="not_computed"`. The separate
`native_interpolation_information()` API now supplies conditional-ARMA prediction
errors with known/estimated-regression components in 20c.2b.2a; optional ARMA
propagation remains 20c.2b.2b. See `interpolation-information-contract.md`. Aliasing,
zero scale, invalid parameters, nonpositive df and numerical range failures
withhold the affected output. Observed and complete calculations have separate
availability statuses. Interruptions are always rethrown, including interruptions
inside the complete projection; they are not converted into numerical failures.

OpenMP standardizes independent owned vector elements. Whitening remains ordered
within each channel. R callbacks and LAPACK stay on the calling thread. No Java
fallback or complete seasonal-adjustment constructor is introduced.

## Evidence and public reporting differences

Mandatory tests cover all 64 existing fitted quarterly/monthly model cases and
32 independent intrinsic-Gaussian chronological innovation calculations. The
latter uses a null space for diffuse coordinates and constrained Gaussian
prediction, without the production state recursion. Complete residuals and
interpolations are checked with independent dense Cholesky/GLS projections.
Closed AR(1) and seasonal-random-walk cases, df/range/unavailable cases, fixed
effects, means, log inputs, units, windows, missing endpoints, serialization,
cancellation/reuse and actual serial/OpenMP execution are covered.

Public JDemetra 3.9.0 reporting is a separate contract. The current accurate
outputs match 104/128 matrix fits; 24 fits (12 cases, both engines) report reflected
missing points and corresponding different diagnostic residuals. These are
recorded as failed parity under the unchanged `2e-8*(1+abs(target))` budget,
not accepted as accurate targets. A fixed seasonal random walk additionally
exposes a compressed public residual basis with no valid per-observation
interpretation. See `tools/reference/residual-notes.md` and the versioned matrix.

## Explicit legacy reporting

`reporting="accurate"` remains the default. The optional `"legacy"` mode takes
the same numeric objective and parameters, without Java, target residuals or
optimizer history. It preserves `observed`, `complete`, the likelihood and
coefficients, and retains accurate points as `accurate_interpolation`. Selected
legacy points and the compact `reported` residual series carry `comparison_only`.

For each consecutive internal gap, its preliminary value is the arithmetic
mean of the adjacent observed original-scale endpoints, transformed afterward.
If `m` is the accurate transformed point and `a` this preliminary fill, the
legacy point is `a + (-1)^(d+D)*(m-a)`. Odd total differencing therefore reflects
the Gaussian point. Endpoint gaps remain unfilled. Stable log arithmetic avoids
overflow in preliminary filling; an unrepresentable original-scale legacy point
is explicitly unavailable without losing the accurate outputs.

With free regression coefficients, the public residual series whitens those
legacy-filled points at the accurately fitted coefficients. Its norm can differ
from likelihood `Q`. Without free regression coefficients, including known means
and fixed user effects, it instead uses a compressed chronological-missing-column
Householder projection. Its norm is `Q`; its calendar labels are end-aligned
reporting dates, **not observation dates**. Accordingly its `source_rows` is empty
and `row_alignment="reported_end_aligned_not_observation_mapping"`. Missing
columns have the differencing parity sign, and elimination is unpivoted.

At mathematically zero QR pivots, tiny AR(1) predictor tails can choose a different
residual basis. Independently derived scalar fast-innovation updates preserve
binary64 multiplication and startup-normalization order for this reporting path.
Neither this arithmetic convention nor the legacy points enter accurate fitting
or covariance. Scaled legacy residuals always use the selected accurate likelihood
variance, rather than their possibly different reported squared norm.

Qualification covers 404 public X-13/TRAMO fits: the original 64 fitted models,
96 focused reporting models, 30 AR(1) rounding cases and 12 fixed-effect/known-mean/
estimated-AR controls per engine. It includes quarterly/monthly data, logs,
consecutive and diffuse-period gaps, differencing up to second order, and AR(1)
coefficients from −0.99 to +0.99. Thirty frozen public-output fixtures run in the
mandatory Java-free suite. This is bounded 3.9.0 reporting evidence, not a promise
of compatibility with arbitrary upstream models or versions. See
`tools/reference/information/2026-11-14-residuals-legacy/` for the matrix and timing
evidence. Prediction errors remain based on the accurate conditional distribution.
