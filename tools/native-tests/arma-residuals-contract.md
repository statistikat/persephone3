# Residual and interpolation points (20c.2b.1a)

The internal, Java-free API is:

```r
native_arma_residuals(context, fitted_coordinates, scale = "ml")
native_arma_residuals(context, fitted_coordinates, scale = "residual_df")
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

Interpolation prediction errors are explicitly `NA` with
`uncertainty_status="not_computed"`. They enter in 20c.2b.2, with fixed-ARMA,
estimated-regression and optional ARMA uncertainty kept separate. Aliasing,
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

The user requested a separate Java-free legacy comparison mode on 2026-11-14.
That is the next commit, 20c.2b.1b. Accurate outputs remain the default; legacy
reporting must not change the likelihood, fitted coefficients, accurate residuals
or future uncertainty calculations.
