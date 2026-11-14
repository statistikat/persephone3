# Additive seasonal normalization (22b.1)

```r
native_x11_seasonal_normalize(series, filters, extend = 0L, mode = "additive")
```

This internal Java-free primitive normalizes already estimated additive seasonal
values on a monthly/quarterly domain. Fixed requests are `stable`, `s3x1`,
`s3x3`, `s3x5`, `s3x9` and `s3x15`, either scalar or one per calendar period.
Named choices must identify every calendar period exactly once. Other modes,
MSR/X11Default requests and spans shorter than two annual cycles are explicitly
unsupported. This is a decomposition building block, not a complete X-11 pass.
The core/header link without R/Rcpp, BLAS or LAPACK.

Apply the centered `2 × frequency` moving average to the supplied seasonal
series, subtract the resulting normalizing mean, then optionally restore
`extend` rows at **both** ends by repeating the nearest normalized annual cycle.
The extension is an explicit nonnegative row count. It changes the returned
calendar but never the normalized values on the supplied span. It restores
seasonal values lost through an earlier symmetric trend filter; it does not
estimate ARIMA forecasts, interpolate missing data or supply uncertainty.

The centered normalizing mean has a half-year unavailable boundary. For each
boundary row, the fixed requested filter and supplied span choose its source:

| Condition, in precedence order | Mean copied to boundary row |
| --- | --- |
| Requested `stable` | Same calendar period one year inward |
| Fewer than five complete years of rows | Same calendar period one year inward |
| Requested 3×15 and fewer than twenty years rounded upward | Same calendar period one year inward |
| Otherwise | Nearest available centered mean |

Central rows always use their own centered mean. The two-cycle minimum ensures
every annual-copy source is central, even for mixed policies. The core reads an
immutable central result, so no endpoint can depend on worker order. Public
shorter-span outputs can involve overlapping boundary copying or exceptions;
those remain outside the advertised domain.

These are **requested fixed filters**, before the seasonal processor's
length-adjusted choices. For example, with five years a requested 3×9 uses
nearest-centered-mean normalization even though its separate seasonal processor
can select stable means for inadequate endpoint support. Do not substitute the
processor's selected labels for these requests. Normalization and seasonal
smoothing have separately qualified length rules.

The result preserves `values` on the restored calendar, `normalization` on the
supplied calendar, both domains, original rows, requested filters, per-phase
endpoint policies/reasons, masks and available rows. `normalization_source_rows`
identifies which centered mean is used at each input row. `source_rows` maps
every returned value to its source on the original normalized span. These
maps are serializable provenance, not learned/fixture-derived coefficients.

Missing values propagate only through nonzero operator support; normalization
never drops or renormalizes observations. A copied missing value remains missing
in restoration. All missing outputs give `no_available_outputs`. Infinite
inputs are invalid. Non-finite filter or subtraction arithmetic fails atomically
with a range error, retaining the existing finite-filter convention. There is
no NaN-based policy decision and no input mutation.

Owned OpenMP covers central means, independent normalized rows and restored
rows. It preserves each row's arithmetic order. Workers read immutable sources
and own distinct outputs; R/Rcpp, serialization and interrupt callbacks stay
on the caller. Standalone serial/OpenMP ASan/UBSan checks cover every observed
interruption point, reuse, ranges and actual workers.

The published mathematical source is Ladiray and Quenneville (2000),
[§4.1.4, page 66, and §4.1.5, pages 76–78](https://www.census.gov/content/dam/Census/library/working-papers/2000/adrm/x11-french.pdf):
annual normalization, nearest central means and same-period seasonal restoration.
The stable/short/mixed endpoint conventions and span gates are independently
qualified through public JD+ 3.9.0 APIs, not attributed to the printed general
rule. No upstream numerical implementation, bytecode or decompilation was read.
Runtime does not load reference targets. B/C/D placement and other modes remain
separate qualification work.
