# Fixed additive initial tables (22b.2)

```r
persephone3:::native_x11_initial_additive(series, filters, mode = "additive")
```

This internal entry point composes B1, B2, B3, B4a, B4anorm and B4d. It accepts
a univariate quarterly or monthly `ts` with at least three annual cycles and
one fixed filter or a filter for each calendar period: `stable`, `s3x1`, `s3x3`,
`s3x5`, `s3x9` or `s3x15`. Named vectors must identify every calendar period
exactly once and are reordered by those labels. Other modes, automatic requests,
shorter inputs, infinities and malformed inputs are rejected explicitly.

For frequency `f`, half-width `h=f/2` and original input `Y` of length `n`:

1. B1 is a copy of the supplied working series, with its original calendar.
2. B2 is the centered annual `2×f` mean, with weights
   `(1/2, 1, ..., 1, 1/2)/f`, on original rows `h+1` through `n-h`.
   Its center is accumulated first, then equal-distance pairs. If an unweighted
   pair overflows, its two weighted terms are added instead. This keeps the
   original finite/range contract while reducing rounding differences before
   subtracting a large level to obtain a small component.
3. B3 is `Y - B2` on those same rows.
4. B4a applies the qualified fixed seasonal processor to B3, using the cropped
   first calendar period and the existing `short_series="x11"` policy.
5. B4anorm applies the qualified additive seasonal normalizer to B4a with no
   restoration. It uses the original requested filters, rather than substituting
   the processor's short-history selections.
6. B4d is `B3 - B4anorm`, again on the cropped calendar.

The three-year minimum leaves the two annual cycles required by normalization
after cropping. Five/twenty-year selection and normalization gates are evaluated
on the cropped history. For example, six original years leave five years: a
requested 3×9 can select stable seasonal means while normalization still uses
nearest central means. Mixed requests always retain their calendar labels.

`tables` contains all six `ts` values in order. `row_basis` maps each table to
one-based original input rows; `available_rows` uses those original row numbers.
`input_domain`, `cropped_domain`, `drop` and cropped `first_period` preserve the
calendar contract. Nested `seasonal` and `normalization` are identical to the
separate primitive calls on B3 and B4a, including selected/requested filters,
endpoint rules, masks and source rows. `complete` means every table value is
available. `status` is `available` when any B4d row is available and otherwise
`no_available_outputs`; it does not claim a fitted or complete decomposition.
`scope="additive_initial_tables_only"` and `complete_b_pass=FALSE` are explicit.

Missing operands propagate through the staged nonzero support. Support is not
simplified algebraically across stages: cancellation of two coefficients does
not make an earlier missing operand observed. There is no imputation or omitted
missing mean. This retains the existing native seasonal contract. Public JD+
stable phase means omit missing observations; six gap controls audit that
difference across 18 B4 tables. Full finite-table agreement is qualified
separately. Overflow/non-finite arithmetic fails atomically under the existing
filter contract; no partial result escapes.

The independent C++17 core has no R/Rcpp, BLAS or LAPACK dependency. Owned
OpenMP loops copy input, compute central trends, subtract components and run
the existing seasonal/normalization primitives. Each output retains its serial
arithmetic order. Workers touch only C++ state; interrupts and R conversion
stay on the caller, and worker errors are rethrown after joining. Shared Rcpp
serialization and R calendar decoration remain outside the core.

The mandatory suite compares every table to frozen public B-step getters and a
matched public primitive chain, and independently composes published filters.
Complete impulse operators, structural missing support, closed linear-trend /
annual-pattern controls, crop calendars, input immutability, strict/range gates,
serialization and 100,003-row worker equality are covered. The standalone
`test_x11_initial.cpp` harness links `numerics.cpp`, `x11_filters.cpp`,
`x11_seasonal_endpoints.cpp`, `x11_seasonal_processing.cpp`,
`x11_normalization.cpp` and `x11_initial.cpp`, with optional OpenMP and sanitizers.
It checks all 42 interruption boundaries and subsequent reuse.

This step has no extreme-value correction, B5 or later B tables, complete B/C/D
passes, ARIMA extension or seasonal-adjustment fitting. See
`tools/reference/x11-initial-notes.md` for evidence and timing scope.
