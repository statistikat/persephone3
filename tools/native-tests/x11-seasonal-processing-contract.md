# Fixed seasonal choices and short-series processing (22a.2b.3a)

```r
native_x11_seasonal_process(series, "s3x5")
native_x11_seasonal_process(series, c("s3x5", "stable", "s3x9", "s3x1"))
native_x11_seasonal_process(series, filters, short_series="reject")
```

This Java-free processor accepts a specified filter for all periods, or one
choice per calendar month/quarter. Choices are `stable`, `s3x1`, `s3x3`, `s3x5`,
`s3x9` and `s3x15`. Unnamed vectors are in calendar-period order, independent
of the series start. A named vector must identify every period exactly once
using `"1"` through `"frequency"`; valid names are reordered explicitly.
Partial/malformed names, implicit recycling and automatic MSR/X11Default choices
are rejected. Each phase must have at least one original calendar position.

The existing `native_x11_filter(..., endpoints="seasonal")` remains unchanged
and continues to require its conservative explicit minimums. The new processor
has an explicit `short_series` policy, defaulting to `"x11"`. `"reject"` requires
the same per-filter support as the existing explicit API; stable filtering only
needs one position per phase. No Java fallback is ever used.

## Qualified policy and mathematical operation

The `"x11"` policy reproduces the **public low-level seasonal processor** on
the pinned matrix. It does not yet establish placement in complete X-11 passes
or the semantics of the top-level X-13 `sfshort` option. The Census manual
describes the stable seasonal mean and separate top-level short-series options;
the precise processor rules here were characterized by public API calls.
[Census reference manual, Tables 7.56 and section 7.19](https://www2.census.gov/software/x-13arima-seats/x13as/unix-linux/documentation/docx13as.pdf).

1. A requested stable filter uses the mean of the complete annual phase.
2. With fewer than five complete years (`floor(n/frequency)<5`), every other
   requested choice also uses its phase mean.
3. A requested 3×15 uses a phase mean when `ceiling(n/frequency)<20`.
   A partial twentieth year therefore permits 3×15 even in phases of length 19.
4. For a remaining 3×N choice, let annual radius be `h=(N+1)/2` and phase
   length be `m`. If `m<=h`, select the stable mean. Otherwise use the symmetric
   kernel wherever its complete window fits. At each boundary use endpoint
   rows `q=0..min(h-1,m-h-1)`, with the previously qualified weights.
5. Any middle rows lacking both full symmetric support and an applicable
   endpoint row use the complete phase mean. For example, 3×5 with five annual
   observations has two endpoint rows on each side and a mean at the middle
   row. For 3×9 with six observations, only the outer two rows use endpoint
   weights and the four middle rows use the phase mean.

These are fixed-choice/length policies, not data-driven selection. Their results
are independent across calendar phases. Requested/selected filters and reasons
are reported separately; a mean fallback is not hidden as the requested filter.
Short middle means retain the requested filter label with their own row method.
Lengths and `available` count calendar positions, including missing positions;
they are not counts after dropping missing values.

## Output, missingness and arithmetic

The serializable `persephone_native_x11_seasonal` result retains full-calendar
`values`, `missing`, `input_domain`, `available_rows` and status. Named vectors
give `requested_filters`, `selected_filters`, `phase_lengths`,
`selection_reasons` and `coefficient_conventions`. `row_methods` identifies every
row's calendar period and `stable`, `symmetric`, `left_endpoint`,
`right_endpoint` or `short_phase_mean` operation. Its `available` field is an
annual support count for asymmetric rows and NA otherwise. This processor has
no single global radius/weight vector when filters differ by phase.

The coefficient conventions remain the existing fixed/rational/3×15 decimal
definitions. At adequate support, all uniform native values and masks are
bit-identical to the existing explicit-filter API. Stable means first determine
the phase's maximum absolute value, then sum normalized values in their original
order and rescale after division by length. This avoids overflow from an
unscaled sum. A missing member invalidates that phase mean. Other filters
propagate only active missing taps, resolving missingness before arithmetic.
There is no omission, renormalization, imputation or input mutation. Infinity
and invalid/range conditions fail explicitly; R NA/NaN use separate masks.

Core/header code links without R/Rcpp, BLAS or LAPACK. Owned OpenMP covers
independent output rows and independent phase-mean accumulators. Mean work is
split into joined chunks of at most 8,192 annual observations per phase, so each
phase retains its exact serial summation order and the caller can interrupt
between bounded chunks. No worker calls R or a callback. Plan construction,
Rcpp copies and metadata conversion stay on the calling thread. Strict
serial/OpenMP ASan/UBSan harnesses check interruption at all 57 observed boundaries
(23 stable, 34 mixed), reuse, exact explicit-filter equality, masks/ranges and
actual worker equality.

## Qualification and remaining scope

31,345 new mandatory assertions cover 756 pinned cases: 612 uniform and 144
mixed, with two frequencies, three starting periods, complete/incomplete years,
the five/twenty-year gates, short middle rows and stable choices. Public basis
probes capture first-phase boundary/middle rows; native full basis application
and independent dense operators cover complete phase support and zero effects
on other phases. Full public points cover every phase. Independent operators
reuse published fixed weights or R virtual constructions, never native weights.
Named/reordered choices, serialization, missingness, extreme finite means,
strict-policy rejection and 100,003-row serial/OpenMP/unit/reversal tests pass.

Coefficient comparisons use absolute `1e-14`; points use
`2e-12*(1+abs(target))`. Prior fixture corpora and strict APIs remain unchanged.
Automatic Henderson/I-C conventions, staged X11Default/MSR choices, B/C/D
passes and a complete native fit remain separate work. See
`tools/reference/x11-seasonal-processing-notes.md` for reference/timing scope.
