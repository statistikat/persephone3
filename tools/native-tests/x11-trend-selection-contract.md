# Henderson selection and I/C conventions (22a.2b.3b)

```r
native_x11_trend_select(series, mode="additive", backcasts=0L, forecasts=0L)
native_x11_trend_policy(12, ic_ratio=3.5)
native_x11_trend_policy(4, length=5L)
```

These internal Java-free primitives qualify a low-level pilot ratio, final
threshold selection and conventional endpoint ratios. They do not run an X-11
iteration, calculate seasonal adjustment, or apply a selected trend filter.
The input is an already seasonally adjusted working series. `log_additive`
expects already-logged values; no transformation is performed. `additive` and
`log_additive` use absolute changes; `multiplicative` and `pseudo_additive` use
relative absolute changes on the qualified positive-input/positive-pilot subset.
Support for these ratio calculations does not imply complete decomposition-mode
support. Explicit negative extension counts are rejected, not interpreted as years.

## Pilot and selection

Explicit backcast/forecast counts are removed before computing the pilot.
Only the symmetric 5-term quarterly or 13-term monthly Henderson interior
contributes. At least two pilot values are required, so the retained input must
have at least 6/14 observations. Let T be that pilot and I be the corresponding
input minus T (absolute mode), or input divided by T (relative mode). Compute
mean `abs(diff(T))` and `abs(diff(I))`, dividing each change by its preceding
component value in relative mode. Their ratio is `raw_ic_ratio`. The
`comparison_ic_ratio` multiplies it by 3 for quarterly data and by 1 for monthly
data. No endpoints, extension values or asymmetric weights enter these means.

The selector uses the comparison scale. Monthly ratios below 1 select length 9,
from 1 up to but excluding 3.5 select 13, and at least 3.5 select 23. Quarterly
ratios below 3.5 select 5 and the rest select 7. Positive infinity is allowed in
the scalar selector and selects the longest filter; negative/missing/NaN ratios
are invalid. The quarterly scaling is therefore consistent with a raw cutoff
of 3.5/3. Exact and adjacent floating-point thresholds are qualified separately.

The endpoint ratio is a different quantity: quarterly lengths through 5 use
0.001 and longer lengths use 4.5; monthly lengths through 9 use 1, lengths 11/13
use 3.5, and longer lengths use 4.5. The fixed-length policy supports the existing
odd lengths 1–101. Identity lengths 1/3 have no asymmetric rows; their returned
ratio is only the public factory's convention. Use the selected length and
`endpoint_ic_ratio` with the existing explicit Musgrave filter. The estimated
comparison ratio is never silently substituted for the conventional endpoint
ratio. The scalar policy requires exactly one of length and comparison ratio.

The pilot/threshold formulation follows the published variable-trend procedure
and is independently characterized by public JD+ calls. Public context controls
show the pilot ignores an explicit trend length and the `excludefcast` flag,
but always removes supplied extension counts. This does not qualify B/C/D stage
placement; earlier stages may restrict the final selector's choices.
[Findley et al. (1998), Appendix A.2](https://www.census.gov/content/dam/Census/library/working-papers/1998/adrm/jbes98.pdf),
[Census X-13 manual, section 7.19](https://www2.census.gov/software/x-13arima-seats/x13as/unix-linux/documentation/docx13as.pdf).

## Output, range and execution

The serializable selection result reports both mean changes, both ratios,
frequency scaling, pilot/change counts, selected length and endpoint ratio.
Named `first`/`last` bounds identify the retained input, pilot values and the
later observation of each first difference in the original full calendar;
empty support uses an empty vector. The original input domain and mode remain
attached. Counts/bounds describe nominal calendar support, including missing
positions, and are not counts after dropping unavailable observations.
Status is explicit: available, insufficient observations, missing
observations, nonpositive relative input/pilot, undefined I/C, or numerical range.
Unavailable selection fields are NA. A constant series has zero changes and an
undefined ratio; it does not acquire the longest filter through NaN comparisons.
Missing values in excluded extensions do not enter the calculation. Missing
values inside the retained input invalidate it; none are omitted or imputed.

Relative calculations require positive retained input and positive pilot values.
This also rejects positive series whose Henderson overshoot makes the pilot
nonpositive. Public signed-denominator outputs on negative components are retained
as unqualified controls, not advertised as supported native ratios. Infinity is
never an input missing value. Core storage/masks and all counts are validated.

The input is normalized by its maximum absolute retained value before filtering.
This keeps finite-filter arithmetic within range and makes the ratio invariant
to units. Nonnegative changes use scale-normalized serial means; additive mean
changes are restored to input units. Lost nonzero values/means and nonfinite
intermediates produce explicit range status. A positive numerator over an exact
zero trend-change mean gives an infinite ratio and the longest valid selection;
zero over zero stays unavailable. This is not a tolerance-based near-zero rule.

Core/header code links without R/Rcpp, BLAS or LAPACK. Owned OpenMP handles the
pilot's independent filter rows, irregular values and changes. All reductions
keep fixed serial order; bounded caller polls cover copies and reductions.
No worker calls R or a callback. Serial/OpenMP ASan/UBSan harnesses cover 490
interruption boundaries (245 per change mode), worker equality and reuse.

## Qualification and remaining scope

376 frozen public ratio cases cover regular inputs, short pilots, extension
trimming, threshold neighborhoods, four working-mode labels and explicit
unsupported/undefined controls. 20 scalar cases cover both frequencies and
exact/adjacent thresholds. All 102 fixed-length default ratios and 292 selected
asymmetric rows are qualified against public factory output. An independent R
minimum-roughness solve supplies pilot coefficients; a separate constrained
minimum-revision solve checks endpoint definitions. Neither uses native weights.
7,915 mandatory R assertions also cover calendar metadata, missingness,
serialization, closed alternating ratios, cubic reproduction, extreme ranges,
unit changes and 100,003-row serial/OpenMP equality.

Ratio/mean checks use scaled `2e-12`; frozen endpoint weights use absolute
`1e-14`. The separate dense minimum-roughness/constrained solve has absolute
`1e-10` weight tolerance at the longest lengths. This looser independent-solve
budget does not relax public coefficient comparisons. Full-fit capability stays
false. Staged seasonal/MSR choices, B/C/D placement and full integration follow.
