# Explicit seasonal endpoints (22a.2b.2a)

```r
native_x11_filter(series, "s3x3", endpoints="seasonal")
native_x11_seasonal_spec("s3x3", 12L, available=0L, side="left")
```

This Java-free primitive applies one explicitly chosen seasonal filter to every
phase of a quarterly or monthly input. Types are `s3x1`, `s3x3`, `s3x5` and
`s3x9` in this initial slice; 3×15 is added separately in
`x11-seasonal15-contract.md`. Each phase needs at least `max(5,N+2)` observations
for the first four 3×N filters.
Partial years and any starting period are supported when that minimum holds.
Symmetric-only remains the default. An I/C ratio, mixed phase choices and
automatic/short-series selection are outside this step.

## Method definitions and independent construction

Let annual radius `h=(N+1)/2`. At a left endpoint with `q` actual same-season
observations to its left (`0<=q<h`), weights apply to annual offsets `-q:h`.
Multiply those offsets by the input frequency to obtain original calendar-row
offsets. Right endpoints mirror both coefficients and offsets exactly.

The 3×1 left boundary is `(61,39)/100`. The published 3×9 left rows, ordered
from the outside (`q=0`) inward, are:

| q | Numerators, denominator 1000 |
| ---: | --- |
| 0 | 246, 221, 197, 173, 112, 51 |
| 1 | 208, 192, 176, 160, 144, 92, 28 |
| 2 | 173, 163, 154, 143, 133, 123, 79, 32 |
| 3 | 141, 137, 132, 128, 123, 117, 113, 75, 34 |
| 4 | 84, 120, 118, 117, 116, 114, 113, 111, 73, 34 |

These are fixed mathematical definitions from Census Technical Paper 15,
Appendix B, Table 1A/D, printed p.61 (PDF p.63), not numerical implementation
constants extracted from Java. Replacing these with a full-precision Musgrave
formula would select a different filter. [Census, 1967](https://www.census.gov/content/dam/Census/library/working-papers/1967/adrm/shiskinyoungmusgrave1967.pdf).

For 3×3 and 3×5, integer convolution constructs exact rational weights from
published virtual-value rules. For 3×3, the closest missing annual value is the
mean of the first three actual values, and the furthest is the second actual
value. For 3×5, the closest two missing values are the mean of the first four,
and the furthest is the third actual value. Apply the symmetric 3×N kernel to
that extended input. The right rules are the exact mirror. These constructions
are explained in ABS, *An Introductory Course on Time Series Analysis*, May
2001, pp.5.20–5.22; the available PDF is a mirror of the ABS-authored document.
[ABS course](https://c.mql5.com/forextsd/forum/85/an_introductory_course_on_time_series_analysis.pdf).

Examples: 3×3 `q=0` gives `(11,11,5)/27`, and 3×5 `q=2` gives
`(9,13,13,13,8,4)/60`. The R oracle independently pads actual annual values and
uses convolution/filtering; it does not call native coefficient construction.
Published fixed tables qualify 3×1/3×9; virtual-extension identities supply the
additional independent qualification for 3×3/3×5.

## Calendars, missingness and execution

All original rows and their `ts` calendar are retained. Central values preserve
the symmetric primitive's arithmetic exactly. Each boundary row records its
calendar period, side and available annual observations. Named `phase_lengths`
use calendar periods, including incomplete years and negative start years.
The result retains the central filter's weights/radius and adds
`minimum_phase_length` and `endpoint_policy="seasonal_x11"`. Endpoint coefficient
specifications separately expose their signed offsets and annual radius.

An active missing tap makes its output unavailable. Other calendar phases are
unaffected, bit for bit. There is no omission, renormalization, gap filling or
replacement of original values. R NA/NaN become a separate mask and zero in
owned storage; core storage must remain finite even when masked. Infinity,
invalid options and arithmetic overflow fail explicitly without mutating input.

The full symmetric pass uses the existing owned OpenMP output path. Small
endpoint loops remain on the calling thread, with a callback per left/right
phase pair. The core and headers link independently of R/Rcpp, BLAS and LAPACK;
the Rcpp adapter owns input copies and serializable output values. Standalone
ASan/UBSan checks cover actual worker equality, all 68 callback cancellation
boundaries, reuse after interruption, rational weights and invalid/range cases.

## Qualification and remaining scope

8,065 new mandatory assertions qualify the additive corpus of 120 cases:
two frequencies, four filters, five lengths
(minimum, minimum+1, minimum+frequency−1, minimum+frequency, and 23 years+3),
and three starting periods. Public basis probes pin all endpoint rows for one
input-relative phase; full points cover every phase. Independent virtual rules,
constant/mirror identities, phase isolation, gaps, exact central arithmetic,
serialization, range and 40,003-observation serial/OpenMP checks supplement
public agreement. Coefficients use absolute `1e-14`; points use
`2e-12*(1+abs(target))`. No acceptance budget was adjusted to fit discrepancies.

Four separate public controls show that S3X1 selects stable phase means for
three/four annual observations. Native explicit endpoints reject those lengths;
the automatic selection policy is a later slice. The 3×15 convention, other
short-series policies and placement in B/C/D passes remain separate. Full native
seasonal-adjustment fitting is still unavailable. See
`tools/reference/x11-seasonal-notes.md` for capture and timing scope.
