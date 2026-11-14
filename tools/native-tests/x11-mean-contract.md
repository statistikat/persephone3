# Centered-trend mean-padding endpoints (22a.2b.1)

```r
native_x11_filter(series, "trend", endpoints="mean")
```

This explicit Java-free primitive extends the existing centered `2 x frequency`
trend average to the full quarterly/monthly input calendar. It does not choose
an X-11 endpoint policy automatically. Henderson and seasonal types do not
accept this mode, and an I/C ratio is invalid here. Symmetric-only remains the
default; Musgrave Henderson endpoints remain a separate option.

## Defined operation

Let `h=frequency/2` and let input length be `n>=h`. Compute the mean of the first
`h` observations and the mean of the last `h` observations. Pad `h` virtual
observations at each boundary using the corresponding mean, apply the ordinary
symmetric trend filter, then retain only the original input rows. The means may
use overlapping windows when `n<2*h`; both sides still have explicit support.
With exactly `n=h`, a complete input is filtered to its overall mean at every row.
Inputs shorter than `h` fail explicitly; there is no shorter-window fallback.

This is an independently authored composition of arithmetic averaging,
constant extension and convolution. Public `FilteredMeanEndPoints` factory/API
behavior was characterized with input basis vectors and full-series applications;
no numerical source, bytecode or implementation constants were inspected or
translated. Independent dense extension and moving-average matrices qualify
both the complete operator and the point values. For quarterly input `1:5`, the
result is `(27/16,35/16,3,61/16,69/16)` with padding means `(1.5,4.5)`.

Central outputs retain exactly the existing filter arithmetic. The result keeps
full-calendar values, mask, central/boundary/available rows and central weights.
It adds named `padding_means` and an `endpoint_rows` table giving the number of
left and right virtual observations used at each boundary row. A row appears
once even when it requires both sides. When no central support exists,
`central_rows` is empty and `central_domain` is NULL; available endpoint outputs
remain valid.

## Missingness, range and execution

A missing member of a boundary mean makes that virtual padding missing. Any
output requiring a missing active tap is unavailable. Internal gaps outside
that output's support do not affect it. There is no omission of missing values,
renormalization or replacement of original observations. Infinities and invalid
frequency/type/mode/length choices fail without modifying input.

Boundary means use a maximum-absolute-value scale before summing, avoiding an
overflowing unscaled sum. The existing checked double filter arithmetic is
retained. Opposite values near DBL_MAX have exact zero boundary means; fused
multiply-add can leave a nonzero filtering residual bounded relative to input
scale. The extreme cancellation control uses `1e-15 * max(abs(input))`, not an
absolute-zero requirement. This is distinct from the unchanged public point
budget on the finite reference matrix. No result is clipped or forced to zero.

The core/header have no R/Rcpp, BLAS or LAPACK dependency. The Rcpp boundary owns
input copies and returns ordinary serializable values. Padded filtering reuses
owned OpenMP with identical per-output arithmetic; bounded copy/crop callbacks
stay on the caller. Strict standalone ASan/UBSan builds cover 26 interruption
boundaries and reuse, masks/ranges and actual serial/OpenMP equality.

## Qualification and remaining scope

1,484 new mandatory assertions cover 22 public cases, all basis vectors,
independent complete operators, constants/mirrors, missing/overlapping supports,
minimum input lengths, negative-year phases, serialization, extreme means and
40,000-observation serial/OpenMP/unit comparisons. Public operator coefficients
use absolute `1e-14`; points use `2e-12*(1+abs(target))`. The frozen matrix spans
all lengths from `h` through `frequency+3`, plus 17, 43 and 257 observations,
for each frequency. Native full-fit capability remains unavailable.

This processor is an explicit building block. Its placement in initial-trend
or seasonal-normalization decomposition passes must still be qualified against
intermediate-table fixtures. Seasonal endpoints, automatic filter/short-series
selection and full B/C/D passes remain separate steps. Reference and timing
scope are in `tools/reference/x11-mean-notes.md`.
