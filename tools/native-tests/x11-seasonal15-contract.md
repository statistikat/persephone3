# Explicit 3×15 seasonal endpoints (22a.2b.2b)

```r
native_x11_filter(series, "s3x15", endpoints="seasonal")
native_x11_seasonal_spec("s3x15", 12L, available=0L, side="left")
```

The explicit seasonal primitive now includes 3×15 endpoints, with the same
full-calendar, phase/mask and central-arithmetic contract as the first four
seasonal filters. This slice requires at least twenty observations in every
phase. It does not choose a stable or shorter filter automatically.

## Independently constructed finite-decimal convention

The symmetric 3×15 kernel is the convolution of uniform averages of lengths
3 and 15, giving seventeen weights with denominator 45 and annual radius 8.
The [Census reference manual, Table 7.56](https://www2.census.gov/software/x-13arima-seats/x13as/unix-linux/documentation/docx13as.pdf)
lists the seasonal filter choice. Its endpoint decimal convention below was
characterized by public JDemetra factory/processor calls, not taken from that
manual or from numerical implementation source.

For a left boundary, extend the annual phase by eight virtual observations,
each equal to the mean of the first five actual observations. Convolve with
the symmetric kernel and collapse the result onto actual observations. For
each endpoint row, round its coefficients to five decimal places, then adjust
the coefficient furthest into the observed sample so that the row sums to one.
Mirror this complete definition at the right boundary. Central rows retain
their unrounded symmetric weights.

The native construction uses integer numerators over 225 for the collapsed
extension. Each coefficient except the closing tap is rounded using integer
arithmetic to units of `1/100000`; there are no half-way ties. The last
numerator is `100000 - sum(previous numerators)`. This avoids accumulating a
floating-point rounding correction. It reproduces all public endpoint rows
without storing a reference coefficient table or reading fixtures at runtime.

For the outer left row, weights are `(16000,16000,16000,16000,16000,6667,6667,
4444,2222)/100000`. With four observations available towards that boundary,
the closing weight is `2219/100000`. All rows have positive weights and
reproduce constants. This is the explicit `coefficient_convention="x11_decimal_5"`;
applying the unrounded virtual extension gives a different filter, whose point
difference is recorded separately. It is not silently substituted.

The independent R oracle forms a dense extension matrix from actual annual
values, composes it with a convolution kernel, then rounds/closes boundary
rows using R arithmetic. It does not use native coefficients or integer
construction. Full basis-vector and actual-input comparisons qualify both
the operator and application. Coefficient errors use absolute `1e-14`;
point errors use `2e-12*(1+abs(target))`. The small closing coefficient is not
subjected to an unintended relative `all.equal` rescaling.

## Support and reporting

The result reports `minimum_phase_length=20`, the decimal convention, named
calendar-phase lengths and each endpoint row's side/available annual support.
Endpoint specifications expose radius 8 and signed original-calendar offsets.
Quarterly/monthly inputs, partial years and negative starting years are
supported. Existing 3×1/3×9 results now identify `published_fixed_weights`,
and 3×3/3×5 identify `virtual_values_rational`; their numerical results and
minimum support are unchanged.

24 separate public controls cover seventeen/nineteen complete years and
partial twentieth years at three starting periods. On this matrix, the public
processor selects phase means when `ceiling(n/frequency)<20`, and the 3×15
operator once the partial twentieth year is present. The native explicit
primitive conservatively rejects every control, since some phases have fewer
than twenty observations. Those captures establish a future selection-policy
test, not an implicit native fallback. Minimum mathematical tap support and
automatic X-11 selection remain distinct contracts.

All active missing taps propagate within their own phase only. Input storage
and outputs obey the existing finite-range checks; input is never mutated.
The symmetric pass reuses owned OpenMP. Small endpoint loops/callbacks stay
on the caller. No new worker calls R, Rcpp, BLAS, LAPACK or Java. Core/header
and standalone harness link without any of those numerical runtimes.

## Qualification

6,389 new mandatory assertions cover 30 public operator/point cases, dense
extension/convolution/closure, basis vectors, constants/mirrors, exact central
arithmetic, phase isolation, gaps, conservative support, serialization, declared
convention differences and 40,003-observation serial/OpenMP/unit checks. The
previous seasonal suite remains at 8,065 passing assertions with only the
formerly unsupported type test updated to an actually unsupported type.

Strict serial/OpenMP ASan/UBSan harnesses check 106 new interruption boundaries
for 3×15 and retain the 68 3×9 boundaries, with reuse and actual worker equality.
Short-series/automatic filter selection and B/C/D passes follow separately.
Evidence and filter-only timing scope are in `tools/reference/x11-seasonal15-notes.md`.
