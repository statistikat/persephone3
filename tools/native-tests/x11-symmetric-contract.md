# Symmetric X-11 filter foundations (22a.1)

```r
native_x11_filter_spec("trend", frequency=12L)
native_x11_filter_spec("s3x5", frequency=12L)
native_x11_filter_spec("henderson", frequency=12L, length=13L)
native_x11_filter(series, "henderson", length=13L)
```

These internal Java-free APIs define and apply central filters; they do not
perform an X-11 decomposition or choose filters automatically. The package's
native full-fit capability remains unavailable. Supported input domains are
quarterly/monthly univariate time series. Explicit types are `trend`, `s3x1`,
`s3x3`, `s3x5`, `s3x9`, `s3x15` and `henderson`. Only Henderson accepts an explicit
length: an odd integer from 1 through 101. Unsupported `stable`/automatic filters,
other frequencies and unimplemented choices fail explicitly.

## Mathematics and independent authorship

The centered trend filter is a `2 x frequency` average: its two outer weights
are `1/(2*frequency)` and its interior weights are `1/frequency`, at consecutive
observation offsets. Seasonal `3 x N` filters convolve uniform three- and N-term
averages, with offsets measured in seasonal periods. The [ABS seasonal-adjustment
explanation](https://www.abs.gov.au/websitedbs/d3310114.nsf/51c9a3d36edfd0dfca256acb00118404/5fc845406def2c3dca256ce100188f8e)
describes these seasonal averages; the [Census X-13 reference manual](https://www2.census.gov/software/x-13arima-seats/x13as/unix-linux/documentation/docx13as.pdf)
identifies the supported named options. Their presence does not imply automatic
selection or complete X-13 parity.

Henderson weights reproduce a cubic and minimize the squared third differences
of the zero-extended weight sequence. This criterion is described in section 4.2
of [Alexandrov et al. (2008)](https://www.census.gov/content/dam/Census/library/working-papers/2008/adrm/rrs2008-03.pdf).
Appendix A.1 of [Findley et al. (1998)](https://www.census.gov/content/dam/Census/library/working-papers/1998/adrm/jbes98.pdf)
gives the equivalent triple-product/quadratic moment construction. For half-width
`m`, set `n=m+2`. The independently authored implementation
evaluates the closed coefficient for `j=-m,...,m`:

```
315 ((n-1)^2-j^2) (n^2-j^2) ((n+1)^2-j^2) (3*n^2-16-11*j^2)
----------------------------------------------------------------
          8*n*(n^2-1)*(4*n^2-1)*(4*n^2-9)*(4*n^2-25)
```

Products use long-double intermediates before conversion to binary64; mirror
weights are assigned identically. An independent constrained minimum-roughness
solve checks lengths through 23, while polynomial reproduction and pinned public
coefficients cover every supported odd length. Lengths 1 and 3 both reduce to the
identity; exact zero exterior taps are removed so their effective radius is zero.
The requested `length_parameter` remains recorded. This agrees with public
`makeHendersonFilter()` coefficients and support; no numerical Java source or
bytecode was inspected or translated.

## Domains, missing values and execution

The definition records type, frequency, requested length parameter, actual term
count, signed integer offsets, weights and effective radius. Values use
`sum_j weight[j]*series[i+offset[j]]`. The existing finite-filter primitive retains
the full input calendar, marks unsupported endpoints missing and propagates
internal missingness only through nonzero taps. There is no padding,
extrapolation, recycling or renormalization. Henderson length 13 has exact zero
interior taps, so missing values at those taps do not affect that output.
All active-tap missingness is resolved before arithmetic. Infinity and non-finite
filter arithmetic fail explicitly; an exception does not mutate the input.

The R result exposes a full-calendar `values` series, missing mask, input and
central domains, central/boundary rows, actually available rows and status.
Internal gaps may leave holes in available rows. Too little context or all-missing
active input gives `no_available_outputs`; the raw vector boundary additionally
supports empty input. `central_domain` describes the support domain, not a claim
that every central observation is available. Arbitrary monthly/quarterly start
phases and negative years retain exact alignment.

The algorithm and header contain no R/Rcpp types and this standalone harness
links without R, BLAS or LAPACK. The Rcpp boundary validates types and owns input
copies. Independent output entries reuse the existing optional OpenMP path with
unchanged per-output arithmetic. R callbacks run only on the calling thread;
interrupts and worker exceptions propagate after joining.

## Qualification scope

The mandatory suite compares 114 filter definitions (two frequencies, the six
moving-average choices and all 51 Henderson lengths) against frozen public
coefficients and sampled applications. Independent tap sums, seasonal harmonic
response, whole cubic reproduction, constrained roughness, calendar/phase,
missing/zero-tap/short/empty/range and invalid-option checks supplement them.
A 40,000-observation path checks actual serial/OpenMP equality and response units.
The standalone sanitizer harness checks all 33 interruption boundaries and reuse.
Prospective budgets are absolute `1e-14` for coefficients, scaled `2e-12` for
filtered values and absolute `1e-9` for the independently solved roughness weights.

Public fixtures use finite input and only public factories, fields, coefficient
accessors and `DataBlock` application. They do not qualify Java missing-value
behavior, asymmetric endpoint filters, filter selection, extreme-value treatment,
B/C/D passes, preadjustment or full seasonal adjustment. Those remain later slices.
