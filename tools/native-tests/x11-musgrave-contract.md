# Explicit Musgrave Henderson endpoints (22a.2a)

```r
native_x11_musgrave_spec(12L, length=13L, available=0L, ic_ratio=3.5)
native_x11_musgrave_spec(12L, length=13L, available=2L, ic_ratio=3.5, side="left")
native_x11_filter(series, "henderson", length=13L,
                  endpoints="musgrave", ic_ratio=3.5)
```

These internal Java-free APIs add explicit asymmetric Henderson filters. The
existing default `endpoints="none"` keeps the symmetric-only result unchanged.
Musgrave mode requires an explicitly supplied finite nonnegative I/C ratio and
an odd Henderson length 1 through 101. Quarterly/monthly frequencies are supported;
there is no automatic length/ratio choice, seasonal endpoint policy or complete
X-11 decomposition. Supplying a ratio without Musgrave mode, or Musgrave mode
with a non-Henderson type, fails explicitly.

## Mathematics and conventions

Appendix A.2 of [Findley et al. (1998)](https://www.census.gov/content/dam/Census/library/working-papers/1998/adrm/jbes98.pdf)
describes a constant-reproducing filter minimizing expected squared revision
under a linear mean and independent irregular noise. Here `r=I/C` is
`E|delta irregular| / |trend slope| = 2*sigma/(sqrt(pi)*abs(beta))`, the reciprocal
of that appendix's `R`. For full symmetric weights `w[1:N]`, the first `M`
available observations, and `c=(M+1)/2`, independent minimization gives

```
v[j] = w[j] + sum(w[(M+1):N])/M
       + (j-c)*sum((i-c)*w[i], i=M+1,...,N)
         / (M*(M^2-1)/12 + pi*r^2/4)
```

The implementation was authored from this mathematical criterion and formula;
no numerical Java source or bytecode was inspected or translated. An independent
rectangular least-squares solve in an orthonormal constant-free basis qualifies
the minimizer, separately from pinned public coefficients. At `r=0`, the limiting
solution reproduces both constants and a linear trend, then minimizes squared
coefficient distance. The closed H5/rightmost weights are
`(-105/572, 105/286, 467/572)`. JDemetra 3.9.0's public factory yields NaN at zero;
that result is recorded as a discrepancy, not used as a numerical target.

For effective Henderson radius `h`, `available=q` lies in `0,...,h-1` and counts
observations between the filtered row and its boundary. Right-side support is
`-h,...,q`; left-side weights and offsets mirror it exactly. A dedicated definition
accessor reports these weights, support, ratio, side and requested length. Ratios
above one use reciprocal scaling to avoid squaring a large finite input. A
vanishing slope correction approaches the constant-only minimum-revision limit.
Negative/nonfinite ratios and invalid sides/support fail explicitly.

## Application, calendars and missing values

The input must contain at least the effective symmetric length `2*h+1`. This
first slice does not implement simultaneous truncation at both boundaries.
Henderson lengths 1 and 3 are canonical identity filters and need only one input;
requesting a separate endpoint definition for them fails because their radius
is zero. Empty input is rejected in Musgrave mode.

Central outputs retain exactly the existing Henderson arithmetic. The first and
last `h` outputs use their corresponding mirrored endpoint weights. The result
retains the full input calendar and central/boundary/available rows; it adds the
explicit ratio and a table of endpoint rows, sides and available support. Its
`weights` field continues to describe the symmetric central filter. Use the
endpoint definition accessor to inspect the individual boundary weights.

A missing nonzero tap makes its output unavailable. Zero taps do not propagate
missingness, and masks are resolved before arithmetic. No observations are
extrapolated, replaced or renormalized. Infinity and nonfinite intermediate
arithmetic fail without modifying input. Constant or linear reproduction is a
real-arithmetic identity; extreme binary64 intermediates can still overflow and
are not silently clipped.

Independent central outputs reuse owned OpenMP. At most 100 boundary outputs
run on the caller, with an interrupt poll before each left/right pair; their
small workload does not justify a separate worker region. R/Rcpp stays at the
binding layer. The standalone harness links without R, BLAS or LAPACK and checks
all 83 cancellation boundaries, reuse, serial/OpenMP identity, masks and ranges.

## Qualification and next scope

The mandatory suite has 11,022 assertions. Frozen public fixtures cover 628
endpoint definitions across both frequencies: every endpoint of lengths
5/7/9/13/23 at seven positive ratios, plus three endpoint positions at every
remaining odd length through 101 and an independently chosen ratio. Both sides,
independent constrained minima, zero/large-ratio limits, constant/linear controls,
short/identity/invalid inputs, serialization, calendar phases, missingness,
response units and 40,000-observation serial/OpenMP application are covered.

Prospective budgets are absolute `1e-14` for public weights, scaled `2e-12` for
points and absolute `2e-9` for the independent least-squares coefficient solve.
Public bulk `AsymmetricEndPoints` application supplements the frozen per-endpoint
DataBlock checks. These tests do not qualify automatic I/C selection, short-series
fallbacks, initial-trend/seasonal endpoints, extreme-value treatment or B/C/D passes.
Those are separate implementation slices. Reference protocol and timing scope
are in `tools/reference/x11-musgrave-notes.md`.
