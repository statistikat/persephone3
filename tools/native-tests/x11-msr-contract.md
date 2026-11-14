# MSR component change tables (22a.2b.3c.2a)

```r
native_x11_msr_table(seasonal, irregular, mode = "additive")
```

This Java-free primitive accepts two already estimated components on identical
monthly/quarterly `ts` domains. It computes per-calendar-phase annual changes,
finite-length corrections and ratios. It does not estimate a seven-term pilot,
trim a partial year or extensions, select a seasonal filter, normalize seasonal
factors or perform an X-11 pass. The separate selector delivered in 22a.2b.3c.2b
now supplies the pilot, trimming, iteration and D-final plan resolution.
The core/header have no R/Rcpp, BLAS or LAPACK dependency.

Additive, log-additive and pseudo-additive labels use mean absolute annual
differences on the supplied working scale; no logarithm is taken. Multiplicative
uses mean absolute annual differences divided by the previous component value
and requires both components to be strictly positive. Public selector probes
separately qualify the pseudo-additive absolute-change convention; it differs
from the Henderson I/C convention. Estimating the components remains separate.
Quarterly tables use the same definition as monthly tables, without an I/C
frequency multiplier. Calendar rotations affect phase labels, not the global
ratio. No caller-supplied extension is excluded implicitly.

The mathematical factors are from Ladiray and Quenneville (2000),
[table 4.112, page 148](https://www.census.gov/content/dam/Census/library/working-papers/2000/adrm/x11-french.pdf).
The **explicit X-11 decimal/change-count convention**, qualified by public JD+
3.9.0 calls, is:

| Phase observations m | Seasonal correction | Irregular correction |
| --- | --- | --- |
| 2 or 3 | 1 | 1 |
| 4 | 3 | round5(90 / (2√842 + 21√2)) |
| 5 | round5(3√2 / (1 + √3)) | round5(60 / (√894 + 2√211)) |
| 6 | round5(5√6 / (8 + √2)) | round5(25√3 / (2√298 + √67)) |
| 7 or more | a k / (b + (k − 6)a), a=round6(√3), b=round6(6√2) | a k / (b + (k − 6)a), a=round6(5√6), b=round6(6√149) |

Here `k=m−1`. The long-phase coefficient rounding is applied separately to
the radical products, before evaluation. Runtime derives these coefficients
from the published radicals; it does not load or fit fixture constants. The
book labels its general formula with observation counts and describes an
observation-weighted global ratio. Public JD+ instead uses adjacent-change
counts in the long-phase factors and in global weighting. These conventions
are recorded explicitly, not attributed to the printed count notation.

Multiply each raw mean by its correction. The phase MSR is corrected irregular
mean / corrected seasonal mean. The global MSR is the ratio of the two
change-count-weighted corrected means. It is not a mean of phase ratios.
The independent R oracle uses ordinary differences, the algebraically divided
factor expression and `weighted.mean`, with an explicit complete-phase gate.
Short-series factors and exact versus rounded radical formulas remain distinct.

Each phase reports observation/change counts, both raw means, factors,
corrected means, component statuses and ratio/status. Zero seasonal movement
with positive irregular movement gives an available positive infinity. Both
zero gives `undefined_msr`; such a phase can coexist with an available global
ratio because the global ratio aggregates means. One observation, missing data,
nonpositive relative components and numerical range failures are separately
labelled. Available component/phase results are retained. Any unavailable
component phase makes the global table `incomplete_phases`; no phase is silently
dropped. If complete global aggregation or division fails its range gate, the
global status is `numerical_range`. Infinite input values are invalid, whereas
an exact zero-denominator positive-infinite ratio is a legitimate result.

Since 22a.2b.3c.2b, finite raw differences are computed directly to retain tiny
represented changes. Per-phase input scaling is used for absolute phases only
when subtraction would overflow; an average can still be finite. Change maxima
normalize the fixed-order serial mean reductions; weighted global means are
normalized and bounded by their largest input. Nonzero underflow and overflow
retain unavailable status. Owned OpenMP computes independent changes, using
bounded regions and deterministic caller reductions. R/Rcpp and interrupt
callbacks stay on the caller; cancellations discard local work and permit reuse.

Prospective evidence covers 640 public cases: both frequencies, three starts,
twelve spans around short/correction boundaries, four mode labels, smooth/rough
components, constants, one constant phase, gaps and signed/zero-denominator
controls, plus sixteen public-selector mode tables qualified using independent
dense mean-extension operators. Native positive-relative restrictions are not relaxed to reproduce
public signed or zero-denominator artifacts. Tests include unit/sign/level
invariants, phase retention, independent formulas, extreme ranges, strict API
validation and serial/worker equality at 100,003 rows. A standalone strict
C++/ASan/UBSan harness additionally checks every interruption boundary and
actual OpenMP teams. Frozen public targets never participate in native runtime.
