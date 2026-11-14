# MSR pilot and seasonal selection (22a.2b.3c.2b)

```r
native_x11_msr_pilot(series, mode = "additive")
native_x11_msr_policy(ratio)
native_x11_msr_select(series, mode = "additive", iteration_policy = "jdplus",
                      backcasts = 0L, forecasts = 0L, max_iterations = 1000L)
native_x11_msr_resolve(plan, selection)
```

These internal Java-free foundations accept monthly/quarterly univariate `ts`
data already on the requested working scale. They do not log the input or run
an X-11 decomposition. The standalone core/header use no R/Rcpp, BLAS or LAPACK.

For each annual phase, the pilot applies a seven-term simple moving average
after extending each end by three repetitions of the mean of its three nearest
real observations. Every phase must have at least three observations. The
seasonal pilot is that average. The irregular MSR component is the input minus
the pilot for additive, log-additive and pseudo-additive modes, or input divided
by the pilot for multiplicative mode. The latter requires strictly positive
input. The absolute-mode remainder is centered at zero, including pseudo-additive;
it is not a pseudo-additive decomposition factor with a unit shift. Constant
annual phases are preserved exactly. Missing, range or positivity failures
return a status and no partial pilot vectors.

The selector removes explicit backcast and forecast **row counts**, shifts the
calendar accordingly, then removes a trailing partial calendar year. It retains
the initial partial year. The public `excludeForecast` flag does not change
the explicit MSR horizon trimming in the qualified JD+ 3.9.0 context. There is
no implicit extension or forecast generation. Each MSR evaluation requires at
least five times the annual frequency in rows. Less initial history selects
the method default 3×5 with zero attempts, no table/pilot, and a missing ratio;
the public placeholder ratio zero is not presented as an estimated statistic.

Each attempt applies the pilot and the separately qualified MSR component table
on the retained prefix. Exact scalar cutoffs are:

| MSR | Result |
| --- | --- |
| [0, 2.5) | 3×3 |
| [2.5, 3.5) | Ambiguous; retry with one fewer year |
| [3.5, 5.5) | 3×5 |
| [5.5, 6.5) | Ambiguous; retry with one fewer year |
| [6.5, +∞] | 3×9 |

The maintainer selected **`jdplus` as the default**: continue removing one year
until the ratio is decisive or another evaluation would have fewer than five
years of rows. The latter condition gives the method default 3×5. Optional
`x11_five_year_cap` additionally defaults to 3×5 after five year removals
(six evaluations) if the ratio is still ambiguous. Decisive ratios take
precedence at the last allowed evaluation. Both policies run without Java.

`max_iterations` is a separate positive resource budget, default 1,000. If
another method evaluation is needed after exhausting it, the result is
`budget_exhausted`, with no selected filter or method default. It retains the
last evaluated pilot/table and the full trace. A method that has already reached
its stopping rule completes even when its last attempt also uses the last
budget unit. Resource limits never silently change method policy.

Results retain the policy, reason, selected request, default flag, iteration
count/budget, extension lengths, trimmed domains and original row bounds.
Each trace row records the attempt, rows, years removed, final original row,
global MSR, band and ratio status. The final nested pilot/table preserve their
own calendars and statuses. Missing/undefined/nonpositive/range/incomplete
results select no filter. Public NaN-to-3×9 or roundoff-dependent constant-series
choices remain unsupported controls. A positive infinite ratio is meaningful
when seasonal movement is exactly zero and irregular movement is positive.

The resolver accepts a canonical, pending all-MSR D-final seasonal plan and a
matching native selection. It repeats an available selected request across
calendar phases and records `msr_selected` or `msr_default`. Failure/budget
results keep missing resolved filters and a pending plan. It preserves both
inputs and attaches the complete selection. It does not apply the filter.
Subsequent fixed seasonal processing may independently choose a stable filter
for insufficient history; that length fallback is distinct from an MSR method
default. Other stages and mixed requests keep their delivered stage policies.

Owned OpenMP evaluates independent pilot rows and component changes in bounded
regions. Phase/input validation, fixed-order reductions, serialization and
interrupt callbacks run on the caller. Per-phase pilot scaling protects means
from overflow and explicitly rejects nonzero underflow. Component changes now
subtract finite raw values directly when representable, preserving tiny
represented changes; absolute phases fall back to normalized subtraction only
if an individual difference would overflow. This can retain a representable
average of an individually overflowing difference. Atomic cancellation discards
local work and permits reuse.

Qualification includes 530 public selector cases under both native policies,
592 public prefix tables, four exact public scalar cutoffs, complete independent
pilot operators, calendar/extension/short/undefined/range and resource controls,
resolution guards, serialization and 100,003-row worker equality. Whole-input
near-cutoff cases stay 1e-5 from the boundary; exact policy tests use the scalar
boundary and adjacent doubles. Standalone serial/OpenMP ASan/UBSan checks cover
354 interruption points with actual workers and no R/BLAS/LAPACK linkage.

Mathematics: Ladiray and Quenneville (2000),
[sections 4.3.9–4.3.10](https://www.census.gov/content/dam/Census/library/working-papers/2000/adrm/x11-french.pdf),
seven-term mean extension and published five-year-removal cap. Public APIs
establish the explicit JD+ iteration and exact-boundary conventions. Numerical
Java source, bytecode and decompilation were not consulted. Runtime never reads
reference values. B/C/D placement, seasonal normalization, extreme-value handling
and a full native X-13 fit remain separate work.
