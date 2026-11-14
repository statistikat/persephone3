# Fixed seasonal processing evidence (22a.2b.3a)

`inst/tinytest/fixtures/native/x11-seasonal-processing/` adds 756 pinned public
cases: 612 uniform and 144 mixed. Each frequency/filter matrix covers three
starting periods, one/two/four years, complete and partial five/six-year support,
seven/ten/eleven years, complete and partial nineteen/twenty years, and a longer
span. The mixed subset concentrates on short and partial-year gates. Public
enum/factory/processor calls capture full points and first-relative-phase basis
responses at all short rows or the outer eight rows plus a middle row on longer
inputs. Unstored coefficients in captured rows are zero.

No native output influenced capture inputs/rows/targets. No numerical source,
bytecode or decompilation was used. Independent annual operators combine
published fixed endpoint definitions, separately qualified R virtual-value
constructions and phase means. The complete native basis is compared with those
operators, while full public points qualify mixed calendar-phase behavior.
SHA-256 manifests pin all captures, generator scripts and deterministic input
definitions; earlier corpora/reports are unchanged.

```sh
Rscript tools/reference/verify-x11-seasonal-processing.R
mkdir /tmp/persephone-x11-processing-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-x11-seasonal-processing.R \
  --live --timing --output=/tmp/persephone-x11-processing-report
```

The prospective benchmark subset has 48 short cases, using start period three:
each uniform choice at five years−1, five years+1 and nineteen years+1, and
three mixed patterns at five years+1 and twenty years+1, for both frequencies.
Eight long cases have 100,003 observations: stable, uniform 3×5, uniform 3×15
and a mixed pattern at each frequency. Long stable work exercises actual owned
OpenMP in both phase means and output copying. Native timing includes calendar,
selection and row-method metadata; public timing includes enum/factory/processor
calls and vector serialization. Use the installed optimized native library.

Three warmed alternating rounds use calibrated batches of at least 50 ms where
possible. Startup/loading and explicit pre-batch GC are excluded; natural
in-batch GC is included. These are fixed-choice processor timings, not
data-driven MSR, full X-11 or fitted-model workloads. The matrix and long input
length differ from previous filter reports, whose totals remain separate.
Mathematical definitions, short-series policies, missingness and execution
contracts are in `tools/native-tests/x11-seasonal-processing-contract.md`.

## Recorded qualification and timing

The installed offline verifier passes 6,055 checks / 756 cases without loading
Java namespaces. The live/timing verifier passes 7,202 checks. Maximum errors
are 3.886e-16 against frozen coefficients, 2.256e-16 against independent
coefficients, 4.993e-15 against frozen points, 5.566e-16 against independent
points and 4.147e-16 against live points. Point errors scale by `1+abs(target)`;
budgets are absolute 1e-14 for coefficients and scaled 2e-12 for points.

The optimized installed build recorded 134,021 timed calls, including warm-up
and calibration retries. Median summed per-call times over the three rounds:

| Group | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 48 short cases (19–241 rows) | 0.007213 | 0.009398 |
| 8 long cases (100,003 rows each) | 0.038353 | 0.010297 |

Native takes about 23% less time on the short matrix and 3.72× the time on the
long matrix. The distinct return metadata remains included. This does not yet
isolate arithmetic from adapter costs or establish a full-fit speed comparison.
The complete machine-readable report, source hashes and actual two-worker
metadata are under `tools/reference/information/2026-11-14-x11-seasonal-processing/`.
Logs are under `/tmp/persephone-x11-selection-final/`.

The full package check passes 94,112 assertions / 50 files with the pre-existing
R-header warning and DESCRIPTION note. Isolated Java-free tests pass 92,783 /
30 native files + 34 backend checks; the independent serial build passes
92,779 / 30 with no OpenMP linkage. The new standalone strict ASan/UBSan serial
and OpenMP harnesses pass all 57 cancellation boundaries without R/BLAS/LAPACK
linkage. 31,345 new mandatory R assertions cover the matrix and invariants.
