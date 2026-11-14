# Henderson selection evidence (22a.2b.3b)

`inst/tinytest/fixtures/native/x11-trend-selection/` freezes public JD+ 3.9.0
AutomaticHenderson ratio/selection and Musgrave factory output. No numerical
source, bytecode, decompilation or implementation constants were inspected.
Public signatures, execution and published mathematical definitions were used.
Independent R pilot ratios agreed in 288 regular prospective cases before
native implementation (maximum scaled error 1.240e-13).

The final matrix adds 32 independently constructed threshold-neighborhood inputs
and 56 controls: 376 ratio cases total, of which 336 have supported available
native results. Twenty scalar cases probe exact/adjacent thresholds, zero, tiny,
large and infinite ratios. Fixed lengths 1–101 at both frequencies give 102
endpoint-default cases; 292 representative endpoint rows pin coefficient
definitions. Capture also checks that explicit trend length 23 and
`excludefcast=TRUE` do not alter the low-level pilot on this matrix. Extension
trimming remains active. There is no full-pipeline/stage equivalence claim.

The independent pilot composes a minimum-third-difference Henderson solve with
ordinary convolution, component removal and first changes. Endpoint checks use
a separate constrained minimum-revision solve. Public NaN-to-longest selections
for undefined ratios and signed-denominator behavior on nonpositive components
remain frozen controls; native outputs report unavailable status. Production
never reads fixtures or invokes Java. The scalar selection and endpoint-default
ratios have separate contracts. See `tools/native-tests/x11-trend-selection-contract.md`.

```sh
Rscript tools/reference/verify-x11-trend-selection.R
mkdir /tmp/persephone-x11-trend-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-x11-trend-selection.R \
  --live --timing --output=/tmp/persephone-x11-trend-report
```

The predeclared timing workload has 52 short cases and four 100,003-row cases.
The short subset uses additive/multiplicative regular inputs with no extensions
and the corresponding threshold neighborhoods. Long cases use the rough input
at both frequencies and both change measures. Native calls include domain,
counts, statuses and selected-filter metadata; public calls include context and
DoubleSeq construction and scalar ratio serialization. These are selection-only
timings, not decomposition or fitted-model timings.

Use the installed optimized library and three warmed alternating rounds, with
calibrated batches of at least 50 ms when possible. Loading/startup and explicit
pre-batch GC are excluded; natural in-batch GC is included. Run without concurrent
CPU-heavy work and retain each backend's raw elapsed times and source hashes.

## Recorded qualification and timing

The installed offline verifier passes 3,143 checks without Java namespaces;
live replay and timing pass 4,661 checks. Maximum scaled-ratio errors are
1.434e-13 against frozen/live public results and 1.135e-13 against the independent
pilot. Public default-weight error is at most 5.552e-16; the independent dense
minimum-roughness/constrained solve reaches 3.799e-11 at long lengths. Budgets
are scaled 2e-12 for ratios/means, absolute 1e-14 for public coefficients and
absolute 1e-10 for the separate dense coefficient solve.

185,729 timed calls include warm-up and calibration. Median summed per-call
times across three alternating rounds:

| Matrix | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 52 short cases, 6–207 rows | 0.001190 | 0.018512 |
| 4 long cases, 100,003 rows each | 0.010739 | 0.010400 |

Native is about 15.6× faster for short inputs and takes about 3.3% longer for
the long matrix. The declared construction/metadata costs remain included;
these timings do not isolate arithmetic or establish complete-fit performance.
The complete report, source hashes and actual two-worker metadata are in
`tools/reference/information/2026-11-14-x11-trend-selection/`.

The full package check passes 102,027 assertions / 51 files, with zero errors
and the pre-existing R-header warning and DESCRIPTION note. Isolated Java-free
validation passes 100,698 native assertions / 31 files + 34 backend checks;
the independent serial build passes 100,694 / 31 without OpenMP linkage.
Strict standalone serial/OpenMP ASan/UBSan checks cover 490 interruption
boundaries without R/Rcpp/BLAS/LAPACK linkage. The new R suite has 7,915
mandatory assertions. Logs are under `/tmp/persephone-x11-trend-selection-final/`
and `/tmp/persephone-x11-trend-selection-next/`.
