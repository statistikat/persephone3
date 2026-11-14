# Explicit seasonal endpoint evidence (22a.2b.2a)

`inst/tinytest/fixtures/native/x11-seasonal/` adds 120 pinned public cases for
quarterly/monthly 3×1, 3×3, 3×5 and 3×9 seasonal filters. The matrix crosses
five lengths with three starting periods for every frequency/filter pair.
It includes exact minimum support, incomplete years and longer series.
`X11SeasonalFiltersFactory` and `X11SeasonalFilterProcessor` supply full outputs;
input basis probes capture all boundary rows of one relative phase. Unstored
coefficients are zero. Full-series points qualify all phases. Four separate
S3X1 controls record stable phase means at three/four annual observations;
that automatic choice remains outside the native explicit-endpoint scope.

Only public class/signature inspection and enum/factory/processor calls were
used. No numerical Java implementation or bytecode was read or translated.
Published Census weights define 3×1/3×9, and published ABS virtual-value rules
independently construct 3×3/3×5. The separate R oracle extends actual annual
values and uses convolution/filtering; it does not use native coefficients.
Exact sources and formulas are in `tools/native-tests/x11-seasonal-contract.md`.
SHA-256 manifests pin captures, their generation scripts and deterministic input
definitions. Existing corpora/reports are unchanged.

```sh
Rscript tools/reference/verify-x11-seasonal.R
mkdir /tmp/persephone-x11-seasonal-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-x11-seasonal.R \
  --live --timing --output=/tmp/persephone-x11-seasonal-report
```

Timings use all 120 short cases and eight 40,003-observation cases, one per
frequency/filter pair. Native includes full filtering and R calendar/phase/
boundary metadata. JDemetra includes enum/factory/processor calls and vector
serialization. Three warmed alternating rounds use calibrated batches of at
least 50 ms where possible. Loading/startup and explicit pre-batch GC are
excluded; natural in-batch GC is included. Use the installed optimized native
library with actual OpenMP workers. These are filter-only workloads, not
complete X-11 decompositions or fitted models. The matrix differs from earlier
filter reports; their totals remain separate.

The offline verifier passes **1,875 checks / 120 cases** without Java namespaces.
Maximum absolute coefficient error is **3.886e-16**, frozen scaled-point error
is **4.986e-15**, and independent virtual-extension scaled-point error is
**4.193e-16**. Prospective `1e-14` coefficient and `2e-12` scaled-point budgets
are unchanged. Independent virtual-extension errors are reported only for
3×3/3×5; 3×1/3×9 are fixed published coefficient definitions.

The live report passes **2,894 checks / 120 cases**. Maximum live scaled-point
error is **3.797e-16**; the other errors and budgets above are unchanged. The
report records **325,690 timed calls**, including warm-up/calibration retries.
Median summed times from three warmed alternating rounds are:

| Input matrix | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 120 short cases, 20–279 observations | 0.022806 | 0.019017 |
| 8 long cases, 40,003 observations each | 0.029077 | 0.003816 |

Native takes about 20% longer for the short matrix and is 7.62× slower for the
long matrix. This comparison includes R calendar/phase/boundary metadata on the
native side and enum/factory/processor/serialization on the public side. It
does not establish a complete decomposition or fitted-model speed ratio.
Versioned raw timings, errors, source hashes and actual two-worker metadata are
in `tools/reference/information/2026-11-14-x11-seasonal/`. Performance remains
an explicit follow-up; numerical budgets and full-calendar output are retained.
