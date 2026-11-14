# Centered-trend mean endpoint evidence (22a.2b.1)

`inst/tinytest/fixtures/native/x11-mean/` adds 22 pinned public cases for quarterly
and monthly centered trend filters. Input lengths cover every integer from the
half-width through frequency+3, plus 17/43/257. Public `FilteredMeanEndPoints`
application supplies full finite-input outputs. Basis-vector probes capture all
operator rows for lengths through 17, and every boundary plus one central row
for longer inputs. Unstored coefficients in those rows are exact zero.

No native result sets the inputs, rows or targets. Only public signatures,
factory calls, symmetric DataBlock application and the bulk endpoint processor
were inspected/executed. No numerical source or bytecode was translated. The
independent oracle composes dense extension and moving-average operators.
SHA-256 manifests pin all artifacts, generation scripts and input definitions;
prior symmetric/Musgrave/model corpora remain unchanged. CSV comparisons use
explicit numerical budgets, not bitwise equality claims.

```sh
Rscript tools/reference/verify-x11-mean.R
mkdir /tmp/persephone-x11-mean-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-x11-mean.R \
  --live --timing --output=/tmp/persephone-x11-mean-report
```

The filter-only benchmark uses all 22 short cases and two 40,000-observation
cases (one per frequency). Native includes padding, filtering, calendar and
boundary metadata. JDemetra includes public factories, symmetric DataBlock and
bulk endpoint application, coefficients and vector serialization. Three warmed
alternating rounds use calibrated batches of at least 50 ms where possible.
Preparation/loading/startup and explicit pre-batch GC are excluded; natural
in-batch GC is included. Use the installed optimized native library with two
actual workers. These workloads are not fitted models or complete X-11 passes;
totals are not comparable with the different earlier filter matrices.

The pinned report passes **350 checks / 22 mean-endpoint cases**. Maximum
absolute independent/public operator error is **4.580e-16**; scaled frozen-point,
independent dense-point and live public-point errors are **4.908e-15**,
**4.202e-16** and **4.211e-16**, respectively. Prospective `1e-14` coefficient
and `2e-12` scaled-point budgets are unchanged. The offline verifier passes
**161 checks** without Java namespaces.

The report records **64,213 calls**, including warm-up/calibration. Median summed
filter-only times from three warmed alternating rounds:

| Input matrix | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 22 short cases, 2–257 observations | 0.003136 | 0.005000 |
| 2 long cases, 40,000 observations each | 0.008267 | 0.001502 |

Native takes about 37% less time for the short matrix and is 5.50× slower for
the long matrix. Native includes padding and calendar/boundary metadata; public
calls include factories, bulk processing, coefficients and serialization.
These are filter-only workloads, not complete decomposition/fitted-model times.
Preparation/startup and explicit pre-batch GC are excluded; natural in-batch GC
is included. The matrix differs from the earlier filter reports, whose totals
remain separate. Versioned raw timings and source hashes are in
`tools/reference/information/2026-11-14-x11-mean/`.
