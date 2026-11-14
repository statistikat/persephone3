# Symmetric X-11 reference evidence (22a.1)

The additive fixture in `inst/tinytest/fixtures/native/x11-symmetric/` records
114 public filter cases: quarterly and monthly centered trend filters, the five
seasonal `3 x N` choices, and all 51 odd Henderson lengths from 1 through 101.
The input is a deterministic formula with a non-January/non-first-quarter start
and incomplete final year. No backend result defines the input or sample dates.
The public `makeHendersonFilter(3)` factory returns identity weights and radius
zero, as does length 1; this canonical support is retained natively.

Only public factory/static-field calls, `weightsToArray()`, bounds and `DataBlock`
application were inspected or executed. Seasonal filters are applied separately
to each calendar phase, then returned to original row positions. No numerical
Java source, bytecode or implementation constants were copied or translated.
Captured CSV values are numerical fixtures with declared tolerances, not a claim
of bitwise serialization parity. SHA-256 manifests pin all three artifact files,
the generation inputs/scripts and the existing backend oracle manifest. Earlier
corpora remain unchanged.

The offline verifier checks hashes, support, constant/cubic reproduction,
independent constrained minimum roughness, direct tap sums and frozen public
points without loading Java namespaces. The live option additionally compares
all filtered rows using pinned JDemetra 3.9.0 APIs. Public comparisons use finite
input; native missing/zero-tap behavior has independent mathematical tests.
Prospective budgets are absolute `1e-14` for coefficients and scaled `2e-12` for
points; the separate minimum-roughness solve uses absolute `1e-9` through length
23. Full seasonal adjustment, asymmetric endpoint weights and automatic filter
selection are outside this evidence.

```sh
Rscript tools/reference/verify-x11-symmetric.R
mkdir /tmp/persephone-x11-filter-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-x11-symmetric.R \
  --live --timing --output=/tmp/persephone-x11-filter-report
```

The filter-only benchmark covers all 114 short inputs (257 observations) and
14 long inputs (40,000 observations) that exercise the worker threshold. Native
includes definition construction, application and R calendar metadata; JDemetra
includes public factory/field access, coefficient access, filtering and output
serialization. These are primitive workloads, not fitted-model or end-to-end
X-11 timings. Three warmed alternating rounds use calibrated batches of at least
50 ms where possible. Preparation/loading/startup and explicit pre-batch GC are
excluded; natural in-batch GC is included. The versioned report is in
`information/2026-11-14-x11-symmetric/`.

The report passes **1,940 checks** and records **338,695 calls**, including
warm-up and calibration. Maximum absolute coefficient error is
`5.552e-16`; maximum scaled frozen/public-live point errors are `4.994e-15` and
`1.296e-15`. Independent tap summation differs by at most `5.339e-16` scaled.
The offline-only verifier passes 817 checks without loading Java namespaces.

| Input matrix | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 114 short cases (257 observations each) | 0.017557 | 0.024255 |
| 14 long cases (40,000 observations each) | 0.047179 | 0.017812 |

These are medians of each round's summed per-filter times. Native is about 28%
faster on the short matrix and 2.65 times slower on the long matrix. The two
actual native worker threads do not establish a speed advantage; range/mask
validation, metadata and R/native boundary costs remain in the measured path.
Further performance work must retain the same numerical and timing protocol.
