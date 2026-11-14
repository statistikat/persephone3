# Musgrave endpoint reference evidence (22a.2a)

The additive `inst/tinytest/fixtures/native/x11-musgrave/` fixture captures 628
public endpoint definitions and 1,256 left/right points at pinned JDemetra 3.9.0.
Monthly and quarterly cases cover every endpoint of lengths 5/7/9/13/23 at I/C
ratios .001/.99/1/3.5/4.5/7/100, plus three endpoint positions for every other odd
length through 101 at `exp(1)`. Left application is the mirrored right public
filter on reversed input. No native output sets inputs, support or targets.

Only public factories, signatures, coefficient/bound accessors and DataBlock
applications were examined. No upstream numerical implementation or bytecode
was inspected or translated. SHA-256 manifests pin coefficients, points,
metadata and generation inputs/scripts. Existing symmetric/model corpora are
unchanged. CSV serialization has numerical tolerances rather than a bitwise claim.

The offline verifier requires no Java namespaces and checks frozen hashes,
constant reproduction, public support/weights/points and an independent
constrained least-squares minimum. Native zero-ratio limits have separate
rational/linear controls; the public zero-ratio NaNs are not accuracy targets.
The live verifier uses the public bulk `AsymmetricEndPoints` processor and
symmetric `DataBlock` application to qualify the whole output calendar of all
158 distinct length/frequency/ratio combinations. This is independent of the
per-endpoint application used during capture.

```sh
Rscript tools/reference/verify-x11-musgrave.R
mkdir /tmp/persephone-x11-musgrave-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-x11-musgrave.R \
  --live --timing --output=/tmp/persephone-x11-musgrave-report
```

The filter-only timing matrix uses 16 short cases (257 observations, both
frequencies, lengths 5/13/23/101 and ratios .5/3.5) plus four long cases (40,000
observations, both frequencies, lengths 13/101 and ratio 3.5). Native includes
calendar/boundary metadata. JDemetra includes public factories, bulk symmetric
and endpoint processors, coefficients and R-vector serialization. Neither path
fits a model or computes a complete decomposition. Three warmed alternating
rounds use calibrated batches of at least 50 ms where possible; startup/loading
and explicit pre-batch GC are excluded, natural in-batch GC included. Use the
installed optimized library, with two actual native workers. This is a different
matrix from the earlier symmetric-only benchmark; compare engines within each
report, not totals across reports.

The pinned report passes **3,916 checks / 628 endpoint definitions**, including
**158 distinct public full-filter applications**. Maximum absolute public-weight
error is **5.552e-16**; scaled frozen-point and live full-calendar errors are
**5.614e-15** and **1.296e-15**. The independent constrained least-squares
coefficient error is **9.381e-13**. Prospective `1e-14` weight, `2e-12` point and
`2e-9` independent-solve budgets are unchanged. The offline verifier passes
**3,149 checks** without loading Java namespaces.

The report records **29,023 calls**, including warm-up/calibration. Median summed
filter-only times from three warmed alternating rounds:

| Input matrix | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 16 short cases, 257 observations each | 0.003055 | 0.007840 |
| 4 long cases, 40,000 observations each | 0.023912 | 0.005198 |

Native takes about 61% less time for this short matrix and is 4.60× slower for
the long matrix. Native includes calendar/endpoint metadata; the public path
uses factories and bulk symmetric/endpoint processors with serialization. These
are filter workloads, not fitted-model or complete X-11 timings. Preparation,
startup and explicit pre-batch GC are excluded; natural in-batch GC is included.
The matrix differs from the symmetric-only benchmark, so compare engines within
each report. Raw timings and source hashes are under
`tools/reference/information/2026-11-14-x11-musgrave/`.
