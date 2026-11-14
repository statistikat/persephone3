# Additive normalization evidence (22b.1)

The implementation follows the annual normalization and seasonal restoration
described in Ladiray and Quenneville (2000),
[§4.1.4 and §4.1.5](https://www.census.gov/content/dam/Census/library/working-papers/2000/adrm/x11-french.pdf).
Public `DefaultSeasonalNormalizer.normalize` calls with validated JD+ 3.9.0
spec/context objects qualify fixed/stable/mixed endpoint conventions. No
numerical Java source, bytecode or decompilation is consulted.

The public normalizer's context offset needs explicit qualification: the
three-argument form interprets the context at the restored domain's start.
The four-argument helper cancels that restoration offset because its context
already starts at the supplied seasonal input. Asymmetric mixed-phase controls
distinguish this from rotating the requested filters when adding rows. Uniform
filters or alternating choices with a half-year extension alone would miss that
error. Calendar start, restoration length and requested-phase labels are varied
separately in the corpus.

The separate frozen corpus has **624 public cases**: 576 regular cases around
two-, five- and twenty-year support gates with fixed/stable/mixed choices,
calendar rotations and optional half-year restoration; 48 constant, linear,
annual-pattern, impulse, gap and all-missing controls include restoration beyond
one year. The independent R oracle builds a dense annual-convolution operator,
selects endpoint source rows, subtracts that operator from the identity and
restores by matching calendar phases. Mandatory tests apply the entire native
operator to every basis vector and independently check each missing input's
nonzero support. Neither capture nor runtime derives values from native targets.

The explicit native minimum is two annual cycles. Below it, public calls can
throw or perform overlapping boundary copies. Those cases are rejected rather
than generalized without a contract. Native also rejects unsupported modes and
unresolved automatic requests. Fixed requested filters are preserved separately
from the seasonal processor's own length fallbacks; the two policies need not
choose the same label. Range handling retains the existing finite-filter gate.

```sh
Rscript tools/reference/verify-x11-normalization.R
mkdir /tmp/persephone-normalization-report
Rscript tools/reference/verify-x11-normalization.R \
  --live --timing --output=/tmp/persephone-normalization-report
```

Offline verification audits the original pinned manifest, new artifacts and
capture source hashes without loading Java, and compares all native points,
normalizing means, source maps and policy reasons. Live verification also
replays every public target. The fixed scaled numerical budget is `2e-12`, with
missing/infinite patterns checked separately. Other frozen corpora remain intact.

The predeclared timing matrix contains 24 short and eight 100,003-row additive
normalizations. Fixed, stable and mixed requests are represented, with/without
half-year restoration. Native includes both source maps, the normalizing means,
domains, masks and policy metadata. Public includes validated context creation
and result serialization. Both normalize already supplied seasonal values;
neither performs seasonal estimation or a B/C/D pass. Three warmed alternating
rounds use calibrated batches of at least 50 ms when possible, capped at 1,000
calls. Loading/startup and explicit pre-batch GC are excluded; natural in-batch
GC is included. Use installed optimized code without competing heavy work and
retain raw rounds, source/fixture hashes and actual worker metadata.

## Qualified local results (2026-11-14)

The focused suite passes 15,594 assertions. Strict serial/OpenMP C++17
ASan/UBSan harnesses pass 12 interruption boundaries with reuse and actual
workers. Standalone linkage has no R/Rcpp, BLAS or LAPACK dependency.

The full installed-package check passes 221,157 assertions / 55 files, with
zero errors and the existing R-header compiler warning and DESCRIPTION note.
Serial native checks pass 219,824 / 35 files without OpenMP linkage. The
isolated Java-free suite passes 219,828 / 35 files plus 34 backend assertions,
with two actual workers. All 316 archived runtime/test/target files match the
working sources. Logs are in `/tmp/persephone-x11-normalization-final/`.

The installed offline verifier passes 2,505 checks; source-mode live replay
passes 3,128. Maximum scaled public, independent and live-target errors are
1.422e-14, 1.599e-14 and 2.369e-15 respectively, within the unchanged 2e-12
budget. Earlier corpora and numerical reports remain intact.

The retained `information/2026-11-14-x11-normalization/` report passes 3,353
live/timing checks and records 87,900 calls including warm-up/calibration.

| Workload (median summed time) | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 24 short normalizations | 0.0009320000 | 0.0125350467 |
| 8 normalizations of 100,003 rows | 0.0297505656 | 0.0144743111 |

Native is about 13.45× faster on the short matrix and takes 2.06× as long on the
long matrix. The richer native output and public context construction are
included as described above; profiling is required to attribute the gap. These
are normalization timings, not full decomposition or fitting performance.
