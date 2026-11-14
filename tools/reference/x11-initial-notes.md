# Fixed additive initial-table evidence (22b.2)

The implementation independently composes the additive X-11 initial tables
described by Ladiray and Quenneville (2000),
[§4.1](https://www.census.gov/content/dam/Census/library/working-papers/2000/adrm/x11-french.pdf),
using already qualified annual, seasonal and normalization primitives.
The [official X-11 description](https://doc.jdemetra.org/m-x11-decomposition)
provides the B/C/D stage context. No numerical Java source, bytecode or
decompilation is read or translated. Public signatures and callable outputs
identify conventions; runtime never reads reference fixtures.

The separate corpus contains 336 prospective cases: 288 smooth controls with
monthly/quarterly frequency, fixed/stable/asymmetric mixed requests, calendar
rotations and cropped-history support boundaries; 48 constant, trend, annual,
impulse, gap and all-missing controls. Every case records both the actual
`X11BStep.process` getters B1/B2/B3/B4a/B4anorm/B4d and a matched public primitive
chain. All 336 public B steps succeed. The chain uses a validated context at the
cropped calendar, public annual/seasonal filters, additive removal and seasonal
normalization. It agrees with the actual B-step getters within the unchanged
`2e-12` scaled budget, including phase rotation and cropped-span fallbacks.

Independent R calculations use the centered annual convolution and previously
qualified published seasonal/normalization operators. Complete impulse tests
verify their composed linear operators. Separate Boolean support propagation
checks every possible missing input, preserving the staged dependency graph
even where algebraic cancellation occurs. Native point results never derive
from Java targets or native filter coefficients in that oracle.

Six gap cases have an intentional missing-policy distinction: stable, mixed
stable and short-history 3×15 requests at both frequencies. Public stable means
omit missing observations; the existing native contract propagates them.
The report retains 18 B4-table audits with mismatch counts and compares only
available native rows numerically there. It does not call these full-table
parity. Their native masks/values are checked independently, while the public
omitted-missing mean is checked against its own explicit arithmetic. All other
table comparisons require identical missingness. No tolerance is widened.

```sh
Rscript tools/reference/verify-x11-initial.R
mkdir /tmp/persephone-initial-report
Rscript tools/reference/verify-x11-initial.R \
  --live --timing --output=/tmp/persephone-initial-report
```

Offline verification audits pinned manifests, artifacts and capture hashes
without loading Java. Live mode replays every B-step getter. Timing uses the
matched public primitive chain to avoid charging JDemetra for the rest of its B
pass. Both return six initial tables. Native additionally returns nested
seasonal/normalization metadata, domains, masks and original-row maps; public
includes validated context creation and table serialization. Neither timed call
fits a model or completes an X-11 pass.

The predeclared matrix has 24 short and eight 100,003-row calls, covering fixed,
stable and mixed requests at both frequencies. Three warmed alternating rounds
use calibrated batches of at least 50 ms where possible, capped at 1,000 calls.
Startup/loading and explicit pre-batch GC are excluded, while natural in-batch
GC is included. Final measurements use the installed optimized package without
competing heavy work and retain raw rounds, source/fixture hashes and worker
metadata.

The focused suite passes 46,713 assertions. All 31,345 seasonal-processing and
15,594 normalization assertions also pass after extracting their shared
serialization/decorating helpers. Strict serial/OpenMP C++17 ASan/UBSan
harnesses pass 42 interruption boundaries with reuse and actual workers; their
linkage has no R/Rcpp, BLAS or LAPACK dependency.

The final full check passes 267,870 assertions / 56 files, with zero errors,
the existing R-header compiler warning and DESCRIPTION note. Serial native
checks pass 266,537 / 36 files without OpenMP linkage. The isolated Java-free
suite passes 266,541 / 36 files plus 34 backend assertions, with two actual
workers. All 328 archived runtime/test/target files match the working sources.
The final logs are in `/tmp/persephone-x11-initial-final-v2/`; the earlier
`-final/` archive predates the symmetric arithmetic refinement and is superseded.

The installed offline verifier passes 10,090 checks. Maximum scaled error is
5.101e-15 against the matched public primitive chain, 4.974e-14 against the
independent composed operators and 4.308e-14 between the public chain and actual
B-step getters. The unchanged budget is 2e-12. The 18 missing-policy audits
remain distinct from full-table parity.

The long-workload preflight initially found remainder errors up to 3.639e-12
when the generic left-to-right annual convolution was subtracted from a level
near 6,000. The initial-table annual mean now evaluates the same published
symmetric kernel center first, then by equal-distance pairs. A guarded weighted
pair handles unweighted pair overflow without losing representable means.
Existing generic filter arithmetic is unchanged. The workload and 2e-12
acceptance budget are unchanged; this composition is requalified before timing.

The retained `information/2026-11-14-x11-initial/` report passes 12,330
live/timing checks and records 28,906 calls including warm-up/calibration.
Live-target replay error is at most 5.099e-15. All 30 source hashes match the
qualified working sources.

| Workload (median summed time) | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 24 short initial-table calls | 0.0056764485 | 0.0291288981 |
| 8 calls with 100,003 rows | 0.1262500000 | 0.0846380952 |

Native is 5.13× faster on the short matrix and takes 1.49× as long on the long
matrix. Both sides perform the six-table initial calculation described above;
native metadata and public context construction are included. The long gap
requires profiling before attribution. These are not complete B/C/D or model-fit
timings. Earlier frozen corpora and reports remain unchanged.
