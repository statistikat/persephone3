# Staged seasonal policy evidence (22a.2b.3c.1)

`inst/tinytest/fixtures/native/x11-seasonal-plan/` contains 336 public JD+ 3.9.0
contexts. Each is built through the public spec builder, validation, frequency
check, context constructor and initial/final/MSR getters. No numerical source,
bytecode or decompilation was used. Public outputs do not depend on native
results. SHA-256 manifests pin capture scripts, inputs and targets, and previous
corpora/reports remain unchanged.

The matrix has 21 request forms at each frequency: eight scalar choices, eight
uniform per-period choices, two almost-all-MSR controls and three mixed rotations.
Each covers four spans (four years−1, five years, nineteen years+1 and twenty
years+1) and two starts, with four mode labels distributed across those cases.
All six explicit B/C/D initial/final stage aliases give 2,016 stage checks. This
qualifies policy resolution, not numerical B/C/D outputs or final MSR selection.

```sh
Rscript tools/reference/verify-x11-seasonal-plan.R
mkdir /tmp/persephone-x11-stage-report
Rscript tools/reference/verify-x11-seasonal-plan.R \
  --live --timing --output=/tmp/persephone-x11-stage-report
```

The timing matrix has 42 request bundles, removing duplicate spans/starts.
Each native bundle invokes all six explicit stage-plan APIs; the public bundle
builds one validated context and serializes its shared initial/final mappings
and global-MSR flag. Native returns additional per-stage statuses, names and
reasons. Both bundles cover all stage policies, but have different construction
and metadata costs. No numeric filtering or MSR estimation is included, and
the bounded policy has no OpenMP workload. Timings must not be presented as
decomposition or fit speed comparisons.

Use three warmed alternating rounds with batches calibrated to at least 50 ms
when possible. Loading and explicit pre-batch GC are excluded; natural in-batch
GC is included. Store raw times, source hashes and exact comparison scope.
See `tools/native-tests/x11-seasonal-plan-contract.md` for pending MSR semantics
and the distinct downstream short-series policy.

## Qualified local results (2026-11-14)

The 24,380 focused assertions, standalone strict C++/ASan/UBSan and 8,070
installed offline checks pass. The full check passes 126,407 assertions / 52
files (zero errors, existing R-header warning and DESCRIPTION note). Separate
serial and Java-free builds pass 125,074 / 32 and 125,078 / 32 + 34 backend
assertions respectively. Standalone linkage is standard C++/system/sanitizers
only; serial package linkage has no OpenMP runtime.

The retained `information/2026-11-14-x11-seasonal-plan/` report passes 8,658
live/timing checks and records 165,319 timed calls including calibration.
Median summed times for 42 request bundles are native 0.002112722 seconds and
JDemetra 0.043115189 seconds (about 20.4×). Scope and construction costs are
described above; this does not measure any numerical decomposition or fit.
