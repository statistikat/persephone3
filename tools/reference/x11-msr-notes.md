# MSR change-table evidence (22a.2b.3c.2a)

The reference uses public `MsrTable.of()` and public mean/ratio getters from
pinned JD+ 3.9.0. No upstream numerical implementation, bytecode or decompilation
was inspected. The independent mathematical source is Ladiray and Quenneville
(2000), [section 4.3.9 and table 4.112](https://www.census.gov/content/dam/Census/library/working-papers/2000/adrm/x11-french.pdf).

Early probes established that direct unrounded radicals do not reproduce the
public finite-length factors. General rounded-radical formulas explain the
differences: five decimal places for short-phase factors, separately rounded
six-decimal radical coefficients for longer phases, and adjacent-change counts
in the latter formula. Partial-year probes also distinguish global change-count
weighting from the printed observation-count description. These are explicit
conventions, not empirical coefficients fitted to each fixture. The precise
formula and supported domains are in `tools/native-tests/x11-msr-contract.md`.

Sixteen frozen public-selector mode probes separately establish that
pseudo-additive MSR uses absolute changes. This is distinct from the Henderson
I/C convention; the reference helper therefore passes `false` to `MsrTable.of`
for that mode. Tests reconstruct the public-selected span using its reported
iteration count, then use an independent dense mean-extension operator to
qualify the component table. This audits mode semantics, not native iteration
or choice. The mode correction was made before this corpus was finalized.

The frozen matrix has 576 regular cases (2 frequencies × 12 spans × 3 starts ×
4 mode labels × 2 shapes) plus 64 constant/one-constant-phase/missing/zero/signed
controls. Capture uses only the public backend. All targets are independently
reproducible from case definitions and the pinned oracle manifest; SHA-256
manifests pin the targets, capture helper and case source. An oracle-only change
to require zero-weight phases was followed by a fresh capture with byte-identical
public target CSV. Later selector qualification corrected pseudo-additive mode
mapping; the final capture includes that correction and the sixteen mode tables.
Earlier committed corpora/reports remain unchanged.

```sh
Rscript tools/reference/verify-x11-msr.R
mkdir /tmp/persephone-msr-report
Rscript tools/reference/verify-x11-msr.R \
  --live --timing --output=/tmp/persephone-msr-report
```

The scaled public/independent budget is `2e-12`; missing/infinite patterns are
checked separately. Native requires positive relative components, so public
zero/signed denominator values remain frozen unsupported controls. Public means
from the unaffected component/phases are still checked. Undefined phase ratios
do not automatically invalidate a global ratio of positive aggregate means.

The predeclared timing matrix has 24 short cases (three spans × both frequencies
× absolute/relative × smooth/rough) and eight 100,003-row cases. Native includes
domain checks, per-phase counts/statuses/raw means/factors and serialization;
public includes DoubleSeq construction and mean/ratio serialization. Both
measure component-table calls, without pilot, trimming, filter selection,
decomposition or fit. Three warmed alternating rounds use calibrated batches
of at least 50 ms when possible. Loading and explicit pre-batch GC are excluded;
natural in-batch GC is included. Run installed optimized code without competing
heavy work and retain raw times, source hashes and actual worker metadata.

## Qualified local results (2026-11-14)

All 25,680 focused assertions pass. Strict C++/ASan/UBSan serial and OpenMP
harnesses pass 316 interruption boundaries, actual workers and reuse; standalone
linkage has no R/Rcpp, BLAS or LAPACK dependency. Installed offline verification
passes 4,434 checks. Maximum scaled public error is 5.822e-15, independent error
1.095e-13 and mode-audit error 5.783e-15; the predeclared budget stays 2e-12.

The final installed-package check passes 152,087 assertions / 53 files, with
zero errors and the existing R-header warning and DESCRIPTION note. Separate
serial and Java-free suites pass 150,754 / 33 and 150,758 / 33 + 34 backend
assertions respectively. The serial build has no OpenMP runtime linkage; the
Java-free build uses two actual workers. Logs are in
`/tmp/persephone-x11-msr-final-v2/`. Earlier `-final/` logs predate the corrected
pseudo-additive mapping and are superseded. Native sources, tests and numerical
targets match the checked archive; later capture metadata clarifies mode-audit
scope while both target CSVs remain byte-identical.

The retained `information/2026-11-14-x11-msr/` report passes 5,282 live/timing
checks, with maximum scaled live-target error 4.720e-15. It records 45,732 timed
calls including warm-up/calibration. Median summed times are:

| Workload | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 24 short tables | 0.007036970 | 0.005251155 |
| 8 tables of 100,003 rows | 0.016504615 | 0.012840519 |

Native takes about 34% and 28.5% longer respectively. Scope and metadata costs
are described above; these comparisons do not measure seasonal adjustment.

`probe-x11-msr-iterations.R` and the separate
`information/2026-11-14-x11-msr-iterations/` evidence document the next method
decision. One independent twenty-year control stays ambiguous for fifteen
successive prefixes, then JD+ selects 3×9 after removing fifteen years. The
published five-year removal cap instead defaults to 3×5. The independent final
table agrees within 3.553e-15. This control establishes a policy difference, not
statistical superiority or exact threshold equalities; the default remains a
maintainer decision before selector implementation.
