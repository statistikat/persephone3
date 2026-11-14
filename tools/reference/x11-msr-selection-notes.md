# MSR selection evidence (22a.2b.3c.2b)

The pilot independently implements the seven-term mean with three-observation
mean extension described in Ladiray and Quenneville (2000),
[§4.3.9, page 147](https://www.census.gov/content/dam/Census/library/working-papers/2000/adrm/x11-french.pdf).
The following section, §4.3.10, page 152, supplies the published ambiguous-band
iteration with at most five year removals. Public JD+ 3.9.0 execution instead
continues while sufficient history remains. The maintainer requested both
policies and selected JD+ behavior as the default. Both are native; the capped
policy does not require or invoke a reference backend.

Reference capture calls validated `X11Spec`/`X11Context`, the public
`MsrFilterSelection` (or dedicated pseudo-additive selector), and its choice,
iteration, default and table getters. Separate prefix audits apply the public
seven-term uniform filter with mean endpoints and public `MsrTable`. Pilot
irregular components are assembled as differences or ratios in the helper.
No numerical Java source, bytecode or decompilation is read. The R oracle builds
dense averaging/extension operators and uses independent annual-difference
formulas; target amplitudes are derived from that oracle, never native outputs.

The frozen corpus `inst/tinytest/fixtures/native/x11-msr-selection/` contains:

- 336 regular cases: two frequencies, seven spans, three starts, four modes and
  smooth/rough inputs.
- 64 explicit extension cases, including both public exclusion-flag settings.
- 96 constant, phase-constant, missing, zero and signed controls, including
  observations excluded by backcast, forecast or calendar trimming.
- 34 targeted controls: both frequencies around each cutoff and inside both
  ambiguous bands, plus a twenty-year policy-difference signal.
- 592 visited prefix tables, final public pilot/table values and four exact
  public ratio equalities. Equality controls retain input amplitudes as
  hexadecimal strings to preserve the original double values.

Exact 2.5 and 5.5 ratios are ambiguous; exact 3.5 selects 3×5 and exact 6.5
selects 3×9. Equalities are qualified as scalar policy; numerical pipeline
fixtures are offset 1e-5 from each boundary. The twenty-year control needs
sixteen JD+ attempts and selects 3×9 after fifteen year removals; the capped
policy takes six attempts and defaults to 3×5. Short-history public ratio zero
is a placeholder; native has no table and reports a missing ratio. Constant
controls can have public NaN or roundoff-driven choices. Native retains explicit
undefined status rather than promoting those artifacts to filter decisions.

```sh
Rscript tools/reference/verify-x11-msr-selection.R
mkdir /tmp/persephone-msr-selection-report
Rscript tools/reference/verify-x11-msr-selection.R \
  --live --timing --output=/tmp/persephone-msr-selection-report
```

Offline verification checks the pinned manifest, frozen artifacts and capture
source hashes, both native policies and independent/pinned targets without
loading Java. Live replay additionally checks all public choices/tables and
the four exact-equality controls. The fixed scaled budget is `2e-12`, with
missing/infinite patterns checked separately. Complete pilot basis operators,
malformed inputs, resolution guards, resource stops, ranges, unit changes,
worker determinism and interruption reuse are mandatory package/core tests.

Predeclared timing compares **the same default JD+ policy**: 26 short selectors
(24 regular plus two policy-difference cases) and eight 100,003-row selectors.
Both include pilot, trimming, MSR tables and iteration. Native also serializes
final pilot vectors, complete domains/statuses and per-attempt trace; public
includes validated context creation and final table getters. Two additional
native capped calls are reported separately because JD+ has no matching option.
These measure selector calls, not X-11 adjustment or fitting. Three warmed,
alternating rounds use batches of at least 50 ms when possible, with at most
1,000 calls per batch. Loading/startup and explicit pre-batch GC are excluded;
natural in-batch GC is included. Use installed optimized code without competing
heavy work and retain source hashes, raw times and actual worker metadata.

## Qualified local results (2026-11-14)

The focused suite passes 53,476 assertions. Strict serial/OpenMP ASan/UBSan
checks pass 354 interruption boundaries with reuse and actual workers. The
component-table stability change also passes the prior 25,680 assertions and
316 serial/OpenMP interruption boundaries. Standalone selector linkage has no
R/Rcpp, BLAS or LAPACK dependency.

Installed checks pass 205,563 assertions / 54 files, with zero errors and the
existing R-header warning and DESCRIPTION note. Serial native tests pass
204,230 / 34 files without OpenMP linkage. The isolated Java-free suite passes
204,234 / 34 files plus 34 backend assertions using two actual workers. Logs are
in `/tmp/persephone-x11-msr-selection-final/`; code, tests and numerical targets
match the checked archive. The installed offline verifier passes 7,859 checks.

The retained `information/2026-11-14-x11-msr-selection/` report passes 9,167
live/timing checks and records 22,062 calls including warm-up/calibration.
Maximum scaled errors are public 1.584e-14, independent
1.006e-14 and live-target 3.942e-15, within the unchanged 2e-12 budget.

| Workload (median summed time) | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 26 short default-policy selectors | 0.0121923570 | 0.0178312631 |
| 8 default-policy selectors of 100,003 rows | 0.0232712418 | 0.0584636364 |

Native takes 31.6% and 60.2% less time respectively for the stated API scope.
Two native capped-policy controls total 0.0009814766 seconds; no public capped
selector is claimed. This does not establish full X-11 performance or parity.
