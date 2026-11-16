# Interim Release Checklist (first pass 2026-11-15; gates re-verified 2026-11-16)

Scope: first pass of item 15 for an **interim release of the rjd3 reference
implementation** on branch `rkalos_backend`. No version bump and no release
are made by this document. Gate states refer to this repository only;
numerical engine qualification belongs to Kalos/rkalos.

Legend: **green** = verified on the Linux qualification host (Ubuntu 24.04,
R 4.5.3, JVM 21); **amber** = partially evidenced, stale, or configured but
unrun; **red** = known open defect or missing gate.

## Gate states

| Gate | State | Evidence and gap |
| --- | --- | --- |
| Source build + install (Linux) | green | `R CMD build`/`R CMD INSTALL` clean in M0 and M1 (private libraries `/tmp/opencode/rlib-d`, `/tmp/opencode/rlib-d1`); 2026-11-16 re-verified from branch head `ae37687`. |
| `R CMD check --no-manual --no-vignettes --no-build-vignettes` | green | No dependency NOTE/WARNING; `shinycssloaders` verified as declared in `Imports` with `importFrom(shinycssloaders,withSpinner)` (the M0 note referred to the machine-wide installation, not DESCRIPTION; `git log -S` dates the Imports entry to `7fda51c`, 2026-07-15). |
| Application test suite (installed package, JVM 21) | green | 21 files, 0 failures/errors/skips; per-file assertion distribution recorded in `IMPLEMENTATION_STATUS.md` as the local baseline (total **1,330** after `ae37687` made the gated `test-extraction.R` assertion unconditional; pre-fix total 1,329). Re-run twice on 2026-11-16 against a fresh `ae37687` build: 1,330/0/0/0 both times. M3 finale (2026-11-16): **1,459 assertions, 0 failures/errors/skips** at the restricted-native-fit commit (flipped parity harness 129). Sigma-default cascade (2026-11-17): **1,476/0** (parity harness 146). |
| Java-free loading + native status | green | `tools/verify-nojava.R --native` passes (re-run 2026-11-16 in a fully isolated library farm without rJava/rjd3*/RProtoBuf); rjd3/rJava stay unloaded; `backend_capabilities("native")` delegates to rkalos 0.1.0. Since M3 finale the script additionally runs the restricted native fit through `perX13(..., backend = "native")$run()` and asserts an out-of-domain refusal, both PASS. `inst/tinytest/test-parity.R` runs green JVM-free (53/53 native-lane assertions, no Java namespace loaded). Java-disabled matrix run (config B): 11 files complete with 594/594 assertions passing, 9 files abort at their first unguarded `$run()` with the explicit no-Java-fallback error — honest, but a future Java-free CI lane needs file-level gates (item 14 finding). |
| Restricted native X-13 fit (Java-free product path) | green | 2026-11-16: `perX13(ts, template = "restricted", ..., backend = "native")$run()` fits via rkalos (schema_version 2, gated on `native_x13_fit_restricted`, never `fit`); scope refusals name violated fields with no Java fallback. Dual-lane parity vs rjd3 on the pinned fixed spec: all final-series max diffs **0.000e+00** (bit-exact) at the pinned 1e-8 policy; envelope is an honest `persephone_native_restricted_fit` (no faked rjd3 shape). 2026-11-17: sigma defaults to `legacy` (= pinned JDemetra behavior) per the maintainer decision, `accurate` selectable; bit-exactness re-measured under the default path, still 0.000e+00. See the M3-finale and sigma-default sections in `IMPLEMENTATION_STATUS.md`. |
| Supported Java policy | green (documentation), amber (CI evidence) | Policy: rjd3 3.9.0 jars require JVM >= 21; documented in README, NEWS and the rjd3 first-use error. Local matrix evidence (2026-11-16): JVM-17 (the site `ldpaths` pin) fails the first jar-class call with `UnsupportedClassVersionError` class-file 65.0 vs 61.0; all JVM-21 runs pass. All Java-using workflows set up Java 21 and the check/coverage workflows install pinned rjd3 tags, **but the workflow edits are declared configuration only — no workflow run has been executed from this branch**. |
| Full `R CMD check` incl. vignettes + PDF manual | green (local Linux), amber (cross-platform) | 2026-11-16: `R CMD build` renders all five vignettes; `R CMD check --as-cran` under JVM 21 with all Suggests present gives **1 WARNING + 3 NOTEs**, all understood and documented in `IMPLEMENTATION_STATUS.md` (CRAN-incoming "New submission"+`Remotes` field — not applicable off-CRAN; LICENSE-not-mentioned relicensing NOTE + `RELEASE_CHECKLIST.md`; `createDashboard` example CPU NOTE; no `tidy` for HTML validation). Tests, vignette build/rebuild and PDF manual OK. macOS/Windows lanes not re-run. |
| Numerical reference evidence (Kalos fixtures, X-11 timing report) | amber | Recorded from the macOS split validation; not reproduced on Linux. A Linux re-run is assigned to the M1 numerical track. |
| P1 product findings F1–F8 | green (Linux re-verified 2026-11-16) | All original `dd38e10` minimal reproductions re-run at HEAD `1ceb6f0` no longer reproduce; see "Linux P1 re-verification F1–F9 — 2026-11-16 (M2)" in `IMPLEMENTATION_STATUS.md`. macOS/Windows re-runs remain part of item 15 CI evidence. |
| F9 dashboard identity hardening | green | `Aggregate` child, duplicate and hostile labels route through unique `node_*` identities; 101 assertions pass; live registry probe confirms deterministic non-root routing. |
| F10 CI and dependency hygiene | amber | Done here: Java 21 across check-full/test-coverage/pkgdown, pinned rjd3 tags in check-full/test-coverage, tinytest log upload. Open: minimum-R (4.1) lane, an upstream-drift lane beside the pinned lane, measured coverage, and a guard that CI cannot silently skip tests. |
| Browser/dashboard manual checks | amber | Headless httpuv start/stop tests pass; interactive browser checks against the installed package have not been repeated on Linux. |
| Domain review of representative outputs | red | No seasonal-adjustment domain reviewer has signed off on this branch. |

| Difficult-model backend robustness | amber | New Linux observation (2026-11-16): a degenerate series makes rjd3toolkit's NA test-statistic conversion throw inside `perX13()$run()` (`.jd2r_test -> statisticaltest`). Backend-side; item-14 lane, follow-up slice. |

## Remaining release actions (item 15)

1. ~~Resolve or explicitly waive every reproduced P1 finding (F1–F8); then F9.~~
   Re-verified fixed on Linux at `1ceb6f0` (2026-11-16); no open reproduced
   P1 finding on this platform. Cross-platform re-runs arrive with the CI
   evidence of action 2.
2. Execute the edited CI workflows on a branch push (authorized action) and
   attach the Linux/macOS/Windows results; add the still-open item-14 lanes.
3. Re-run the Linux numerical comparison evidence for the Kalos/rkalos
   fixtures and the timing protocol, or re-record the macOS evidence.
4. Repeat browser checks against the installed source package. (Full
   `R CMD check` with rendered vignettes and PDF manual completed on Linux on
   2026-11-16; macOS/Windows check lanes remain open.)
5. Domain review of weighting, calendars, uncertainty and quality classes.
6. Version bump + NEWS + support requirements (Java >= 21 for rjd3 fits) as a
   deliberate, separately authorized release decision.

A local green check alone is not release approval; merge/publish only within
the user's authorization.
