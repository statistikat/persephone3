# Persephone implementation status and next-work plan

Updated: **2026-11-15**. The repository split is integrated on
`rkalos_backend`. **rkalos is a required dependency**.

## Repository ownership

| Repository | Responsibility and implementation information |
| --- | --- |
| [kalos](../kalos/IMPLEMENTATION_STATUS.md) | Independent C++17 engine, OpenBLAS provider, numerical roadmap/contracts, canonical fixtures and JDemetra comparisons |
| [rkalos](../rkalos/IMPLEMENTATION_STATUS.md) | Lean R/Rcpp API, R-configured BLAS/LAPACK provider, pinned core snapshot and native R tests |
| [pykalos](../pykalos/IMPLEMENTATION_STATUS.md) | Minimal Python package calling Kalos; full model bindings and portable wheels deferred |
| Persephone (this repository) | R6 objects, hierarchy, calendars, plots, dashboards, backend envelopes and product integration |

Dependency graph: `persephone3 → rkalos → kalos`, with `pykalos → kalos`.
The four sibling checkouts live under `~/git/seasonaladjustment`.
Each contains its own implementation status.

## Completed integration changes

- Declared required rkalos Imports and pinned its development source in Remotes.
- Delegated native capability reporting to rkalos and removed the embedded C++,
  Rcpp registration/build files and native numerical wrappers/tests.
- Moved numerical contracts, reference tools and native fixtures to Kalos;
  native R tests and generated fixture snapshots now belong to rkalos.
- Retained the unchanged historical workflow fixtures needed by Persephone.
- Preserved public R6/specification/result/forecast behavior and persisted backend
  selection. `rjd3` remains the default; native seasonal-adjustment fitting still
  raises an explicit unsupported error without a Java fallback.
- Preserved original branch/history and the full pre-split implementation record
  in Kalos's historical documentation. No registry release or default-branch merge.

## Split validation

Kalos passed 37 test programs in each of the serial, OpenMP and sanitizer builds
and passed an installed CMake consumer test. The installed Python wheel passed
three smoke tests. The rkalos archive passed 266,549 OpenMP assertions and
266,545 serial assertions. Persephone passed 35 backend assertions in a library
without Java-related packages.

The Persephone source archive passed all **1,330 application assertions**,
including the reference backend and dashboard tests. The two R CMD check
warnings concern missing generated vignettes in the no-vignettes build.

The preserved pre-split X-11 initial-table timing report records 0.00568 seconds
for native versus 0.02913 for JDemetra+ across 24 short calls, and 0.12625 versus
0.08464 seconds across eight calls with 100,003 rows. See the [Kalos reference
report](../kalos/validation/reference/information/2026-11-14-x11-initial/)
for the workloads and measurement protocol.

## Linux (x86_64) baseline qualification — 2026-11-15 (M0)

First qualification run of this branch on a new Linux machine. Everything below
was executed on this machine on 2026-11-15; nothing is carried over from the
macOS record above.

**Machine and toolchain.** Ubuntu 24.04.4 LTS, x86_64. R 4.5.3 (2026-03-11) at
`/opt/R/4.5.3/bin/R`. g++ 13.3.0. OpenJDK 21.0.11 and OpenJDK 17 are installed.

**Java and rjd3.** `rjd3x13`/`rjd3tramoseats` 3.9.0, `rjd3toolkit` 3.9.1,
`rjd3jars` 0.0.6, `rJava` 1.0-18 and `RProtoBuf` 0.4.28 were already present in
the shared user library. The rjd3 3.9.0 jars are class-file version 65 (Java
21), but this machine's R site profile (`/opt/R/4.5.3/lib/R/etc/ldpaths`) pins
`JAVA_HOME` to Java 17, so rJava starts JVM 17.0.19 and every jar-backed call
fails with `UnsupportedClassVersionError`. Exporting
`JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64` (with its `lib/server` prepended
to `LD_LIBRARY_PATH`) selects JVM 21.0.11, after which spec construction and
fitting work. A supported-Java decision is still open (item 14 / F10).

**Install notes.** The site `Rprofile.site` hard-codes `.libPaths()` and
overrides `R_LIBS_USER`; a user profile (`R_PROFILE_USER=/tmp/opencode/rprofile-d.R`)
that prepends the private library was used instead. `rkalos` 0.1.0 was installed
read-only from a `git archive` snapshot of `rkalos@18630ee` (a sibling agent was
editing the live checkout); it compiled without warnings against R's
`-llapack -lblas` and loads `capabilities()` without attaching rjd3 or rJava.
`persephone3` 0.1.0 (branch `rkalos_backend`, `06a9a31`) was built with
`R CMD build --no-build-vignettes` and `R CMD INSTALL`ed with no warnings or
notes. One hard Import was missing machine-wide: `shinycssloaders` 1.1.0 was
installed from CRAN into the private library. The same build skipped rendered
vignettes, consistent with the two pre-existing R CMD check vignette warnings.

**Runtime verification (installed package, JVM 21.0.11).**
`backend_capabilities("rjd3")` reports status `reference` with all three rjd3
packages available. `perX13(AirPassengers)$run()` completed in 2.24 s (final
`d11final` last six: 483.5, 480.1, 483.3, 496.8, 487.6, 488.9);
`perTramo(AirPassengers)$run()` completed in 0.50 s (final `sa` last six:
484.1, 476.7, 481.5, 495.4, 487.8, 490.6). Library attachment keeps rjd3/rJava
lazy. `backend_capabilities("native")` delegates to rkalos (status
`unimplemented`, `fit = FALSE`); constructing `perX13`/`perTramo` with
`backend = "native"`, calling the internal `backend_fit`/`backend_default_spec`
with `backend = "native"`, and selecting `options(persephone.backend = "native")`
each raise "The native backend does not yet implement … No Java fallback was
used." After all native attempts, zero rjd3 or rJava namespaces were loaded.

**Application test suite.** `tinytest::test_package("persephone3")` under JVM
21: 20 test files, **1,329 assertions, 0 failures, 0 errors, 0 skips**.
No suggested dependency was absent, so nothing was skipped; the
guarded rjd3 reference sections in `test-backend.R`,
`test-backend-integration.R` (32), `test-backend-reinitialization.R` (14) and
the fitted-value fixtures in `test-extraction.R`, `test-fix-outliers.R`,
`test-forecasts.R`, `test-diagnostic-values.R` and `test-hierarchy-values.R`
all ran against real Java fits. Dashboard tests started and stopped local
httpuv servers successfully. This run reports one assertion fewer than the
1,330 recorded in the split-validation macOS run; no skip was recorded and the
difference is unexplained — all executed assertions passed.

**Still macOS-qualified only.** The 1,330-assertion split-validation run and
the R CMD check results above it, the X-11 initial-table timing report, and all
Kalos/rkalos numerical fixture evidence predate this machine and were not
re-run here. M1 on Linux must additionally re-run the numerical comparison
evidence, pin the Java policy into CI, and resolve the 1-assertion discrepancy.

## Linux application groundwork — 2026-11-15 (M1)

Application-track groundwork on this machine (item 14 plus interim-release
preparation). Engine-side M1 work is tracked separately.

**Dependency declaration.** The M0 note about a missing `shinycssloaders`
import referred to the machine-wide installation; the declaration itself was
already correct since `f9cb071`: `Imports:` lists `shinycssloaders`,
`R/imports.R` carries `@importFrom shinycssloaders withSpinner`, NAMESPACE
contains the matching `importFrom`, and the only usage is
`inst/app/modules/mod_table.R`. A full `pkg::` usage scan of `R/` and
`inst/` found no other undeclared dependency (`grid` is base-priority).
`R CMD check --no-manual --no-vignettes --no-build-vignettes` on the
unmodified branch head (`d966039`) gave 2 WARNINGs, both the pre-existing
missing-vignettes pair, and no dependency NOTE. After the M1 edits below the
same check still shows no dependency NOTE/WARNING; the only new finding is
one deliberate NOTE, "File LICENSE is not mentioned in the DESCRIPTION file",
caused by the license change below while `LICENSE`/`LICENSE.md` intentionally
keep the old MIT notice.

**Supported Java policy.** rjd3 3.9.0 jars are class-file version 65, so
reference-backend fits require a JVM 21 or newer. The codebase has no Java
runtime capability probe (`backend_capabilities()` deliberately reports
`runtime_checked = FALSE`, asserted by `test-backend.R`), so no probe
machinery was added; the version check would require initializing Java.
The policy is instead documented (README install section, NEWS) and stated in
the existing rjd3 first-use error raised by `backend_rjd3_function()`, which
still names the missing optional package first.

**CI (configured, not executed).** All Java-using workflows already set up
Temurin Java 21 (check-full, test-coverage since `ba049c3`, pkgdown);
native-foundations remains intentionally Java-free. The check and coverage
workflows fetched rjd3 through the floating `@*release` ref selector; those
six lines now pin the qualified tags (`rjdverse/rjd3toolkit@v3.9.1`,
`rjdverse/rjd3x13@v3.9.0`, `rjdverse/rjd3tramoseats@v3.9.0`, verified to
exist upstream and matching the locally qualified versions). No separate
pinned-deps job was added because that would restructure CI. These are
declared-configuration changes: **no workflow was run from this branch as
part of M1.** Open F10 remnants: minimum-R (4.1) lane, an upstream-drift
lane beside the pinned lane, measured coverage, silent-skip guard.

**1-assertion discrepancy — localized and fixed.** The per-file Linux
distribution (installed package, JVM 21.0.11, locale `en_US.UTF-8`) is the
new local baseline; the pre-fix total was 1,329 with zero failures, errors
or skips:

| Test file | Assertions | Test file | Assertions |
| --- | --- | --- | --- |
| test_fix.R | 4 | test-extraction.R | 177 → **178** |
| test-backend-integration.R | 32 | test-fix-outliers.R | 188 |
| test-backend-reinitialization.R | 14 | test-forecasts.R | 180 |
| test-backend.R | 88 | test-genTd.R | 90 |
| test-batch.R | 10 | test-hierarchical-plot.R | 36 |
| test-dashboard-identities.R | 101 | test-hierarchical.R | 10 |
| test-dashboard-probe.R | 12 | test-hierarchy-values.R | 79 |
| test-dashboard.R | 43 | test-misc.R | 12 |
| test-diagnostic-values.R | 180 | test-plot.R | 11 |
| | | test-single.R | 10 |
| | | test-specification-updates.R | 52 |

The difference localized to `test-extraction.R`, whose
`if (nrow(out) > 3L) expect_true(all(out$autoout[-seq_len(3)]))` gate runs
once per (method × frequency) fit. Measured here, the quarterly TramoSeats
`rsa3` fixture detects only the three manual outliers, so the gate fired 3/4
times on Linux versus apparently 4/4 on the macOS split-validation machine
(the backend's automatic outlier detection decides the count — not a locale
or option issue). Since `all()` over an empty remainder is `TRUE`, the gate
was replaced by the equivalent unconditional assertion, making the count
deterministic: **1,330 assertions, 0 failures, 0 errors, 0 skips** on Linux,
matching the macOS split-validation total.

**License metadata.** Per the user's relicensing decision, `DESCRIPTION` now
reads `License: GPL (>= 3)`; `LICENSE`, `LICENSE.md` and NEWS are
intentionally untouched, which produces the single documented top-level-file
NOTE above.

**Interim release checklist.** First pass of item 15 recorded in
`RELEASE_CHECKLIST.md` with per-gate green/amber/red states.

## Linux M1 regression matrix and release verification — 2026-11-16 (Agent D)

Item 14 groundwork executed as actual runs: the tinytest suite under a documented
Java-policy regression matrix, re-check of the 1,329-vs-1,330 baseline claim, and
the item-15 first-pass verification including a full `R CMD check --as-cran`.
Scripts, logs and the check tree live in `/tmp/opencode/m1d2/`.

**What was tested.** Fresh `R CMD build` + `R CMD INSTALL` of branch head
`ae37687` into a clean scratch library `/tmp/opencode/rlib-d1`; nothing in the
user or site libraries was modified. `rkalos` was built from the read-only
sibling checkout at its then-HEAD `a36a925` (core pin `cd004b08…` confirmed via
`rkalos::core_version()`; Agent B's later `5342b89` postdates this matrix).
Versions: R 4.5.3, rJava 1.0-18, rjd3toolkit 3.9.1, rjd3x13 3.9.0,
rjd3tramoseats 3.9.0, rjd3jars 0.0.6, tinytest 1.4.3, shinycssloaders 1.1.0
(machine-local, see below), JVM 21.0.11.

**Java-policy regression matrix** (installed-package tinytest suite, 20 files;
driver `run_matrix.R` in the scratch directory):

| Config | Driver | Files | Assertions | Pass | Fail | Error | Skip |
| --- | --- | --- | --- | --- | --- | --- | --- |
| A — JVM 21 (`JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64`, rJava + rjd3 on path) | one `test_package()` | 20 | 1,330 | 1,330 | 0 | 0 | 0 |
| B — Java disabled (`JAVA_HOME` unset; rJava/rjd3*/RProtoBuf absent from every search path) | per-file runs | 20 (11 complete, 9 file-abort) | 594 | 594 | 0 | 0 | 0 |
| C — site-pinned JVM 17 probe | single fit | — | — | — | — | — | — |

Config A was run twice; the per-file distribution matches the `ae37687`
baseline exactly (test_fix 4, backend-integration 32, backend-reinitialization
14, backend 88, batch 10, dashboard-identities 101, dashboard-probe 12,
dashboard 43, diagnostic-values 180, **extraction 178**, fix-outliers 188,
forecasts 180, genTd 90, hierarchical-plot 36, hierarchical 10,
hierarchy-values 79, misc 12, plot 11, single 10, specification-updates 52).
The 1,329-vs-1,330 discrepancy was re-checked and is **fully explained**: the
M0 run recorded test-extraction.R = 177, both runs here give 178 — exactly the
single backend-count-gated assertion that `ae37687` localized and made
unconditional. No test was relaxed.

Config B ran all 20 files individually (per-file `run_test_dir` + tryCatch,
because one `test_package()` run stops at the first file-level error). The 11
files whose Java-free sections exist passed every collected assertion
(backend-reinitialization 0, backend 35, dashboard-identities 101,
dashboard-probe 12, diagnostic-values 163, extraction 0, fix-outliers 0,
forecasts 100, genTd 90, hierarchical-plot 36, hierarchy-values 57 — the
`requireNamespace` gates print a message and contribute no assertions, hence
0 skips). The 9 files with an unguarded top-level `$run()`
(`test_fix.R`, `test-backend-integration.R`, `test-batch.R`,
`test-dashboard.R`, `test-hierarchical.R`, `test-misc.R`, `test-plot.R`,
`test-single.R`, `test-specification-updates.R`) abort at the first fit with
the package's explicit error: "The rjd3 backend requires the optional package
'rjd3toolkit' and its compatible Java runtime; the rjd3 3.9.0 jars require a
JVM 21 or newer. Install them before using backend = \"rjd3\"." No silent
fallback anywhere; no namespace matching `^rJava$|^rjd3` was ever loaded
before or after. **Item-14 finding:** the rjd3-absent lane does not degrade
gracefully suite-wide — gating today is per-section, not per-file — so a
future Java-free CI lane needs file-level gates or a documented
expected-abort list. Nothing was changed to make the matrix look better.

Config C (policy evidence): with `JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64`
(the value `ldpaths` pins), `requireNamespace("rjd3x13")` and the pure-R spec
builders still succeed, and the first jar-class call then fails with
`java.lang.UnsupportedClassVersionError: jdplus/x13/base/api/x13/X13Spec has
been compiled by a more recent version of the Java Runtime (class file version
65.0), this version of the Java Runtime only recognizes class file versions up
to 61.0`. JVM ≥ 21 is a hard requirement and the failure is first-fit-deferred
— exactly the misleading behavior the documented policy warns about.

**Native-unsupported honesty paths.** `tools/verify-nojava.R --native` was run
against a fully isolated library (475-package symlink farm without rJava,
rjd3toolkit/x13/tramoseats/jars, RProtoBuf and other rJava importers; no
`JAVA_HOME`) → PASS. The M0 script `verify_native_unsupported.R` passes under
both A and B: `backend_capabilities("native")` reports
`status="unimplemented"`, `fit=FALSE`; all four native routes plus the
option route raise the explicit "No Java fallback was used" error; afterwards
the count of loaded rJava/rjd3 namespaces is 0.

**Java environment findings.** Host `java -version` reports 21.0.11, but the
R-HOME `etc/ldpaths` pins `JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64`, and
`R CMD javareconf -e` without `JAVA_HOME` resolves to Java 17.0.19 (JNI probe
links with either JDK); with `JAVA_HOME=…java-21…` it resolves to 21.0.11.
Inside config A, `rJava` reports `java.version 21.0.11`. Reference fits
reproduce the M0 values: X-13 `d11final` last 6 = 483.5, 480.1, 483.3, 496.8,
487.6, 488.9; TRAMO/SEATS `sa` last 6 = 484.1, 476.7, 481.5, 495.4, 487.8,
490.6. M0 host notes verified with one correction: R is at
`/opt/R/4.5.3/bin/R` (`/usr/local/bin/R` is a symlink via `/opt/R/R-default`),
and the site `Rprofile.site` `.libPaths()` override **drops** `R_LIBS`
entries in non-`--vanilla` R, so scratch-library builds need `R_LIBS=` (for
`--vanilla` subprocesses such as vignette rendering) **and**
`R_PROFILE_USER=` (for non-vanilla subprocesses) together.

**shinycssloaders declaration re-check.** Confirmed already declared; no
DESCRIPTION/NAMESPACE change made (none needed). `git log -S shinycssloaders
-- DESCRIPTION` shows it entered `Imports:` in `7fda51c` (2026-07-15) — an
ancestor of `f9cb071` cited last week — with the redundant `Suggests` entry
removed in `0992a4b`; `R/imports.R` has `@importFrom shinycssloaders
withSpinner` and NAMESPACE carries the matching import. The M0 note referred
to the machine installation: 1.1.0 exists only in scratch libraries here; it
was copied into `/tmp/opencode/rlib-d1` for these runs, nothing new was
installed on the machine.

**R CMD build + R CMD check --as-cran (JVM 21, suggests all present,
`_R_CHECK_FORCE_SUGGESTS_` left enabled).** `R CMD build` with vignettes:
**OK — all five vignettes render** (the earlier 2-WARNING missing-vignette
state was an artifact of `--no-build-vignettes` builds).
`R CMD check --as-cran --output=check-as_cran persephone3_0.1.0.tar.gz`:
**Status: 1 WARNING, 3 NOTEs** (exit 0; run completed 2026-11-16):

- WARNING, CRAN incoming feasibility: "New submission" + unknown DESCRIPTION
  field 'Remotes' — inherent to this internal package using GitHub Remotes;
  not applicable off-CRAN.
- NOTE, top-level files: `LICENSE` not mentioned in DESCRIPTION (the
  documented GPL-relicensing artifact: DESCRIPTION says GPL (>= 3) while
  `LICENSE`/`LICENSE.md` still carry the MIT notice pending the license
  decision) + non-standard `RELEASE_CHECKLIST.md`.
- NOTE, examples: `createDashboard` CPU time 4.86 s > 5 s (user+system).
- NOTE, HTML manual validation skipped: `tidy` not installed.

Tests, vignettes (build + rebuild), PDF manual, Rd documentation and the
dependency check are all OK; "checking package dependencies ... INFO" only
flags the 23-package Imports breadth; **no missing-dependency finding**, so
the shinycssloaders declaration is confirmed by the check itself.

**Interim release checklist verification (item 15, first pass).** Updated
`RELEASE_CHECKLIST.md` in place. Met now: source build + install; application
suite under JVM 21 (1,330/0/0/0, twice); Java-free loading + native honesty
(repo gate + isolated farm + config B); full `R CMD check --as-cran` with
vignettes and PDF manual locally (1 not-applicable WARNING, 3 documented
NOTEs); Java policy documented with local A/B/C evidence; workflows pinned
(check-full/test-coverage/pkgdown set up Temurin 21; pinned rjd3 tags; still
configured-only, never run from this branch). Unmet (unchanged): open P1
findings F1–F8 and F9; macOS/Windows lanes; CI executions; browser checks;
domain review; version bump decision; LICENSE-file text vs GPL decision.

**Not run.** Any GitHub workflow; minimum-R(4.1) and upstream-drift lanes;
coverage measurement; macOS/Windows; interactive browser checks; numerical
fixture/timing re-runs (M1 numerical track); HTML-manual validation (no
`tidy`); PDF manual was built but not spell-checked.

**Reproduction (all from `/tmp/opencode/m1d2`).**

```sh
mkdir -p /tmp/opencode/rlib-d1 /tmp/opencode/rlib-empty /tmp/opencode/m1d2
printf '.libPaths(c("/tmp/opencode/rlib-d1", .libPaths()))\n' > rprofile-d1.R
cp -r /tmp/opencode/rlib-d/shinycssloaders /tmp/opencode/rlib-d1/   # 1.1.0, machine gap
env -u LD_LIBRARY_PATH R CMD build --no-build-vignettes /data/home/kowa/seasonaladjustment/rkalos
env -u LD_LIBRARY_PATH R CMD INSTALL --library=/tmp/opencode/rlib-d1 rkalos_0.1.0.tar.gz
env -u LD_LIBRARY_PATH R CMD build --no-build-vignettes /data/home/kowa/seasonaladjustment/persephone3
env -u LD_LIBRARY_PATH R_PROFILE_USER=$PWD/rprofile-d1.R R CMD INSTALL \
  --library=/tmp/opencode/rlib-d1 persephone3_0.1.0.tar.gz

# Config A — JVM 21 (full suite as in M0)
env -u LD_LIBRARY_PATH JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 \
  R_LIBS=/tmp/opencode/rlib-d1:/data/home/kowa/R/x86_64-pc-linux-gnu-library/4.5:/opt/R/site-library/4.5 \
  R_LIBS_USER=/data/home/kowa/R/x86_64-pc-linux-gnu-library/4.5 R_LIBS_SITE=$PWD/../rlib-empty \
  Rscript --vanilla run_matrix.R "A-jvm21" full

# Config B — Java disabled (rlib-nojava-d: symlink farm of user+site libraries
# minus rJava and everything importing it, incl. rjd3* and RProtoBuf)
env -u LD_LIBRARY_PATH -u JAVA_HOME \
  R_LIBS=/tmp/opencode/rlib-d1:/tmp/opencode/rlib-nojava-d \
  R_LIBS_USER=$PWD/../rlib-empty R_LIBS_SITE=$PWD/../rlib-empty \
  Rscript --vanilla run_matrix.R "B-java-disabled" per-file

# Honesty gates (B-style isolation; also ran under A)
env -u LD_LIBRARY_PATH -u JAVA_HOME Rscript --vanilla verify_native_unsupported.R
env -u LD_LIBRARY_PATH -u JAVA_HOME Rscript --vanilla \
  /data/home/kowa/seasonaladjustment/persephone3/tools/verify-nojava.R \
  --library=/tmp/opencode/rlib-iso-d --native
# Config C — JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64, probe_c_jvm17.R

# Full build + as-cran check (needs R_LIBS for --vanilla vignette renderers and
# R_PROFILE_USER for non-vanilla subprocesses because of the site .libPaths())
env -u LD_LIBRARY_PATH JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 \
  R_LIBS=/tmp/opencode/rlib-d1:/data/home/kowa/R/x86_64-pc-linux-gnu-library/4.5:/opt/R/site-library/4.5 \
  R_LIBS_SITE=/tmp/opencode/rlib-empty R_PROFILE_USER=$PWD/rprofile-d1.R \
  R CMD build /data/home/kowa/seasonaladjustment/persephone3
mkdir -p check-as_cran
env -u LD_LIBRARY_PATH JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 \
  R_LIBS=/tmp/opencode/rlib-d1:/data/home/kowa/R/x86_64-pc-linux-gnu-library/4.5:/opt/R/site-library/4.5 \
  R_LIBS_SITE=/tmp/opencode/rlib-empty R_PROFILE_USER=$PWD/rprofile-d1.R \
  R CMD check --as-cran --output=check-as_cran persephone3_0.1.0.tar.gz
```

## Linux P1 re-verification F1–F9 — 2026-11-16 (M2)

All original minimal reproductions from the reassessment baseline (`dd38e10`)
were executed against the installed package at HEAD `1ceb6f0` on this Linux
host (R 4.5.3, JVM 21.0.11, rjd3 stack as in the M1 matrix, locale
`en_US.UTF-8`). Reproduction scripts and outputs: `/tmp/opencode/m2d/`
(`repro.R`, `repro2.R`, `repro3.R`, `repro4.R`). The interim-release
ambiguity ("findings table red but items 6–13 delivered") is resolved: the
table records the historical baseline; empirically every finding now behaves
per its delivered contract.

| Finding | Verdict on Linux at `1ceb6f0` | Evidence |
| --- | --- | --- |
| F1 fixOutlier after fit | **fixed** | `perX13(AirPassengers)` run → `$fixOutlier()` → rerun, and TRAMO/SEATS likewise, complete without the `argument is of length zero` error; repeated fixing adds no duplicate specifications. |
| F2 public forecast API | **fixed** | `$forecasts` returns a `persephone_forecasts` contract for both methods; hierarchy `$forecastsDirect`/`$forecastsIndirect` evaluate without the array error. |
| F3 outlier classification/counts | **fixed** | Manual AO at 1953-07-01 reports `autoout = FALSE`, correct type/date, no coercion warnings; zero-outlier diagnostics counts are `0/0/0`, not `NA`. |
| F4 hierarchy decisions | **fixed** | With `root$run()` then flags (`inner$indirect <- FALSE`, ancestors indirect): `root$adjusted` equals the selected-child sum exactly (maxdiff 0) while the selected-vs-fully-indirect gap on the AirPassengers-based fixture is **36.8904** — the same magnitude as the recorded 36.89045 defect — so the child decision is genuinely honored. Flipping the inner choice switches `adjusted` accordingly; `adjustedIndirect` equals the fully-indirect leaf sum. |
| F5 short weight alignment | **fixed** | Original fixture: first weighted aggregate is 51 (defect value 8.692308); a late-start weight series now fails with "Weights for 'a' are missing the beginning of the component history." |
| F6 calendar regressors | **fixed** | `genTd(2020, 2021, hd = list("easter+1"), weight = 0)` gives zero effect (maxdiff 0 vs the old 1.593642); `freq = 1` now fails with "freq must be 4 or 12."; output shape 7/6/7 matches the rewritten documentation. |
| F7 diagnostics/dashboard agreement | **fixed** | With controlled `p = 0.01`: table and single dashboard both show TD-regression **Good** and residual-TD **Bad** (reversed rule gone); categorical seasonality **Present** survives in the table. The suite additionally walks real dashboard cards for both engines and both boundary sets. |
| F8 template replacement | **fixed** | `updateParams(template = "rsa0")` on an `rsa3` object changes the spec and matches a fresh `rsa0`; hierarchy `updateParams(template = )` no longer throws `'what' must be a function or character string`. |
| F9 dashboard identities | **fixed** | `dashboard_registry` on a tree with a child literally named `Aggregate`, duplicate labels, and a hostile label (quotes, markup, `&`, `/`) assigns unique `node_*` ids, routes `node_1` to the child (not the root), and is deterministic. 101 assertions in `test-dashboard-identities.R` cover escaping and selection. |
| F10 CI matrix | **configured only** | Java 21 + pinned rjd3 tags are declared; still no workflow execution from this branch (push is not authorized). |

Method notes: the F4 probe must use the supported sequence — run the root
first, then set `indirect` flags; constructing with flags pre-set and never
running the child hierarchy separately leaves the child's aggregate outputs
NULL (expected, reconstruction is the supported mutation contract). One new
observation, **outside this package**: a deliberately degenerate 48-month
synthetic series (linear trend plus a strict 3-period pattern) makes
`rjd3toolkit`'s protobuf-to-R conversion fail with
`missing value where TRUE/FALSE needed` at `if (pval < 0)` inside
`.jd2r_test -> statisticaltest` (NA test statistic), surfacing during
`perX13()$run()`. This is a backend-side robustness gap for difficult models
(item 14 "difficult-model compatibility" lane), not a regression of F1–F8;
reproducer: `/tmp/opencode/m2d/f4debug2.R`.

**Suite state unchanged:** 20 files, **1,330 assertions, 0 failures/errors/
skips** at `1ceb6f0` (same per-file distribution as the M1 baseline). No
package code was changed by this re-verification; no new tests were needed
because every existing and probe check already passes.

## Linux item-14 parity harness — 2026-11-16 (M3)

Backend-parity test harness delivered as `inst/tinytest/test-parity.R`
(97 assertions, 3.6 s). No native fits exist, and the harness says so
explicitly instead of faking a comparison.

**Harness structure.** File-level capability gate (skips whole file without
the rjd3 stack, unlike the nine legacy files that abort). One cached rjd3
reference fit per specification, reused by every check. `parity_summary()`
reduces a fit to metric-level canonical data (component names, time domain,
lengths, NA masks, forecast count, last-6/mean/sd/sum of `y`/`t`/`sa`) —
deliberately no whole-object snapshots. `parity_compare(reference, candidate)`
encodes the parity contract (exact structural fields, absolute tolerances on
numeric metrics, violations returned as a character vector). `parity_native_fit()`
is the single documented TODO hook: it must return `NULL` while
`rkalos::capabilities()$fit` is `FALSE` (verified against the installed
rkalos 0.1.0, which exports primitives but no fitting entry point), and the
harness asserts that `NULL` plus the honest native error path on every spec
instead of silently skipping the lane.

**Spec matrix.** {x13, tramoseats} × {rsa0, rsa3} × {monthly AirPassengers,
quarterly aggregate} = 8 fits, plus one hierarchical case (two components,
`adjustedDirect` vs `adjustedIndirect` distinctness re-asserted as a routing
guard). Per spec: five structural invariants, one self-parity check (summary
extraction determinism), two native-capability assertions (fit FALSE, status
`unimplemented`), the explicit no-Java-fallback construction error, and the
NULL native hook.

**Tolerance evidence (cross-session stability study).** Driver
`/tmp/opencode/m3d/stability.R`: all nine reference fits summarized in two
separate R sessions (same qualified stack: rjd3 3.9.0/3.9.1, jars 0.0.6,
JVM 21.0.11, R 4.5.3). **98 of 98 numeric metrics bit-identical** (max
absolute and relative difference 0; `identical()` true for every vector).
Consequently the pinned tolerances are pure safety margins for JVM
minor-version/platform drift, ~1e7× below the 0.1 reporting precision:
1e-8 absolute for all numeric metrics; names/lengths/counts/NA masks exact;
the time domain is compared as exact integers (year, period, frequency,
length) because the backend-stored decimal `tsp` end is not canonical in
floating point (this exact float representation issue surfaced while
authoring the harness). Any stack upgrade that moves a digit inside 1e-8
requires a recorded re-baseline; widening tolerances silently is prohibited
by the file header.

**Known-issue encoding (difficult-model lane).** The M2 finding — rjd3toolkit
`if (pval < 0)` crashing on an NA test statistic for the degenerate 48-month
series — is regression-protected in the same file with a three-way contract
(justified in the header): success ⇒ assert a sane fit and the pin retires
itself; pinned message ⇒ green (documented current state); a *different*
error on an English/C `LC_MESSAGES` ⇒ red on purpose because the failure
contract changed. A non-English message locale skips, since the pinned
signature is an R interpreter string and would otherwise false-fail.

**Suite state:** 21 files, **1,427 assertions (1,330 baseline + 97 new),
0 failures/errors/skips**.

## Linux M3 finale — restricted native X-13 fit in `run()` — 2026-11-16 (Agent D)

The restricted fixed-coefficient X-13 fit (rkalos 10afd3e, core 2b93ab0,
schema_version 2) is wired into the Persephone product path:
`perX13(ts, template = "restricted", speclist = ..., backend = "native")$run()`
performs a real Java-free seasonal adjustment. The headline measurement —
the first native-vs-rjd3 numeric agreement in the family — is **bit-exact:
all final-series max diffs 0.000e+00** against the pinned rjd3 reference,
inside the pinned 1e-8 policy by construction.

**Gating contract (verbatim).** Gate on
`identical(rkalos::capabilities()$native_x13_fit_restricted, TRUE)` plus the
`capabilities()$restricted_fit` scope block. `capabilities()$fit` (unrestricted
fitting) is *never* consulted and stays FALSE. The refusal text and the honest
capability reporting of earlier milestones are preserved verbatim for all
out-of-domain requests, so `tools/verify-nojava.R` and the pinned wording in
`test-backend.R` keep matching.

**Scope mapping (Persephone specification → `native_x13_fit()` request).**

- Template: the native backend constructs exactly one specification, sentinel
  `template = "restricted"` (class `persephone_native_spec`, deliberately not
  a JD3 spec and not classed `persephone_spec`). All automatic templates
  (rsa0–rsa5c) are refused naming `fixed_coefficients_only`; `perTramo` with
  native additionally names the entry points, since the qualified fit is
  x13-only.
- Seasonal filter: `seasonal` must state one pinned fixed filter per calendar
  period (stable/s3x1/s3x3/s3x5/s3x9/s3x15); `x11default`/`msr` and missing
  requests are refused naming `fixed_seasonal_requests_only`.
- Trend: `trend_length` must be a stated odd integer ≥ 5; the automatic 0 is
  refused naming `fixed_trend_length_only`.
- Sigma policy: `sigma_policy = "legacy" | "accurate"` is a REQUIRED new spec
  field with no default (mirrors the core contract; NULL refused naming
  `sigma_policy_required`). `lower_sigma`/`upper_sigma` default 1.5/2.5.
- Regression variables: fixed coefficients only — every variable must carry
  `class`/`effect` from the pinned vocabulary plus one finite coefficient per
  numeric column of series length; anything estimated is refused naming
  `fixed_coefficients_only`.
- Horizons: none exist in the native specification (zero_horizon_only);
  requests carrying forecast fields are refused as not part of the restricted
  specification. `forecasts`/`forecastsDirect` return NULL and the
  availability table marks all forecast fields `not_provided`.
- Frequency: monthly only (`monthly_only`, frequency 12); other frequencies
  are refused at construction. Mode: additive (default), multiplicative,
  log_additive, pseudo_additive, per `restricted_fit$modes`; log flag must be
  TRUE/FALSE. Contexts and rjd3 user-defined output descriptors are rjd3-only
  and refused naming the field.
- Refusal wording (example, verbatim shape): `The native backend does not yet
  implement seasonal-adjustment fitting for 'x13': <core detail>. Violated
  scope field(s): sigma_policy_required. No Java fallback was used. Select
  backend = "rjd3" to use the reference implementation.` Multiple violated
  fields are named together; the rjd3 path and objects are unchanged.
- Access seam: `native_x13_fit` exists in the rkalos namespace but is not
  yet exported; the backend reaches it with an explicit
  `get0(..., asNamespace("rkalos"))` (documented in the code).

**Native result envelope (`persephone_native_restricted_fit`).** Honest class
in the versioned `$result` — NOT an rjd3-shaped `$output` and not classed
`persephone_result`: versioned fields (schema_version, method, backend,
specification, status, domain, components, availability, provenance) plus
`components$t/sa/i` as ts objects (D12final/D11final/D13final), the raw
rkalos result (`$raw`, `$results`, `$preadjustment`, `$selection`), and
provenance echoes of the fit (mode_requested/mode_effective, sigma policy,
trend length, mode forcing, scope string, core commit) plus a deterministic
request fingerprint (two rolling prime-modulus hashes, `xxxxxxxx-xxxxxxxx`,
attached as `persephone_request_fingerprint`). `$is_stale` follows the
existing signature machinery (spec updates flip it; reverting unflips).
`$adjusted`/`adjustedDirect` expose the final SA ts on the native path.
A print method summarizes status/mode/policy/fingerprint/last-3-SA.

**Parity harness flipped (headline).** `test-parity.R` was restructured so
the native lane (gate contract, refusal matrix, product-path restricted fit
with pinned session-stability metrics, fingerprint determinism) runs
**without Java or rjd3** — only the rjd3 reference sections are gated.
`parity_native_fit()` returned to its intended role: it returns the native
candidate summary for the pinned fixed specification (s3x15 everywhere,
Henderson 13, additive, legacy sigma, 1.5/2.5, zero horizons, no variables)
and NULL for everything outside the restricted domain (asserted for all
eight automatic-template matrix specs). The dual-lane case builds the rjd3
reference from raw `x13_fast` (rsa0 + transform none + tradingdays None +
`set_x11(S3X15, henderson = 13, Additive, fcasts = 0, bcasts = 0)` —
the product-level rjd3 lane itself cannot carry zero-horizon outputs because
the pre-existing forecast contract rejects rjd3's residual `cal_f`/`y_ef`
series against zero point forecasts; untouched here, pre-existing limitation)
and compares via `parity_compare` at the pinned 1e-8: **violations: none;
full-series max diffs for sa, t and i all exactly 0.000e+00**; lengths, NA
masks and the integer time domain exact. A violation prints per-component
max diffs before failing (STOP-with-diagnostics; widening is prohibited).

**Java-free proof.** `test-parity.R` runs green in a JVM-free Rscript against
a symlink-farm library without rJava/rjd3*/RProtoBuf and JAVA_HOME unset:
53/53 native-lane assertions pass and the file asserts that no Java namespace
was loaded. `tools/verify-nojava.R` now additionally runs the restricted fit
through the product path and asserts the out-of-domain refusal (both PASS).

**Suite state:** 21 files, **1,459 assertions (1,330 baseline + 129 in the
flipped parity harness), 0 failures/errors/skips** on the qualified JVM 21
stack; parity file 97 → 129 assertions (net +32).

**Follow-ups recorded (not defects):** (1) the rjd3 lane cannot `$run()`
zero-horizon outputs (forecast-contract rejection of `cal_f`/`y_ef`) —
asymmetric with the native lane where zero horizons are the domain;
(2) `native_x13_fit()` is not exported from rkalos (namespace seam in use);
(3) a user-facing `speclist` validation for `seasonal` lengths could move
some refusals from fit time to construction time.

## Next application work

1. Continue item 14: difficult-model and selection-boundary product compatibility,
   platform CI and presentation/reporting parity, using Kalos-owned evidence.
2. Connect a restricted native fit only when Kalos completes its qualified X-11
   path (items 22b/22c). Delegate engine calls through rkalos; keep R6 workflows here.
3. Complete product parity item 26, then review item 27's default-backend cutover.
   Full Java removal remains gated by delivered features and independent evidence.

The next numerical item is 22b.3a in Kalos. Its existing sigma-weight default
decision is documented there and is not part of this repository-split decision.
Accurate covariance/residual defaults, Java-free legacy comparison options and
the selected JDemetra MSR default are unchanged.

## Application correctness record

The following application findings and items 6–15 are retained from the original
reassessment. Historical test totals refer to their recorded pre-split baseline;
current numerical qualification belongs to Kalos/rkalos.

## Findings at the reassessment baseline

Priority **P1** means a correctness issue to resolve before release sign-off.
**P2** means compatibility, input handling, or delivery hardening. Source
locations refer to `dd38e10` and may move as fixes are implemented.
On 2026-11-16 every finding below was re-executed on Linux at HEAD `1ceb6f0`:
all P1 reproductions (F1–F8) and F9 no longer reproduce — see "Linux P1
re-verification F1–F9 — 2026-11-16 (M2)"; the rows retain the historical
baseline state.

| ID | Priority | Finding and evidence | Relevant source |
| --- | --- | --- | --- |
| F1 | P1 | After fitting `AirPassengers`, both `perX13(...)$fixOutlier()` and `perTramo(...)$fixOutlier()` fail with `argument is of length zero`. The implementation reads the old `output$regarima` structure. Its hierarchy path also refers to `private` outside an R6 method. | `R/fixOutlier.R:26`, `:78`, `:96` |
| F2 | P1 | Fitted X-13 `$forecasts` and hierarchy `$forecastsDirect` return `NULL` although `output$user_defined$y_f` contains 12 values. `$forecastsIndirect` then fails with `'x' must be an array of at least two dimensions`. | `R/persephone.R:235`, `:239`; `R/perHts.R:461` |
| F3 | P1 | A manually specified `AO` at `1953-07-01` is reported as `autoout = TRUE`; coercing its character specification date to numeric emits an NA warning. Aggregate extraction additionally reads the nested `$params` list rather than `$paramsDirect`. A fitted no-outlier model reports `NA` counts despite backend `regression.nout = 0`. | `R/get_functions.R:143`, `:152`, `:192`; `R/diagnosticsTable.R:223` |
| F4 | P1 | A parent using indirect adjustment ignores a nested child's `indirect = FALSE` choice. It always recurses to the leaves. A non-identical-series fixture differs from the sum of the chosen child results by **36.89045**, while matching fully indirect leaf aggregation exactly. | `R/perHts.R:489`, `:552`, `:558` |
| F5 | P1 | Short time-varying weights are accepted, but their last values replace the entire observation span. With 24 monthly observations, first component values 1 and 101, and 12 months of weights `1:12` and `rep(1,12)`, the first aggregate is **8.692308**, not **51** using the first observed weights. The implementation should preserve aligned history or reject incomplete input. | `R/perHts.R:590`, `:602` |
| F6 | P1 | `genTd(firstYear=2020, lastYear=2021, hd=list("easter+1"), weight=0)` still changes centered weekday regressors by up to **1.593642**. Easter centering does not apply the holiday weight. Additional contract mismatches: documented `NewYearsDay` input errors; `freq=1` silently returns monthly data; returned column counts are 7/6/7 rather than the documented working-day layout. | `R/genTd.R:9`, `:18`, `:237`, `:244`, `:299`, `:305` |
| F7 | P1 | With a controlled regression TD p-value of 0.01, `diagnosticsTable()` returns **Good**, while the single dashboard returns **Bad**. Both dashboard implementations reverse the API rule. The backend categorical seasonality result **Present** is coerced to numeric and displayed as **Undefined**. | `R/diagnosticsTable.R:175`; `R/dashboard_single.R:157`, `:403`; `R/dashboard_compare.R:147`, `:282` |
| F8 | P1 | For both single-series methods, `updateParams(template="rsa0")` leaves an existing `rsa3` specification identical, unlike a fresh `rsa0` object. A hierarchy call with that argument errors with `'what' must be a function or character string`. Template replacement/update semantics are not consistent across classes. | `R/persephone.R:67`; `R/perX13.R:28`; `R/perTramo.R:28`; `R/perHts.R:363` |
| F9 | P2 | A hierarchy component named `Aggregate` yields duplicate UI paths and resolves to the root instead of the child. **Source inspection:** names also enter checkbox HTML with `escape=FALSE`, CSS selectors, and inline JavaScript without context-appropriate escaping. Quotes, markup, and `/` require safe identities independent of display labels. | `inst/app/helpers.R:6`, `:18`; `inst/app/modules/mod_table.R:54`, `:98`; `inst/app/server.R:51` |
| F10 | P2 | **Source inspection:** current CI tests release/devel R, not declared minimum R 4.1; check and coverage jobs use Java 21 and 17 respectively, while pkgdown does not explicitly set up Java. Dependencies resolve moving GitHub refs. The check workflow searches for `testthat` logs although this package uses tinytest. | `DESCRIPTION`; `.github/workflows/check-full.yaml:27`, `:53`, `:136`; `test-coverage.yaml`; `pkgdown.yaml` |

### Minimal reproduction examples

These reproduce failures at the `dd38e10` assessment baseline. Consult the
delivery log for fixes implemented since then; resolved examples no longer
produce the annotated failure. They are not proposed usage patterns.

```r
library(persephone3)

x <- perX13(AirPassengers)
x$run()
x$fixOutlier()                         # currently errors
is.null(x$forecasts)                   # TRUE
length(x$output$user_defined$y_f)      # 12
x$diagnosticsTable()[, c("nOutliers", "nAutoout", "nRecentout")]
# Currently NA rather than zero for this fitted model.

x <- perX13(AirPassengers, template = "rsa3")
before <- x$params
x$updateParams(template = "rsa0")
identical(before, x$params)            # TRUE: requested template was not applied

x <- perX13(AirPassengers, addout.type = "AO", addout.date = "1953-07-01")
x$run()
x$getOutliers()                       # manual AO currently marked automatic

g <- genTd(firstYear = 2020, lastYear = 2021,
           hd = list("easter+1"), weight = 0)
max(abs(g[[3]][, 1:6] - g[[2]]))       # approximately 1.593642, should be 0
```

### Why the baseline tests missed these cases

- `test_fix.R` checks `fixOutlier()` only **before** fitting, then checks that
  the fitted output exists without calling the fixer again.
- Calendar tests assert only that the returned list has three elements.
- Hierarchy tests predominantly assert classes/non-NULL results and time
  spans; they do not assert nested decision semantics or weighted values.
- Forecast plot fixtures use `user_defined$y_f`; they never exercise the public
  forecast bindings that read a different output path.
- Dashboard tests inspect HTML, titles, rendering, and lifecycle behavior, but
  not agreement of displayed numbers/statuses with the diagnostic API.
- The hierarchy test labelled as a known quality-table issue only asserts the
  hierarchy object is non-NULL. It is not evidence of quality-report accuracy.

## Correctness plan and delivery status

The original acceptance criteria are retained below. Items 6–13 have now been
implemented and tested; their delivery details and explicit limitations are in
the log above. Items 14–15 remain broader compatibility/release work. Numerical
backend items 16–27 follow later in this document.

### 6. Normalize outlier extraction and empty-result semantics — delivered

**Addresses:** F3. **Dependencies:** none. **Scope:** `R/get_functions.R`,
`R/diagnosticsTable.R`, their consumers, and new extraction tests.

- Read the direct specification appropriate to the object: single `$params`,
  hierarchical `$paramsDirect`. Normalize supported date representations using
  explicit type handling, without coercing ISO strings to numbers.
- Give no-outlier results a stable typed representation, preferably a zero-row
  table. Reserve missing/unknown values for genuinely unavailable information.
  Update existing plot/report consumers together to avoid breaking their
  `all(is.na(...))` and `$date` assumptions.
- Recheck fixed/estimated regression coefficient mapping, standard errors, and
  recent-outlier boundaries against the installed backend structures.

**Acceptance:** manual AO/LS/TC entries are not labelled automatic for either
method, monthly/quarterly data, or aggregate objects; zero-outlier counts are
zero; no coercion warnings; fixed coefficients have documented uncertainty
fields; legacy/current supported date representations have explicit fixtures.

### 7. Migrate `fixOutlier()` to the current backend — delivered

**Addresses:** F1. **Depends on:** 6. **Scope:** `R/fixOutlier.R`, method wrappers,
manual, and `test_fix.R`.

- Replace legacy result paths and `usrdef.*` updates with the supported rjd3
  extraction/setter path behind the adapter in item 17. Express the operation
  through the common specification contract so it also works with the native
  engine. Remove free-function references to R6 `private`.
- Define whether the protected tail is measured in months or observations;
  the current documentation says months while indexing uses observations.
  Convert quarterly windows explicitly and validate zero/negative/oversized
  spans.
- Preserve existing manual outliers, add eligible detected entries once, and
  limit re-detection to the intended tail. Define aggregate-only versus
  recursive behavior rather than relying on incidental default propagation.

**Acceptance:** run → fix → rerun succeeds for X-13 and TRAMO/SEATS; eligible
outliers remain specified, recent ones remain eligible for detection, repeated
fixing is idempotent, and no-outlier/disabled-detection cases work. Include
monthly and quarterly, single and hierarchical cases with actual fitted data.

### 8. Make specification updates explicit and consistent — delivered

**Addresses:** F8; model-maintenance semantics. **Dependencies:** none for the
basic update fix; reuse item 6 fixtures where relevant.

- Specify and implement the difference between replacing a template, replacing
  a complete specification, and applying incremental parameter changes.
  Validate supported templates/blocks before mutating any object. Define these
  rules once in the versioned specification contract from item 17.
- Apply consistent behavior to single, batch, aggregate-only, recursive, and
  selected-component updates. Do not silently ignore `template` or forward it
  to a nonexistent setter.
- Decide and document how changed specifications mark existing results stale.
  Today outputs remain available unchanged until rerun; distinguish this state
  from a fit for the current settings without silently changing the public API.
- Clarify whether `fixModel()` freezes ARIMA orders or coefficients. Its code
  sets orders and disables automatic selection; the manual currently promises
  that parameters will not be re-estimated. Test the chosen contract.

**Acceptance:** `rsa3` → `rsa0` matches a fresh `rsa0` baseline when replacing a
specification; unrelated settings survive incremental updates; invalid updates
leave the prior state intact; recursive/selective updates touch only intended
nodes; rerun/state behavior and model-fixing behavior are tested and documented.

### 9. Correct hierarchy decisions and weight alignment — delivered

**Addresses:** F4/F5. **Dependencies:** none; specification tests from 8 help
construct differentiated model fixtures. **Scope:** aggregation in `R/perHts.R`.

- Separate fully indirect leaf aggregation from aggregation of each child's
  selected direct/indirect result. Honor a nested child's decision for the
  latter; keep both operations explicit.
- Align weights by actual time index. Preserve supplied historical values and
  either reject incomplete histories or implement a documented extension rule.
  Carrying the last value forward must not overwrite earlier weights.
- Resolve the documentation's weighted-sum language versus the implementation's
  `weighted.mean`. Choose a stable contract for index aggregation and explain
  nesting and normalization; do not silently switch arithmetic.
- Validate dimensions, frequency, named-column alignment, missing/nonfinite
  weights, and zero total weight. Define whether public weight/component
  mutation is supported; if it is, recompute aggregate state consistently.

**Acceptance:** hand-computable weighted/unweighted fixtures agree numerically;
three-level mixed decisions produce the expected selected-child aggregate;
fully indirect results remain distinct where appropriate; the short-weight
reproduction either gives 51 initially or fails clearly before calculation;
monthly/quarterly and mismatched-span cases are covered.

### 10. Restore the public forecast API — delivered

**Addresses:** F2. **Depends on:** 9 for indirect aggregation.

- Define/document what each public forecast field returns: original, adjusted,
  and/or trend forecasts, with stable names, class, frequency, and time span.
  Use supported current backend fields instead of `output$final$forecasts`.
- Make single, batch, direct hierarchy, and indirect hierarchy accessors agree
  on that contract. Handle an unavailable/zero horizon explicitly.
- Align future weights under the policy from 9. Keep forecast points separate
  from uncertainty: do not sum standard errors/intervals without a justified
  covariance assumption.

**Acceptance:** actual X-13/TRAMO-SEATS monthly/quarterly fits expose forecasts
matching requested backend series; indirect points equal independently
aggregated component points; forecast dates/horizons are correct; pre-run,
missing-output, mixed-horizon, and weighted cases have defined behavior.

### 11. Validate and correct calendar regressors — delivered

**Addresses:** F6. **Dependencies:** none; can proceed independently of 6–10.

- Apply holiday weights consistently to observed counts and Easter centering.
  Verify exact/approximate distributions with small, independently calculated
  calendars, including duplicate dates and fixed/Easter holiday collisions.
- Replace `eval(parse(...))` with a strict Easter-offset parser and date
  arithmetic. Validate year order, supported frequency, holiday syntax, and
  weight length/values.
- Either implement named holidays or remove that unsupported promise through
  an explicit compatibility decision. Reconcile output documentation with the
  actual raw counts, six contrasts, and leap-year column; do not imply a working
  day regressor that is absent.

**Acceptance:** zero weight has zero calendar effect; fractional weights scale
corrections appropriately; leap-year and quarterly sums are correct; duplicate
holiday handling is deterministic; invalid inputs fail clearly; output shape,
column meaning, and results are tested under more than one locale.

### 12. Unify numerical diagnostics and reporting — delivered

**Addresses:** F7 and downstream F3 effects. **Depends on:** 6.

- Extract shared metric values and classification rules used by tables,
  single dashboards, comparison dashboards, and quality reports.
- Distinguish regression-effect significance from residual diagnostics: the
  direction of a good p-value is not the same. Preserve categorical seasonality
  separately from residual-seasonality p-values.
- Audit quality-report field paths, filter labels, top-three outliers, missing
  values, and zero-valued series. Treat overall backend quality as its own
  result; do not infer it solely from Q/Q2.

**Acceptance:** all surfaces display the same value/status for a shared metric;
boundary cases at 0.001/0.01/0.05/0.1 are explicit; categorical `Present` is not
silently lost; zero and unavailable are distinct; representative X-13 and
TRAMO/SEATS reports match backend values or independently calculated references.

### 13. Decouple dashboard identities from labels — delivered

**Addresses:** F9. **Dependencies:** none; coordinate data display with 12.

- Use opaque stable identifiers for nodes and a lookup map to actual component
  paths. Keep `Aggregate` as a display label, not a reserved routing token.
- Escape label text and HTML attributes and serialize JavaScript data correctly.
  Avoid concatenating labels into selectors or inline event handlers.
- Test repeated select/close/reopen cycles, nested nodes, table sorting/paging,
  and cleanup of observers/outputs. Remove unconditional debug printing of
  entire series tables from normal app startup.
- Investigate startup deadline enforcement: the loop checks elapsed time, but
  a blocking URL probe has no per-probe timeout. Add a bounded probe and a
  stalled-server test if it can exceed `startup.timeout`.

**Acceptance:** `Aggregate`, duplicate leaf labels in different branches,
spaces, quotes, markup, Unicode, and path separators are either supported
unambiguously or rejected early by a documented rule; no label executes as
markup/script; reopened panels show current data and do not accumulate work;
startup failure/interruption leaves no child process or open port.

### 14. Establish a numerical and compatibility regression matrix — partial

**Addresses:** F10 and the testing gaps above. **Depends on:** correctness fixes
for a green final matrix; the test infrastructure can be prepared earlier.

- Add direct-backend parity tests using the exact same data/specification for
  both methods. Use explicit numeric tolerances and structural invariants,
  avoiding brittle whole-object snapshots tied to a Java version.
- Cover templates, monthly/quarterly inputs, mixed/nested hierarchies, calendars,
  manual/automatic/fixed regressors, missing values, and reruns. Replace the
  existing known-issue placeholder assertions with substantive checks.
- Test declared minimum R/rjd3/Shiny versions where supported, or raise minimums
  with evidence. Make the supported Java policy explicit and align/check all
  workflows, including pkgdown.
- Run a pinned dependency lane for reproducibility and a current-upstream lane
  for drift detection. Capture session/package/Java versions with artifacts.
- Print/upload tinytest logs, ensure CI cannot silently skip tests when a
  suggested dependency is absent, measure coverage by critical function, and
  retain a full documentation/manual build lane. Add a non-deploying pkgdown
  build on pull requests; deployment remains a separate authorized action.

**Acceptance:** supported macOS/Windows/Linux jobs pass, compatibility claims
have actual results, failures expose useful test logs, and versioned artifacts
allow the numerical baseline to be reproduced. Do not claim a coverage
percentage until it is measured.

### 15. Release review and handoff — repeated at each release

**Depends on:** all P1 items, identity hardening, and the compatibility evidence
required for the release's supported environments.

An interim release can stabilize the existing rjd3 implementation after items
6–14. The later **native-default, Java-free release additionally depends on
items 16–27**. An interim release does not complete the backend migration.

- Rebuild the source package, run full checks/tests/vignettes/PDF manual, and
  repeat browser checks against the resulting installed package.
- Review representative outputs with a seasonal-adjustment domain reviewer,
  especially weighting, calendars, uncertainty, and quality classifications.
- Update NEWS, examples, support requirements, and this status file; document
  any intentionally unsupported behavior or migration in API/schema.
- Push/open a review and merge/publish only within the user's authorization.
  A local green check alone is not release approval.

**Acceptance:** no open reproduced P1 finding, no unexplained numerical
regression, clean package checks, successful supported CI matrix and browser
flows, and an explicit release decision with documented scope.
