# Persephone implementation status and next-work plan

Updated: **2026-11-24**. The repository split is integrated on
`rkalos_backend`. **rkalos is a required dependency**.

## Stock X-13 backend contract checks (2026-11-24)

The method-filtered capability now lists stock estimated templates for X-13
and omits that field for TRAMO-SEATS. The complete source check exposed two
older tests that expected RSA0 construction to fail. They now verify native
RSA0 construction, RSA4 refusal and Java's lazy initialization. The installed
backend and reinitialization tests passed **97/97** and **16/16**.

## M5b stock estimated X-13 in Persephone (2026-11-24)

`perX13(series, template, backend = "native")` supports the original monthly
RSA0, RSA1 and RSA2c templates with rkalos `0.1.0.9023` at `f53ee8f`.
The fitted object exposes the estimated model, likelihood, dated residuals,
observed components and twelve point forecasts. RSA2c exposes its fitted
calendar regressors and their coefficients in the quality report and
regressor table. The stock model retains the fixed airline orders.

Across nine published fits, the isolated Java-free suite passed **315/315**
and the live JDemetra+ suite passed **549/549**. The live checks include
all six observed components, five available forecast series and selected
calendar and outlier report fields. The general parity suite passed
206 Java-free and 403 live checks, reporting passed 46 and 58 respectively,
and all 19 dashboard checks passed.

## Full source verification for the family M5 checkpoint (2026-11-24)

With the published rkalos `0.1.0.9021` dependency at `45cf058`,
Persephone `21f09f3cf41a32d10aab0dbfda251736d49f7d60` passed
**7,488/7,488** tinytest results in its full source check. Examples,
rebuilt vignettes and the PDF manual passed; `R CMD check` reported
`Status: OK`. The pinned rkalos source passed **308,774/308,774**
serial and **308,778/308,778** OpenMP checks. The family M5 report is
committed in the development repository at `77f6b98`.

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

## M6 native combined SI in tables, dashboards and quality reports (2026-11-24)

Persephone pins rkalos `0.1.0.9022` at
`c52aba9846f84275df9ac2d72f30dafc39a4ff05`. The normalized results for
TRAMO-SEATS and fixed X-13 carry the combined SI diagnostic, and the table,
dashboard and quality report show its public categorical result. A short
series reports missing. The two RSA3 fits with a missing stochastic seasonal
component correctly report `None` from their irregular series.

The 24-product fixture panel passed **171/171** Java-free and **211/211**
with live JDemetra. A separate live regression checked the public combined
SI category on all 29 calendar-adjusted RSAfull products (**1,872/1,872**
assertions across that file). Existing diagnostic and reporting checks passed
in both installations. The automatic X-13 decoder has its own qualification.

## M5 RSAfull backend capability inventory (2026-11-24)

The public backend inventory test now checks all four monthly stock TRAMO
templates, including RSAfull. This resolves the sole failed assertion
in the initial 7,488-result source check at `65ed210`. The corrected
backend file passed **94/94** live and **41/41** Java-free assertions.
The full rkalos `0.1.0.9021` package suites passed **308,774/308,774**
serial and **308,778/308,778** OpenMP.

## M5 ordinary native RSAfull with calendar-adjusted products (2026-11-24)

`perTramo(series, "rsafull", backend = "native")` uses published rkalos
`0.1.0.9021` at `45cf058c46f225005ce73ebe04e1b96b6587d907`. All 29
public stock fits select calendar regressors, orders, transformations and
outliers from the original monthly observations. Working-day and individual
weekday contrasts, leap year and Easter appear in the regression table;
the quality report records the selected count and effects. Observed and
forecast calendars are dated and use the original series scale. Custom
fixed-order and manual-outlier changes retain the ordinary stock restrictions.

The panel matched all 26,160 merged component values and 5,232 calendar
effects bit-for-bit against the frozen public reference. The live reference
additionally checked report labels, coefficients, point forecasts and
original-series forecast uncertainty. RSAfull passed **1,029/1,029**
Java-free and **1,843/1,843** live checks. Including the other three TRAMO
templates and reporting gives **3,383/3,383** Java-free and **5,238/5,238**
live results. The earlier `67db3fa` full source check passed **5,645/5,645**
with examples and rebuilt vignettes; the separate PDF manual has 58 pages.

## Twenty monthly native RSA3 products and missing seasonal uncertainty (2026-11-24)

Persephone pins rkalos `0.1.0.9020` at `c3170b14bc9d2e8570389556f9d4cbf6e176435d`.
Twenty stock RSA3 inputs cover reference retries in addition to seasonal
restarts and fast-mean refits. The native result has 13,980 exact merged
component and forecast observations, 2,105 dated innovations and the
original-series forecast errors from all twenty fitted models.

Two models have a neutral final seasonal factor and no stochastic seasonal
component. Their conditional seasonal forecast uncertainty carries an
explicit unavailable reason. The remaining point and error series retain
their domains, and the SI plot explains the missing seasonal component on
both backends. The installed application passed **1,311/1,311** Java-free
and **1,933/1,933** live-reference checks. A separate full source check of
`e356743` passed **4,823/4,823** tests, examples, rebuilt vignettes and the
PDF manual with `Status: OK`.

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

## Maintainer sigma-rule default cascaded into the native spec — 2026-11-17 (Agent D)

Kalos maintainer decision (2026-11-17), cascaded from rkalos 20af662
(restricted_fit block schema_version 2): the X-11 sigma-rule product
default is **legacy** — bit-exact against the pinned
`DefaultExtremeValuesCorrector` / JDemetra behavior — and **"accurate"**
stays selectable. The layering follows the agreed split: the core keeps
the policy explicit (raw `.native_*` entries unchanged, still refusing a
missing policy), rkalos applies the default at its interface, and Persephone
mirrors the split at its spec layer.

**Field migration.** `restricted_fit$sigma_policy_required = TRUE` no longer
exists; the gate and scope checks read `restricted_fit$sigma_policy_default`,
verified in `identical(., "legacy")` form by `backend_native_gate()` (a build
stating a different default is refused naming `sigma_policy_default` — the
Persephone native spec never guesses a sigma default). `sigma_policy = NULL`
on `persephone_native_spec` now means NOT STATED and receives the qualified
default when the request is built; an explicitly stated `"legacy"`/`"accurate"`
always wins; any other stated value is refused naming `sigma_policy_default`
(the former `sigma_policy_required` violation path is gone; the preserved
"does not yet implement … No Java fallback" framing is unchanged for all
remaining violations).

**Provenance design.** The envelope keeps the core echoes verbatim
(`sigma`, `sigma_set`) and adds one compound string,
`provenance$sigma_policy`: `"legacy (default)"` when the policy was omitted,
`"legacy (explicit)"` / `"accurate (explicit)"` when stated. The
default/explicit distinction is Persephone-side knowledge (the interface has
already applied the default before the core sees it), so the compound string —
not the core echo — is the honest record; the print method shows it.

**Request fingerprint.** The fingerprint hashes the EFFECTIVE policy, so an
omitted and an explicit-"legacy" request produce bit-identical results and
identical fingerprints (asserted), while `"accurate"` changes the fingerprint
(scope-visible change).

**Parity (default path end-to-end).** The dual-lane fixed specification now
OMITS the sigma policy. Measured 2026-11-17 on rkalos 20af662 (core pin
unchanged, 2b93ab0): rjd3-vs-native final SA/trend/irregular max diffs all
exactly **0.000e+00** — the default maps to the same pinned rule, so the
M3-finale bit-exactness persisted through the migration; the pinned native
session-stability baseline metrics (last-6/mean/sd/sum) are unchanged.
An explicit `"accurate"` smoke check was added: it runs through the product
path, the policy is echoed (`sigma = "accurate"`, sigma-stage records carry
`policy = "accurate"`, honestly distinct from the legacy run's echoes), the
provenance records `"accurate (explicit)"`, and the finals coincide with
legacy on this series (no pinned values; the rkalos fixtures' documented
legacy/accurate divergence is at the sigma-stage weight/state level, and
none of the pinned fit fixtures diverges in the finals). The former M3
missing-sigma refusal test became this default-applied proof;
`tools/verify-nojava.R` now runs the restricted fit with an omitted policy
(asserting `"legacy (default)"`) and refuses a stated-invalid policy.

**Suite state:** 21 files, **1,476 assertions (1,330 baseline + 146 in the
parity harness), 0 failures/errors/skips** on the qualified JVM 21 stack
(parity file 129 → 146, net +17). JVM-free lane: parity file 70/70 with no
Java namespace loaded; `tools/verify-nojava.R` PASS without Java.

## Linux M4 — automatic native X-13 fit (rsa3 decode table) — 2026-11-21

**Pins.** rkalos Remotes moved `83016bd` → `00c3a88` (bundled Kalos core
`f500162`); the scratch lane `tmp_opencode/rlib-m4` built rkalos from a
`git archive` snapshot with `KALOS_OPENBLAS_ROOT` (static `libopenblas.a`,
`.so` self-bound: zero dynamic BLAS/LAPACK references). Capabilities at the
new pin: schema 2, `native_x13_fit_automatic = TRUE`, automatic sigma default
`"legacy"`. BEFORE any product change the existing suites were reconciled at
the new pin: JVM lane 1,476 assertions / 0 failures; JVM-free parity 70/70 —
exact baseline reproduction, no pin drift.

**Product path.** `perX13(ts, "rsa3", backend = "native")$run()` is the
automatic lane: the specification is the PINNED JDemetra+ rsa3 decode table
(measured from the pinned rjd3 3.9.0 template-spec dump, captured 2026-11-20;
transform AUTO aiccDiff -2; automodel grid with ub1/ub2 as reciprocals of the
JVM values 1.0309278350515465/1.1363636363636365; outliers AO/LS/TC full span
maxiter 30 tcrate 0.7; AMI 1e-7/1e-5/defva 0/predcv 0.14286/ljbox 0.95; mean
tests 1.96/1.0, tsig 1; X-11 msr-pinned, automatic Henderson, zero horizons,
msr_policy "jdplus", neutral mode request). The ONLY adjustable field is the
sigma policy (NULL = default `"legacy"`; `"accurate"` selectable; invalid
values refused at run() naming `sigma_policy_default`); every other
`speclist` field — horizons, transform, arima, x11, sigmas, mode — is refused
at construction naming `automatic_spec_pinned`, because the module state is
decode data, not user data. Result envelope `persephone_native_automatic_fit`
(honest sibling of the restricted one: t/sa/i available, everything else
`not_provided`, forecasts NULL, `sigma_provenance` echo, decision chain
carried verbatim). The request fingerprint hashes the full decode state plus
the effective sigma policy; omitted and explicit-`"legacy"` requests are
fingerprint-identical (restricted-lane precedent), `"accurate"` differs.
Mode semantics: the request value `"additive"` is the neutral JVM-UNKNOWN
request; `mode_effective` (`"multiplicative"` on log=TRUE series) is
`X13Kernel.forceMode`'s log-driven derivation, mirrored verbatim by the core
(`x13_automatic.cpp` forceMode copy).

**Template envelope (measured, contract surprise).** Of the six rsa
templates, exactly ONE fits the qualified automatic envelope: rsa3. rsa0
(transform Level + automodel disabled), rsa1 (automodel disabled) and rsa2c
(automodel disabled + TD2/leap-year REMOVE + Easter ADD) plus rsa4/rsa5c
(TD2/TD7 + Easter ADD — the jar dump shows Easter is ADD on BOTH, not only
TD) refuse at specification construction, naming the measured template facts;
the 21-I deferrals quote the pinned core status
`unsupported:x13 automatic fit defers calendar/easter/TD/user detection (21-I)`
verbatim. This is narrower than the aspirational plan row "rsa0–rsa3 fully
supported": the automatic pipeline IS the automatic-modeling route
(`isAutoModelling()==false` is a dead branch in the contract), so the
automodel-disabled templates were never inside the envelope.

**Fail-closed matrix (asserted, verbatim).** Construction: rsa0/rsa1/rsa2c/
rsa4/rsa5c named refusals; perTramo names `native_x13_fit_automatic`;
rsa3@freq-4 names `monthly_only`; speclist mutations name
`automatic_spec_pinned`. Run: invalid sigma names `sigma_policy_default`;
NA series echoes `unsupported:x13 automatic fit requires a complete finite
series` with fingerprint and no-Java-fallback framing; contexts and
user-defined outputs refuse as rjd3 features.

**Parity evidence (pinned, byte-stable).** Fixtures
`inst/tinytest/fixtures/native/parity-m4/`: `jvm-rsa3-{air,ao,seas}.txt` —
raw `x13_fast(rsa3 + x11.fcasts=0/bcasts=0, userdefined=c("sa","t","i"))` on
AirPassengers and the rkalos oracle 'ao'/'seas' inputs, EACH captured in two
independent JVM processes with byte-identical results (`cmp` clean, series
counted as estimation-touching: double-captured per doctrine), plus the two
fixture input series. The native lane (JVM-free) reproduces all 13 decision
lines BIT-identically on all three series (log, d/bd, p/q/bp/bq, mean,
variable set, neffective, nparams, final Henderson 9/13/13, final seasonal
s3x3/s3x15/s3x9-broadcast). Tier families at the contract bands with the
measured worsts (pinned in test output, never widened): landing coefficients
4.1e-4 vs 1e-2; loglik 2.0e-9 / AICC 2.0e-8 vs 2e-3; sa (d11final) 1.7e-7 vs
1e-6; t (d12final) 8.2e-8 vs 1e-5; irregular (d13 class) 1.97e-4 vs the
contract's own measured d13 class ~2e-4 (G2 landing-basin drift, documented
in the contract's horizon envelope; deterministic across processes — the
seas worst sits inside the contract class without any band adjustment).
AirPassengers is bit-exact (0.0) on every family including d13. The JVM-lane
section re-verifies the LIVE stack against the pinned captures (decisions
byte-identical) before comparing tiers.

**CI (pinned provider).** `native-foundations.yaml` and `check-full.yaml`
build the OpenBLAS provider from the kalos lockfile pin (0.3.34) via
`tools/build-openblas.py` fetched from `statistikat/kalos@f500162a…` (the
bundled core commit) and export `KALOS_OPENBLAS_ROOT` before the rkalos
source build; Windows keeps the Rtools-supplied OpenBLAS that rkalos'
configure detects. `checkout@v5` bumps preserved.

**Suite state.** JVM lane: 21 files, **1,685 assertions, 0 failures**
(parity 146 → 354, backend 88 → 89, all other files byte-equal to the 21H
baseline rows). JVM-free lane: parity file **197/197** with no Java namespace
loaded; `tools/verify-nojava.R --native` PASS including the automatic lane
(final SA pinned bit-exact). NOTE (host, not package): two long whole-suite
JVM sessions were SIGKILLed mid-run by the shared host; the suite was
re-run per-file (own Rscript per file) and completed green — recorded for
honesty, not a Persephone defect. The suite runner's tally now uses the CI
`all(as.logical(res))` semantics after an earlier broken aggregation silently
reported zeros (caught by the parity file's own violation report, fixed,
both lanes re-run clean).

## Linux M4-gap — automodel-OFF templates vs the restricted route (rsa0/rsa1/rsa2c) — 2026-11-21

**Pin advance (honest order).** rkalos moved `00c3a88` → `b90d367` (the
qualified restricted route is at `10afd3e` and untouched since; the b90d367
delta is the automatic_fit horizon qualification). rlib-m4 rebuilt from a
`git archive` snapshot (core.lock.json unchanged: core `f500162a…`),
`KALOS_OPENBLAS_ROOT` provider, `.so` with zero dynamic BLAS/LAPACK
references. BEFORE fixtures/product work the first no-Java parity run at the
new pin exposed exactly ONE pre-existing assertion block pinned to the old
automatic_fit schema (schema 2L / `x11$zero_horizon_only`): re-baselined to
schema 3L / `horizon_envelope` with a recorded reason (the rkalos b90d367
surface change itself); the restricted `zero_horizon_only = TRUE` assertion
was KEPT. No other pre-existing assertion moved.

**Decode (LIVE, double-JVM byte-identical, doctrine-grade).** The six
templates were re-captured from the pinned jars through
`X13Spec.fromString -> X13.toBuffer -> Spec.parseFrom().toString()` (proto
TextFormat — `toProtoString` does not exist on X13Spec) plus the fully
expanded rjd3 `x13_spec` walk (probe sha256 `d3525549f06d…`), and the
PRODUCT-lane demand runs `x13_fast(series, x13_spec(template))` with template
defaults intact on the three parity series (probe sha256 `77ea273a6a32…`).
Two independent JVM processes per capture; 12/12 decode files and 9/9 demand
files byte-identical (`cmp` clean); fixtures
`inst/tinytest/fixtures/native/refusals-m4gap/` (21 files). rsa3 continuity
anchor: this session's proto dump matches the pinned M4 rsa3 decode table.
Measured facts: rsa0 transform LEVEL (the rsa1-vs-rsa0 proto contrast is the
proof: `transformation: FN_AUTO` vs silent default), automodel FALSE, outlier
types EMPTY (rsa0 detects nothing — measured even on the planted-AO series);
rsa1/rsa2c FN_AUTO + AO/LS/TC detection; rsa2c TD2/TEST_REMOVE + Easter
STANDARD/TEST_ADD; all three: theta/btheta PARAMETER_UNDEFINED (ESTIMATED —
landing bits measured), SEASONAL_FILTER_MSR, automatic Henderson 0, nfcasts
-1 = 12 forecast months (measured `nforecast=12` in all nine demand runs).
Surprise recorded: TEST_REMOVE drops the TD2 columns but the LEAP-YEAR
variable stays and is ESTIMATED (with ESTIMATED easter).

**Verdict: 0 of 3 native-qualified — evidence-backed refusals on BOTH routes.**
The restricted fixed-coefficient envelope refuses each template's own request
verbatim: seasonal msr → `unsupported:x13 fit fixed seasonal requests only`;
Henderson 0 → `unsupported:x13 fit fixed Henderson trend length`; forecast
12 (or -1) → `unsupported:x13 fit requires zero horizons`; estimated
coefficients have NO carrier (non-finite carrier throws `x13 preadjust
coefficients are finite`) — all four measured against the live installed core
(f500162), asserted in the JVM-free lane, quoted verbatim in the refusals.
The rsa3 horizon-forcing precedent does NOT rescue them: their decisions
(log, outlier picks, filters — measured flipping across the three series)
depend on estimation the restricted route never performs. No fixed-spec
mapping is pinnable without overfitting series-dependent run outputs
(final Henderson 9 vs 13, S3X3/S3X5/S3X9) — pinning those would be mapping
fudge, so none was attempted; no silent adaptation, no new rkalos call sites.
The "rsa0–rsa3 fully supported" clause is now CLOSED by evidence: rsa3 native
(M4); rsa0/rsa1/rsa2c natively impossible on both qualified routes, refused
by name with the measured facts; rsa4/rsa5c refusals untouched (21-I verbatim).

**Product change.** `backend_native.R`: the three refusals now frame BOTH
envelopes ("outside the qualified native envelopes (automatic and
restricted)"), name the restricted scope fields
(`fixed_coefficients_only`, `fixed_seasonal_requests_only`,
`fixed_trend_length_only`, `zero_horizon_only`) and quote the pinned core
statuses; header carries the probe hashes. `tools/verify-nojava.R` gained the
rsa0 restricted-fact assertions plus one LIVE-core msr-status check. No
export surface change (NAMESPACE/man untouched).

**Parity.** No native lane exists for the refused names, so there is nothing
to compare against rjd3 finals; the JVM lane carries the substitute contract:
the live stack must reproduce the pinned decode captures (proto + spec walk,
all six templates) and the pinned demand captures (three refused templates ×
three series) BYTE-IDENTICALLY — 21 stack-pin assertions; the JVM-free lane
asserts the refusal texts and the live-core aspect matrix (45 assertions).

**Suite state.** JVM lane (per-file, own Rscript each): 21 files, **1,753
assertions, 0 failures** = the M4 baseline 1,685 with the parity file 354 →
422 (+68: 45 M4-gap matrix + 2 schema-3 re-baseline + 21 stack-pins) and every
other file's source unchanged in git; backend.R 89 as recorded at M4, totals
reconcile exactly. JVM-free lane: parity file **244/244** (was 197; +45 +2);
`tools/verify-nojava.R --native` PASS including the new restricted-fact and
LIVE-core msr-status checks. CI untouched (M4's pinned-provider wiring already
covers rkalos). Friction: the user library shadows rlib-m4 unless `R_LIBS`
(first priority) is set — `R_LIBS_SITE` sorts AFTER `R_LIBS_USER`;
README.Rmd lacks the M4-era native paragraph that README.md carries
(pre-existing drift, left untouched).

## Linux M5 — core-pin advance f500162 -> 0e064dd, CI-leg forensics, requalification — 2026-11-23

**Pin advance (mirrors here; rkalos advance owned by the parallel batch).**
rkalos main moves locally `b90d367` -> `49e2b117033d522f9c3ac38e9d7109279ac3fc97`
("Advance core pin to 0e064dd; CI-FDLIBM/CI-FIX-2 pinned transcendental surface
vendored"): core `f500162a3be15…` -> `0e064dd18eb45568fc35e1bed337161598ace36f`
(67 kalos commits: CI-FDLIBM transcendental routing with the
PinnedLevenbergMarquardt landing machine, CI-FIX-2 vendored glibc-hostlineage
kernels, items 23-A..F TRAMO machines including the new `kalos::tramo_fit`,
21-I.1 calendar generation, 24-C WK extraction). **NOT YET PUSHED:** `49e2b11`
is a LOCAL commit on rkalos main; every CI leg that installs rkalos via
DESCRIPTION `Remotes:` (native-foundations `local::.` AND check-full
`install_deps`) stays RED until the user pushes rkalos main, and this branch
must be pushed for native-foundations to run at all. Remotes mirrors the SHA
with a matching in-file warning; the provider checkouts in
`native-foundations.yaml`/`check-full.yaml` bump `ref:` to the bundled core
(`build-openblas.py` + `dependencies/openblas.lock.json` are byte-identical
between `f500162` and `0e064dd`, OpenBLAS stays 0.3.34 -- doctrine sync, zero
functional drift). Historical provenance comments naming f500162
(`R/backend_native.R` header, `test-parity.R`) stay: they record WHERE the
measured facts came from, and the JVM-free lane re-reproduces them at the new
core (below).

**tramo_fit surface exists, unconsumed.** The vendored core now carries
`kalos::tramo_fit` (`src/tramo_fit.cpp`, `include/kalos/tramo_fit.hpp` plus 43
TRAMO/SEATS/calendar units), all OUTSIDE the R package's compiled closure --
uncompiled, unbound, no R entry point, no `backend_native.R` routing. Consumer
routing is a later slice (24-D per the kalos roadmap).

**Snapshot-identity cross-check (why this requalification transfers).** The
qualified build was produced INDEPENDENTLY in SCRATCH (git-archive of
rkalos@b90d367 + rkalos' own `tools/sync-core.py --revision 0e064dd`, Makevars
OBJECTS += `hostlineage.o`, `hostlineage_tables.o`) and diffs byte-identical
against the committed `49e2b11` tree in EVERY file except the two Makevars
templates, where only the placement of the same two OBJECTS lines differs
(OBJECTS has no link-order semantics; the sets are equal). Lock self-verified:
186 core / 112 fixtures / 331 evidence entries.

**Ubuntu-leg root cause (CI forensics; supersedes the stale-pin hypothesis).**
The failing step in native-foundations runs #9 (d8b335c) and #10 (bf996ff) is
`Rscript tools/verify-nojava.R --native` via the GitHub check annotation
`identical(as.numeric(obj$adjusted)[142:144], c(499.376731443704, …) is not
TRUE` -- the bit-exact final-SA triple of the automatic rsa3 fit (lines 91-92
of the script), NOT a status/string assertion. The pinned core f500162 already
answers every status the script quotes verbatim (`git grep` at the pinned
refs: the restricted `unsupported:x13 fit …` strings live in `src/x13_fit.cpp`
at f500162 and 0e064dd), rkalos@00c3a88 carries the same core, the script does
not branch on `automatic_fit$schema_version` (00c3a88's schema-2L surface
passes its reads), and the LOCAL reproduction at rkalos b90d367 (zero-horizon
bits byte-identical to 00c3a88 per the b90d367 message) PASSES
`--native` -- so this is a pre-CI-FIX-2 MACHINE-DEPENDENT Bfgs landing
(the core records exactly this "CI run-#17 signature, machine-independent"
basin flip), and the LM landing + vendored transcendental kernels delivered by
`0e064dd` are the fix. Ubuntu is expected to go GREEN once rkalos 49e2b11 is
pushed AND this branch is pushed; measured here, the bit-exact triple is
reproduced by the advanced core on the qualification host (verify-nojava PASS
below).

**Requalification -- JVM-free lane (exact replay of the recorded counts).**
Before (rkalos b90d367, core f500162, merged tree bf996ff) / after
(49e2b11-equivalent snapshot, core 0e064dd): parity file **244/244 ->
244/244** (0 failures, no Java namespace loaded); `tools/verify-nojava.R
--native` **PASS -> PASS** including the bit-exact automatic final triple
499.37673144370382/484.86271289531953/485.24840286656132 and the LIVE-core
restricted-status checks, provider-bound `rkalos.so` both lanes. Pinned tier
bands UNTOUCHED (no widening anywhere); measured worst rel-diffs before ->
after: air theta 2.42e-05 -> 1.9e-08, btheta 1.12e-04 -> 4.02e-08, ll/aicc
~1e-9 -> ~1e-12, finals stay 0.0; ao theta 4.11e-04 -> 6.84e-04 (band 1e-2),
btheta 3.77e-05 -> 6.58e-05, ud_sa 2.87e-08 -> 4.85e-08 (band 1e-6), ud_t
5.08e-08 -> 8.59e-08 (band 1e-5), ud_i 7.53e-08 -> 1.27e-07 (band 2e-4);
seas lands BIT-EXACT on every family (theta/btheta/phi/ll/aicc/finals 0.0,
ud_i 1.97e-04 -> 1.62e-15). All 13 decision lines stay BIT-identical on all
three series. No band widened; counts identical; the only movements are
inside-pinned-band re-measurements (the M4-recorded determinism note on the
seas d13-class worst is superseded by this: the LM machine reaches it).

**Requalification -- JVM lane (host-qualified: Temurin 21 + CRAN rjd3
3.9.1/3.9.0/3.9.0 + pinned jars, per-file own Rscript).** Before (bf996ff at
core f500162): **22 files, 1,766 assertions, 0 failures** -- the recorded
M4-gap 1,753 EXACTLY reproduced file-by-file (backend.R 89, parity 422, …)
plus the merge's upstream `test-outlier-cv.R` (13, first local run, green).
After (core 0e064dd): **22 files, 1,766 assertions, 0 failures** -- the
per-file tallies are IDENTICAL before/after (clean diff across all 22 files;
parity 422 including its live-JVM stack-pin sections). No host kills this
batch; both lanes sequential per isolation doctrine.

**Merged upstream surface (bf996ff port) -- offline smoke, both cores.**
An offline path EXISTS: the cached rjd3 result fixtures
`inst/tinytest/fixtures/backend/` drive 18 combinations (x13/tramoseats x
monthly/quarterly x automatic/fixed/manual) through `getOutliers()` (incl. the
ported `crit.value` rule: explicit spec value wins, else `calcOutlierCrit`
from the estimation domain), `getRegressors()` (incl. the `ftests` attribute
channel -- none of the 18 fixtures carries joint-F TD data, so the attribute
is absent, matching the merge's `attr <- NULL` semantics), the diagnostics
row (`nSigRecentout`/`nMstatOutside` columns present; the public
`diagnosticsTable()` wrapper needs the R6 `$iterate` and is JVM-lane-covered),
and `summary.persephone` rendering -- **PASS without Java and without rjd3 on
the library path, at both cores**; the jdplus threshold anchors of
`calcOutlierCrit` re-verified. The live-TD `ftests` probe (rsa2c on
AirPassengers, rjd3 lane, Temurin 21): **PASS on the advanced lane** --
`getRegressors()` carries a 1-row `ftests` attribute
(group/value/df1/df2/pvalue), `getOutliers()` reports `crit.value`
3.88983776394041 (the computed n=144 threshold, equal to the
`calcOutlierCrit` anchor), the public `diagnosticsTable()` R6 wrapper renders
both new columns and `summary()` prints its full 35-line report.

**macOS/Windows native-foundations legs -- dated verdict: platform-evidence
work, owned by M6 (posture: informational-until-M6, mirroring the kalos macOS
decision).** Both legs fail INSIDE `setup-r-dependencies` installing
`local::.` -- the rkalos SOURCE BUILD fails (macOS: brew toolchain + provider
build present and succeeds, pak build fails; Windows: no provider step runs at
all, `KALOS_OPENBLAS_ROOT` absent). Identical failures pre- and post-merge
(runs #9/#10), NOT merge-induced and outside this batch's cross-platform scope.
Per PACKAGE_FAMILY_AND_AGENT_PLAN the M6 row owns "Linux/macOS/Windows (incl.
standalone Windows) qualification" / "Windows/Linux CI green". Proposed
NON-GATING change (deliberately NOT committed, maintainer call): add
`continue-on-error: true` to the `integration` job of native-foundations.yaml
with a dated comment pointing at this section until M6 delivers.

**Friction (host, not product).** The site Rprofile hard-sets `.libPaths()` at
startup, so scratch libraries only enter NON-vanilla sessions via profiles;
`R CMD INSTALL`'s lazy-load child needs the scratch dep dir in `R_LIBS_SITE`
(`R_LIBS`/`R_LIBS_USER` are overridden); the shinycssloaders machine gap
persists and was carried into `rlib-m5-base`/`rlib-m5-adv` byte-identically
(cp of the rlib-m4 copy, 1.1.0). Build lanes:
`tmp_opencode/rlib-m5-base` (b90d367 + bf996ff) and `tmp_opencode/rlib-m5-adv`
(49e2b11-tree snapshot + bf996ff), logs `m5-*.log`.

## M5 application work on the published rkalos interface

The application now pins rkalos `e15af7f` (0.1.0.9002). Both restricted and automatic native X-13 fits are available. Native TRAMO-SEATS is waiting for the core assembly, its oracle and an exported R fit. Item 28 governs the fixed-structure templates. The product default remains rjd3.

### Single-series fitted charts (application items 14 and 26)

Implemented and checked on macOS ARM64 on 2026-11-23. The dygraph and Highchart use the observed input and the fitted native trend and seasonally adjusted components. Previously the dygraph showed an original-only preview after a native fit and the Highchart failed. A restricted fit and an automatic fit with a detected outlier passed **26/26** Java-free assertions on serialized values, calendar dates, markers, and the before-fit preview. The rjd3 plot suites passed **11/11** single-series and **36/36** hierarchy assertions on Temurin 21.0.11. The native-foundations workflow runs the new chart checks. [Chart regressions](inst/tinytest/test-native-charts.R)

### Seasonal, irregular and calendar effects charts (application items 14 and 26)

Implemented and checked on macOS ARM64 on 2026-11-23. Both chart engines display the effects and forecasts supplied by the fit. Both native routes expose irregular effects on the observed calendar. The Java-free chart suite passed **36/36**, including exact widget values for both fits. The reference plot suite passed **16/16**, including the case where only the seasonal effect has a forecast. [Effects chart](R/plotSeasIrrCal.R) · [Reference plot tests](inst/tinytest/test-plot.R)

### Single-series dashboard and spectra (application items 14 and 26)

Implemented and checked on macOS ARM64 on 2026-11-23. The single-series dashboard shows the native decomposition effects, adjusted spectrum and original spectrum. The residual panel identifies unavailable diagnostics. Previously the dashboard embedded plot errors for those panels. The interactive and static spectrum functions share the same component selection. The Java-free chart suite passed **68/68**, including both rendered dashboards and exact comparisons with independently computed periodograms for observed, adjusted and irregular series. The reference plot and diagnostic suites passed **16/16** and **180/180**. [Single-series dashboard](R/dashboard_single.R) · [Java-free chart tests](inst/tinytest/test-native-charts.R)

### Reference-backend reinitialization (application item 14)

The package check exposed an outdated expectation that constructing the default native X-13 model fails. The isolated process now checks the successful `rsa3` default and the refusal for `rsa0`, then exercises both rjd3 methods through descriptor-pool detach and recovery. With rjd3toolkit 3.9.1 and Temurin 21.0.11, **15/15** assertions passed. [Reinitialization test](inst/tinytest/test-backend-reinitialization.R)

### Summary method compatibility (application item 14)

The package check flagged the summary method's argument name. The method and its documentation now use the S3 generic's `object` argument. On installed native and rjd3 fits, `summary(object = fit)` printed the report and returned the original fit invisibly. [Summary method](R/summary.R)

### Package verification for this M5 application checkpoint

On macOS ARM64, R 4.5.2 built the source archive and five vignettes. The full `R CMD check` used the published rkalos pin, rjd3toolkit 3.9.1 and Temurin 21.0.11. It passed the examples, S3 methods, vignette rebuild and PDF manual checks. **1,867/1,875** tinytest assertions passed. The sole check error comprises eight previously recorded byte comparisons between direct `rjd3x13::x13_fast()` output and the frozen `rsa0`, `rsa1` and `rsa2c` demand files on this architecture. All eight failures are in `test-parity.R`. Platform parity remains open. The localhost Shiny suite passed **43/43**. The Java-free chart suite passed **68/68** on the isolated package library. [Parity harness](inst/tinytest/test-parity.R) · [Earlier reproduction](adaption_in_the_implemetation_needed.md)

### X-13 reference replay on ARM64 and x86_64

The eight failing demand-file comparisons above came from HotSpot math intrinsics in the pinned Temurin 21.0.11 JVM. On the same Mac with the same 13 jars, the x86_64 JVM reproduced all nine historical captures. The ARM64 JVM and a second x86_64 run with the six intrinsics disabled agreed byte-for-byte on all nine full records. Only the estimated MA coefficients and, in three cases, regression coefficients varied between the profiles. The parity suite chooses the reference from the JVM architecture, checks the complete record and its SHA-256, and retains all twelve byte comparisons for the decoded specifications. The installed ARM64 replay passed **30/30** assertions. [Kalos reference and replay tools](https://github.com/statistikat/kalos/tree/1df61bffd7462f746895aa767465d17fc25c3316/validation/evidence/native/x13-refusal-jvm-profiles) · [Parity suite](inst/tinytest/test-parity.R)

### Platform qualification for the Windows provider

In published native-foundations run 45261254113, Ubuntu and macOS passed. Windows failed during the rkalos source install because the compiler could not find the OpenBLAS C API. Both application workflows now invoke the same provider setup. It builds the checksum-locked LP64 library with UCRT64 on Windows, verifies the static archive and LAPACKE header, and sets the provider root before installing rkalos. GitHub Actions execution on the updated workflow remains a platform gate. [Windows failure](https://github.com/statistikat/persephone3/actions/runs/45261254113/job/137996279512) · [Provider setup](.github/actions/setup-kalos-provider/action.yml)

### Automatic X-13 forecast delivery (application items 10 and 26)

The rsa3 route accepts forecast and backcast months or negative whole years, with zero extensions by default for existing requests. Fit provenance records the requested fingerprint and effective horizons. Public point forecasts use the dated A1a and D11a/D12a/D16a/D18a tables. The irregular forecast and standard errors carry explicit unavailability. The calendar-effect series covers the native forecast window. The rjd3 `cal_f` series has its own domain.

The installed application passed **48/48** assertions without Java and **58/58** with the live rjd3 reference on macOS ARM64. The five AirPassengers point series matched their corresponding reference periods within the existing application tolerance of 1e-8, with a maximum absolute difference of 4.21e-12. All **180/180** existing forecast assertions passed. The local rkalos dependency commit must be published before this revision can install from GitHub. [Forecast tests](inst/tinytest/test-native-forecasts.R) · [R dependency](DESCRIPTION)

The installed-package CI smoke test now fits the default zero-horizon request, updates the same object to six forecast months, and verifies the dated A1a values in the public result. It passed with the Java-related R packages absent and the JVM disabled on macOS ARM64. [Java-free smoke test](tools/verify-nojava.R)

### Native forecast presentation for single series and hierarchies

Both single-series widgets and the aggregate dygraph plot the dated native forecast points. Confidence intervals use the periods for which the backend supplies standard errors. The indirect hierarchy returns level forecasts from its fitted children and records that their cross-covariance is unavailable. The Java-free forecast suite passed **56/56**, including point values read back from the widgets and a two-child aggregate. With rjd3 present the same file passed **66/66**. All **36/36** reference-backed hierarchy chart assertions also passed. [Single-series chart](R/plot_single.R) · [Hierarchy chart](R/plot_hierarchical.R) · [Forecast regressions](inst/tinytest/test-native-forecasts.R)

### Native batch and hierarchy dashboards (items 14 and 26)

The batch constructor and raw-series coercion now accept the restricted native X-13 template. Previously both checked it against the rjd3 template list before it reached the native constructor. On macOS ARM64 the installed package passed **19/19** Java-free assertions covering an existing object beside a raw series, cloned child state, refusal of unsupported templates, aggregate rendering, and the HTTP startup and shutdown of both Shiny dashboards. The reference batch and hierarchy-value suites passed **10/10** and **79/79**. [Dashboard regression](inst/tinytest/test-native-dashboard.R)

### Zero and short X-13 forecast horizons in the reference backend

The reference X-13 product route now handles explicit zero and short forecast horizons. At zero months, `perX13(..., backend = "rjd3")$run()` succeeds and returns `NULL` forecasts. At three months it retains the three matching original-series standard errors for plotting. The twelve-month preprocessing series remains accessible through `$output`. The fixed M3 reference fit now runs through both public backends. Its trend, adjusted and irregular series matched direct `x13_fast` exactly. The installed package passed **195/195** forecast, **16/16** chart and **93/93** backend assertions and all four fixed-fit product comparisons. [Forecast contract](R/forecasts.R) · [Live regression tests](inst/tinytest/test-forecasts.R)

### Native TRAMO-SEATS RSA0 product fit

`perTramo(series, "rsa0", backend = "native")$run()` now estimates and decomposes complete monthly series through rkalos 0.1.0.9004. The product returns dated observed, trend, seasonally adjusted, seasonal and irregular series, with 24 forecast months by default. Both direct and indirect hierarchy results are available. The summary and diagnostics table report the fitted airline model, and the regressors table carries the estimated mean.

Across six frozen public JVM cases, all **3,960** fitted and forecast point values matched bit-for-bit through the R6 product path. It passed **160/160** Java-free assertions and **190/190** with the live reference. All five AirPassengers conditional SEATS forecast standard errors matched the JVM stochastic decomposition at every month. Its conditional series uncertainty differs from the preprocessing model's `y_ef` (maximum absolute difference 1.994175). The dedicated SEATS field exposes the conditional values. The public original-series model uncertainty remains unavailable at this checkpoint. RSA3, full automatic TRAMO and models requiring SEATS approximation remain open. [Product regression](inst/tinytest/test-native-tramoseats.R) · [Pinned binding](https://github.com/statistikat/rkalos/commit/e1f99993d91646be69a58f5bc0cfa7a9093a1bd5)

The preceding JVM-profile application source archive passed a full macOS ARM64 `R CMD check` with status **OK**, including **1,948/1,948** assertions, five vignettes and the PDF manual. This completed the earlier eight-failure reference replay. A separate package check covers the RSA0 changes. [Cross-architecture parity regression](inst/tinytest/test-parity.R)

### Original-series RSA0 forecast uncertainty

With rkalos 0.1.0.9005 the public RSA0 forecast and its chart confidence interval use the TRAMO preprocessing model's standard errors. The conditional SEATS errors remain available for all five decomposition series. The installed package passed **175/175** assertions without Java and **207/207** with rjd3. All six corpus fits exposed the dated original-series uncertainty. The 24-month AirPassengers model errors matched the JVM's explicit `y_ef(24)` reference within the core tolerance, and both chart bounds matched the public model errors exactly. [Forecast qualification](inst/tinytest/test-native-tramoseats.R) · [Pinned binding](https://github.com/statistikat/rkalos/commit/e92f07604d2e95a678e479c784f28a50cc6c8d7a)

### Native EUROSTAT quality reports (item 26)

Native X-13 reports now show the final Henderson length and seasonal filters selected by the D pass, including mixed per-month filters. The qualified RSA3 and TRAMO RSA0 reports show zero trading-day regressors and identify the absent leap-year and moving-holiday terms. Fixed X-13 designs preserve unknown calendar subtypes. Maximum relative adjustment now accepts matching monthly or quarterly calendars when R stores their endpoints one floating-point unit apart.

The reporting suite passed **46/46** assertions without Java and **58/58** with rjd3. For AirPassengers, all six compared quality-report columns agreed between backends for both RSA3 and RSA0. The reference diagnostics suite passed **180/180**. [Report implementation](R/generate_Qr_List.R) · [Product checks](inst/tinytest/test-native-reporting.R)

### Observed X-13 components in the public result

Both native X-13 routes now return the observed A1 series and the seasonal D16 series, including restored deterministic seasonal effects. Calendar-free designs also expose the neutral calendar effect. The restricted route leaves the public calendar field unavailable for manually classified calendar regressors because their allocation in D18 can differ from the reference preprocessing calendar effect. The raw D18 table remains accessible.

The installed package passed **83/83** chart checks with and without Java, including a seasonal regressor and a user-assigned calendar effect. The forecast suite passed **56/56** without Java and **72/72** with the JVM. All three observed fields matched the reference on the nonzero-horizon RSA3 fit within 1e-8 and exactly on the fixed additive fit. [Component normalization](R/backend_native.R) · [Chart regressions](inst/tinytest/test-native-charts.R) · [Fixed-model reference](inst/tinytest/test-parity.R)

### Native seasonal–irregular plots (item 26)

The public SI-ratio plot and single-series dashboard now show the fitted X-13 D8, D9 and D10 tables on their full calendar. The log-additive path restores D8 and D9 to ratio scale and uses the fitted, bias-corrected D10. RSA0 uses the final SEATS seasonal and irregular components on the observed calendar. Both plotting engines share these series and phase means.

The installed package passed **89/89** Java-free SI-ratio assertions and **135/135** with the live JVM, including the D9 missing positions, four fixed decomposition modes, RSA3 with three backcasts and six forecasts, and RSA0. The maximum absolute X-13 difference across the compared X-13 chart series was 2.56e-15 or less. RSA0 matched exactly. The Java-free dashboard and chart suite passed **83/83** and the existing reference plotting suite passed **16/16**. [Plot inputs](R/plotSiRatios.R) · [Chart and reference checks](inst/tinytest/test-native-si-ratios.R)

The earlier RSA0 application archive at rkalos 0.1.0.9004 completed its full macOS ARM64 package check with **Status OK** and **2,177/2,177** assertions, including vignette rebuilds and the PDF manual.

### Preprocessing calendar effects on both native routes

Fixed X-13 now combines the A6 trading-day and A7 moving-holiday effects on the fitted scale. This keeps user regressors assigned to the calendar component in D18 and out of the public preprocessing calendar effect. The six fixed weekday coefficients and an Easter effect matched the live reference to 1e-12 on both additive and multiplicative scales. The additive RSA0 calendar effect is zero on every observed and requested forecast month, including the explicit 24-month reference horizon.

Chart and RSA0 regressions passed **84/84** and **187/187** without Java and **90/90** and **222/222** with the pinned JVM. The preceding observed-components change also passed the full **439/439** parity suite. [X-13 chart regressions](inst/tinytest/test-native-charts.R) · [SEATS calendar regressions](inst/tinytest/test-native-tramoseats.R)

### Native RSA0 preprocessing residuals in the dashboard

The product exposes the TRAMO residual time series and its residual standard error through `$result$diagnostics$residuals`. All six residual plots, the residual spectrum and the single-series dashboard use the same fields. The AirPassengers residuals cover February 1950 through December 1960. All 131 values, the scalar standard error and the five static plot data sets matched the live JVM reference exactly.

The installed application passed **20/20** residual assertions without Java and **28/28** with rjd3. Adjacent RSA0 tests passed **187/187** and **222/222** respectively. The existing reference plot suite passed **16/16**. The pinned R binding separately qualified all 570 dated residuals and six standard errors across the public corpus. [Product regression](inst/tinytest/test-native-residuals.R) · [Pinned residual binding](https://github.com/statistikat/rkalos/commit/9e87b84f737b14cb4830e971098cc17a8350ed04)

### Native RSA0 residual independence in dashboards and reports

The application now requires rkalos `0.1.0.9007` at `472258e`.
Native RSA0 diagnostics read the public Ljung–Box statistic and
upper-tail probability from `$output$residuals$lb`. The dashboard and
diagnostics table apply the existing independence classification. The
Eurostat quality table includes the unrounded `Ljung.Box.p.value`.
For AirPassengers the statistic is 40.99196917295244, the p-value is
0.0082592271291273, and the Independence card displays **0.0083 / Bad**.
A 37-observation fit yields an undefined test and a missing report value.

The new product tests passed **20/20** in the isolated Java-free library
and **24/24** with live JDemetra+ 3.9.0, including a bit-for-bit
statistic and probability comparison. Adjacent residual and diagnostics
tests passed **20/20** and **164/164** without Java, and **28/28** and
**181/181** with Java. At the preceding application checkpoint
`29fb9d3`, source-package checking finished **OK**, including all
**2,452** test results, examples, rebuilt vignettes and the PDF manual.
[Product regression](inst/tinytest/test-native-residual-independence.R) ·
[Pinned R diagnostic](https://github.com/statistikat/rkalos/commit/472258e792e083af0b81b55939ad066ebb1af55b)

## M5 monthly RSA1 with additive AO and LS effects (2026-11-24)

The application now pins rkalos `0.1.0.9012` at `468c3e8`. Two
additional public series select level adjustment and contain one AO and
an LS/AO pair. The native application restored all 1,200 observed and
forecast component values exactly and exposed the detected locations
and coefficients in its diagnostics. The eight-case product suite passed
**375/375** without Java and **575/575** with live JDemetra. Adjacent
RSA0 and SI suites passed **276/276** and **357/357** respectively.

The preceding `104f952` RSA1 application archive completed its full
source check with **Status OK**, **2,959/2,959** assertions, rebuilt
vignettes and PDF manual.
[Product qualification](inst/tinytest/test-native-tramoseats-rsa1.R)

## M5 monthly RSA1 application fits (2026-11-24)

The application pins rkalos `0.1.0.9011` at `7026c3b`. A complete monthly
series can now run through `perTramo(series, "rsa1", backend = "native")`.
TR1 selects the transformation and estimates the airline model, mean and
automatic outliers. The result exposes residuals and merged product
components on their calendars. The fitted mode determines the calendar
effect (zero or one) and forecast scales. Reports retain estimated AO/LS
coefficients and positions. SI plots use the WK components before
restoring outlier effects. The original-series RSA1 forecast standard
error is marked unavailable.

The six public inputs matched **60 component rows and 3,960 values
bit-for-bit**. The product tests passed **280/280** with Java excluded
and **430/430** with the live JDemetra reference. A three-backcast,
six-forecast mixed-outlier rerun preserved calendar alignment. The
adjacent RSA0, SI, fix-model, reporting and backend suites passed
**379/379** and **525/525** respectively.

The `bc0f285` independence and `e18231a` normality archives both
completed source checks with **Status OK**, rebuilt vignettes and PDF
manuals. Their full test suite counts were **2,477/2,477** and
**2,529/2,529**.
[Product reference replay](inst/tinytest/test-native-tramoseats-rsa1.R) ·
[Pinned R dependency](https://github.com/statistikat/rkalos/commit/7026c3b6fa70ea08a5b877933f58f2e6df8784c5)

## M5 — monthly RSA0 fits that need SEATS approximation

The application now pins rkalos 0.1.0.9008 at `b6fb5f9`. A 156-month
airline series that raised an approximation error with 0.1.0.9007 now
runs through `perTramo(..., "rsa0", backend = "native")`. For the observed
series and 24-month forecast, all six component grids and their 1,080 values
match JDemetra exactly. The calendar forecast comparison requests the same
24-month horizon from JDemetra. Automatic RSA3 and Full remain open.

On macOS the new regression passed 5/5 checks in a Java-free installation
and 29/29 with JDemetra. The adjacent RSA0 and independence tests passed
207/207 Java-free and 246/246 with JDemetra.
[Pinned R implementation](https://github.com/statistikat/rkalos/commit/b6fb5f9efd10aa48825988c984503744bb73661e)


### Native RSA0 residual normality in dashboards and reports

The product pins rkalos `0.1.0.9009` at `c8265a1`. For native RSA0
fits the diagnostics table and dashboard use the public Doornik–Hansen
statistic and upper-tail probability. The Eurostat report exposes the
unrounded value in `Doornik.Hansen.p.value`. AirPassengers matched the
live JVM statistic (7.770552141622452) and probability
(0.020542156888630703) exactly. The dashboard displayed **0.0205 /
Uncertain**. A 37-observation fit produced a defined normality test
alongside an undefined Ljung–Box test.

Product normality checks passed **18/18** without Java and **22/22**
with JDemetra. Adjacent diagnostic, independence and reporting checks
passed **231/231** without Java and **264/264** with JDemetra.
[Product regression](inst/tinytest/test-native-residual-normality.R) ·
[Pinned R binding](https://github.com/statistikat/rkalos/commit/c8265a17a35ae77907904844497017c6f51fc350)

### RSA3 observed calendar with nonzero horizons

The fixed X-13 calendar allocation also matched RSA3 specifications through their common R class. On an extended fit that selected A6 and A7 from the extended calendar. The product now applies the fixed allocation only to the restricted template and uses the observed D18 table for RSA3. With three backcasts and six forecasts, all 144 observed calendar effects align with the original series and the six forecast effects keep their future dates. The forecast suite passed **58/58** assertions without Java and **74/74** with the reference. [Forecast domains](inst/tinytest/test-native-forecasts.R)

### Fixing model orders and outliers on native fits

Calling `fixModel()` on native RSA0 now returns the existing airline specification, whose orders are already fixed. Requesting fixed orders on automatic RSA3 identifies the automatic-selection constraint. The restricted X-13 fit reports that it has no estimated ARIMA orders. Previously `fixOutlier()` silently ignored attempts to freeze RSA3 outliers. It now reports the unsupported change before mutating any selected model. Fixed native specifications and requests protecting the entire series keep their existing fit state.

The installed package passed **16/16** Java-free assertions, including a fitted series with a detected outlier, and **192/192** existing reference checks for both methods, hierarchies and batches. [Native model-update checks](inst/tinytest/test-native-fix-model.R) · [Reference outlier regressions](inst/tinytest/test-fix-outliers.R)

## Next application work

Continue item 14 for difficult models, selection boundaries and supported platforms. Item 26 also covers dashboards, quality reports and forecasts. Qualify further TRAMO templates as their core routes become available. Item 27 requires the product-parity evidence and a separate default-backend review.

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

## Monthly transitory changes in native RSA1 (2026-11-24)

Persephone now requires rkalos `0.1.0.9013` at `3ea923a`. The
automatic RSA1 route restores TC effects with the stock monthly decay
of 0.7 on the level and log scales. The twelve-case product test
checks all 120 JVM component rows and 7,560 values bit-for-bit,
including four transitory-change fits and their estimated outlier
names, dates and coefficients. It passed **598/598** checks in an
isolated Java-free library and **898/898** with the live rjd3 backend,
including the calendar series, forecast domains and SI chart data.
[RSA1 product replay](inst/tinytest/test-native-tramoseats-rsa1.R)

## Monthly RSA1 residual tests in reports (2026-11-24)

The rkalos `0.1.0.9014` dependency at `03d6cbd` brings the public
TR1 residual standard error, Ljung–Box independence and Doornik–Hansen
normality tests to native RSA1 output. The dashboard, diagnostics table
and quality report use their independently calculated native statistics
and probabilities. A series with 24 residuals reports undefined
independence and a finite normality probability.

All twelve JVM reference fits passed **734/734** RSA1 assertions in
the Java-free library and **1,082/1,082** with live rjd3, covering
the 60 diagnostic scalars, report classifications and downstream
component and forecast series. The existing RSA0 independence and
normality suites also passed in both environments.
[RSA1 product and reporting tests](inst/tinytest/test-native-tramoseats-rsa1.R)

## First accepted monthly TR3 identification in native RSA3 (2026-11-24)

Persephone now pins rkalos `0.1.0.9015` at `29abd91`. Native
`perTramo(series, "rsa3")` estimates the orders, transformation, mean
and automatic outliers from the observations. Six public monthly fitted
models matched all **573 dated residuals and 3,960 component and
forecast values bit-for-bit**. The product displays the estimated AR
and seasonal AR coefficients, and exposes the core refusal when the
first identification comparison requires another round. Fixing automatic
orders or detected outliers also raises the corresponding typed refusal.

The RSA3 product replay passed **295/295** assertions in isolation
without Java and **445/445** with the live monthly JDemetra pipeline.
Both earlier templates and the capability suite also passed. The Java-free
parity suite checked the actual method/template rows and passed **243/243**. The
preceding diagnostics commit `350c249` completed the full source
package check with **3,611/3,611** tests, rebuilt vignettes, PDF manual
and **Status OK**. The earlier `61e9840` passed **3,427/3,427** and
also completed the source check with **Status OK**.
[RSA3 product replay](inst/tinytest/test-native-tramoseats-rsa3.R)

## Original-series forecast intervals for native RSA1 (2026-11-24)

The rkalos `0.1.0.9016` pin at `61a6733` supplies dated original-series
forecast standard errors for raw monthly RSA1. The product normalizer
selects these errors for the original-series forecast and chart. The
conditional SEATS errors remain available under
`result$seats_forecast_standard_errors`. The twelve public fits
matched all **288 preprocessing forecast errors** within the registered
tolerance and all **288 conditional forecast errors** exactly. Both
chart bounds matched the public series uncertainty at all horizons.
Tests exercised 24 forecast months, custom 3/6 backcasts and forecasts,
and a zero horizon.

The updated RSA1 product suite passed **810/810** in a Java-free
library and **1,182/1,182** against live JDemetra. The existing RSA0
and RSA3 suites also passed in both environments.
[RSA1 uncertainty and chart checks](inst/tinytest/test-native-tramoseats-rsa1.R)

## Expanded monthly RSA3 model identification (2026-11-24)

Persephone now pins rkalos `0.1.0.9017` at `e8724b4`. The stock monthly
RSA3 route includes the observed seasonal-model restart, multiple-outlier
fits and a fourth estimation pass after the fast regression test removes
the mean. The 13-model product test checks **1,196 dated residuals and
8,340 component and forecast values** against the frozen public JVM
results bit-for-bit. It passed **639/639** assertions without Java and
**964/964** with live JDemetra. All three raw monthly routes passed in
both environments. The previous six-model RSA3 release `eff545e`
completed the full source check with **4,057/4,057** tests, rebuilt
vignettes, the PDF manual and **Status OK**.
[Expanded RSA3 qualification](inst/tinytest/test-native-tramoseats-rsa3.R)

## Selected-model residual tests in native RSA3 reports (2026-11-24)

With rkalos `0.1.0.9018` at `6676740`, the normalized native RSA3 result
includes the public residual standard error, Ljung–Box independence and
Doornik–Hansen normality tests. The independence degrees of freedom follow
the fitted AR and MA orders. The dashboard, diagnostic table and quality
report were checked against all 13 selected models and **65 public JVM
scalars**. A series with 24 residuals reports undefined independence and
a finite normality probability.

The RSA3 product test passed **734/734** assertions with Java hidden and
**1,111/1,111** with the live rjd3 pipeline. The RSA1 and existing RSA0
diagnostic tests also passed in both environments. The preceding RSA1
forecast release `2ad393e` completed its full source check with
**4,157/4,157** tests, rebuilt vignettes, PDF manual and **Status OK**.
[Selected-model reporting tests](inst/tinytest/test-native-tramoseats-rsa3.R)

## Original-series forecast intervals for native RSA3 (2026-11-24)

The rkalos `0.1.0.9019` dependency at `c471ab2` supplies dated
original-series forecast standard errors from the selected TR3 model.
Persephone uses them in the forecast contract and chart confidence band.
For all 13 raw RSA3 fits, the **312 model forecast errors** matched the
public JVM within `2e-8 * (1 + abs(reference))`. All **312 conditional
SEATS series errors** remained separately available and matched exactly.
Tests also checked both chart bounds and a custom six-month forecast.

The RSA3 suite passed **816/816** assertions without Java and
**1,219/1,219** with live JDemetra. RSA0 and RSA1 product suites also
passed in both environments. [RSA3 forecasts and intervals](inst/tinytest/test-native-tramoseats-rsa3.R)

### Windows package checks and localized R errors

The degenerate X-13 regression now compares its error with the message
produced by the running R interpreter for a missing condition. This removed
an undefined test helper reached by the hosted Windows check and keeps the
assertion active in translated R sessions. All 440 live-reference parity
checks passed with English messages and all 440 passed with German messages.
The hosted Windows rerun remains part of platform qualification.
