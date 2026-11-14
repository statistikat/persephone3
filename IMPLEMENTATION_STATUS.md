# Persephone implementation status and next-work plan

Updated: **2026-11-14**. The repository split is integrated on
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

The preserved pre-split X-11 initial-table timing report records 0.00568 seconds
for native versus 0.02913 for JDemetra+ across 24 short calls, and 0.12625 versus
0.08464 seconds across eight calls with 100,003 rows. See the [Kalos reference
report](../kalos/validation/reference/information/2026-11-14-x11-initial/)
for the workloads and measurement protocol.

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
