# Implementation status

Reviewed: 2026-11-13

## Current snapshot

Package version: **0.1.0**. The working tree is based on `master` commit
`463aca8`, pulled from `origin/master`. Items 1–3 are organized as separate
commits on `codex/implementation-items-1-3`. The implementation HEAD is
`9ba14a3`. The package remains experimental; implemented features do not by
themselves establish release readiness.

| Item | Commit | Change |
| --- | --- | --- |
| 1 | `d8a742c` | Add forecasts and outlier markers to hierarchical plots |
| 2 | `f9cb071` | Correct package dependency declarations and namespace imports |
| 3 | `9ba14a3` | Refresh persephone3 guides and document implementation status |

The branch remains local and has not been pushed. Items 4 and 5 continue on
this branch as separate commits. Validation results are recorded per item.

## Progress against the numbered implementation items

1. **Hierarchical plotting — implemented.** `forecasts` and `showOutliers`
   default to `TRUE`, while existing positional arguments remain compatible.
   Original-aggregate forecasts use the direct model and include 95% prediction
   intervals when matching standard errors are available. Missing standard
   errors fall back to a line; missing forecasts are omitted. AO, LS, and TC
   markers refer to the aggregate model, with monthly/quarterly period matching
   and out-of-range outliers omitted. The manual and 32 focused assertions cover
   these semantics. Browser visual validation remains outstanding.
2. **Dependency declarations — complete and checked.**
   Added `R6` to Imports and moved `forecast` from Suggests to Imports because
   `autoplot()` calls it unconditionally. Removed unused direct `cli` and
   `rlang` declarations; transitive requirements remain managed by dependencies.
   Added explicit namespace imports for R6 construction, the rjd3toolkit setter
   table, dependencies used by the bundled Shiny app, and functions identified
   by package checks. Removed the redundant `library(shiny)` in the dashboard
   launcher; the app scripts load Shiny themselves and the launcher calls
   `shiny::runApp()`. Installed `shinycssloaders` in a temporary validation
   library, without changing the user's normal R library.
3. **Documentation migration — complete for the current guides.** Updated
   `README.Rmd` and regenerated `README.md` with current guide links, batch and
   hierarchy examples, diagnostics, and dashboard usage. Corrected specification
   access in the hierarchy guide (`$paramsDirect` for aggregate specifications,
   `$params` for single components, and nested `$automodel$enabled` fields).
   The explicit ARIMA example now disables automatic model selection. Added
   hierarchical forecast/outlier documentation, enabled the diagnostics and
   simplified-outlier examples, clarified that the regional dataset example
   covers 26 countries rather than a complete EU aggregate, and made its random
   weights reproducible. Added diagnostics to the pkgdown navigation.
4. **Dashboard regression coverage — implemented.** Added 36 assertions for
   single/comparison HTML output, escaped and default titles, exported files,
   invalid/unfitted inputs, batch/hierarchy startup from a different working
   directory, timeout handling, and shutdown. The launcher resolves the loaded
   package location and supports both installed and development packages.
   `launch.browser = FALSE` permits headless use; `startup.timeout` bounds startup,
   and `attr(process, "url")` exposes the local address. Failed startup cleans
   up the child process. The app now honors custom titles. Added the directly
   required `htmlwidgets` dependency and documented the launcher controls.
5. **Release validation — in progress.** Current-source tests pass; package
   checks, browser validation, and release cleanup are tracked below.

## Implemented capabilities

| Area | Implementation present | Main source files |
| --- | --- | --- |
| Single-series adjustment | R6 interfaces for X-13 and TRAMO/SEATS, model execution, parameter updates, and access to results | `R/persephone.R`, `R/perSingle.R`, `R/perX13.R`, `R/perTramo.R` |
| Batch processing | Collections of component series, execution, parameter handling, and component access | `R/perBatch.R` |
| Hierarchical processing | Nested components, direct and indirect adjusted series, aggregation, and traversal | `R/perHts.R`, `R/iterate.R` |
| Model maintenance | Helpers to fix ARIMA models and outliers | `R/fixModel.R`, `R/fixOutlier.R` |
| Calendar regressors | Trading-day and Easter-related regressor generation | `R/genTd.R`, `R/eaDist_approx.R`, `R/eaDist_exact.R` |
| Diagnostics and reporting | Diagnostic tables, quality-report tables, and result extraction helpers | `R/diagnosticsTable.R`, `R/generateQrTable.R`, `R/generate_Qr_List.R`, `R/get_functions.R` |
| Visualization | Series, residual, seasonal/irregular/calendar, SI-ratio, and spectral plots; multiple plotting backends | `R/plot*.R`, `R/autoplot.R` |
| Single-series dashboards | HTML dashboards for one series and comparison of two series, with optional file output | `R/dashboard_single.R`, `R/dashboard_compare.R` |
| Batch/hierarchy dashboard | Shiny application launched through a background `callr` process | `R/createDashboard.R`, `inst/app/` |

The public namespace exports the main constructors, plotting functions,
diagnostic helpers, and dashboard entry points. `createDashboard()` dispatches
single-series objects to HTML output and batch/hierarchical objects to Shiny.

## Tests and delivery infrastructure

- Nine active test files live in `inst/tinytest/`, covering single series,
  batches, hierarchies, plotting, model/outlier fixes, calendar regressors, and
  miscellaneous helpers. `tests/tinytest.R` runs them during package checks
  when `tinytest` is available.
- Development scripts and older test copies also exist under `inst/devel/`;
  that directory is excluded from package builds by `.Rbuildignore`.
- The R-CMD-check workflow configures macOS and Windows with release R, plus
  Ubuntu with release and development R. It sets up Java 21 and treats check
  warnings as failures.
- A separate coverage workflow configures `covr::codecov()` and Java 17.
- A pkgdown workflow, `_pkgdown.yml`, generated manuals, and five vignettes are
  present.

These are repository configurations, not evidence that the latest CI runs pass.
Remote CI results and coverage percentages were not checked.

## Validation

- Item 4: all **133 assertions** in nine test files passed against a fresh
  installation, including 36 dashboard assertions. The new dashboard tests
  also passed with the development package loaded. Local-server tests required
  localhost access outside the execution sandbox. Log:
  `/tmp/persephone-item4-tests.log`.

- All **97 assertions** in the eight active tinytest files passed against the
  current source loaded with `pkgload::load_all()`, including the 32 new
  hierarchical plotting assertions. Existing model-configuration warnings were
  emitted; no assertions failed.
- Earlier smoke tests rendered X-13 and TRAMO/SEATS aggregate forecasts using
  the updated plotting/extraction code and installed model classes.
- The package source archive builds successfully. `IMPLEMENTATION_STATUS.md`
  is excluded from the R package build via `.Rbuildignore`.
- `shinycssloaders` 1.1.0 is installed in `/tmp/persephone3-status-lib` for these
  checks. Installed rjd3 packages 3.9.0 and Shiny 1.14.0 satisfy the declared
  version minimums; R is 4.5.2.
- Final `R CMD check --no-manual --no-build-vignettes --no-vignettes` completed
  with **0 errors, 3 warnings, and 0 notes**. Dependency checks, loading with
  stated dependencies, code analysis, examples, and installed-package tests
  passed. The three remaining warnings concern existing non-ASCII strings in
  `R/dashboard_single.R` and two reports of missing built vignette outputs
  (vignettes were deliberately not built). Repository index access was blocked
  during the check, so package availability was checked using local libraries.
- A parsed-source audit of `R/` and `inst/app/` found no undeclared namespace
  dependencies. All declared packages are available in the validation libraries.
- Final check log: `/tmp/persephone3-final-check/persephone3.Rcheck/00check.log`.
- Documentation validation after item 3: `README.Rmd` regenerated successfully,
  and all five vignettes rendered to HTML against the current source. Reviewed
  the hierarchy output to confirm three `FALSE` settings and the subsequent
  component-specific `TRUE`, rather than silently displaying `NULL`. Rendered
  HTML is under `/tmp/persephone3-doc-review`. Pandoc reported a deprecated
  MathJax option; the initial README HTML preview could not fetch remote badge
  images. Final Markdown generation used `html_preview = FALSE`.
- Dashboard-launch examples remain `eval = FALSE` to avoid starting services
  during documentation builds. Item 4 removes the source-checkout working-directory
  requirement and updates README accordingly.
- The earlier package check excluded PDF manual generation and vignette builds.
  Vignettes have since rendered individually, but a new package check with
  built vignettes and PDF manual validation has not been run. Browser/dashboard
  visual checks and remote CI status remain unverified.

## Remaining work

- **Next implementation item: 5 — release validation.** Run checks with built
  vignettes and the PDF manual, resolve remaining warnings, and inspect the
  dashboards and hierarchical plots in a browser.
- Resolve remaining package-check findings before release, including non-ASCII
  strings in `R/dashboard_single.R`.
- Run a package check with the now-renderable vignettes included, and validate
  the PDF manual in a complete documentation toolchain.
- Visually inspect the new hierarchical forecast intervals and outlier markers
  in a browser, and smoke-test all dashboard modes.

The abstract `stop("implement ...")` hooks in `R/persephone.R` are intentional
base-class interfaces, not evidence of missing concrete model implementations.
