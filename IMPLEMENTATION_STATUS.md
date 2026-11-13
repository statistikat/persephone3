# Implementation status

Reviewed: 2026-11-13

## Current snapshot

Package version: **0.1.0**. Work is on `codex/implementation-items-1-3`, based on
`master` commit `463aca8`. The branch is local and has not been pushed.

| Item | Commit | Result |
| --- | --- | --- |
| 1 | `d8a742c` | Hierarchical forecasts and outlier plotting |
| 2 | `f9cb071` | Dependency declarations and namespace imports |
| 3 | `9ba14a3` | README and vignette migration |
| 4 | `2d10eb3` | Dashboard regression tests and portable launcher |
| 5 | Release-validation commit accompanying this report | Full checks and browser-discovered fixes |

## Progress against the implementation items

1. **Hierarchical plotting — complete.** Forecasts and 95% prediction intervals
   use the original aggregate's direct model; missing standard errors fall back
   to a line. AO, LS, and TC markers use monthly/quarterly period matching and
   omit out-of-range events. `forecasts` and `showOutliers` are independently
   configurable. Release validation corrected marker/legend colors when forecast
   intervals collapse multiple data columns. There are 36 focused assertions.
2. **Dependency declarations — complete.** Added direct R6/forecast dependencies,
   removed unused declarations, and made namespace requirements explicit.
   Dashboard validation additionally identified the direct `htmlwidgets`
   dependency and namespace registration requirement.
3. **Documentation migration — complete.** Updated README and all five guides,
   corrected specification access and explicit ARIMA settings, documented
   hierarchical plotting, clarified the regional dataset coverage, and added
   diagnostics navigation. README is regenerated from `README.Rmd`.
4. **Dashboard regression coverage — complete.** Tests cover HTML generation and
   export, default/custom/escaped titles, invalid and unfitted inputs,
   batch/hierarchy startup outside the checkout, timeout handling, shutdown,
   aggregate panels, and reactive selection changes. The launcher resolves the
   loaded package location, supports installed/development packages, honors
   custom titles, and exposes `launch.browser`, `startup.timeout`, and
   `attr(process, "url")`. Failed startups clean up the child process. There are
   43 dashboard assertions after release-validation regressions were added.
5. **Local release validation — complete.**
   Removed non-ASCII CSS comments and made manual inequality notation portable
   to LaTeX. Browser checks found and resolved inconsistent outlier colors,
   blocked aggregate panels, self-invalidating panel caching, and narrow plot
   cards. Added responsive card sizing and regression checks. Updated NEWS.

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

## Validation evidence

- Final full `R CMD check` passed with **0 errors, 0 warnings, 0 notes**.
  All 56 R source, test, and app files match the checked source archive.
- The suite contains **144 assertions in nine files**, including 36 hierarchical
  plotting and 43 dashboard assertions. The full installed-package suite passed
  during release checks; source-level dashboard tests also passed.
- `R CMD build` includes all five vignettes. Full `R CMD check` runs examples,
  tests, vignette rebuilding, and PDF manual generation without exclusion flags.
- Browser checks with Playwright cover single-series and comparison dashboards,
  batch series selection, hierarchy aggregate/component selection, monthly and
  quarterly forecast/outlier plots, and the narrower embedded dashboard layout.
  Corrected aggregate/component rendering has no application console errors;
  the local test servers report only missing optional `favicon.ico` requests.
- Validation environment: macOS, R 4.5.2, rjd3 packages 3.9.0. The temporary R
  library is `/tmp/persephone3-status-lib`; the user's normal library was not
  modified. Local-server tests require permission to bind localhost ports.
- Build/check logs, the built package and PDF manual, browser snapshots, and
  screenshots are under `/tmp/persephone-release-validation`. These are local
  temporary evidence, not files shipped in the repository. All validation
  browser sessions and fixture servers were stopped after inspection.

## Delivery and remaining release steps

All five implementation items have been addressed locally. The package remains
experimental; these checks do not establish support for every environment.

- Push the branch and run the configured macOS, Windows, and Linux CI matrix
  before merging or publishing. Remote CI and coverage results are unverified.
- Broader browser/device testing and domain review of numerical results remain
  appropriate before a production release.
- No package publication, remote push, or merge has been performed.

The repository's workflows cover multi-platform R CMD check, coverage, and
pkgdown. Development/archived scripts under `inst/devel/` and this status report
are excluded from the package build. Abstract `stop("implement ...")` hooks in
`R/persephone.R` are intentional base-class interfaces.
