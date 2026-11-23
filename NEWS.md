# persephone3 (development version)

* Native monthly RSA3 now identifies SARIMA orders and returns dated SEATS
  components and forecasts for fits accepted at the first reference comparison.

* Native RSA3 now supports seasonal-controller restarts and the fourth
  estimation pass after the fast regression test removes a mean.

* Native RSA1 original-series forecast intervals use the fitted TR1
  model on both level and log scales.

* Native RSA1 exposes the residual standard error and reports residual
  normality and independence in the dashboard, diagnostics table and quality report.

* Native RSA1 now restores automatically estimated monthly transitory changes
  on both model scales in components and forecasts.

* Extended native RSA1 to level-scale AO and LS estimation and restoration.

* Added native RSA1 adjustment for complete monthly series with automatic
  transformation and outlier estimation, dated SEATS components and forecasts.

* Native monthly RSA0 fits report the public Doornik–Hansen residual
  normality test in the dashboard, diagnostics table and Eurostat report.

* The automodel-OFF templates rsa0/rsa1/rsa2c were re-decoded live from the
  pinned JDemetra+ jars (double-JVM byte-identical captures) and evaluated
  against the restricted fixed-coefficient route in addition to the automatic
  one. They remain refused - by evidence, on both routes: the decoded
  specifications state estimated (non-fixed) ARIMA/regression coefficients
  and request SEASONAL_FILTER_MSR filters with the automatic Henderson 0 and
  nfcasts = -1 (12 forecast months), which the qualified core refuses
  verbatim ("unsupported:x13 fit fixed seasonal requests only", "... fixed
  Henderson trend length", "... requires zero horizons"); the candidate fixed
  values are series-dependent run outputs, so no fixed-spec mapping exists to
  pin. The refusals now name the violated scope fields and measured facts of
  both envelopes; the pinned decode/demand fixtures are re-verified against
  the live reference stack in the test suite. The rsa4/rsa5c refusals
  (calendar deferral) are unchanged.
* The native backend now also qualifies the automatic X-13 fit for the pinned
  template: `perX13(ts, "rsa3", backend = "native")` runs the rkalos
  automatic pipeline (`capabilities()$native_x13_fit_automatic`) without Java.
  The specification is the pinned JDemetra+ rsa3 decode table - the sigma
  policy is the only adjustable field, horizons stay zero, monthly series
  only; the other rsa templates refuse by name (automodel disabled, or
  calendar/Easter/trading-day detection deferred). Decisions are bit-exact
  against the reference implementation at the qualification tiers.
* The native backend (`backend = "rjd3"` remains the default) now provides the
  restricted fixed-coefficient X-13 fit without Java through
  `perX13(ts, template = "restricted", speclist = ..., backend = "native")`:
  fixed coefficients, fixed seasonal/Henderson requests, zero horizons,
  monthly series; the sigma policy defaults to "legacy" (the pinned
  JDemetra behavior, maintainer decision 2026-11-17) and "accurate" stays
  selectable; every other native
  request fails explicitly and never falls back silently.

* Hierarchical plots show forecasts with prediction intervals and aggregate
  outlier markers, with independent display switches and consistent colors.
* Dashboard startup works from any working directory for installed and
  development packages. Headless startup, startup timeouts, and the local URL
  are available through `createDashboard()`.
* Dashboards validate unfitted inputs, preserve default/custom titles, render
  aggregate panels, and refresh selected series without stale cached output.
  Plot cards adapt to the available width.
* Direct dependency declarations, README examples, and user guides are updated.
* Supported Java policy: reference-backend fits require a JVM 21 or newer
  because the rjd3 3.9.0 jars are compiled for Java 21; loading the package
  and reporting backend capabilities still work without Java. CI now installs
  Java 21 consistently and pinned rjd3 release tags.
* Regression coverage includes dashboard exports and lifecycle behavior. Manual
  notation is portable to PDF generation.

# persephone3 0.1.0
* First working version with minimally functional dashboard
