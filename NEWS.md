# persephone3 (development version)

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
