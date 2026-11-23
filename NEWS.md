# persephone3 (development version)
* In `summary()`, the joint F test of the trading day regressors is now
  printed directly below the table of the trading day coefficients (before
  the "Mean / Leap year" table); the joint F test of the user-defined
  regressors keeps its section at the end. The residual diagnostics of
  `plotResiduals()` use open circles in the normal Q-Q plot and unfilled
  bars (frame only) in the histogram of the standardized residuals.
* The single series dashboard (`createDashboard()` / `dashboard_single()`)
  is now organised in tabs (Overview, Plots, Trading days, Outliers,
  Quality); the plots use the plotly engine and omit their own titles
  (`main = ""` now suppresses a plot title) because the cards above the
  plots carry the titles, the spectra of the original and
  the seasonally adjusted series are overlaid and the residual diagnostics
  use `plotResiduals(which = "overview")`. The tables of the new tabs are
  built by the new exported functions `getTradingDaysTable()` (trading day
  coefficients with the joint F test), `getOutlierTable()` (outlier, mean
  and leap year coefficients) and `getQualityTable()` (quality indicators);
  none of the two coefficient tables has a `Group` column and only the
  joint F test of the trading days is reported, not one with the outliers
  together. The four combined calendar tests of the Quality tab carry the
  names they have in JDemetra+ ("Combined test on SA", "Combined test on
  irregular", each for all observations and for the last year) and show
  their p-values. A significant trading day F test is now classified as
  "Good" (significant trading day effects are the expected result). The
  derived Sunday effect (`Sunday (derived)` in `summary()` and in
  `getTradingDaysTable()`) takes the parameterisation of the trading day
  regressors into account: minus the sum of the six day effects for
  contrasts against Sunday (the JDemetra+ defaults), minus one seventh of
  the sum for centered regressors.
  The overview table of the Shiny dashboard (`createDashboard()` for
  `perHts()` objects) shows the quality indicators (TotalQ, Q, Q2,
  Normality, Independence, Seasonality, Outliers) for every series.
  After every redraw of that table (sorting, paging, filtering) the server
  now sends the set of selected series back to the browser, so unchecking a
  row always closes the corresponding tab again.
* `perHts()` can now handle many aggregates and sub-aggregates at once and
  adjusts chain linked series indirectly in the correct way:
  + New `groups` argument: a named list with one entry per aggregate series
    giving the names of its sub-series, e.g.
    `groups = list(C20_21 = c("C20", "C21"), C = c("C20", "C21", "C22"))`.
    All series (the sub-series and the aggregates) are supplied at one level
    and every series is adjusted exactly once; aggregates are always built
    from their leaf series. `weights` is a named list with one weight set
    (vector, `ts` or `mts`) per aggregate; weights of a lower frequency are
    repeated within their periods and weights that do not cover the whole
    span are extended by their first and last value. Without `groups`
    everything works as before.
  + New `cl_weights` and `ref_year` arguments for chain linked series: the
    indirect adjustment is computed in the unchained space (the adjusted
    sub-series are divided by their chain linking factors, weighted
    together, chain linked again with the factor of the aggregate and
    re-referenced to their mean in `ref_year`), instead of summing up the
    adjusted chain linked sub-series, which does not give the aggregate.
    `$getUnchainedIndirect()` returns the aggregate before it is chain
    linked again.
  + New `adjustment` argument (`"both"`, `"direct"` or `"indirect"`) to
    choose which adjustments are computed; with `"indirect"` the aggregate
    series are not adjusted themselves, which saves a lot of computing time
    for many aggregates.
  + New `td` argument: trading day regressors for the individual series,
    either one regressor set for all series or a named list with one entry
    per series. The new `user` argument works in the same way for further
    user-defined regressors of the regression model (see the `user` argument
    of `perX13()`); without `groups` a single set of regressors is also used
    for the aggregate series itself, which is estimated on its own there.
  + In `groups` mode `$adjustedDirect`, `$adjustedIndirect` (and
    `$forecastsIndirect`) are named lists with one element per aggregate,
    and `$getSeries(aggregates = TRUE)`, `$getAdjustedDirect(agg)`,
    `$getAdjustedIndirect(agg)` and `$getComponent(agg)` address individual
    aggregates. `plot()` and `plot(plotEngine = "plotly")` gained a `series`
    argument which aggregate to plot.
  + The quality overview printed for a hierarchical object (also available as
    a data.frame via the new `$qualityTable()` method) gained an `agg` column:
    series that are no aggregates have `agg = "none"`, an aggregate gets a
    row with `agg = "direct"` for its own adjustment and, if the indirect
    adjustment is computed, a second row with `agg = "indirect"` (with
    `adjustment = "indirect"` only the indirect row is shown). The new
    columns `seasTest` and `tdTest` give the p-values of the tests on
    residual seasonality (Qs) and on trading day effects of the adjusted
    series of the row, which allows choosing between the direct and the
    indirect adjustment for every aggregate. The `nOutliers` column of an
    indirect row shows the number of the unique outliers of the sub-series
    the aggregate is built from (an outlier present in several sub-series
    is counted once).
* New `summary()` method for single series objects (`summary(obj)` and
  `obj$summary()`): frequency, span and number of observations, the model
  definition (ARIMA model, transformation, leap year, number of trading day
  regressors, Easter effect, number of outliers), the regression
  coefficients with standard error, t value and p-value in one table per
  group (one row per outlier, per trading day regressor and for the mean
  and leap year), the estimates of the ARIMA parameters (one row per
  estimated AR/MA and seasonal AR/MA parameter), the joint F tests of the
  trading days and of the trading days with the outliers, the decomposition
  (additive or multiplicative, final moving average and trend-cycle filter)
  and, at the end, the M-statistics (Q and Q-M2) and the overall quality
  indicator.
* In `summary()`, the trading day regressors are now labelled by their
  names instead of the generic calendar group names: the regressors are
  shown as Monday to Saturday (the `group_*` names of
  `rjd3toolkit::calendar_td()` map to the days of the default grouping),
  and when the regression consists of the six day regressors the effect of
  the reference day Sunday is derived as minus the sum of the six effects
  and added as a row "Sunday (derived)".
* New `td_labels` argument of `perTd()` and `td_context()`: a character
  vector naming the trading day regressors (e.g.
  `c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")`);
  the labels are registered with the regressors in the modelling context
  and shown in `summary()`.
* The leap year effect (the "Leap year" line of `summary()`) counts as
  included if a dedicated leap year regressor is part of the regression or
  if the series is log transformed with a leap year or length-of-month
  adjustment (`transform.adjust = "leapyear"` or `"lom"`, which include the
  leap day effect implicitly); a user-defined leap year regressor is now
  shown in the Mean / Leap year table as well. The `LeapYear` column of the
  eurostat quality report table (`$generateQrTable()`) follows the same
  rule.
* `diagnosticsTable()` and the `$diagnosticsTable()` method gained a `view`
  argument: with `view = TRUE` the diagnostics are shown as an interactive
  table (DT) in the viewer pane, with readable column headers and
  explanatory tooltips and the quality classes in coloured fonts (green,
  orange, red) on a black page (`dark`, the default, = TRUE; `dark = FALSE`
  for a light page); the default (`view = FALSE`) still returns the plain
  data.frame.
* New function `perTd()`: seasonal adjustment with user-defined trading day
  regressors (from a `rjd3toolkit` national calendar or from precomputed
  regressors such as `genTd()[[3]]`), for both x13 and tramoseats.
* `perX13()`, `perTramo()` and `$updateParams()` gained a `td` argument that
  takes a calendar, precomputed trading day regressors or a `td_context()`
  list and wires them up like `perTd()`. The full output of `genTd()` can
  now be passed directly (`td = genTd(...)`), its centered day regressors
  are used then. A regressor span not covering the series is rejected with
  an error showing both spans.
* `perX13()`, `perTramo()` and `$updateParams()` gained a `user` argument for
  user-defined regressors of the regression model that are no trading days,
  e.g. the leap year regressor `genTd()[[3]][, "lpYear", drop = FALSE]`. It
  takes a `ts`/`mts` object with one regressor per column or a named list of
  such objects, registers the regressors in the modelling context and adds
  them to the regression model (`rjd3toolkit::add_usrdefvar()`). Together
  with `td` this estimates the trading days and the further regressors in one
  model: `perX13(ts, td = tdAT, user = lpY)`. `summary()` shows their
  coefficients (a regressor named like a leap year is reported as the leap
  year effect, others in a table of their own with a joint F test);
  tramoseats does not estimate both kinds of regressors at once and warns
  when they are given together. A `context` of one's own now keeps the
  regressors that are already in it when `td` or `user` adds further ones
  (they used to be replaced by the new ones).
* The `td.test` argument is validated against the specification's valid
  values (`"Add"`, `"Remove"` or `"None"`, optionally with `"Joint_F"`, for
  x13; `"Separate_T"`, `"Joint_F"` or `"None"` for tramoseats); previously,
  an invalid value such as `td.test = "Joint_F"` for x13 failed with the
  unhelpful java error `'arg' should be one of "REMOVE", "ADD", "NONE"`.
* The default of the `contrasts` argument of `perTd()`/`td_context()` is now
  `TRUE` (centered regressors, as in `rjd3toolkit::calendar_td()` and
  `genTd()`); the previous uncentered regressors were nearly collinear with
  the regression constant, which made x13 often estimate the model in logs
  with the trend and s.a. series returned on the log scale.
* Highcharter is no longer used by the package: `plot()`,
  `plotSeasIrrCal()`, `plotSiRatios()` and `plotSpectrum()` now draw their
  interactive plots with dygraphs and/or plotly (`plotEngine` argument,
  dygraphs is the default for the time series plots, plotly for the SI-ratio
  and spectral plots; `hchart()` and the `plotSeasIrrCalHc()`,
  `plotSiRatiosHc()` and `plotSpectrumHc()` functions are no longer
  available). The plots keep the style of the former highcharter plots:
  smoothed lines and the default Highcharts colours.
* Time series plots (`plot()`) are drawn with smoothed lines by default; the
  new arguments `smooth` and `rangeSelector` control the smoothing and the
  range selector. The range selector is now off by default. The smoothing
  uses a monotone piecewise cubic interpolation, which cannot overshoot the
  data or oscillate between the observations (the previous not-a-knot cubic
  spline produced small ripples, especially in wide plots; it is still
  available with `smooth = "spline"`). The smoothed series are no longer
  extrapolated beyond their observations, which previously blew up at the
  end of the plot when forecasts were shown and ruined the scaling of the
  y-axis. The widgets resize with their container instead of keeping the
  fixed 960px width.
* The series of the time series plots (`plot()`) are drawn in the default
  Highcharts colours (blue, dark grey, light green, grey dashed forecast)
  and the forecast is shown with its 95% confidence band, like in
  `plotSeasIrrCal()`; the time axis of the monthly and quarterly series
  shows the exact month/quarter starts.
* Outliers in the time series plots are marked with coloured dots and a
  discreet dashed vertical line in a lighter shade of the same colour, and
  the legend (explaining the outlier colours) stays visible, instead of
  dashed vertical lines and labels that overlapped the title in small
  windows.
* The SI-ratio dots of `plotSiRatios()` are drawn smaller (both engines) so
  that the underlying seasonal factor lines remain visible.
* `plotSpectrum()` (both engines) accepts several series types
  (`tsType = c("original", "sa", "irregular")`) and several plot types
  (`plotType = c("arSpec", "periodogram")`): the spectra of several series
  types are overlaid in a single plot (with a legend), and for several plot
  types a named list with one such plot per plot type is returned.
* `plotResiduals()` gained `which = "overview"`: the residuals over time
  with the Q-Q plot of the standardized residuals beside them (the
  residuals are twice as wide as the Q-Q plot), and the acf, the pacf and
  the histogram of the standardized residuals (with the normal curve, its
  bars now in the colour of the series lines) in a second row, in one
  plotly figure. The panels are identified by their y-axis labels
  ("Residuals", "Standardized Residuals", "ACF", "PACF" and "Density").
  The points of the Q-Q plots (`which = "nqq"` as well) are drawn in the
  colour of the series lines instead of black.
* The interactive plots fill the plot pane of RStudio and Positron: a plot
  shown on its own uses the whole pane and is redrawn whenever the pane is
  resized, so a small pane gives a small plot instead of a plot that has to be
  scrolled. Previously the plotly plots kept their fixed 400px height and the
  plots did not follow the size of the pane at all (neither programme applies
  the sizing policy of htmlwidgets to a widget shown on its own). The height is
  measured in the page of the widget itself and travels with the widget, so it
  works in both programmes. In knitr documents (reports, vignettes) and in the
  dashboards the plots keep their own size as before,
  `options(persephone3.plotFill = FALSE)` switches the filling off, and
  `options(persephone3.plotHeight = 300)` (and `persephone3.plotWidth`) gives a
  plot a size of its own instead.
* Moving the mouse over an interactive plot draws a vertical line at its
  position (as the crosshair of the former highcharter plots) and shows the
  values of all plotted series at this date together; the dots at the hovered
  point of every series are no longer drawn. In the dygraphs plots the
  highlight point at the cursor is gone as well, its cursor line marks the
  position.
* Repeated dates in the fine time grid of the smoothed plots (which made
  dygraphs and plotly draw vertical lines at the month/quarter boundaries
  when zooming in) are fixed.
* `getOutliers()` now handles regression variables with more than one
  coefficient (e.g. a trading day variable with one coefficient per group);
  previously `print()` failed on models with user-defined trading days.
* The interactive plots highlight the series the cursor is on: the hovered
  line is drawn bold and all other series fade, while the tooltip still
  shows the values of all series at the cursor position. In the plotly
  plots (`plot(plotEngine = "plotly")`, `plotSeasIrrCal()`,
  `plotSiRatios()`, `plotSpectrum()` and the residual plots) a click on a
  series fixes its highlighting until a click on another series or on
  empty space lifts it; the dygraphs plots (`plot()`, `plotSeasIrrCal()`)
  highlight the hovered series as well.
* `plotSeasIrrCal(plotEngine = "plotly")` no longer shows the range slider
  below the plot; `rangeSelector` is `NULL` by default now, which keeps the
  range selector of the dygraphs plot and hides the slider of the plotly
  plot. Both can still be switched on and off explicitly.
* `plotSpectrum()` gained a `layout` argument: `"overplot"` (the default)
  overlays all spectra in one plot as before, `"panels"` places every
  combination of `plotType` and `tsType` in a framed panel of its own
  (a single plot object is returned). The panels form a grid of at most
  two equally wide columns and as many rows as needed - two spectra side
  by side, three in two rows, six in three rows - and every panel keeps
  the height:width ratio a plot of its own would have, whatever their
  number.
  The spectral lines are now coloured from a fixed palette (the first
  series in blue), and the plotly engines mark the seasonal frequencies in
  blue and the trading day frequencies in red as the ggplot2 engine does.
* The confidence lines of the acf, pacf and acf2 plots of `plotResiduals()`
  are drawn in red like the other limit lines of the package; in the
  `"overview"` figure the panels are further apart and their y-axis titles
  are drawn in a small font, so that the titles no longer reach into the
  panel on their left.
* The blue of all plots (the lines of the original series and of the
  seasonal component, the confidence band of the forecasts, the spectrum
  lines and the mean lines of the SI-ratio plots) is now a darker blue
  (dodgerblue3, `#1874CD`) instead of the bright Highcharts blue `#7cb5ec`.

# persephone3 0.1.0
* First working version with minimally functional dashboard
