# Exercise both public native routes. Read back the points that the widgets
# would serialize, including their calendar and the automatic outlier markers.
fixed <- perX13(AirPassengers, "restricted", backend = "native",
                speclist = list(seasonal = rep("s3x3", 12L), trend_length = 13L))
expect_identical(plot(fixed)$x$attrs$labels[-1], "Original")

observed <- AirPassengers
observed[70] <- observed[70] * 3
automatic <- perX13(observed, "rsa3", backend = "native")
# M13 P5 (2026-11-28): on the SE-carrying automatic lane the dygraph uses the
# range-series layout and serializes every numeric column as the
# (low, value, high) triple; the restricted route keeps plain vectors. Read
# the centre value back from either layout.
dy_center <- function(col) if (is.list(col)) {
  vapply(col, function(v) if (!length(v)) NA_real_ else v[[2L]], numeric(1))
} else as.numeric(col)
for (obj in list(fixed, automatic)) {
  obj$run()
  expected <- list(Original = obj$ts, `Seasonally Adjusted` = obj$result$components$sa,
                   Trend = obj$result$components$t)
  dates <- zoo::as.Date(obj$ts)
  dy <- obj$plot()
  # M12 alignment (2026-11-27): the automatic lane's default one-year horizon
  # extends the dygraph axis (observed 144 + 12 forecast months, the forecast
  # tail itself is pinned in test-native-forecasts.R); the observed columns
  # are read back on the observed window.
  dy_dates <- as.Date(dy$x$data[[1L]])
  expect_identical(dy_dates[seq_along(dates)], dates)
  expect_identical(length(dy_dates),
                   length(dates) + if (identical(obj, automatic)) 12L else 0L)
  for (name in names(expected)) {
    index <- match(name, dy$x$attrs$labels)
    expect_identical(dy_center(dy$x$data[[index]])[seq_along(dates)],
                     as.numeric(expected[[name]]))
  }
  hc <- obj$hchart()$x$hc_opts
  series_names <- vapply(hc$series, "[[", character(1), "name")
  # M12 alignment (2026-11-27): the automatic lane requests the template's
  # one-year horizon, so its chart gains the forecast series. M13 P5
  # (2026-11-28, rkalos 0.1.0.9033 / core 69f83be): the automatic lane also
  # publishes the forecast standard errors, so the band appears — the pinned
  # identity below mirrors the certified estimated-route (x13-rsa0) arm; the
  # restricted route has no forecasts and stays band-free by the equality.
  expected_series <- sort(tolower(c(names(expected),
                                    if (identical(obj, automatic))
                                      c("forecast", "confidence interval"))))
  expect_identical(sort(series_names), expected_series)
  if (identical(obj, automatic)) {
    fc <- obj$forecasts
    band <- hc$series[[match("confidence interval", series_names)]]
    expect_identical(length(band$data), length(fc$points$y))
    expect_identical(vapply(band$data, "[[", numeric(1), "low"),
                     as.numeric(fc$points$y - 1.96 * fc$standard_errors$y))
    expect_identical(vapply(band$data, "[[", numeric(1), "high"),
                     as.numeric(fc$points$y + 1.96 * fc$standard_errors$y))
  }
  for (name in names(expected)) {
    points <- hc$series[[match(tolower(name), series_names)]]$data
    expect_equal(vapply(points, "[[", numeric(1), 1L),
                 as.numeric(dates) * 86400000, tolerance = 0)
    expect_identical(vapply(points, "[[", numeric(1), 2L), as.numeric(expected[[name]]))
  }
  effects <- plotSeasIrrCal(obj, plotEngine = "dygraph", annualComparison = 1L)
  labels <- c("Seasonal Component", "Irregular Component", "Calendar Effects")
  # With the automatic lane's default forecast horizon the effects chart adds
  # the two forecast extension series (seasonal, calendar) and extends the
  # dygraph axis; the fixed route keeps the observed-only chart.
  fc_labels <- c("Forecasts Seasonal Comp.", "Forecasts Calendar Eff.")
  is_auto <- identical(obj, automatic)
  expect_identical(effects$x$attrs$labels[-1L],
                   if (is_auto) c(labels, fc_labels) else labels)
  eff_dates <- as.Date(effects$x$data[[1L]])
  expect_identical(eff_dates[seq_along(dates)], dates)
  expect_identical(length(eff_dates),
                   length(dates) + if (is_auto) 12L else 0L)
  hc_effects <- obj$plotSeasIrrCal()$x$hc_opts$series
  expect_identical(vapply(hc_effects, "[[", character(1), "name"),
                   if (is_auto) c(labels, fc_labels) else labels)
  fields <- list(obj$output$finals$tables$d16, obj$output$finalIrregular, obj$output$finals$tables$d18)
  for (i in seq_along(fields)) {
    expect_identical(as.numeric(effects$x$data[[i + 1L]])[seq_along(dates)],
                     as.numeric(fields[[i]]))
    expect_identical(vapply(hc_effects[[i]]$data, "[[", numeric(1), 2L), as.numeric(fields[[i]]))
  }
  if (is_auto) {
    # The extension series carry the dated forecast tables (12 points each);
    # on the dygraph they follow the three observed columns (data[[5]]/[[6]]).
    win <- (length(dates) + 1L):length(eff_dates)
    expect_identical(as.numeric(effects$x$data[[5L]])[win],
                     as.numeric(obj$output$finals$tables$d16a))
    expect_identical(as.numeric(effects$x$data[[6L]])[win],
                     as.numeric(obj$output$finals$tables$d18a))
    expect_identical(lengths(lapply(hc_effects[4:5], function(s) s$data)), c(12L, 12L))
  }
  expect_identical(obj$result$components$y, obj$output$preadjustment$tables$a1)
  for (type in c("original", "sa", "irregular")) {
    signal <- switch(type, original = diff(obj$ts), sa = diff(obj$result$components$sa),
                     irregular = obj$result$components$i)
    reference <- stats::spec.pgram(signal, plot = FALSE)
    static <- plotSpectrum(obj, "ggplot", type, "periodogram")$data
    interactive <- plotSpectrum(obj, "hc", type, "periodogram")$x$hc_opts$series[[1L]]$data
    expect_identical(static$spec, as.numeric(reference$spec))
    expect_identical(static$freq, as.numeric(reference$freq) / frequency(signal))
    expect_identical(vapply(interactive, "[[", numeric(1), "y"), static$spec)
    expect_identical(vapply(interactive, "[[", numeric(1), "x"), static$freq)
  }
  dashboard <- as.character(dashboard_single(obj))
  expect_true(grepl("Decomposition effects", dashboard, fixed = TRUE))
  # The residual spectrum and the dashboard residual panel follow the fit. The
  # automatic route carries the dated preprocessing residual bundle that rkalos
  # 0.1.0.9028 attaches to the automatic fit, so its residual spectrum is the
  # periodogram of those residuals and the dashboard panel is populated; a
  # restricted fit supplies no residuals and keeps the honest refusal plus the
  # "unavailable" note (plotSpectrum_series falls back to the RegARIMA block,
  # which the native fixed-specification route does not have).
  if (!is.null(obj$result$diagnostics$residuals)) {
    resid <- obj$result$diagnostics$residuals$tsres
    resid_ref <- stats::spec.pgram(resid, plot = FALSE)
    resid_static <- plotSpectrum(obj, "ggplot", "residuals", "periodogram")$data
    expect_identical(resid_static$spec, as.numeric(resid_ref$spec))
    expect_identical(resid_static$freq, as.numeric(resid_ref$freq) / frequency(resid))
    expect_true(grepl("Residual diagnostics", dashboard, fixed = TRUE))
    expect_false(grepl("Residual diagnostics are unavailable", dashboard, fixed = TRUE))
  } else {
    expect_error(plotSpectrum(obj, tsType = "residuals"), "does not supply")
    expect_true(grepl("Residual diagnostics are unavailable", dashboard, fixed = TRUE))
  }
  expect_false(grepl("Plot error:", dashboard, fixed = TRUE))
  if (identical(obj, automatic)) {
    outliers <- obj$getOutliers()
    expect_true(nrow(outliers) > 0L)
    expect_true(all(outliers$type %in% dy$x$attrs$labels))
    lines <- hc$xAxis$plotLines
    expect_equal(vapply(lines, "[[", numeric(1), "value"),
                 as.numeric(outliers$date) * 86400000, tolerance = 0)
  }
}

# A deterministic seasonal regressor is restored after the X-11 pass.
fixed$updateParams(variables = list(list(class = "user", effect = "seasonal",
  coefficient = 10, columns = list(january = as.numeric(cycle(AirPassengers) == 1L)))))
fixed$run()
expect_false(identical(fixed$output$results$d10, fixed$output$finals$tables$d16))
expect_identical(fixed$result$components$s, fixed$output$finals$tables$d16)
fixed$updateParams(variables = list(list(class = "user", effect = "calendar_effect",
  coefficient = 10, columns = list(january = as.numeric(cycle(AirPassengers) == 1L)))))
fixed$run()
expect_identical(as.numeric(fixed$result$components$cal), rep(0, length(AirPassengers)))
expect_false(identical(fixed$result$components$cal, fixed$output$finals$tables$d18))
expect_identical(fixed$result$components$s, fixed$output$finals$tables$d16)
expect_true("Calendar Effects" %in% plotSeasIrrCal(fixed, plotEngine = "dygraph")$x$attrs$labels)

# Fixed trading days and Easter are nonzero, independently of a user variable
# allocated to the calendar component. Compare the typed preprocessing effect
# to the reference on both the level and log scales.
if (requireNamespace("rjd3x13", quietly = TRUE)) {
  td <- rjd3toolkit::td(s = AirPassengers)
  weights <- c(1, 2, -2, 1, -3, 2) / 50
  easter <- rjd3toolkit::easter_variable(s = AirPassengers, duration = 6, correction = "PreComputed")
  for (log in c(FALSE, TRUE)) {
    mode <- if (log) "multiplicative" else "additive"
    variables <- list(list(class = "days_related", effect = "calendar_effect", coefficient = weights,
      columns = lapply(seq_along(weights), function(i) as.numeric(td[, i]))),
      list(class = "moving_holiday", effect = "calendar_effect", coefficient = 0.1,
        columns = list(as.numeric(easter))),
      list(class = "user", effect = "calendar_effect", coefficient = 0.2,
        columns = list(as.numeric(cycle(AirPassengers) == 1L))))
    fixed$updateParams(log = log, mode = mode, variables = variables)
    fixed$run()
    spec <- rjd3toolkit::set_transform(rjd3x13::x13_spec("rsa0"), fun = if (log) "Log" else "None")
    spec <- rjd3toolkit::set_tradingdays(spec, option = "TradingDays", automatic = "Unused",
      test = "None", autoadjust = FALSE, leapyear = "None", coef = weights, coef.type = "Fixed")
    spec <- rjd3toolkit::set_easter(spec, enabled = TRUE, type = "Standard", test = "None",
      duration = 6, coef = 0.1, coef.type = "Fixed")
    ref <- rjd3x13::x13_fast(AirPassengers, spec = spec, userdefined = "cal")$user_defined$cal
    actual <- fixed$result$components$cal
    expect_identical(c(start(actual), end(actual), length(actual)),
                     c(start(ref), end(ref), length(ref)))
    expect_true(max(abs(as.numeric(actual) - as.numeric(ref))) < 1e-12)
    expect_true(max(abs(as.numeric(actual) - as.numeric(fixed$output$finals$tables$d18))) > 0.1)
  }
}
