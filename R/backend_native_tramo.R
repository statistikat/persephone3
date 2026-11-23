# Both stock monthly templates estimate the coefficients of the airline model.
validate_native_tramo_spec <- function(spec, ts) {
  if (!inherits(spec, "persephone_native_tramo_spec") || !identical(spec$method, "tramoseats") ||
      length(spec$template) != 1L || !spec$template %in% c("rsa0", "rsa1") || !identical(spec$frequency, 12L) ||
      anyDuplicated(names(spec)) ||
      !setequal(names(spec), c("method", "template", "frequency", "backcast", "forecast")))
    stop("Invalid native TRAMO-SEATS specification.", call. = FALSE)
  minimum <- if (spec$template == "rsa0") 18L else 24L
  if (stats::frequency(ts) != 12 || !all(is.finite(ts)) || length(ts) < minimum)
    stop(toupper(spec$template), " requires at least ", minimum, " complete finite monthly observations.", call. = FALSE)
  counts <- vapply(c("backcast", "forecast"), function(key) {
    x <- spec[[key]]
    if (!is.numeric(x) || is.object(x) || !is.null(dim(x)) || length(x) != 1L ||
        !is.finite(x) || x != trunc(x) || abs(x) > .Machine$integer.max)
      stop(key, " must be a whole number in the supported index range.", call. = FALSE)
    if (x < 0) -as.double(x) * 12 else as.double(x)
  }, numeric(1))
  if (length(ts) + sum(counts) > .Machine$integer.max)
    stop("Extended TRAMO series exceeds the supported index range.", call. = FALSE)
  invisible(spec)
}

backend_native_tramo_fit <- function(ts, spec, context, userdefined) {
  validate_native_tramo_spec(spec, ts)
  if (!is.null(context) || !all(userdefined %in% userdefined_default))
    backend_native_refusal("seasonal-adjustment fitting", "tramoseats", "tramo_seats_fit.templates",
                           paste0(toupper(spec$template), " accepts no external context or custom output descriptors; "))
  fit <- rkalos::native_tramoseats_fit(ts, spec$template, forecast = spec$forecast, backcast = spec$backcast)
  if (!identical(fit$status, "ok"))
    stop("Native TRAMO-SEATS ", toupper(spec$template), " refused this fit: ", fit$status, ". No Java fallback was used.",
         call. = FALSE)
  fit
}

normalize_native_tramo_result <- function(fit, spec, ts) {
  if (inherits(spec, "persephone_spec")) spec <- spec$parameters
  validate_native_tramo_spec(spec, ts)
  multiplicative <- identical(fit$mode, "multiplicative")
  neutral_calendar <- function(series) 0 * series + as.integer(multiplicative)
  components <- list(y = fit$series, t = fit$finalTrend, sa = fit$finalSA,
                     s = fit$finalSeasonal, i = fit$finalIrregular, cal = neutral_calendar(fit$series))
  fields <- c(y = "Series", t = "Trend", sa = "SeasonallyAdjusted", s = "Seasonal", i = "Irregular")
  rows <- function(table, information) {
    table <- table[table$information == information, , drop = FALSE]
    if (!nrow(table)) return(list())
    index <- match(fields, table$component)
    if (anyNA(index) || anyDuplicated(table$component)) stop("Incomplete TRAMO-SEATS component rows.")
    stats::setNames(as.list(table$values[index]), names(fields))
  }
  conditional <- rows(fit$final, "StdevForecast")
  errors <- conditional
  reason <- list()
  if (identical(fit$series_forecast_stdev_status, "available")) {
    errors$y <- fit$series_forecast_stdev
  } else {
    errors$y <- NULL
    reason$y <- paste("TRAMO forecast uncertainty:", fit$series_forecast_stdev_status %||% "unavailable")
  }
  points <- rows(fit$merged, "Forecast")
  if (length(points)) points$cal <- neutral_calendar(points$y)
  forecasts <- new_forecast_contract(points, errors, observed = ts,
    mode = if (multiplicative) "Multiplicative" else "Additive", error_reasons = reason)
  availability <- data.frame(
    field = c(names(components), paste0(names(components), "_f"), "y_ef"),
    state = c(vapply(components, forecast_state, character(1)),
              if (is.null(forecasts)) rep("not_provided", 6L) else forecasts$availability$points,
              if (is.null(forecasts)) "not_provided" else forecasts$availability$standard_errors[1L]),
    stringsAsFactors = FALSE, row.names = NULL)
  model <- fit$model
  model$coefficients <- list(ma = model$theta, sma = model$btheta)
  fingerprint <- native_text_fingerprint(paste(c("tramoseats", spec$template, stats::start(ts),
                                                  spec$backcast, spec$forecast), collapse = "/"))
  structure(list(schema_version = 1L, method = "tramoseats", backend = "native",
                 specification = spec, status = list(code = "success", convergence = "not_reported"),
                 domain = backend_domain(ts), components = components, forecasts = forecasts,
                 diagnostics = list(residuals = fit$residuals), availability = availability,
                 model = model, likelihood = fit$likelihood,
                 seats_forecast_standard_errors = conditional,
                 provenance = list(template = spec$template, mode_effective = fit$mode,
                                   core_commit = rkalos::core_version()$commit,
                                   request_fingerprint = fingerprint), raw = fit),
            class = "persephone_native_tramo_fit")
}
