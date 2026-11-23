# RSA0 uses the fixed airline specification and estimates its coefficients.
validate_native_tramo_spec <- function(spec, ts) {
  if (!inherits(spec, "persephone_native_tramo_spec") || !identical(spec$method, "tramoseats") ||
      !identical(spec$template, "rsa0") || !identical(spec$frequency, 12L) ||
      anyDuplicated(names(spec)) ||
      !setequal(names(spec), c("method", "template", "frequency", "backcast", "forecast")))
    stop("Invalid native RSA0 specification.", call. = FALSE)
  if (stats::frequency(ts) != 12 || !all(is.finite(ts)) || length(ts) <= 17L)
    stop("RSA0 requires at least 18 complete finite monthly observations.", call. = FALSE)
  counts <- vapply(c("backcast", "forecast"), function(key) {
    x <- spec[[key]]
    if (!is.numeric(x) || is.object(x) || !is.null(dim(x)) || length(x) != 1L ||
        !is.finite(x) || x != trunc(x) || abs(x) > .Machine$integer.max)
      stop(key, " must be a whole number in the supported index range.", call. = FALSE)
    if (x < 0) -as.double(x) * 12 else as.double(x)
  }, numeric(1))
  if (length(ts) + sum(counts) > .Machine$integer.max)
    stop("Extended RSA0 series exceeds the supported index range.", call. = FALSE)
  invisible(spec)
}

backend_native_tramo_fit <- function(ts, spec, context, userdefined) {
  validate_native_tramo_spec(spec, ts)
  if (!is.null(context) || !all(userdefined %in% userdefined_default))
    backend_native_refusal("seasonal-adjustment fitting", "tramoseats", "tramo_seats_fit.templates",
                           "RSA0 accepts no external context or custom output descriptors; ")
  fit <- rkalos::native_tramoseats_fit(ts, "RSA0", forecast = spec$forecast, backcast = spec$backcast)
  if (!identical(fit$status, "ok"))
    stop("Native TRAMO-SEATS RSA0 refused this fit: ", fit$status, ". No Java fallback was used.",
         call. = FALSE)
  fit
}

normalize_native_tramo_result <- function(fit, spec, ts) {
  if (inherits(spec, "persephone_spec")) spec <- spec$parameters
  validate_native_tramo_spec(spec, ts)
  components <- list(y = fit$series, t = fit$finalTrend, sa = fit$finalSA,
                     s = fit$finalSeasonal, i = fit$finalIrregular, cal = NULL)
  fields <- c(y = "Series", t = "Trend", sa = "SeasonallyAdjusted", s = "Seasonal", i = "Irregular")
  rows <- function(table, information) {
    table <- table[table$information == information, , drop = FALSE]
    if (!nrow(table)) return(list())
    index <- match(fields, table$component)
    if (anyNA(index) || anyDuplicated(table$component)) stop("Incomplete RSA0 component rows.")
    stats::setNames(as.list(table$values[index]), names(fields))
  }
  conditional <- rows(fit$final, "StdevForecast")
  errors <- conditional
  errors$y <- NULL
  forecasts <- new_forecast_contract(rows(fit$merged, "Forecast"), errors, observed = ts,
    mode = "Additive", point_reasons = list(cal = "RSA0 has no calendar regressors."),
    error_reasons = list(y = "The TRAMO original-series forecast standard error is unavailable."))
  availability <- data.frame(
    field = c(names(components), paste0(names(components), "_f"), "y_ef"),
    state = c(vapply(components, forecast_state, character(1)),
              if (is.null(forecasts)) rep("not_provided", 6L) else forecasts$availability$points,
              if (is.null(forecasts)) "not_provided" else forecasts$availability$standard_errors[1L]),
    stringsAsFactors = FALSE, row.names = NULL)
  model <- fit$model
  model$coefficients <- list(ma = model$theta, sma = model$btheta)
  fingerprint <- native_text_fingerprint(paste(c("tramoseats", "rsa0", stats::start(ts),
                                                  spec$backcast, spec$forecast), collapse = "/"))
  structure(list(schema_version = 1L, method = "tramoseats", backend = "native",
                 specification = spec, status = list(code = "success", convergence = "not_reported"),
                 domain = backend_domain(ts), components = components, forecasts = forecasts,
                 diagnostics = list(), availability = availability, model = model, likelihood = fit$likelihood,
                 seats_forecast_standard_errors = conditional,
                 provenance = list(template = "rsa0", mode_effective = fit$mode,
                                   core_commit = rkalos::core_version()$commit,
                                   request_fingerprint = fingerprint), raw = fit),
            class = "persephone_native_tramo_fit")
}
