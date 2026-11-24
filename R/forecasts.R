# Public forecast values use a backend-independent, serializable contract.
forecast_component_names <- function() c("y", "t", "sa", "s", "i", "cal")

forecast_mode <- function(output) {
  mode <- output$user_defined$mode %||% output$user_defined$decomposition.mode
  if (!is.character(mode) || length(mode) != 1L || is.na(mode) || !nzchar(mode)) {
    return("unknown")
  }
  mode
}

forecast_series <- function(value, component, observed = NULL, error = FALSE) {
  if (is.null(value) || !length(value)) return(NULL)
  if (!stats::is.ts(value) || !is.numeric(value) || !is.null(dim(value))) {
    stop("Forecast '", component, "' must be a univariate numeric ts.", call. = FALSE)
  }
  if (any(is.infinite(value)) || (error && any(value < 0, na.rm = TRUE))) {
    stop("Forecast '", component, "' has invalid ",
         if (error) "standard errors." else "nonfinite values.", call. = FALSE)
  }
  if (!is.null(observed)) {
    if (frequency(value) != frequency(observed)) {
      stop("Forecast frequency must match the observed series.", call. = FALSE)
    }
    expected <- tsp(observed)[2] + 1 / frequency(observed)
    if (abs((tsp(value)[1] - expected) * frequency(value)) > 1e-6) {
      stop("Forecasts must begin immediately after the observed series.", call. = FALSE)
    }
  }
  value
}

forecast_state <- function(value, reason = NULL) {
  if (is.null(value)) return(if (is.null(reason)) "not_provided" else "unavailable")
  if (all(is.na(value))) return("missing")
  if (anyNA(value)) return("partially_missing")
  "available"
}

new_forecast_contract <- function(points, standard_errors = list(), observed = NULL,
                                  mode = "unknown", aggregation = "direct",
                                  point_reasons = list(), error_reasons = list()) {
  components <- forecast_component_names()
  validate_fields <- function(values, name) {
    if (!is.list(values) || (length(values) && (is.null(names(values)) ||
        anyNA(names(values)) || anyDuplicated(names(values)) ||
        any(!names(values) %in% components)))) {
      stop(name, " must be a list named by forecast component.", call. = FALSE)
    }
  }
  validate_fields(points, "points")
  validate_fields(standard_errors, "standard_errors")
  points <- stats::setNames(lapply(components, function(name) {
    forecast_series(points[[name]], name, observed)
  }), components)
  if (all(vapply(points, is.null, logical(1)))) return(NULL)
  present <- points[!vapply(points, is.null, logical(1))]
  frequencies <- vapply(present, frequency, numeric(1))
  if (any(frequencies != frequencies[1])) {
    stop("Forecast component frequencies must match.", call. = FALSE)
  }
  standard_errors <- stats::setNames(lapply(components, function(name) {
    value <- forecast_series(standard_errors[[name]], name, observed, error = TRUE)
    if (!is.null(value) && (is.null(points[[name]]) ||
        frequency(value) != frequency(points[[name]]) ||
        abs(tsp(value)[1] - tsp(points[[name]])[1]) > 1e-9 ||
        length(value) > length(points[[name]]))) {
      stop("Forecast standard errors must cover an aligned prefix of their point forecasts.",
           call. = FALSE)
    }
    value
  }), components)
  if (!is.character(mode) || length(mode) != 1L || is.na(mode)) mode <- "unknown"
  scales <- stats::setNames(c(rep("level", 3), rep(switch(mode,
    Additive = "level", Multiplicative = "factor", "unknown"), 3)), components)
  for (name in components) {
    if (!is.null(standard_errors[[name]]) &&
        length(standard_errors[[name]]) < length(points[[name]])) {
      error_reasons[[name]] <- paste0("Standard errors cover only the first ",
        length(standard_errors[[name]]), " of ", length(points[[name]]), " forecast periods.")
    }
  }
  availability <- data.frame(
    component = components,
    points = vapply(components, function(name) forecast_state(points[[name]], point_reasons[[name]]),
                     character(1)),
    standard_errors = vapply(components, function(name) {
      state <- forecast_state(standard_errors[[name]], error_reasons[[name]])
      if (state %in% c("available", "partially_missing") &&
          length(standard_errors[[name]]) < length(points[[name]])) "partial_horizon" else state
    }, character(1)),
    point_reason = vapply(components, function(name) point_reasons[[name]] %||% "", character(1)),
    uncertainty_reason = vapply(components, function(name) error_reasons[[name]] %||% "", character(1)),
    stringsAsFactors = FALSE, row.names = NULL)
  structure(list(schema_version = 1L, points = points,
                 standard_errors = standard_errors,
                 domains = lapply(points, backend_domain),
                 standard_error_domains = lapply(standard_errors, backend_domain), mode = mode,
                 scales = scales, aggregation = aggregation,
                 availability = availability), class = "persephone_forecasts")
}

forecasts_from_output <- function(output, observed, method = NULL, parameters = NULL) {
  if (is.null(output)) return(NULL)
  x13 <- identical(method, "x13")
  # X-13 preprocessing can return a calendar effect and model uncertainty
  # for a full year even when the requested X-11 horizon is zero.
  if (x13 && isTRUE(parameters$x11$nfcasts == 0)) return(NULL)
  components <- forecast_component_names()
  points <- stats::setNames(lapply(components, function(name) {
    output$user_defined[[paste0(name, "_f")]]
  }), components)
  se <- output$user_defined$y_ef
  if (x13 && length(points$y) > 0L && length(se) > length(points$y)) {
    se <- forecast_series(se, "y", observed, error = TRUE)
    se <- stats::window(se, end = stats::end(points$y))
  }
  new_forecast_contract(points, list(y = se), observed,
                        mode = forecast_mode(output))
}

native_automatic_forecasts <- function(fit, observed) {
  if (!identical(fit$status, "available") || !isTRUE(fit$effective_forecast > 0L)) return(NULL)
  tables <- fit$finals$tables
  points <- list(y = fit$preadjustment$tables$a1a, t = tables$d12a, sa = tables$d11a,
                 s = tables$d16a, cal = tables$d18a)
  if (any(lengths(points) != fit$effective_forecast))
    stop("Incomplete automatic X-13 forecast tables.", call. = FALSE)
  mode <- switch(fit$mode_effective, additive = "Additive", multiplicative = "Multiplicative")
  fields <- forecast_component_names()
  reasons <- stats::setNames(as.list(rep("The fit supplies no forecast standard errors.", length(fields))), fields)
  errors <- list()
  if (identical(fit$series_forecast_stdev_status, "available")) {
    if (length(fit$series_forecast_stdev) != fit$effective_forecast)
      stop("Incomplete stock X-13 original-series forecast standard errors.", call. = FALSE)
    errors$y <- fit$series_forecast_stdev
    reasons$y <- NULL
  } else if (!is.null(fit$series_forecast_stdev_status)) {
    reasons$y <- paste("X-13 forecast uncertainty:", fit$series_forecast_stdev_status)
  }
  new_forecast_contract(points, standard_errors = errors, observed = observed, mode = mode,
                        point_reasons = list(i = "The fit supplies no final irregular forecast table."),
                        error_reasons = reasons)
}

forecasts_from_result <- function(result, output, observed) {
  if (inherits(result, c("persephone_native_restricted_fit", "persephone_native_tramo_fit",
                         "persephone_native_automatic_fit"))) return(result$forecasts)
  # Evaluating the observed domain also checks hierarchy aggregation inputs.
  force(observed)
  if (inherits(result$forecasts, "persephone_forecasts")) return(result$forecasts)
  # Accommodate results created before the normalized forecast contract existed.
  forecasts_from_output(output, observed, result$method, result$specification$parameters)
}

aggregate_forecast_contracts <- function(forecasts, weights = NULL,
                                        aggregation = "selected_children") {
  if (!length(forecasts) || all(vapply(forecasts, is.null, logical(1)))) return(NULL)
  if (any(vapply(forecasts, function(x) {
    !is.null(x) && !inherits(x, "persephone_forecasts")
  }, logical(1)))) {
    stop("Each component forecast must use the Persephone forecast contract.", call. = FALSE)
  }
  component_names <- names(forecasts)
  if (is.null(component_names)) component_names <- paste0("component", seq_along(forecasts))
  names(forecasts) <- component_names
  # Each output field has its own domain (TRAMO calendar forecasts and errors
  # can be shorter than level forecasts). Validate equality across children
  # separately for each field when it is actually aggregated below.
  representative <- lapply(forecasts, function(x) {
    if (is.null(x)) return(NULL)
    x$points[!vapply(x$points, is.null, logical(1))][[1]]
  })
  representative <- representative[!vapply(representative, is.null, logical(1))]
  if (length(forecasts) == 1L) {
    hts_align_weights(weights, representative[[1]], component_names)
    result <- forecasts[[1]]
    result$aggregation <- aggregation
    return(result)
  }

  fields <- forecast_component_names()
  points <- stats::setNames(vector("list", length(fields)), fields)
  reasons <- list()
  additive <- all(vapply(forecasts, function(x) identical(x$mode, "Additive"), logical(1)))
  for (field in fields) {
    values <- lapply(forecasts, function(x) x$points[[field]])
    absent <- component_names[vapply(values, is.null, logical(1))]
    if (length(absent)) {
      reasons[[field]] <- paste0("Forecast unavailable for components: ", paste(absent, collapse = ", "), ".")
    } else if (field %in% c("s", "i", "cal") && !additive) {
      reasons[[field]] <- paste0("Cannot aggregate ", field,
        " across multiplicative, mixed or unknown decomposition scales.")
    } else {
      points[[field]] <- hts_aggregate_series(values, weights)
    }
  }
  uncertainty <- stats::setNames(rep(list(paste0("Cross-component forecast covariance is unavailable; ",
    "standard errors are not summed.")), length(fields)), fields)
  new_forecast_contract(points, mode = if (additive) "Additive" else "unknown",
                        aggregation = aggregation, point_reasons = reasons,
                        error_reasons = uncertainty)
}

aggregate_component_forecasts <- function(components, weights = NULL,
                                         fully_indirect = FALSE) {
  forecasts <- lapply(components, function(component) {
    if (fully_indirect && inherits(component, "hierarchicalTimeSeries")) {
      component$forecastsIndirect
    } else component$forecasts
  })
  aggregate_forecast_contracts(forecasts, weights,
    aggregation = if (fully_indirect) "fully_indirect" else "selected_children")
}
