# Internal preparation contract for future native model pipelines. This does not
# construct a native specification or claim a supported seasonal-adjustment fit.
native_regular_domain <- function(series, name = "series") {
  if (!stats::is.ts(series) || !is.numeric(series) || is.complex(series) ||
      !is.null(dim(series)) || !length(series)) {
    stop(name, " must be a nonempty univariate numeric ts.", call. = FALSE)
  }
  frequency <- stats::frequency(series)
  if (!frequency %in% c(4, 12)) {
    stop(name, " must have monthly or quarterly frequency.", call. = FALSE)
  }
  ordinal <- stats::tsp(series)[1L] * frequency
  if (!is.finite(ordinal) || abs(ordinal - round(ordinal)) > 1e-6) {
    stop(name, " must start on an exact monthly or quarterly period.", call. = FALSE)
  }
  ordinal <- round(ordinal)
  year <- floor(ordinal / frequency)
  period <- ordinal - year * frequency + 1
  domain <- .native_time_domain(year, period, frequency, length(series))
  domain$start_ordinal <- ordinal
  domain
}

native_horizon <- function(value, name) {
  if (!is.numeric(value) || is.complex(value) || length(value) != 1L ||
      is.na(value) || !is.finite(value) || value < 0 || value != floor(value) ||
      value > .Machine$integer.max) {
    stop(name, " must be a finite nonnegative integer within the supported range.", call. = FALSE)
  }
  as.integer(value)
}

native_regressor_columns <- function(regressors) {
  if (is.null(regressors)) return(list())
  if (stats::is.ts(regressors) && is.matrix(regressors)) {
    if (!is.numeric(regressors) || is.complex(regressors)) {
      stop("Regressors must be numeric.", call. = FALSE)
    }
    labels <- colnames(regressors)
    result <- lapply(seq_len(ncol(regressors)), function(i) regressors[, i])
    names(result) <- labels
  } else if (is.list(regressors) && !is.data.frame(regressors)) {
    result <- regressors
  } else {
    stop("Regressors must be a named list of univariate ts or a named mts.", call. = FALSE)
  }
  if (length(result) && (is.null(names(result)) || anyNA(names(result)) ||
      any(!nzchar(names(result))) || anyDuplicated(names(result)))) {
    stop("Regressors must have distinct nonempty names.", call. = FALSE)
  }
  result
}

# Regressors must cover the entire requested observed/backcast/forecast domain.
# No recycling, truncation of requested horizons, padding or extrapolation is
# performed. Missing response observations are allowed, missing regressors in
# the required domain are not. Extra supplied history/future periods are cropped.
native_prepare_inputs <- function(series, regressors = NULL, transformation = "none",
                                  forecast_horizon = 0L, backcast_horizon = 0L) {
  domain <- native_regular_domain(series)
  forecast_horizon <- native_horizon(forecast_horizon, "forecast_horizon")
  backcast_horizon <- native_horizon(backcast_horizon, "backcast_horizon")
  count <- length(series) + as.double(forecast_horizon) + as.double(backcast_horizon)
  if (count > .Machine$integer.max) {
    stop("Required domain exceeds the supported matrix row range.", call. = FALSE)
  }
  if (!is.character(transformation) || length(transformation) != 1L ||
      is.na(transformation) || !transformation %in% c("none", "log")) {
    stop("transformation must be 'none' or 'log'.", call. = FALSE)
  }
  start <- domain$start_ordinal - backcast_horizon
  year <- floor(start / domain$frequency)
  period <- start - year * domain$frequency + 1
  required <- .native_time_domain(year, period, domain$frequency, count)
  required$start_ordinal <- start
  columns <- native_regressor_columns(regressors)
  if (count * length(columns) > .Machine$integer.max) {
    stop("Regressor matrix exceeds the supported element range.", call. = FALSE)
  }
  values <- matrix(numeric(), nrow = count, ncol = 0L)
  if (length(columns)) {
    aligned <- lapply(seq_along(columns), function(i) {
      column <- columns[[i]]
      label <- paste0("Regressor '", names(columns)[i], "'")
      supplied <- native_regular_domain(column, label)
      if (supplied$frequency != domain$frequency) {
        stop(label, " frequency does not match the response.", call. = FALSE)
      }
      offset <- start - supplied$start_ordinal
      if (offset < 0 || offset + count > length(column)) {
        stop(label, " does not cover the complete observed/backcast/forecast domain.", call. = FALSE)
      }
      value <- as.numeric(column)[seq_len(count) + offset]
      if (any(!is.finite(value))) {
        stop(label, " contains missing or nonfinite values in the required domain.", call. = FALSE)
      }
      value
    })
    values <- do.call(cbind, aligned)
    colnames(values) <- names(columns)
  }
  transformed <- .native_transform(as.numeric(series), transformation)
  as_series <- function(value) stats::ts(value, start = domain$start,
                                        frequency = domain$frequency)
  structure(list(schema_version = 1L, original = series,
                 transformed = as_series(transformed$values),
                 missing = transformed$missing,
                 log_jacobian = as_series(transformed$log_jacobian),
                 transformation = transformation, observed_domain = domain,
                 required_domain = required,
                 regressors = values,
                 observed_rows = backcast_horizon + seq_along(series),
                 forecast_horizon = forecast_horizon, backcast_horizon = backcast_horizon),
            class = "persephone_native_inputs")
}
