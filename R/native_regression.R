# Internal concentrated regression likelihood with fixed SARIMA coefficients.
# This is not automatic model selection or a native seasonal-adjustment fit.
native_regression_flag <- function(value, name) {
  if (!is.logical(value) || !is.null(dim(value)) || length(value) != 1L || is.na(value)) {
    stop(name, " must be one nonmissing logical value.", call. = FALSE)
  }
  value
}

native_regression_order <- function(value, name) {
  if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) ||
      length(value) != 1L || is.na(value) || !is.finite(value) ||
      value != floor(value) || !value %in% 0:2) {
    stop(name, " must be a whole number in 0..2.", call. = FALSE)
  }
  as.integer(value)
}

native_regression_coefficients <- function(coefficients, labels) {
  result <- stats::setNames(rep(NA_real_, length(labels)), labels)
  if (is.null(coefficients)) return(result)
  if (!is.numeric(coefficients) || is.complex(coefficients) || !is.null(dim(coefficients)) ||
      any(is.nan(coefficients)) || any(!is.na(coefficients) & !is.finite(coefficients))) {
    stop("coefficients must be a named numeric vector of finite values or NA for free coefficients.", call. = FALSE)
  }
  if (length(coefficients) && (is.null(names(coefficients)) || anyNA(names(coefficients)) ||
      any(!nzchar(names(coefficients))) || anyDuplicated(names(coefficients)) ||
      any(!names(coefficients) %in% labels))) {
    stop("coefficients must have distinct names matching the design columns.", call. = FALSE)
  }
  result[names(coefficients)] <- coefficients
  result
}

native_regression_inputs <- function(series, regressors = NULL, coefficients = NULL,
    d = 0L, seasonal_d = 0L, mean = 0, transformation = "none",
    estimation_start = NULL, estimation_end = NULL, include_mean = FALSE,
    include_drift = FALSE, forecast_horizon = 0L, backcast_horizon = 0L) {
  include_mean <- native_regression_flag(include_mean, "include_mean")
  include_drift <- native_regression_flag(include_drift, "include_drift")
  d <- native_regression_order(d, "d")
  seasonal_d <- native_regression_order(seasonal_d, "seasonal_d")
  if (include_mean && d + seasonal_d > 0L) {
    stop("include_mean requires a stationary model; use include_drift for a differenced mean.", call. = FALSE)
  }
  if (include_drift && d + seasonal_d == 0L) {
    stop("include_drift requires differencing; use include_mean for a stationary model.", call. = FALSE)
  }
  if (include_mean && (!is.numeric(mean) || is.complex(mean) || length(mean) != 1L ||
                      is.na(mean) || mean != 0)) {
    stop("Use mean = 0 with include_mean to avoid two level-mean specifications.", call. = FALSE)
  }
  columns <- native_regressor_columns(regressors)
  generated <- c(character(), if (include_mean) "mean", if (include_drift) "drift")
  if (any(generated %in% names(columns))) stop("Generated mean/drift names conflict with supplied regressors.", call. = FALSE)
  if (length(columns) + length(generated) > 32L) stop("Regression likelihood supports at most 32 design columns.", call. = FALSE)
  window <- native_likelihood_window(series, estimation_start, estimation_end)
  inputs <- native_prepare_inputs(window$series, columns, transformation,
                                   forecast_horizon, backcast_horizon)
  if (include_mean) inputs$regressors <- cbind(inputs$regressors, mean = 1)
  if (include_drift) {
    # Delta(B)x_t=1. Changing the origin adds only a homogeneous component,
    # which the fixed initial-history diffuse measure absorbs. Retain a single
    # one-based source index (first input observation t=1) across windows and
    # requested future/past contexts.
    time <- seq_len(inputs$required_domain$length) + window$source_rows[1L] -
      inputs$backcast_horizon - 1
    order <- d + seasonal_d
    drift <- time^order / (factorial(order) * inputs$observed_domain$frequency^seasonal_d)
    if (any(!is.finite(drift))) stop("Generated drift exceeds the finite numeric range.", call. = FALSE)
    inputs$regressors <- cbind(inputs$regressors, drift = drift)
  }
  labels <- colnames(inputs$regressors)
  if (is.null(labels)) labels <- character()
  fixed <- native_regression_coefficients(coefficients, labels)
  design <- inputs$regressors[inputs$observed_rows, , drop = FALSE]
  list(window = window, inputs = inputs, labels = labels, fixed = fixed,
       design = design, generated = generated, d = d, seasonal_d = seasonal_d)
}

native_regression_likelihood <- function(series, regressors = NULL, coefficients = NULL,
    ar = numeric(), ma = numeric(), seasonal_ar = numeric(), seasonal_ma = numeric(),
    d = 0L, seasonal_d = 0L, mean = 0, variance = 1, transformation = "none",
    estimation_start = NULL, estimation_end = NULL, include_mean = FALSE,
    include_drift = FALSE, forecast_horizon = 0L, backcast_horizon = 0L,
    rank_tolerance = 1e-10) {
  prepared <- native_regression_inputs(series, regressors, coefficients, d,
    seasonal_d, mean, transformation, estimation_start, estimation_end,
    include_mean, include_drift, forecast_horizon, backcast_horizon)
  window <- prepared$window
  inputs <- prepared$inputs
  labels <- prepared$labels
  fixed <- prepared$fixed
  design <- prepared$design
  generated <- prepared$generated
  d <- prepared$d
  seasonal_d <- prepared$seasonal_d
  result <- .native_regression_likelihood(as.numeric(inputs$transformed), design,
    unname(fixed), ar, ma, seasonal_ar, seasonal_ma, inputs$observed_domain$frequency,
    d, seasonal_d, mean, variance, rank_tolerance)
  names(result$coefficients) <- labels
  free_names <- labels[is.na(fixed)]
  for (field in c("unscaled_covariance", "fixed_covariance", "ml_covariance", "residual_covariance")) {
    dimnames(result[[field]]) <- list(free_names, free_names)
  }
  colnames(result$whitened_design) <- free_names
  adjustment <- native_likelihood_adjustment(inputs,
    d + inputs$observed_domain$frequency * seasonal_d, window$source_rows)
  as_series <- function(values) stats::ts(values, start = inputs$observed_domain$start,
                                          frequency = inputs$observed_domain$frequency)
  identified <- all(is.finite(result$coefficients))
  traces <- NULL
  final_kernel <- NULL
  if (identified) {
    final_kernel <- .native_sarima_likelihood(result$residual_series, ar, ma,
      seasonal_ar, seasonal_ma, inputs$observed_domain$frequency, d, seasonal_d, mean, variance)
    traces <- native_likelihood_traces(final_kernel, inputs$observed_domain)
    traces$predictions <- traces$predictions + as_series(result$fitted_effect)
  }
  context_effect <- if (identified) drop(inputs$regressors %*% result$coefficients) else
    rep(NA_real_, inputs$required_domain$length)
  if (identified && any(!is.finite(context_effect))) {
    stop("Fitted effects exceed the finite numeric range in the requested context.", call. = FALSE)
  }
  states <- rep(if (identified) "estimated" else "unidentified", length(labels))
  states[!is.na(fixed)] <- "fixed"
  names(states) <- labels
  structure(list(schema_version = 1L, backend = "native",
    operation = "fixed_sarima_regression_likelihood", initialization = "fresh_prior_at_span_start",
    parameters = list(ar = as.numeric(ar), ma = as.numeric(ma),
      seasonal_ar = as.numeric(seasonal_ar), seasonal_ma = as.numeric(seasonal_ma),
      d = d, seasonal_d = seasonal_d, period = inputs$observed_domain$frequency,
      mean = result$mean, variance = result$variance),
    span = list(source_domain = window$source_domain, estimation_domain = inputs$observed_domain,
                source_rows = window$source_rows), inputs = inputs,
    regression = list(status = result$status, coefficients = result$coefficients,
      coefficient_states = states, specified_coefficients = fixed, free_names = free_names,
      rank = result$rank, free_count = result$free_count, residual_df = result$residual_df,
      covariance_status = result$covariance_status, rank_tolerance = result$rank_tolerance,
      whitening_norm_ratio = stats::setNames(result$whitening_norm_ratio, free_names),
      numerically_zero_columns = stats::setNames(result$numerically_zero_columns, free_names),
      unscaled_covariance = result$unscaled_covariance,
      covariance_fixed_scale = result$fixed_covariance,
      covariance_ml = result$ml_covariance, covariance_residual = result$residual_covariance,
      finite_rows = result$finite_rows,
      finite_source_rows = window$source_rows[result$finite_rows],
      whitened_response = result$whitened_response, whitened_design = result$whitened_design,
      generated_columns = generated, drift_policy = if (include_drift) "unit_differenced_mean_one_based_source_index" else NULL),
    effects = list(fixed = as_series(result$fixed_effect), fitted = as_series(result$fitted_effect),
      residual_series = as_series(result$residual_series),
      context = stats::ts(context_effect, start = inputs$required_domain$start,
                           frequency = inputs$required_domain$frequency)),
    likelihood = list(status = result$likelihood_status, profile_status = result$profile_status,
      normalization = "fixed_initial_diffuse_coordinates", objective = "concentrated_ml",
      transformed_fixed = result$fixed_log_likelihood,
      transformed_profiled = result$profiled_log_likelihood,
      adjusted_fixed = result$fixed_log_likelihood + adjustment$log_jacobian,
      adjusted_profiled = result$profiled_log_likelihood + adjustment$log_jacobian,
      profiled_variance = result$profiled_variance,
      observed_count = result$observed_count, diffuse_count = result$diffuse_count,
      finite_count = result$finite_count,
      nparams_profiled = if (identified) result$free_count + 1L else NA_integer_),
    adjustment = adjustment, traces = traces,
    trace_uncertainty = "conditional_on_coefficients", kernel = final_kernel,
    regression_kernel = result), class = "persephone_native_regression_likelihood")
}
