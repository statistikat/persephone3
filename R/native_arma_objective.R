# Internal serializable parameter/likelihood contract; no optimizer is run here.
native_arma_parameter_map <- function(ar = numeric(), ma = numeric(),
    seasonal_ar = numeric(), seasonal_ma = numeric(), initial = NULL,
    reflection_margin = 1e-8) {
  structure(.native_arma_parameter_map(list(ar = ar, ma = ma,
    seasonal_ar = seasonal_ar, seasonal_ma = seasonal_ma), initial,
    reflection_margin), class = "persephone_native_arma_parameters")
}

native_arma_coordinates <- function(parameters, coordinates) {
  if (!inherits(parameters, "persephone_native_arma_parameters") ||
      !identical(parameters$schema_version, 1L)) stop("Unsupported ARMA parameter map.", call. = FALSE)
  if (!is.null(names(coordinates)) &&
      !identical(names(coordinates), names(parameters$initial_coordinates))) {
    stop("Named ARMA coordinates must follow the parameter map's names and order.", call. = FALSE)
  }
  coordinates
}

native_arma_decode <- function(parameters, coordinates = parameters$initial_coordinates) {
  coordinates <- native_arma_coordinates(parameters, coordinates)
  .native_arma_decode(parameters$specification, parameters$initial, coordinates,
                      parameters$reflection_margin)
}

native_arma_scalar <- function(value, name, fallback) {
  if (is.null(value)) return(fallback)
  if (!is.numeric(value) || is.complex(value) || is.factor(value) ||
      !is.null(dim(value)) || length(value) != 1L || !is.finite(value)) {
    stop(name, " must be one finite numeric scalar.", call. = FALSE)
  }
  as.numeric(value)
}

native_arma_objective_setup <- function(series, regressors = NULL, coefficients = NULL,
    ar = numeric(), ma = numeric(), seasonal_ar = numeric(), seasonal_ma = numeric(),
    initial = NULL, d = 0L, seasonal_d = 0L, mean = 0, transformation = "none",
    estimation_start = NULL, estimation_end = NULL, include_mean = FALSE,
    include_drift = FALSE, forecast_horizon = 0L, backcast_horizon = 0L,
    rank_tolerance = 1e-10, reflection_margin = 1e-8) {
  parameters <- native_arma_parameter_map(ar, ma, seasonal_ar, seasonal_ma,
                                         initial, reflection_margin)
  mean <- native_arma_scalar(mean, "mean", 0)
  rank_tolerance <- native_arma_scalar(rank_tolerance, "rank_tolerance", 1e-10)
  if (rank_tolerance <= 0 || rank_tolerance >= 1) stop("rank_tolerance must be strictly between zero and one.", call. = FALSE)
  prepared <- native_regression_inputs(series, regressors, coefficients, d,
    seasonal_d, mean, transformation, estimation_start, estimation_end,
    include_mean, include_drift, forecast_horizon, backcast_horizon)
  period <- prepared$inputs$observed_domain$frequency
  d <- prepared$d
  seasonal_d <- prepared$seasonal_d
  if (d + seasonal_d > 0L && mean != 0) stop("Integrated SARIMA requires mean zero; use include_drift or a drift regressor.", call. = FALSE)
  orders <- lengths(parameters$specification)
  states <- max(orders["ar"] + period * orders["seasonal_ar"],
                1 + orders["ma"] + period * orders["seasonal_ma"])
  if (states > 16L || states + d + period * seasonal_d > 32L) {
    stop("ARMA objective exceeds the 16 stationary / 32 total state bounds.", call. = FALSE)
  }
  adjustment <- native_likelihood_adjustment(prepared$inputs,
    d + period * seasonal_d, prepared$window$source_rows)
  structure(list(schema_version = 1L, backend = "native",
    operation = "concentrated_sarima_objective", parameters = parameters,
    prepared = prepared, period = period, d = d, seasonal_d = seasonal_d,
    mean = mean, rank_tolerance = rank_tolerance, adjustment = adjustment,
    normalization = "fixed_initial_diffuse_coordinates",
    objective = "negative_adjusted_concentrated_ml", variance_estimator = "Q_over_nfinite"),
    class = "persephone_native_arma_objective")
}

native_arma_objective <- function(objective,
    coordinates = objective$parameters$initial_coordinates) {
  if (!inherits(objective, "persephone_native_arma_objective") ||
      !identical(objective$schema_version, 1L)) stop("Unsupported ARMA objective contract.", call. = FALSE)
  coordinates <- native_arma_coordinates(objective$parameters, coordinates)
  p <- objective$parameters
  data <- objective$prepared
  result <- .native_arma_objective(as.numeric(data$inputs$transformed), data$design,
    unname(data$fixed), p$specification, p$initial, coordinates, objective$period,
    objective$d, objective$seasonal_d, objective$mean, objective$rank_tolerance,
    p$reflection_margin, objective$adjustment$log_jacobian)
  if (!is.null(result$likelihood)) names(result$likelihood$regression_coefficients) <- data$labels
  structure(c(result, list(objective = objective$objective,
    normalization = objective$normalization, adjustment = objective$adjustment,
    coordinates = coordinates)), class = "persephone_native_arma_objective_value")
}
