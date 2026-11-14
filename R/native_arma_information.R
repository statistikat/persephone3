# Observed profile curvature is computed separately from optimization and from
# the data-independent transfer screen. It is not JDemetra covariance parity.
native_arma_information <- function(objective,
    coordinates = objective$parameters$initial_coordinates, control = list(),
    method = "profile_ml", covariance = "accurate", legacy_state = NULL) {
  if (!inherits(objective, "persephone_native_arma_objective") ||
      !identical(objective$schema_version, 1L)) stop("Unsupported ARMA objective contract.", call. = FALSE)
  coordinates <- native_arma_coordinates(objective$parameters, coordinates)
  p <- objective$parameters; data <- objective$prepared
  result <- .native_arma_information(as.numeric(data$inputs$transformed), data$design,
    unname(data$fixed), p$specification, p$initial, coordinates, objective$period,
    objective$d, objective$seasonal_d, objective$mean, objective$rank_tolerance,
    p$reflection_margin, objective$adjustment$log_jacobian, control, method, covariance, legacy_state)
  labels <- sub("[.]reflection", ".coefficient", names(p$initial_coordinates))
  for (field in c("coefficients", "gradient", "gradient_error", "difference_steps")) {
    if (length(result[[field]]) == length(labels)) names(result[[field]]) <- labels
  }
  for (field in c("hessian", "hessian_error", "covariance", "normal_information", "normal_information_error",
      "accurate_covariance", "legacy_information")) if (!is.null(result[[field]])) {
    dimnames(result[[field]]) <- list(labels, labels)
  }
  for (field in c("residual_jacobian", "residual_jacobian_error", "legacy_jacobian")) if (!is.null(result[[field]])) {
    dimnames(result[[field]]) <- list(as.character(data$window$source_rows[result$residual_center$rows]),labels)
  }
  if (!is.null(result$residual_center)) {
    result$residual_center$source_rows <- data$window$source_rows[result$residual_center$rows]
    result$residual_center$missing_source_rows <- data$window$source_rows[result$residual_center$missing_rows]
    names(result$residual_center$regression_coefficients) <- data$labels
  }
  result$standard_errors <- setNames(rep(NA_real_, length(labels)), labels)
  if (!is.null(result$covariance)) result$standard_errors[] <- sqrt(diag(result$covariance))
  result$parameter_diagnostics <- native_arma_identification_labels(result$parameter_diagnostics, p)
  if (!is.null(result$center$likelihood)) names(result$center$likelihood$regression_coefficients) <- data$labels
  result$objective <- "negative_concentrated_log_likelihood_without_constant_observation_Jacobian"
  result$adjustment <- objective$adjustment
  structure(result, class = "persephone_native_arma_information")
}
