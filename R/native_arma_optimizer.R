# Fixed-order optimization only; no automatic model selection or native SA fit.
native_arma_optimize <- function(objective,
    coordinates = objective$parameters$initial_coordinates, control = list()) {
  if (!inherits(objective, "persephone_native_arma_objective") ||
      !identical(objective$schema_version, 1L)) stop("Unsupported ARMA objective contract.", call. = FALSE)
  coordinates <- native_arma_coordinates(objective$parameters, coordinates)
  p <- objective$parameters
  data <- objective$prepared
  result <- .native_arma_optimize(as.numeric(data$inputs$transformed), data$design,
    unname(data$fixed), p$specification, p$initial, coordinates, objective$period,
    objective$d, objective$seasonal_d, objective$mean, objective$rank_tolerance,
    p$reflection_margin, objective$adjustment$log_jacobian, control)
  labels <- names(p$initial_coordinates)
  names(result$coordinates) <- names(result$initial_coordinates) <- labels
  working <- sub("[.]reflection", ".unit_reflection", labels)
  for (field in c("working_coordinates", "gradient", "gradient_error", "difference_steps", "difference_schemes")) {
    names(result[[field]]) <- working
  }
  for (field in c("initial", "final")) {
    if (!is.null(result[[field]]$likelihood)) names(result[[field]]$likelihood$regression_coefficients) <- data$labels
  }
  structure(c(result, list(objective = objective$objective,
    normalization = objective$normalization, adjustment = objective$adjustment)),
    class = "persephone_native_arma_optimization")
}
