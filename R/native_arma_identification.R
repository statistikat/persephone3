# Structural screen only: no observation information or inferential covariance.
native_arma_identification_labels <- function(result, parameters) {
  labels <- sub("[.]reflection", ".coefficient", names(parameters$initial_coordinates))
  if (!is.null(result$numerator_jacobian)) {
    colnames(result$numerator_jacobian) <- labels
    rownames(result$numerator_jacobian) <- paste0("lag", seq_len(nrow(result$numerator_jacobian)) - 1L)
    names(result$column_norms) <- labels
  }
  structure(result, class = "persephone_native_arma_identification")
}

native_arma_identification <- function(parameters, period,
    coordinates = parameters$initial_coordinates, rank_tolerance = 1e-10,
    near_tolerance = 1e-4) {
  coordinates <- native_arma_coordinates(parameters, coordinates)
  result <- .native_arma_identification(parameters$specification, parameters$initial,
    coordinates, period, parameters$reflection_margin, rank_tolerance, near_tolerance)
  native_arma_identification_labels(result, parameters)
}
