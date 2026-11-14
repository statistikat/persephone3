# Accurate conditional Gaussian prediction errors at internal missing rows.
# This API has no legacy reporting option; those points are comparison-only.
native_interpolation_information <- function(objective,
    coordinates=objective$parameters$initial_coordinates,scale="ml") {
  if(!inherits(objective,"persephone_native_arma_objective") ||
      !identical(objective$schema_version,1L))stop("Unsupported ARMA objective contract.",call.=FALSE)
  if(!is.character(scale) || length(scale)!=1L || is.na(scale) || !is.null(dim(scale)) ||
      !scale %in% c("ml","residual_df"))stop("Prediction scale must be 'ml' or 'residual_df'.",call.=FALSE)
  coordinates <- native_arma_coordinates(objective$parameters,coordinates)
  p <- objective$parameters;data <- objective$prepared
  result <- .native_interpolation_information(as.numeric(data$inputs$transformed),data$design,
    unname(data$fixed),p$specification,p$initial,coordinates,objective$period,
    objective$d,objective$seasonal_d,objective$mean,objective$rank_tolerance,
    p$reflection_margin,objective$adjustment$log_jacobian,scale)
  domain <- data$inputs$observed_domain;source <- data$window$source_rows
  result$source_rows <- source[result$rows]
  result$transformed <- data$inputs$transformed;result$original <- data$inputs$original
  if(result$point_available)result$transformed[result$rows] <- result$points_transformed
  original <- if(length(result$rows))tryCatch(.native_inverse_transform(result$points_transformed,
    data$inputs$transformation),error=identity)else numeric()
  result$original_point_status <- if(result$point_available)"available"else"point_unavailable"
  result$points_original <- rep(NA_real_,length(result$rows))
  if(inherits(original,"error")) {
    result$original_point_status <- "original_scale_range"
    result$original_point_message <- conditionMessage(original)
  } else {
    result$points_original <- original
    result$original[result$rows] <- original
  }
  result$standard_errors_scale <- "transformed"
  result$standard_errors_original <- if(data$inputs$transformation=="none")result$standard_errors else
    rep(NA_real_,length(result$rows))
  result$original_uncertainty_status <- if(data$inputs$transformation=="none")result$status else "lognormal_moments_not_computed"
  aligned <- rep(NA_real_,domain$length)
  if(result$available)aligned[result$rows] <- result$standard_errors
  result$aligned_standard_errors <- stats::ts(aligned,start=domain$start,frequency=domain$frequency)
  labels <- as.character(result$source_rows)
  for(field in c("covariance","known_regression_covariance","regression_covariance")) {
    if(!is.null(result[[field]]))dimnames(result[[field]]) <- list(labels,labels)
    else if(result$status=="no_internal_missing")result[[field]] <- matrix(numeric(),0L,0L)
  }
  result$unfilled_rows <- which(is.na(result$transformed))
  result$unfilled_source_rows <- source[result$unfilled_rows]
  result$original_missing <- data$inputs$missing
  result$point_convention <- if(data$inputs$transformation=="log")"exponentiated_transformed_point"else"Gaussian_conditional_point"
  result$mean_adjustment <- "none"
  result$scale_convention <- if(scale=="ml")"Q_over_n_finite"else"Q_over_n_minus_free_regression_minus_free_ARMA"
  result$variance_uncertainty <- "plug_in_scale_no_scale_parameter_propagation"
  result$comparison_only <- FALSE
  result$span <- list(source_domain=data$window$source_domain,estimation_domain=domain,source_rows=source)
  if(!is.null(result$center$likelihood))names(result$center$likelihood$regression_coefficients) <- data$labels
  result$adjustment <- objective$adjustment;result$transformation <- data$inputs$transformation
  result$backend <- "native"
  structure(result,class="persephone_native_interpolation_information")
}
