# Fitted finite innovations and complete-span comparison residuals are different
# time-aligned vectors when observations are missing. Neither is a forecast or
# a leverage-studentized residual; coefficients use the entire selected window.
native_arma_residuals <- function(objective,
    coordinates=objective$parameters$initial_coordinates, scale="ml") {
  if (!inherits(objective,"persephone_native_arma_objective") ||
      !identical(objective$schema_version,1L)) stop("Unsupported ARMA objective contract.",call.=FALSE)
  if (!is.character(scale) || length(scale)!=1L || is.na(scale) || !is.null(dim(scale)) ||
      !scale %in% c("ml","residual_df")) stop("Residual scale must be 'ml' or 'residual_df'.",call.=FALSE)
  coordinates <- native_arma_coordinates(objective$parameters,coordinates)
  p <- objective$parameters;data <- objective$prepared
  result <- .native_arma_residuals(as.numeric(data$inputs$transformed),data$design,
    unname(data$fixed),p$specification,p$initial,coordinates,objective$period,
    objective$d,objective$seasonal_d,objective$mean,objective$rank_tolerance,
    p$reflection_margin,objective$adjustment$log_jacobian,scale)
  domain <- data$inputs$observed_domain;source <- data$window$source_rows
  n <- domain$length;missing <- data$inputs$missing
  as_series <- function(value)stats::ts(value,start=domain$start,frequency=domain$frequency)
  align <- function(value,rows,available) {
    answer <- rep(NA_real_,n)
    if (available) answer[rows] <- value
    as_series(answer)
  }
  observed <- result$observed
  observed$source_rows <- source[observed$rows]
  observed$unit_innovations <- align(observed$unit_innovations,observed$rows,result$available)
  observed$standardized_innovations <- align(observed$standardized_innovations,observed$rows,result$available)
  kind <- rep(if(result$available)"diffuse"else"unavailable",n)
  kind[missing] <- "missing"
  if(result$available)kind[observed$rows] <- "finite"
  observed$row_kind <- kind
  observed$basis <- "observed_finite_innovations_conditional_on_fitted_coefficients"
  result$observed <- observed
  complete <- result$complete
  complete$source_rows <- source[complete$rows]
  complete$missing_source_rows <- source[complete$missing_rows]
  complete$unit_residuals <- align(complete$unit_residuals,complete$rows,complete$available)
  complete$scaled_residuals <- align(complete$scaled_residuals,complete$rows,complete$available)
  complete$rows_were_missing <- missing[complete$rows]
  if(length(complete$regression_coefficients)==length(data$labels))names(complete$regression_coefficients) <- data$labels
  result$complete <- complete
  transformed <- data$inputs$transformed;original <- data$inputs$original
  imputed <- if(complete$available)complete$missing_rows else integer()
  point <- if(complete$available)complete$interpolated else numeric()
  original_point <- if(length(point)) .native_inverse_transform(point,data$inputs$transformation) else numeric()
  if(length(imputed)) {
    transformed[imputed] <- point;original[imputed] <- original_point
  }
  result$interpolation <- list(status=if(!complete$available)complete$status else
      if(length(imputed))"available"else"no_internal_missing",
    available=complete$available,rows=imputed,source_rows=source[imputed],
    points_transformed=point,points_original=original_point,
    transformed=transformed,original=original,
    unfilled_rows=which(is.na(transformed)),unfilled_source_rows=source[is.na(transformed)],
    original_missing=missing,
    point_convention=if(data$inputs$transformation=="log")"exponentiated_transformed_point"else"Gaussian_conditional_point",
    mean_adjustment="none",standard_errors=rep(NA_real_,length(imputed)),
    uncertainty_status=if(!complete$available)"point_unavailable"else if(length(imputed))"not_computed"else"not_required")
  if(!is.null(result$center$likelihood))names(result$center$likelihood$regression_coefficients) <- data$labels
  result$scale_convention <- if(result$scale=="ml")"Q_over_n_finite"else"Q_over_n_minus_free_regression_minus_free_ARMA"
  result$span <- list(source_domain=data$window$source_domain,estimation_domain=domain,source_rows=source)
  result$adjustment <- objective$adjustment
  result$transformation <- data$inputs$transformation
  result$backend <- "native"
  structure(result,class="persephone_native_arma_residuals")
}
