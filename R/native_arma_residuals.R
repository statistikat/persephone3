# Fitted finite innovations and complete-span comparison residuals are different
# time-aligned vectors when observations are missing. Neither is a forecast or
# a leverage-studentized residual; coefficients use the entire selected window.
native_arma_residuals <- function(objective,
    coordinates=objective$parameters$initial_coordinates, scale="ml", reporting="accurate") {
  if (!inherits(objective,"persephone_native_arma_objective") ||
      !identical(objective$schema_version,1L)) stop("Unsupported ARMA objective contract.",call.=FALSE)
  if (!is.character(scale) || length(scale)!=1L || is.na(scale) || !is.null(dim(scale)) ||
      !scale %in% c("ml","residual_df")) stop("Residual scale must be 'ml' or 'residual_df'.",call.=FALSE)
  if (!is.character(reporting) || length(reporting)!=1L || is.na(reporting) || !is.null(dim(reporting)) ||
      !reporting %in% c("accurate","legacy")) stop("Residual reporting must be 'accurate' or 'legacy'.",call.=FALSE)
  coordinates <- native_arma_coordinates(objective$parameters,coordinates)
  p <- objective$parameters;data <- objective$prepared
  result <- .native_arma_residuals(as.numeric(data$inputs$transformed),data$design,
    unname(data$fixed),p$specification,p$initial,coordinates,objective$period,
    objective$d,objective$seasonal_d,objective$mean,objective$rank_tolerance,
    p$reflection_margin,objective$adjustment$log_jacobian,scale,reporting,data$inputs$transformation)
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
  result$accurate_status <- result$status
  result$accurate_available <- result$available
  result$comparison_only <- reporting=="legacy"
  compact_series <- function(value,rows) {
    if(!length(rows))return(numeric())
    ordinal <- domain$start_ordinal+rows[1L]-1L
    stats::ts(value,start=c(ordinal %/% domain$frequency,ordinal %% domain$frequency+1L),frequency=domain$frequency)
  }
  reported <- list(status=complete$status,available=complete$available,basis=complete$basis,
    calendar_rows=complete$rows,source_rows=complete$source_rows,
    row_alignment="complete_finite_calendar",sum_squares=complete$sum_squares,
    unit_residuals=compact_series(as.numeric(complete$unit_residuals[complete$rows]),complete$rows),
    scaled_residuals=compact_series(as.numeric(complete$scaled_residuals[complete$rows]),complete$rows))
  if(reporting=="legacy") {
    legacy <- result$legacy
    result$accurate_interpolation <- result$interpolation
    result$interpolation$comparison_only <- TRUE
    result$interpolation$point_convention <- "jdplus_3.9.0_legacy_reported_point"
    result$interpolation$uncertainty_status <- "comparison_only"
    result$interpolation$status <- legacy$status
    result$interpolation$available <- legacy$available
    original_point <- if(legacy$available && length(imputed))tryCatch(
      .native_inverse_transform(legacy$interpolated_transformed,data$inputs$transformation),error=identity)else numeric()
    if(inherits(original_point,"error")) {
      legacy$available <- FALSE;legacy$status <- "original_scale_range"
      legacy$message <- conditionMessage(original_point)
      result$interpolation$status <- legacy$status;result$interpolation$available <- FALSE
    }
    result$interpolation$transformed <- data$inputs$transformed
    result$interpolation$original <- data$inputs$original
    result$interpolation$points_transformed <- result$interpolation$points_original <- numeric()
    if(legacy$available) {
      result$interpolation$points_transformed <- legacy$interpolated_transformed
      result$interpolation$points_original <- original_point
      result$interpolation$transformed[imputed] <- legacy$interpolated_transformed
      result$interpolation$original[imputed] <- original_point
    } else {
      result$interpolation$rows <- result$interpolation$source_rows <- integer()
      result$interpolation$standard_errors <- numeric()
    }
    result$interpolation$unfilled_rows <- which(is.na(result$interpolation$transformed))
    result$interpolation$unfilled_source_rows <- source[is.na(result$interpolation$transformed)]
    reported <- list(status=legacy$status,available=legacy$available,basis=legacy$basis,
      calendar_rows=legacy$calendar_rows,source_rows=if(legacy$compressed)integer()else source[legacy$calendar_rows],
      row_alignment=if(legacy$compressed)"reported_end_aligned_not_observation_mapping"else"complete_finite_calendar",
      sum_squares=legacy$sum_squares,unit_residuals=compact_series(legacy$unit_residuals,legacy$calendar_rows),
      scaled_residuals=compact_series(legacy$scaled_residuals,legacy$calendar_rows))
    result$available <- result$available && legacy$available
    if(!legacy$available)result$status <- paste0("legacy_",legacy$status)
    result$legacy <- legacy
  }
  reported$comparison_only <- result$comparison_only
  reported$normalization_source <- "accurate_likelihood"
  result$reported <- reported
  if(!is.null(result$center$likelihood))names(result$center$likelihood$regression_coefficients) <- data$labels
  result$scale_convention <- if(result$scale=="ml")"Q_over_n_finite"else"Q_over_n_minus_free_regression_minus_free_ARMA"
  result$span <- list(source_domain=data$window$source_domain,estimation_domain=domain,source_rows=source)
  result$adjustment <- objective$adjustment
  result$transformation <- data$inputs$transformation
  result$backend <- "native"
  structure(result,class="persephone_native_arma_residuals")
}
