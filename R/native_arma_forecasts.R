# Explicit endpoint predictions from a prepared fixed-order ARMA objective.
# The future/past regressors must already cover every requested period.
native_arma_forecasts <- function(objective,coordinates=objective$parameters$initial_coordinates,
    forecast_horizon=objective$prepared$inputs$forecast_horizon,
    backcast_horizon=objective$prepared$inputs$backcast_horizon,
    forecast_error_horizon=forecast_horizon,backcast_error_horizon=backcast_horizon,
    scale="ml") {
  if(!inherits(objective,"persephone_native_arma_objective") ||
      !identical(objective$schema_version,1L))stop("Unsupported ARMA objective contract.",call.=FALSE)
  f <- native_horizon(forecast_horizon,"forecast_horizon")
  b <- native_horizon(backcast_horizon,"backcast_horizon")
  fe <- native_horizon(forecast_error_horizon,"forecast_error_horizon")
  be <- native_horizon(backcast_error_horizon,"backcast_error_horizon")
  if(fe>f || be>b)stop("Error horizons must not exceed their point horizons.",call.=FALSE)
  if(!is.character(scale) || length(scale)!=1L || is.na(scale) || !is.null(dim(scale)) ||
      !scale %in% c("ml","residual_df"))stop("Prediction scale must be 'ml' or 'residual_df'.",call.=FALSE)
  coordinates <- native_arma_coordinates(objective$parameters,coordinates)
  p <- objective$parameters;data <- objective$prepared;inputs <- data$inputs
  if(f>inputs$forecast_horizon || b>inputs$backcast_horizon)
    stop("Requested prediction horizons exceed the prepared regressor context.",call.=FALSE)
  n <- length(inputs$transformed)
  context_rows <- inputs$backcast_horizon-b+seq_len(b+n+f)
  design <- inputs$regressors[context_rows,,drop=FALSE]
  result <- .native_forecast_information(as.numeric(inputs$transformed),design,unname(data$fixed),
    b,f,p$specification,p$initial,coordinates,objective$period,objective$d,objective$seasonal_d,
    objective$mean,objective$rank_tolerance,p$reflection_margin,objective$adjustment$log_jacobian,scale)
  domain <- inputs$observed_domain
  starts <- c(backcast=domain$start_ordinal-b,forecast=domain$start_ordinal+n)
  ordinals <- c(starts['backcast']+seq_len(b)-1,starts['forecast']+seq_len(f)-1)
  result$covariance_ordinals <- unname(ordinals)
  result$source_positions <- data$window$source_rows[1L]-b+result$rows-1L
  labels <- as.character(ordinals)
  for(field in c("covariance","known_regression_covariance","regression_covariance"))
    if(!is.null(result[[field]]))dimnames(result[[field]]) <- list(labels,labels)
  as_series <- function(values,start) {
    if(!length(values))return(NULL)
    year <- floor(start/domain$frequency)
    stats::ts(values,start=c(year,start-year*domain$frequency+1),frequency=domain$frequency)
  }
  point_rows <- list(backcast=seq_len(b),forecast=b+seq_len(f))
  error_rows <- list(backcast=b-be+seq_len(be),forecast=b+seq_len(fe))
  result$points_transformed <- lapply(names(point_rows),function(direction)
    as_series(result$points_transformed[point_rows[[direction]]],starts[[direction]])) |>
    stats::setNames(names(point_rows))
  original <- lapply(result$points_transformed,function(points) {
    if(is.null(points))return(NULL)
    tryCatch(.native_inverse_transform(as.numeric(points),inputs$transformation),error=identity)
  })
  result$original_point_status <- vapply(original,function(value)
    if(!result$point_available)"point_unavailable"else if(inherits(value,"error"))"original_scale_range"else"available",character(1))
  result$points_original <- lapply(names(point_rows),function(direction) {
    value <- original[[direction]]
    if(inherits(value,"error"))value <- rep(NA_real_,length(point_rows[[direction]]))
    as_series(value,starts[[direction]])
  }) |> stats::setNames(names(point_rows))
  result$standard_errors <- list(
    backcast=as_series(result$standard_errors[error_rows$backcast],domain$start_ordinal-be),
    forecast=as_series(result$standard_errors[error_rows$forecast],starts[['forecast']]))
  result$standard_errors_original <- if(inputs$transformation=="none")result$standard_errors else
    lapply(result$standard_errors,function(x)if(is.null(x))NULL else x*NA_real_)
  result$standard_errors_scale <- "transformed"
  result$original_uncertainty_status <- if(inputs$transformation=="none")result$status else "lognormal_moments_not_computed"
  result$point_domains <- lapply(result$points_transformed,backend_domain)
  result$error_domains <- lapply(result$standard_errors,backend_domain)
  result$point_horizons <- c(backcast=b,forecast=f);result$error_horizons <- c(backcast=be,forecast=fe)
  result$forecasts <- new_forecast_contract(list(y=result$points_original$forecast),
    if(inputs$transformation=="none")list(y=result$standard_errors_original$forecast)else list(),observed=inputs$original,
    error_reasons=if(inputs$transformation=="log")list(y="Original-scale lognormal errors are not computed.")else list())
  result$point_convention <- if(inputs$transformation=="log")"exponentiated_transformed_point"else"Gaussian_conditional_point"
  result$mean_adjustment <- "none";result$transformation <- inputs$transformation
  result$scale_convention <- if(scale=="ml")"Q_over_original_n_finite"else"Q_over_original_n_minus_free_regression_minus_free_ARMA"
  result$variance_uncertainty <- "plug_in_scale_no_scale_parameter_propagation"
  result$span <- list(source_domain=data$window$source_domain,estimation_domain=domain,source_rows=data$window$source_rows)
  if(!is.null(result$center$likelihood))names(result$center$likelihood$regression_coefficients) <- data$labels
  result$adjustment <- objective$adjustment;result$backend <- "native";result$comparison_only <- FALSE
  structure(result,class="persephone_native_arma_forecasts")
}
