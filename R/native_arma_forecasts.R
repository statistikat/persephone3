# Explicit endpoint predictions from a prepared fixed-order ARMA objective.
# The future/past regressors must already cover every requested period.
native_arma_forecasts <- function(objective,coordinates=objective$parameters$initial_coordinates,
    forecast_horizon=objective$prepared$inputs$forecast_horizon,
    backcast_horizon=objective$prepared$inputs$backcast_horizon,
    forecast_error_horizon=forecast_horizon,backcast_error_horizon=backcast_horizon,
    scale="ml",method="conditional",control=list()) {
  if(!inherits(objective,"persephone_native_arma_objective") ||
      !identical(objective$schema_version,1L))stop("Unsupported ARMA objective contract.",call.=FALSE)
  f <- native_horizon(forecast_horizon,"forecast_horizon")
  b <- native_horizon(backcast_horizon,"backcast_horizon")
  fe <- native_horizon(forecast_error_horizon,"forecast_error_horizon")
  be <- native_horizon(backcast_error_horizon,"backcast_error_horizon")
  if(fe>f || be>b)stop("Error horizons must not exceed their point horizons.",call.=FALSE)
  if(!is.character(scale) || length(scale)!=1L || is.na(scale) || !is.null(dim(scale)) ||
      !scale %in% c("ml","residual_df"))stop("Prediction scale must be 'ml' or 'residual_df'.",call.=FALSE)
  if(!is.character(method) || length(method)!=1L || is.na(method) || !is.null(dim(method)) ||
      !method %in% c("conditional","delta_profile_ml"))stop("Unsupported prediction uncertainty method.",call.=FALSE)
  if(!is.list(control) || !is.null(dim(control)))stop("Prediction information control must be a list.",call.=FALSE)
  if(method=="conditional" && length(control))stop("Derivative controls require delta_profile_ml.",call.=FALSE)
  if(method=="delta_profile_ml" && scale!="ml")stop("ARMA propagation requires ML scale.",call.=FALSE)
  coordinates <- native_arma_coordinates(objective$parameters,coordinates)
  p <- objective$parameters;data <- objective$prepared;inputs <- data$inputs
  if(f>inputs$forecast_horizon || b>inputs$backcast_horizon)
    stop("Requested prediction horizons exceed the prepared regressor context.",call.=FALSE)
  n <- length(inputs$transformed)
  context_rows <- inputs$backcast_horizon-b+seq_len(b+n+f)
  design <- inputs$regressors[context_rows,,drop=FALSE]
  result <- .native_forecast_information(as.numeric(inputs$transformed),design,unname(data$fixed),
    b,f,p$specification,p$initial,coordinates,objective$period,objective$d,objective$seasonal_d,
    objective$mean,objective$rank_tolerance,p$reflection_margin,objective$adjustment$log_jacobian,scale,method,control)
  domain <- inputs$observed_domain
  starts <- c(backcast=domain$start_ordinal-b,forecast=domain$start_ordinal+n)
  ordinals <- c(starts['backcast']+seq_len(b)-1,starts['forecast']+seq_len(f)-1)
  result$covariance_ordinals <- unname(ordinals)
  result$source_positions <- data$window$source_rows[1L]-b+result$rows-1L
  labels <- as.character(ordinals)
  for(field in c("covariance","known_regression_covariance","regression_covariance",
      if(method=="delta_profile_ml")c("conditional_covariance","arma_increment")))
    if(!is.null(result[[field]]))dimnames(result[[field]]) <- list(labels,labels)
  if(method=="delta_profile_ml") {
    theta <- sub("[.]reflection",".coefficient",names(p$initial_coordinates))
    for(field in c("sensitivity","sensitivity_error"))if(!is.null(result[[field]]))
      dimnames(result[[field]]) <- list(labels,theta)
    if(length(result$sensitivity_steps)==length(theta))names(result$sensitivity_steps) <- theta
    if(!is.null(result$arma_information)) {
      result$arma_information <- native_arma_information_labels(result$arma_information,objective)
      result$arma_information_status <- result$arma_information$status
      result$arma_parameter_covariance <- result$arma_information$covariance
      result$arma_coefficients <- result$arma_information$coefficients
    } else {
      result$arma_information_status <- if(result$sensitivity_status=="not_required")"not_required"else"not_evaluated"
      result$arma_parameter_covariance <- if(!length(theta))matrix(numeric(),0L,0L)else NULL
      result$arma_coefficients <- setNames(rep(NA_real_,length(theta)),theta)
    }
  }
  locations <- result$points_transformed
  result$original_moments <- if(result$point_available)native_model_prediction_moments(
    locations,result$covariance,inputs$transformation)else NULL
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
  error_series <- function(values)list(
    backcast=as_series(values[error_rows$backcast],domain$start_ordinal-be),
    forecast=as_series(values[error_rows$forecast],starts[['forecast']]))
  result$standard_errors <- error_series(result$standard_errors)
  if(method=="delta_profile_ml")result$conditional_standard_errors <- error_series(result$conditional_standard_errors)
  means <- if(inputs$transformation=="none")locations else
    if(!is.null(result$original_moments))unname(result$original_moments$means)else rep(NA_real_,length(locations))
  result$means_original <- lapply(names(point_rows),function(direction)
    as_series(means[point_rows[[direction]]],starts[[direction]])) |> stats::setNames(names(point_rows))
  result$original_mean_status <- if(inputs$transformation=="none")result$point_status else
    if(!is.null(result$original_moments))result$original_moments$mean_status else "uncertainty_unavailable"
  result$covariance_original <- if(inputs$transformation=="none")result$covariance else result$original_moments$covariance
  result$original_covariance_status <- if(!is.null(result$original_moments))result$original_moments$covariance_status else "uncertainty_unavailable"
  original_errors <- if(!is.null(result$original_moments))unname(result$original_moments$standard_errors)else rep(NA_real_,length(locations))
  result$standard_errors_original <- if(inputs$transformation=="none")result$standard_errors else list(
    backcast=as_series(original_errors[error_rows$backcast],domain$start_ordinal-be),
    forecast=as_series(original_errors[error_rows$forecast],starts[['forecast']]))
  result$standard_errors_scale <- "transformed"
  result$original_uncertainty_status <- if(inputs$transformation=="none")result$status else
    if(!is.null(result$original_moments))result$original_moments$standard_error_status else "uncertainty_unavailable"
  result$point_domains <- lapply(result$points_transformed,backend_domain)
  result$error_domains <- lapply(result$standard_errors,backend_domain)
  result$point_horizons <- c(backcast=b,forecast=f);result$error_horizons <- c(backcast=be,forecast=fe)
  result$forecasts <- new_forecast_contract(list(y=result$points_original$forecast),
    list(y=result$standard_errors_original$forecast),observed=inputs$original)
  if(!is.null(result$forecasts)) {
    result$forecasts$uncertainty_distribution <- if(inputs$transformation=="log")"lognormal"else"Gaussian"
    result$forecasts$interval_convention <- if(inputs$transformation=="log")"exponentiate_Gaussian_quantiles"else"Gaussian_quantiles"
  }
  result$point_convention <- if(inputs$transformation=="log")"exponentiated_transformed_point"else"Gaussian_conditional_point"
  result$mean_adjustment <- "none";result$transformation <- inputs$transformation
  result$scale_convention <- if(scale=="ml")"Q_over_original_n_finite"else"Q_over_original_n_minus_free_regression_minus_free_ARMA"
  result$variance_uncertainty <- "plug_in_scale_no_scale_parameter_propagation"
  result$span <- list(source_domain=data$window$source_domain,estimation_domain=domain,source_rows=data$window$source_rows)
  if(!is.null(result$center$likelihood))names(result$center$likelihood$regression_coefficients) <- data$labels
  result$adjustment <- objective$adjustment;result$backend <- "native";result$comparison_only <- FALSE
  result$uncertainty_method <- method
  structure(result,class="persephone_native_arma_forecasts")
}
