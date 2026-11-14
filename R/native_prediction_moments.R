# Moments of the plug-in predictive Gaussian distribution, before/after an
# identity or log inverse transform. Standard errors are not quantile intervals.
native_prediction_moments <- function(location,covariance,transformation="none") {
  out <- .native_prediction_moments(location,covariance,transformation)
  if(!is.null(out$covariance))dimnames(out$covariance) <- dimnames(covariance)
  labels <- rownames(covariance)
  if(!is.null(labels))for(field in c("medians","means","standard_errors"))names(out[[field]]) <- labels
  out$mean_convention <- if(transformation=="log")"exp_location_plus_half_variance"else"Gaussian_location"
  out$point_convention <- if(transformation=="log")"inverse_point_median"else"Gaussian_location"
  out$interval_convention <- if(transformation=="log")"exponentiate_Gaussian_quantiles"else"Gaussian_quantiles"
  structure(out,class="persephone_native_prediction_moments")
}

# Model callers retain their valid transformed result if back-transformation
# cannot certify representable original moments. Interrupts are not caught.
native_model_prediction_moments <- function(location,covariance,transformation) {
  if(is.null(covariance))return(NULL)
  # Identity outputs are the already qualified kernel result. A second
  # factorization must not discard a covariance whose tiny process component
  # was rounded below a much larger shared regression component.
  if(transformation=="none") {
    labelled <- stats::setNames(location,rownames(covariance))
    return(structure(list(available=TRUE,medians=labelled,means=labelled,
      covariance=covariance,standard_errors=sqrt(diag(covariance)),
      median_status="available",mean_status="available",covariance_status="available",
      standard_error_status="available",median_message="",mean_message="",
      covariance_message="",standard_error_message="",transformation="none",
      distribution="Gaussian",mean_convention="Gaussian_location",
      point_convention="Gaussian_location",interval_convention="Gaussian_quantiles"),
      class="persephone_native_prediction_moments"))
  }
  tryCatch(native_prediction_moments(location,covariance,transformation),error=function(error)
    structure(list(available=FALSE,means=rep(NA_real_,length(location)),
      medians=rep(NA_real_,length(location)),standard_errors=rep(NA_real_,length(location)),
      covariance=NULL,median_status="not_evaluated",mean_status="original_moments_unavailable",
      covariance_status="original_moments_unavailable",standard_error_status="original_moments_unavailable",message=conditionMessage(error),
      transformation=transformation,distribution=if(transformation=="log")"lognormal"else"Gaussian",
      interval_convention=if(transformation=="log")"exponentiate_Gaussian_quantiles"else"Gaussian_quantiles"),
      class="persephone_native_prediction_moments"))
}
