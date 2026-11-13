# Internal fixed-model evaluation. It estimates no ARMA/regression parameters
# and is not an X-13/TRAMO-SEATS fit or a replacement for their constructors.
native_span_ordinal <- function(value, frequency, name) {
  if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) ||
      length(value) != 2L || anyNA(value) || any(!is.finite(value)) ||
      any(value != floor(value)) || value[2L] < 1 || value[2L] > frequency) {
    stop(name, " must be c(year, period) with whole values and a valid period.",
         call. = FALSE)
  }
  .native_time_domain(value[1L], value[2L], frequency, 0L)
  ordinal <- value[1L] * frequency + value[2L] - 1
  if (!is.finite(ordinal) || abs(ordinal) > 2^53 - 1) {
    stop(name, " exceeds the exact period range of R time series.", call. = FALSE)
  }
  unname(ordinal)
}

native_likelihood_window <- function(series, estimation_start = NULL,
                                     estimation_end = NULL) {
  source <- native_regular_domain(series)
  first <- source$start_ordinal
  last <- first + length(series) - 1
  if (abs(first) > 2^53 - 1 || abs(last) > 2^53 - 1) {
    stop("The input domain exceeds the exact period range of R time series.", call. = FALSE)
  }
  start <- if (is.null(estimation_start)) first else
    native_span_ordinal(estimation_start, source$frequency, "estimation_start")
  end <- if (is.null(estimation_end)) last else
    native_span_ordinal(estimation_end, source$frequency, "estimation_end")
  if (start < first || end > last || start > last || end < first) {
    stop("The estimation window must lie entirely inside the input domain.", call. = FALSE)
  }
  if (start > end) {
    stop("estimation_start must not follow estimation_end.", call. = FALSE)
  }
  rows <- seq.int(start - first + 1, end - first + 1)
  year <- floor(start / source$frequency)
  period <- start - year * source$frequency + 1
  selected <- stats::ts(as.numeric(series)[rows], start = c(year, period),
                        frequency = source$frequency)
  # A whole ordinal can fit in a double while its fractional-year ts encoding
  # cannot represent a requested month. Reject any such relabeling on creation.
  reconstructed <- native_regular_domain(selected)
  if (reconstructed$start_ordinal != start ||
      round(stats::tsp(selected)[2L] * source$frequency) != end) {
    stop("The estimation window cannot be represented exactly by an R time series.",
         call. = FALSE)
  }
  list(series = selected, source_domain = source, source_rows = rows)
}

# The window endpoints are inclusive (year, period) pairs. Excluded earlier
# observations never condition the model: initialization is fresh at its start.
# Selection precedes transformation; missing times inside the window survive.
native_fixed_likelihood <- function(series, ar = numeric(), ma = numeric(),
                                    seasonal_ar = numeric(), seasonal_ma = numeric(),
                                    d = 0L, seasonal_d = 0L, mean = 0, variance = 1,
                                    transformation = "none", estimation_start = NULL,
                                    estimation_end = NULL) {
  window <- native_likelihood_window(series, estimation_start, estimation_end)
  inputs <- native_prepare_inputs(window$series, transformation = transformation)
  kernel <- .native_sarima_likelihood(as.numeric(inputs$transformed), ar, ma,
    seasonal_ar, seasonal_ma, inputs$observed_domain$frequency,
    d, seasonal_d, mean, variance)

  observed <- which(!inputs$missing)
  degree <- length(kernel$difference_polynomial) - 1L
  # Pinned JD+ direct-window convention: exclude k CALENDAR positions after
  # the first observed value, not k observed values or the diffuse update rows.
  # This is an explicit compatibility adjustment to an improper likelihood;
  # it is not asserted to be a universal original-scale probability density.
  selected <- observed
  if (transformation == "log" && length(observed)) {
    selected <- observed[observed - observed[1L] >= degree]
  }
  terms <- rep(0, length(window$series))
  terms[selected] <- as.numeric(inputs$log_jacobian)[selected]
  correction <- sum(terms)
  if (!is.finite(correction)) {
    stop("The log-Jacobian correction is nonfinite.", call. = FALSE)
  }
  as_series <- function(values) stats::ts(values, start = inputs$observed_domain$start,
                                         frequency = inputs$observed_domain$frequency)
  policy <- if (transformation == "log") "jdplus_3.9.0_calendar_window" else "identity"
  structure(list(
    schema_version = 1L, backend = "native", operation = "fixed_sarima_likelihood",
    initialization = "fresh_prior_at_span_start",
    parameters = list(ar = as.numeric(ar), ma = as.numeric(ma),
      seasonal_ar = as.numeric(seasonal_ar), seasonal_ma = as.numeric(seasonal_ma),
      d = kernel$d, seasonal_d = kernel$seasonal_d, period = kernel$period,
      mean = kernel$model$mean, variance = kernel$model$variance),
    span = list(source_domain = window$source_domain,
                estimation_domain = inputs$observed_domain,
                source_rows = window$source_rows),
    inputs = inputs,
    likelihood = list(status = kernel$likelihood_status,
      profile_status = kernel$profile_status,
      normalization = kernel$likelihood_normalization,
      transformed_fixed = kernel$fixed_log_likelihood,
      transformed_profiled = kernel$profiled_log_likelihood,
      adjusted_fixed = kernel$fixed_log_likelihood + correction,
      adjusted_profiled = kernel$profiled_log_likelihood + correction,
      profiled_variance = kernel$profiled_variance,
      observed_count = kernel$observed_count,
      diffuse_count = kernel$diffuse_count, finite_count = kernel$finite_count),
    adjustment = list(policy = policy, difference_degree = degree,
      local_rows = selected, source_rows = window$source_rows[selected],
      terms = as_series(terms), log_jacobian = correction),
    traces = list(predictions = as_series(kernel$predictions),
      innovations = as_series(kernel$innovations),
      standardized_innovations = as_series(kernel$standardized_innovations),
      finite_variances = as_series(kernel$finite_variances),
      diffuse_variances = as_series(kernel$diffuse_variances),
      update_kind = as_series(kernel$update_kind),
      rank_before = as_series(kernel$rank_before), rank_after = as_series(kernel$rank_after)),
    kernel = kernel), class = "persephone_native_likelihood")
}
