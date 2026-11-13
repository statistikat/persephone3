# Restricted 20a.1 pilot: fixed parameters and exact stationary initialization.
# No integration/diffuse state, coefficient optimization, or SA engine is tested.
stationary <- getFromNamespace(".native_stationary_likelihood", "persephone3")
arma_kernel <- getFromNamespace(".native_arma_likelihood", "persephone3")
arma <- function(y, ar = numeric(), ma = numeric(), sar = numeric(), sma = numeric(),
                 period = 12, mean = 0, variance = 1) {
  arma_kernel(y, ar, ma, sar, sma, period, mean, variance)
}

# White noise: complete fixed-scale Gaussian density, including its constants.
y <- c(2, -1, NA, 3, 0, NaN, 4)
white <- arma(y, mean = .75, variance = 2.5)
centered <- y - .75
expect_equal(white$predictions, rep(.75, length(y)))
expect_equal(white$innovations, centered)
expect_equal(white$innovation_variances, rep(1, length(y)))
expect_equal(white$observed_count, sum(!is.na(y)))
expect_equal(white$sum_squared_standardized, sum(centered^2, na.rm = TRUE))
expect_equal(white$fixed_log_likelihood, sum(dnorm(y, .75, sqrt(2.5), log = TRUE), na.rm = TRUE))
expect_equal(white$profiled_variance, mean(centered^2, na.rm = TRUE))
expect_equal(white$profiled_log_likelihood,
             sum(dnorm(y, .75, sqrt(white$profiled_variance), log = TRUE), na.rm = TRUE))
expect_equal(white$profile_status, "available")
expect_equal(white$initial_covariance, matrix(1))

# AR(1) conditional formulas include leading, internal, consecutive and trailing
# gaps. A gap advances time, rather than collapsing the observed data vector.
for (phi in c(-.65, .7, .9999)) {
  y <- c(NA, NA, 1.4, -.3, NA, NA, .8, NA, -.4, NA)
  mean <- .4
  predicted <- rep(mean, length(y))
  variance <- rep(1 / (1 - phi^2), length(y))
  previous <- NA_integer_
  for (i in seq_along(y)) {
    if (!is.na(previous)) {
      gap <- i - previous
      predicted[i] <- mean + phi^gap * (y[previous] - mean)
      variance[i] <- sum(phi^(2 * (0:(gap - 1))))
    }
    if (!is.na(y[i])) previous <- i
  }
  fit <- arma(y, ar = phi, mean = mean, variance = 1.7)
  expect_equal(fit$predictions, predicted, tolerance = 1e-9)
  expect_equal(fit$innovation_variances, variance, tolerance = 1e-9)
  expect_equal(fit$innovations, y - predicted, tolerance = 1e-9)
  expect_equal(fit$standardized_innovations, (y - predicted) / sqrt(variance), tolerance = 1e-9)
  expect_equal(fit$fixed_log_likelihood,
               sum(dnorm(y, predicted, sqrt(1.7 * variance), log = TRUE), na.rm = TRUE),
               tolerance = 1e-9)
  expect_equal(fit$initial_covariance, matrix(1 / (1 - phi^2)), tolerance = 1e-9)
}

# Match R's explicitly selected Rossignol stationary initialization and its
# profiled likelihood convention. KalmanLike$Lik omits additive constants and
# is per observed value; KalmanRun$resid contains v/sqrt(F), not raw innovations.
y <- sin(seq_len(90) / 4) + cos(seq_len(90) / 11) / 3
cases <- list(
  list(ar = .5, ma = .25),
  list(ar = c(.6, -.2), ma = c(.3, .1)),
  list(ar = numeric(), ma = c(1.2, -.4)),
  list(ar = .9999, ma = -.2),
  list(ar = -.9, ma = .8)
)
for (parameters in cases) {
  for (missing in list(integer(), c(1:3, 9, 20:24, 88:90))) {
    values <- y
    values[missing] <- NA_real_
    native <- arma(values, ar = parameters$ar, ma = parameters$ma)
    model <- stats::makeARIMA(parameters$ar, parameters$ma, numeric(), SSinit = "Rossignol2011")
    reference <- stats::KalmanLike(values, model)
    run <- stats::KalmanRun(values, model)
    n <- sum(!is.na(values))
    expect_equal(native$initial_covariance, model$Pn, tolerance = 2e-8)
    expect_equal(native$profiled_variance, reference$s2, tolerance = 2e-8)
    expect_equal(native$profiled_log_likelihood,
                 -n * (reference$Lik + .5 * (log(2 * pi) + 1)), tolerance = 2e-8)
    expect_equal(native$standardized_innovations, run$resid, tolerance = 2e-8)
    expect_equal(native$final_state, as.numeric(run$states[nrow(run$states), ]), tolerance = 2e-8)
    expect_true(native$numerics$lyapunov_scaled_residual < 1e-10)
  }
}

# Seasonal multiplication has explicit signs and lag conventions, independently
# checked against hand-expanded monthly polynomials and stats::arima fixed fits.
monthly <- arma(y, ar = .2, ma = .1, sar = .4, sma = -.5, period = 12, mean = .3)
expect_equal(monthly$expanded_ar, c(.2, rep(0, 10), .4, -.08))
expect_equal(monthly$expanded_ma, c(.1, rep(0, 10), -.5, -.05))
for (period in c(4, 12)) {
  for (missing in list(integer(), c(1, 15:18, 90))) {
    values <- y
    values[missing] <- NA_real_
    native <- arma(values, ar = .2, ma = .1, sar = .4, sma = -.5, period = period, mean = .3)
    reference <- stats::arima(values, order = c(1, 0, 1),
      seasonal = list(order = c(1, 0, 1), period = period),
      fixed = c(.2, .1, .4, -.5, .3), include.mean = TRUE,
      transform.pars = FALSE, method = "ML", SSinit = "Rossignol2011")
    expect_equal(native$profiled_log_likelihood, reference$loglik, tolerance = 2e-8)
    expect_equal(native$profiled_variance, reference$sigma2, tolerance = 2e-8)
    expect_equal(native$model$transition, reference$model$T, tolerance = 1e-12)
    expect_equal(native$model$process_covariance, reference$model$V, tolerance = 1e-12)
  }
}

# A separate observed-data multivariate-normal oracle checks a generic system
# with observation noise and rank-deficient process covariance.
local({
  transition <- matrix(c(.5, -.15, .2, .4), 2)
  loading <- matrix(c(.8, .3), 2, 1)
  observation <- c(1, -.4)
  h <- .2
  values <- c(NA, .7, -.2, NA, .4, .1, NA, -.9)
  native <- stationary(values, transition, observation, loading, h, mean = .25, variance = 1.8)
  independent_prior <- matrix(0, 2, 2)
  power <- diag(2)
  for (i in 0:500) {
    independent_prior <- independent_prior + power %*% tcrossprod(loading) %*% t(power)
    power <- power %*% transition
  }
  expect_equal(native$initial_covariance, independent_prior, tolerance = 1e-10)
  reference <- list(T = transition, Z = observation, V = tcrossprod(loading), h = h,
                    a = c(0, 0), P = matrix(0, 2, 2), Pn = independent_prior)
  run <- stats::KalmanRun(values - .25, reference)
  expect_equal(native$standardized_innovations, run$resid, tolerance = 1e-10)
  covariance <- matrix(0, length(values), length(values))
  power <- diag(2)
  for (lag in 0:(length(values) - 1)) {
    gamma <- drop(observation %*% power %*% independent_prior %*% observation)
    covariance[abs(row(covariance) - col(covariance)) == lag] <- gamma
    power <- power %*% transition
  }
  diag(covariance) <- diag(covariance) + h
  seen <- which(!is.na(values))
  centered <- values[seen] - .25
  observed_covariance <- 1.8 * covariance[seen, seen]
  logdet <- as.numeric(determinant(observed_covariance, logarithm = TRUE)$modulus)
  gaussian <- -.5 * (length(seen) * log(2 * pi) + logdet +
                      drop(crossprod(centered, solve(observed_covariance, centered))))
  expect_equal(native$fixed_log_likelihood, gaussian, tolerance = 1e-10)
  for (i in seq_along(values)) {
    previous <- seen[seen < i]
    predicted <- .25
    prediction_variance <- covariance[i, i]
    if (length(previous)) {
      predicted <- predicted + drop(covariance[i, previous, drop = FALSE] %*%
        solve(covariance[previous, previous, drop = FALSE], values[previous] - .25))
      prediction_variance <- prediction_variance - drop(covariance[i, previous, drop = FALSE] %*%
        solve(covariance[previous, previous, drop = FALSE], covariance[previous, i, drop = FALSE]))
    }
    expect_equal(native$predictions[i], predicted, tolerance = 1e-10)
    expect_equal(native$innovation_variances[i], prediction_variance, tolerance = 1e-10)
  }
  expect_equal(native$initial_covariance - transition %*% native$initial_covariance %*% t(transition),
               tcrossprod(loading), tolerance = 1e-10)
})

# Stability covers unobserved/unforced modes too, not just positive observed
# variances. Complex roots are represented by real rotation blocks.
rotation <- function(radius) radius * matrix(c(cos(.7), sin(.7), -sin(.7), cos(.7)), 2)
for (radius in c(.5, .99)) {
  fit <- stationary(c(.2, NA, -.1), rotation(radius), c(1, 0), diag(2), .1)
  expect_equal(fit$initial_covariance, diag(1 / (1 - radius^2), 2), tolerance = 1e-9)
}
expect_error(stationary(1:3, rotation(1.01), c(1, 0), diag(2)), "not certified stationary")
expect_error(arma(1:3, ar = 1), "singular or numerically rank deficient")
expect_error(arma(1:3, ar = 1.01), "not certified stationary")
expect_error(stationary(1:3, diag(c(.5, 1.1)), c(1, 0), matrix(c(1, 0), 2, 1)),
             "not certified stationary")
nonnormal <- matrix(c(.5, 0, 10, .5), 2)
non <- stationary(c(.4, -.2, .1), nonnormal, c(1, 0), diag(2))
expect_equal(non$initial_covariance - nonnormal %*% non$initial_covariance %*% t(nonnormal),
             diag(2), tolerance = 1e-9)
expect_true(non$numerics$stability_certificate_rcond > 1e-12)
expect_error(stationary(1:3, matrix(c(.5, 0, 1e8, .5), 2), c(1, 0), diag(2)),
             "rank deficient|ill-conditioned")

# Boundary semantics remain explicit; zero estimated variance is not a finite
# optimized likelihood, and all-missing data contribute zero fixed log density.
zero <- arma(rep(0, 5))
expect_equal(zero$profiled_variance, 0)
expect_equal(zero$profiled_log_likelihood, Inf)
expect_equal(zero$profile_status, "zero_scale_boundary")
for (values in list(numeric(), rep(NA_real_, 4))) {
  empty <- arma(values, ar = .6)
  expect_equal(empty$observed_count, 0)
  expect_equal(empty$fixed_log_likelihood, 0)
  expect_true(is.na(empty$profiled_variance))
  expect_true(is.na(empty$profiled_log_likelihood))
  expect_equal(empty$profile_status, "no_observations")
}
measurement_only <- stationary(1:3, matrix(.4), 1, matrix(0), observation_variance = .7)
expect_equal(measurement_only$initial_covariance, matrix(0))
expect_equal(measurement_only$innovation_variances, rep(.7, 3))
expect_error(stationary(1:3, matrix(.4), 1, matrix(0)), "positive innovation variance")

expect_error(arma(c(1, Inf)), "infinity")
expect_error(arma(1:3, ar = NA_real_), "finite")
expect_error(arma(1:3, sma = Inf), "finite")
expect_error(arma(1:3, mean = NA_real_), "mean must be finite")
for (variance in c(0, -1, Inf, NA_real_)) expect_error(arma(1:3, variance = variance), "variance")
for (period in c(0, 1.5, Inf, NA_real_)) expect_error(arma(1:3, period = period), "period")
expect_error(arma(1:3, sar = c(.2, .1), period = 12), "pilot bound")
expect_error(stationary(1:3, diag(17), rep(1, 17), diag(17)), "at most 16")
expect_error(stationary(1:3, matrix(.2), 1, matrix(1, 1, 17)), "at most 16")
expect_error(stationary(1:3, diag(2), 1, diag(2)), "dimensions")
expect_error(stationary(1:3, matrix(.2), 1, matrix(1), -1), "nonnegative")
expect_error(stationary(1:3, matrix(.2), Inf, matrix(1)), "finite")
expect_error(stationary(1:3, matrix(.2), 1, matrix(1e308)), "non-finite")
expect_error(arma(1e308), "non-finite")
expect_error(arma(1e-200), "underflows")
expect_equal(arma(c(1, 2, 3))$innovations, c(1, 2, 3))

# Rcpp validates storage and dimensions before numeric conversion. Complex
# imaginary parts and multivariate observations must never be silently dropped.
for (invalid in list(1 + 2i, "1", TRUE, factor("1"), list(1))) {
  expect_error(arma(invalid), "real numeric")
  expect_error(arma(1:3, ar = invalid), "real numeric")
  expect_error(arma(1:3, ma = invalid), "real numeric")
  expect_error(arma(1:3, sar = invalid), "real numeric")
  expect_error(arma(1:3, sma = invalid), "real numeric")
  expect_error(stationary(1:3, matrix(.2), invalid, matrix(1)), "real numeric")
}
for (invalid in list(matrix(1:6, 2), array(1:4, c(2, 1, 2)), array(1:3, 3))) {
  expect_error(arma(invalid), "vector without matrix or array dimensions")
  expect_error(arma(1:3, ar = invalid), "vector without matrix or array dimensions")
  expect_error(stationary(1:3, matrix(.2), invalid, matrix(1)),
               "vector without matrix or array dimensions")
}
for (invalid in list(matrix(1 + 2i), matrix("1"), matrix(TRUE), array(1, c(1, 1, 1)), 1)) {
  expect_error(stationary(1:3, invalid, 1, matrix(1)), "real numeric|numeric matrix")
  expect_error(stationary(1:3, matrix(.2), 1, invalid), "real numeric|numeric matrix")
}
for (invalid in list(1 + 2i, "1", TRUE, c(1, 2), matrix(1), factor("1"))) {
  expect_error(arma(1:3, period = invalid), "real numeric|vector without")
  expect_error(arma(1:3, mean = invalid), "real numeric|vector without")
  expect_error(arma(1:3, variance = invalid), "real numeric|vector without")
  expect_error(stationary(1:3, matrix(.2), 1, matrix(1), observation_variance = invalid),
               "real numeric|vector without")
}
expect_equal(stationary(1:3, matrix(0), 1, matrix(1))$fixed_log_likelihood,
             arma(1:3)$fixed_log_likelihood)
