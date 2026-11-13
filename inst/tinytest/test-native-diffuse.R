# Exact diffuse, fixed-parameter likelihood tests. The primary oracle unrolls
# the joint Gaussian model and integrates its initial coordinates analytically;
# it neither calls the native filter nor substitutes a large initial variance.
diffuse_kernel <- getFromNamespace(".native_diffuse_likelihood", "persephone3")
sarima_kernel <- getFromNamespace(".native_sarima_likelihood", "persephone3")
stationary_kernel <- getFromNamespace(".native_stationary_likelihood", "persephone3")

diffuse <- function(y, transition = matrix(1), observation = 1,
                    noise_loading = matrix(1), initial_state = 0,
                    finite_loading = matrix(0), diffuse_loading = matrix(1),
                    observation_variance = 0, mean = 0, variance = 1) {
  diffuse_kernel(y, transition, observation, noise_loading, initial_state,
                 finite_loading, diffuse_loading, observation_variance, mean, variance)
}
sarima <- function(y, ar = numeric(), ma = numeric(), sar = numeric(), sma = numeric(),
                   period = 12, d = 0, seasonal_d = 0, mean = 0, variance = 1) {
  sarima_kernel(y, ar, ma, sar, sma, period, d, seasonal_d, mean, variance)
}

# Unconditional finite covariance and diffuse design from the model equations.
# This uses no innovations, gains, rank updates, or native output model fields.
joint_model <- function(n, transition, observation, noise_loading, initial_state,
                        finite_loading, diffuse_loading, observation_variance = 0,
                        mean = 0) {
  m <- nrow(transition)
  q <- tcrossprod(noise_loading)
  powers <- covariances <- vector("list", n)
  power <- diag(m)
  covariance <- tcrossprod(finite_loading)
  x <- matrix(0, n, ncol(diffuse_loading))
  center <- numeric(n)
  for (i in seq_len(n)) {
    powers[[i]] <- power
    covariances[[i]] <- covariance
    x[i, ] <- drop(observation %*% power %*% diffuse_loading)
    center[i] <- mean + drop(observation %*% power %*% initial_state)
    power <- transition %*% power
    covariance <- transition %*% covariance %*% t(transition) + q
  }
  sigma <- matrix(0, n, n)
  for (j in seq_len(n)) for (i in j:n) {
    sigma[i, j] <- sigma[j, i] <-
      drop(observation %*% powers[[i - j + 1L]] %*% covariances[[j]] %*% observation)
  }
  diag(sigma) <- diag(sigma) + observation_variance
  list(sigma = sigma, x = x, center = center)
}

# Integrate beta in y = center + X beta + epsilon, epsilon ~ N(0, variance*Sigma),
# with Lebesgue measure d beta in the supplied initial-coordinate basis.
flat_prior_oracle <- function(values, joint, variance = 1) {
  seen <- which(!is.na(values))
  y <- values[seen] - joint$center[seen]
  sigma <- joint$sigma[seen, seen, drop = FALSE]
  x <- joint$x[seen, , drop = FALSE]
  upper <- chol(sigma)
  whitened_y <- forwardsolve(t(upper), y)
  r <- ncol(x)
  logdet_x <- 0
  if (r) {
    whitened_x <- forwardsolve(t(upper), x)
    singular <- svd(whitened_x, nu = 0, nv = 0)$d
    stopifnot(length(singular) == r, min(singular) > max(singular) * 1e-10)
    decomposition <- qr(whitened_x, LAPACK = TRUE)
    coefficients <- qr.coef(decomposition, whitened_y)
    errors <- whitened_y - whitened_x %*% coefficients
    logdet_x <- 2 * sum(log(abs(diag(qr.R(decomposition))[seq_len(r)])))
  } else errors <- whitened_y
  sse <- sum(errors^2)
  degrees <- length(seen) - r
  logdet <- 2 * sum(log(diag(upper))) + logdet_x
  scale <- if (degrees > 0) sse / degrees else NA_real_
  list(fixed = -.5 * (degrees * log(2 * pi * variance) + logdet + sse / variance),
       profile = if (degrees > 0) -.5 * (degrees * (log(2 * pi * scale) + 1) + logdet) else NA_real_,
       scale = scale, sse = sse, logdet = logdet, degrees = degrees)
}

# Gaussian conditioning after the previous observations identify every diffuse
# coordinate. This independently checks finite-phase predictions and variances.
flat_prediction <- function(values, joint, at) {
  previous <- which(!is.na(values) & seq_along(values) < at)
  covariance <- joint$sigma[previous, previous, drop = FALSE]
  x <- joint$x[previous, , drop = FALSE]
  cross <- joint$sigma[at, previous, drop = FALSE]
  y <- values[previous] - joint$center[previous]
  inverse_x <- solve(covariance, x)
  information <- crossprod(x, inverse_x)
  beta <- solve(information, crossprod(x, solve(covariance, y)))
  discrepancy <- joint$x[at, , drop = FALSE] - cross %*% inverse_x
  predicted <- joint$center[at] + drop(joint$x[at, , drop = FALSE] %*% beta +
    cross %*% solve(covariance, y - x %*% beta))
  variance <- joint$sigma[at, at] - drop(cross %*% solve(covariance, t(cross))) +
    drop(discrepancy %*% solve(information, t(discrepancy)))
  c(prediction = predicted, variance = variance)
}

# A random walk with gaps has one flat initial level. Differences between
# successive observed values have variance equal to the elapsed number of steps.
y <- c(NA, NA, 2, NA, 5, NA, NA, 1, NA)
seen <- which(!is.na(y))
gaps <- diff(seen)
increments <- diff(y[seen])
walk <- diffuse(y, variance = 1.7)
expect_equal(walk$initial_diffuse_rank, 1)
expect_equal(walk$remaining_diffuse_rank, 0)
expect_equal(walk$observed_count, 3)
expect_equal(walk$diffuse_count, 1)
expect_equal(walk$finite_count, 2)
expect_equal(walk$last_diffuse_index, 3)
expect_equal(walk$update_kind, c(0L, 0L, 1L, 0L, 2L, 0L, 0L, 2L, 0L))
expect_equal(walk$rank_before, c(1, 1, 1, rep(0, 6)))
expect_equal(walk$rank_after, c(1, 1, rep(0, 7)))
expect_equal(walk$diffuse_variances, c(1, 1, 1, rep(0, 6)))
expect_equal(walk$finite_variances, c(0, 1, 2, 1, 2, 1, 2, 3, 1))
expect_equal(walk$predictions, c(0, 0, 0, 2, 2, 5, 5, 5, 1))
expect_equal(walk$standardized_innovations,
             c(NA, NA, NA, NA, 3 / sqrt(2), NA, NA, -4 / sqrt(3), NA))
expect_equal(walk$sum_squared_standardized, sum(increments^2 / gaps))
expect_equal(walk$sum_log_finite_variances, sum(log(gaps)))
expect_equal(walk$sum_log_diffuse_variances, 0)
expect_equal(walk$fixed_log_likelihood,
             sum(dnorm(increments, 0, sqrt(1.7 * gaps), log = TRUE)), tolerance = 1e-12)
expect_equal(walk$profiled_variance, mean(increments^2 / gaps), tolerance = 1e-12)
expect_equal(walk$profiled_log_likelihood,
             sum(dnorm(increments, 0, sqrt(walk$profiled_variance * gaps), log = TRUE)), tolerance = 1e-12)
expect_equal(walk$final_state, 1)
expect_equal(walk$final_finite_covariance, matrix(1))
expect_equal(walk$final_diffuse_covariance, matrix(0))
expect_equal(walk$likelihood_status, "available")
expect_equal(walk$profile_status, "available")
expect_equal(diffuse(y + 50, variance = 1.7)$fixed_log_likelihood, walk$fixed_log_likelihood)

# Diffuse coordinates carry a measure: doubling the initial loading changes
# log likelihood by -log(2), with unchanged scale, predictions and finite errors.
scaled <- diffuse(y, diffuse_loading = matrix(2), variance = 1.7)
expect_equal(scaled$fixed_log_likelihood, walk$fixed_log_likelihood - log(2), tolerance = 1e-12)
expect_equal(scaled$profiled_log_likelihood, walk$profiled_log_likelihood - log(2), tolerance = 1e-12)
expect_equal(scaled$profiled_variance, walk$profiled_variance, tolerance = 1e-12)
expect_equal(scaled$predictions, walk$predictions, tolerance = 1e-12)
expect_equal(scaled$sum_log_diffuse_variances, log(4), tolerance = 1e-12)

# Local I(2), with unknown initial level and lag. Two observations separated
# by three periods identify the two coordinates with determinant magnitude 3.
i2_transition <- matrix(c(2, 1, -1, 0), 2)
i2_model <- list(transition = i2_transition, observation = c(1, 0),
                 noise_loading = matrix(c(1, 0), 2), initial_state = c(0, 0),
                 finite_loading = matrix(0, 2, 1), diffuse_loading = diag(2))
i2_values <- c(2, 3, 1, 5, 4, 8, 6)
i2 <- do.call(diffuse, c(list(y = i2_values, variance = .8), i2_model))
second_difference <- diff(i2_values, differences = 2)
expect_equal(i2$diffuse_count, 2)
expect_equal(i2$finite_count, length(i2_values) - 2)
expect_equal(i2$innovations[-(1:2)], second_difference, tolerance = 1e-11)
expect_equal(i2$finite_variances[-(1:2)], rep(1, length(second_difference)), tolerance = 1e-11)
expect_equal(i2$fixed_log_likelihood, sum(dnorm(second_difference, sd = sqrt(.8), log = TRUE)), tolerance = 1e-11)
identified_only <- do.call(diffuse, c(list(y = c(NA, 2, NA, NA, 5)), i2_model))
expect_equal(identified_only$diffuse_count, 2)
expect_equal(identified_only$finite_count, 0)
expect_equal(identified_only$fixed_log_likelihood, -log(3), tolerance = 1e-11)
expect_equal(identified_only$likelihood_status, "available")
expect_equal(identified_only$profile_status, "no_finite_innovations")
expect_true(is.na(identified_only$profiled_variance))
expect_true(is.na(identified_only$profiled_log_likelihood))

# Joint covariance/flat-prior integration, including overlapping proper and
# diffuse initial loadings, correlated process shocks, gaps and fixed means.
fixtures <- list(
  list(transition = matrix(1), observation = 1, noise_loading = matrix(.7),
       initial_state = .2, finite_loading = matrix(.3), diffuse_loading = matrix(1),
       observation_variance = .2, mean = .4),
  c(i2_model, list(observation_variance = .3, mean = -.2)),
  list(transition = diag(c(1, .6)), observation = c(1, .4),
       noise_loading = matrix(c(.4, .2), 2), initial_state = c(0, .3),
       finite_loading = diag(c(0, .4)), diffuse_loading = matrix(c(1, 0), 2),
       observation_variance = .25, mean = .1),
  list(transition = matrix(c(0, 1, 1, 0), 2), observation = c(1, 0),
       noise_loading = matrix(0, 2, 1), initial_state = c(.1, -.1),
       finite_loading = matrix(c(1, 0), 2), diffuse_loading = matrix(c(0, 1), 2),
       observation_variance = .3, mean = 0)
)
values <- c(.3, .4, .2, NA, 1.1, .5, NA, -.3, .9, .1)
for (settings in fixtures) {
  for (missing in list(integer(), c(1, 2, 9, 10))) {
    observed <- values
    observed[missing] <- NA_real_
    joint <- do.call(joint_model, c(list(n = length(observed)), settings))
    for (variance in c(.4, 1, 2.5)) {
      expected <- flat_prior_oracle(observed, joint, variance)
      actual <- do.call(diffuse, c(list(y = observed, variance = variance), settings))
      expect_equal(actual$fixed_log_likelihood, expected$fixed, tolerance = 1e-9)
      expect_equal(actual$profiled_log_likelihood, expected$profile, tolerance = 1e-9)
      expect_equal(actual$profiled_variance, expected$scale, tolerance = 1e-9)
      expect_equal(actual$sum_squared_standardized, expected$sse, tolerance = 1e-9)
      expect_equal(actual$sum_log_finite_variances + actual$sum_log_diffuse_variances,
                   expected$logdet, tolerance = 1e-9)
      expect_equal(actual$finite_count, expected$degrees)
    }
    # Conditional joint Gaussian formulas are valid after all initial
    # coordinates have been identified, even at a later missing observation.
    for (at in seq_along(observed)) {
      previous <- which(!is.na(observed) & seq_along(observed) < at)
      design <- joint$x[previous, , drop = FALSE]
      if (length(previous) < ncol(design) || qr(design)$rank < ncol(design)) next
      prediction <- flat_prediction(observed, joint, at)
      expect_equal(actual$predictions[at], unname(prediction["prediction"]), tolerance = 1e-9)
      expect_equal(actual$finite_variances[at], unname(prediction["variance"]), tolerance = 1e-9)
    }
  }
}

# A nonorthogonal change of the two initial-coordinate units has the expected
# determinant offset; neither an implicit orthonormalization nor sigma scaling
# may erase this fixed-coordinate normalization.
change <- matrix(c(2, -.2, .3, 1.4), 2)
base <- do.call(diffuse, c(list(y = i2_values), i2_model))
changed_model <- i2_model
changed_model$diffuse_loading <- change
changed <- do.call(diffuse, c(list(y = i2_values), changed_model))
offset <- as.numeric(determinant(change, logarithm = TRUE)$modulus)
expect_equal(changed$fixed_log_likelihood, base$fixed_log_likelihood - offset, tolerance = 1e-10)
expect_equal(changed$profiled_log_likelihood, base$profiled_log_likelihood - offset, tolerance = 1e-10)
expect_equal(changed$profiled_variance, base$profiled_variance, tolerance = 1e-10)
expect_equal(changed$final_finite_covariance, base$final_finite_covariance, tolerance = 1e-10)

# Bounded random full-rank systems exercise non-axis-aligned diffuse subspaces,
# overlapping proper variation and different signal/noise scales. Preserve RNG.
local({
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(7241)
  for (trial in seq_len(5)) {
    settings <- list(transition = diag(c(1, .7, .4)), observation = rnorm(3),
      noise_loading = matrix(rnorm(6), 3), initial_state = rnorm(3),
      finite_loading = matrix(rnorm(6), 3), diffuse_loading = matrix(rnorm(6), 3),
      observation_variance = .2, mean = .3)
    values <- rnorm(14)
    values[c(1, 5, 6, 14)] <- NA_real_
    variance <- c(.1, .3, 1, 3, 10)[trial]
    joint <- do.call(joint_model, c(list(n = length(values)), settings))
    expected <- flat_prior_oracle(values, joint, variance)
    actual <- do.call(diffuse, c(list(y = values, variance = variance), settings))
    expect_equal(actual$fixed_log_likelihood, expected$fixed, tolerance = 2e-8)
    expect_equal(actual$profiled_log_likelihood, expected$profile, tolerance = 2e-8)
    expect_equal(actual$profiled_variance, expected$scale, tolerance = 2e-8)
    expect_equal(actual$diffuse_count, 2)
    expect_equal(actual$remaining_diffuse_rank, 0)
  }
})

# Seasonal random walks are independent walks for each calendar position.
for (period in c(4, 12)) {
  transition <- matrix(0, period, period)
  transition[1, period] <- 1
  transition[cbind(2:period, 1:(period - 1))] <- 1
  loading <- matrix(c(1, rep(0, period - 1)), period)
  seasonal <- list(transition = transition, observation = drop(loading),
    noise_loading = loading, initial_state = rep(0, period),
    finite_loading = matrix(0, period, 1), diffuse_loading = diag(period))
  values <- sin(seq_len(4 * period + 5) / 3) + seq_len(4 * period + 5) / 20
  values[c(period + 2, 2 * period + 2, length(values))] <- NA_real_
  residual <- elapsed <- numeric()
  for (season in seq_len(period)) {
    seen <- which(!is.na(values) & (seq_along(values) - season) %% period == 0)
    residual <- c(residual, diff(values[seen]))
    elapsed <- c(elapsed, diff(seen) / period)
  }
  fit <- do.call(diffuse, c(list(y = values, variance = 1.3), seasonal))
  expect_equal(fit$diffuse_count, period)
  expect_equal(fit$finite_count, sum(!is.na(values)) - period)
  expect_equal(fit$fixed_log_likelihood, sum(dnorm(residual, 0, sqrt(1.3 * elapsed), log = TRUE)), tolerance = 1e-10)
  expect_equal(fit$profiled_variance, mean(residual^2 / elapsed), tolerance = 1e-10)
  unavailable <- values
  unavailable[seq(2, length(values), by = period)] <- NA_real_
  unresolved <- do.call(diffuse, c(list(y = unavailable), seasonal))
  expect_equal(unresolved$remaining_diffuse_rank, 1)
  expect_equal(unresolved$likelihood_status, "unresolved_diffuse")
  expect_true(is.na(unresolved$fixed_log_likelihood))
  expect_true(is.na(unresolved$profiled_log_likelihood))
}

# The explicit SARIMA builder must agree with stationary likelihoods of exact
# complete-data differences, including monthly airline and seasonal I(2).
cases <- list(
  list(ar = numeric(), ma = .4, sar = numeric(), sma = numeric(), period = 12,
       d = 1, seasonal_d = 0, expanded_ar = numeric(), expanded_ma = .4),
  list(ar = .3, ma = numeric(), sar = numeric(), sma = numeric(), period = 4,
       d = 2, seasonal_d = 0, expanded_ar = .3, expanded_ma = numeric()),
  list(ar = .3, ma = .2, sar = .25, sma = -.4, period = 4,
       d = 1, seasonal_d = 1, expanded_ar = c(.3, 0, 0, .25, -.075),
       expanded_ma = c(.2, 0, 0, -.4, -.08)),
  list(ar = numeric(), ma = .2, sar = numeric(), sma = -.4, period = 12,
       d = 1, seasonal_d = 1, expanded_ar = numeric(),
       expanded_ma = c(.2, rep(0, 10), -.4, -.08)),
  list(ar = numeric(), ma = numeric(), sar = numeric(), sma = numeric(), period = 12,
       d = 0, seasonal_d = 2, expanded_ar = numeric(), expanded_ma = numeric())
)
values <- sin(seq_len(96) / 3) + cos(seq_len(96) / 7) + (seq_len(96) / 30)^2
for (settings in cases) {
  differenced <- values
  if (settings$d) differenced <- diff(differenced, differences = settings$d)
  if (settings$seasonal_d) differenced <- diff(differenced, lag = settings$period, differences = settings$seasonal_d)
  rmodel <- stats::makeARIMA(settings$expanded_ar, settings$expanded_ma, numeric(), SSinit = "Rossignol2011")
  reference <- stats::KalmanLike(differenced, rmodel)
  arguments <- settings[setdiff(names(settings), c("expanded_ar", "expanded_ma"))]
  fit <- do.call(sarima, c(list(y = values), arguments))
  expect_equal(fit$profiled_log_likelihood,
               -length(differenced) * (reference$Lik + .5 * (log(2 * pi) + 1)), tolerance = 2e-8)
  expect_equal(fit$profiled_variance, reference$s2, tolerance = 2e-8)
  expect_equal(fit$finite_count, length(differenced))
  expect_equal(fit$diffuse_count, settings$d + settings$period * settings$seasonal_d)
  expect_equal(fit$expanded_ar, settings$expanded_ar)
  expect_equal(fit$expanded_ma, settings$expanded_ma)
}
expect_equal(sarima(values, d = 1, seasonal_d = 1)$difference_polynomial,
             c(1, -1, rep(0, 10), -1, 1))

# An independently assembled integrated model, using R's stationary ARMA prior
# and explicit history equations, validates gaps without differencing them away.
integrated_joint <- function(n, expanded_ar, expanded_ma, difference) {
  stationary <- stats::makeARIMA(expanded_ar, expanded_ma, numeric(), SSinit = "Rossignol2011")
  r <- nrow(stationary$T)
  k <- length(difference) - 1L
  m <- r + k
  observation <- c(stationary$Z, -difference[-1])
  transition <- matrix(0, m, m)
  transition[seq_len(r), seq_len(r)] <- stationary$T
  transition[r + 1L, ] <- observation
  if (k > 1) transition[cbind(r + 2:k, r + seq_len(k - 1L))] <- 1
  loading <- matrix(c(1, expanded_ma, rep(0, m - length(expanded_ma) - 1L)), m)
  finite <- matrix(0, m, r)
  finite[seq_len(r), ] <- t(chol(stationary$Pn))
  diffuse <- matrix(0, m, k)
  diffuse[r + seq_len(k), ] <- diag(k)
  joint_model(n, transition, observation, loading, rep(0, m), finite, diffuse)
}
gap_cases <- list(
  list(ar = .3, ma = .2, sar = numeric(), sma = numeric(), period = 4,
       d = 1, seasonal_d = 0, expanded_ar = .3, expanded_ma = .2,
       difference = c(1, -1)),
  list(ar = numeric(), ma = .2, sar = numeric(), sma = -.4, period = 12,
       d = 1, seasonal_d = 1, expanded_ar = numeric(),
       expanded_ma = c(.2, rep(0, 10), -.4, -.08),
       difference = c(1, -1, rep(0, 10), -1, 1))
)
for (settings in gap_cases) {
  values <- sin(seq_len(50) / 3) + seq_len(50) / 20
  values[c(1, 2, 17:19, 31, 49, 50)] <- NA_real_
  joint <- integrated_joint(length(values), settings$expanded_ar,
                            settings$expanded_ma, settings$difference)
  expected <- flat_prior_oracle(values, joint, variance = 1.6)
  arguments <- settings[setdiff(names(settings), c("expanded_ar", "expanded_ma", "difference"))]
  actual <- do.call(sarima, c(list(y = values, variance = 1.6), arguments))
  expect_equal(actual$fixed_log_likelihood, expected$fixed, tolerance = 2e-8)
  expect_equal(actual$profiled_log_likelihood, expected$profile, tolerance = 2e-8)
  expect_equal(actual$profiled_variance, expected$scale, tolerance = 2e-8)
  expect_equal(actual$finite_count, expected$degrees)
}

# Exact stored-history structure preserves zero diffuse projections in this
# second-order seasonal gap pattern. It must succeed rather than reject a
# supported structural case as numerically ambiguous.
limited_values <- sin(seq_len(90) / 4) + seq_len(90) / 20
limited_values[c(1, 3, 5, 6, 16, 21, 23, 86)] <- NA_real_
second_order_gap <- sarima(limited_values, period = 4, seasonal_d = 2)
joint <- integrated_joint(length(limited_values), numeric(), numeric(),
                          c(1, rep(0, 3), -2, rep(0, 3), 1))
expected <- flat_prior_oracle(limited_values, joint)
expect_equal(second_order_gap$fixed_log_likelihood, expected$fixed, tolerance = 1e-7)
expect_equal(second_order_gap$profiled_log_likelihood, expected$profile, tolerance = 1e-7)
expect_equal(second_order_gap$remaining_diffuse_rank, 0)

# Exact homogeneous-history rank now certifies the combined-order zero pivots
# in this former conservative-rejection case. Success is mandatory.
limited <- sarima(limited_values, period = 4, d = 2, seasonal_d = 1)
joint <- integrated_joint(length(limited_values), numeric(), numeric(),
                          c(1, -2, 1, 0, -1, 2, -1))
expected <- flat_prior_oracle(limited_values, joint)
expect_equal(limited$fixed_log_likelihood, expected$fixed, tolerance = 1e-7)
expect_equal(limited$profiled_log_likelihood, expected$profile, tolerance = 1e-7)
expect_equal(limited$remaining_diffuse_rank, 0)

# No diffuse coordinates reduces exactly to the existing proper stationary
# Gaussian contract, while an unobserved coordinate must not produce an objective.
ordinary <- c(.2, NA, -.1, .7, .3)
proper <- diffuse(ordinary, matrix(.5), 1, matrix(1), 0,
                  matrix(sqrt(1 / .75)), matrix(numeric(), 1, 0), .2, .3, 1.4)
stationary <- stationary_kernel(ordinary, matrix(.5), 1, matrix(1), .2, .3, 1.4)
expect_equal(proper$fixed_log_likelihood, stationary$fixed_log_likelihood, tolerance = 1e-11)
expect_equal(proper$profiled_log_likelihood, stationary$profiled_log_likelihood, tolerance = 1e-11)
expect_equal(proper$standardized_innovations, stationary$standardized_innovations, tolerance = 1e-11)
expect_equal(proper$diffuse_count, 0)
hidden <- diffuse(ordinary, diag(c(.5, 1)), c(1, 0), matrix(c(1, 0), 2), c(0, 0),
                  matrix(c(sqrt(1 / .75), 0), 2), matrix(c(0, 1), 2), .2)
expect_equal(hidden$update_kind, c(2L, 0L, 2L, 2L, 2L))
expect_equal(hidden$diffuse_count, 0)
expect_equal(hidden$remaining_diffuse_rank, 1)
expect_equal(hidden$likelihood_status, "unresolved_diffuse")
expect_equal(hidden$profile_status, "unresolved_diffuse")
expect_true(is.na(hidden$fixed_log_likelihood))
expect_true(is.na(hidden$profiled_variance))
expect_true(all(is.finite(hidden$standardized_innovations[!is.na(ordinary)])))
for (observation in list(c(1, 1), c(1, 0))) {
  mixed_hidden <- diffuse(1:6, diag(2), observation, diag(2), c(0, 0),
                          diag(2), matrix(c(2, .2, .3, 1.5), 2), .2)
  expect_equal(mixed_hidden$remaining_diffuse_rank, 1)
  expect_equal(mixed_hidden$likelihood_status, "unresolved_diffuse")
  expect_true(is.na(mixed_hidden$fixed_log_likelihood))
  expect_equal(mixed_hidden$diffuse_count, 1)
  expect_equal(mixed_hidden$update_kind, c(1L, rep(2L, 5)))
}
expect_error(diffuse(c(NA, 1), transition = matrix(0)), "rank|propagat|direction")
for (values in list(numeric(), rep(NA_real_, 3), 1)) {
  unavailable <- do.call(diffuse, c(list(y = values), i2_model))
  expect_equal(unavailable$likelihood_status, "unresolved_diffuse")
  expect_true(is.na(unavailable$fixed_log_likelihood))
}
one <- diffuse(1)
expect_equal(one$fixed_log_likelihood, 0)
expect_equal(one$profile_status, "no_finite_innovations")
expect_true(is.na(one$profiled_variance))
zero <- diffuse(rep(2, 5))
expect_equal(zero$profiled_variance, 0)
expect_equal(zero$profiled_log_likelihood, Inf)
expect_equal(zero$profile_status, "zero_scale_boundary")
empty_proper <- diffuse(numeric(), finite_loading = matrix(1),
                        diffuse_loading = matrix(numeric(), 1, 0))
expect_equal(empty_proper$fixed_log_likelihood, 0)
expect_equal(empty_proper$likelihood_status, "no_observations")
expect_equal(empty_proper$profile_status, "no_finite_innovations")

# Malformed shapes, missing model inputs, unsupported integration/drift and
# dependent initial coordinate bases must fail before becoming optimizer values.
expect_error(do.call(diffuse, c(list(y = 1:5), modifyList(i2_model,
  list(diffuse_loading = matrix(c(1, 0, 2, 0), 2))))), "rank|independent")
expect_error(diffuse(1:5, diffuse_loading = matrix(NA_real_)), "finite")
expect_error(diffuse(1:5, diffuse_loading = matrix(0)), "rank|independent")
expect_error(diffuse(1:5, initial_state = NA_real_), "finite")
expect_error(diffuse(1:5, initial_state = c(0, 0)), "dimensions")
expect_error(diffuse(1:5, observation = c(1, 0)), "dimensions")
expect_error(diffuse(1:5, diffuse_loading = matrix(c(1, 0), 2)), "rows")
expect_error(diffuse(1:5, finite_loading = matrix(numeric(), 1, 0)), "bound")
expect_error(diffuse(1:5, noise_loading = matrix(numeric(), 1, 0)), "bound")
expect_error(diffuse(1:5, transition = diag(33)), "32-state")
expect_error(diffuse(1:5, transition = matrix(1, 2, 3)), "dimension")
expect_error(diffuse(1:5, observation_variance = -1), "nonnegative")
expect_error(diffuse(c(1, Inf)), "finite|infinity")
expect_error(diffuse(c(0, 1e-200)), "underflow")
expect_error(diffuse(1:5, transition = diag(2), observation = c(1, 0),
                     noise_loading = diag(2), initial_state = c(0, 0),
                     finite_loading = diag(2),
                     diffuse_loading = matrix(c(1e-14, 1), 2)),
             "numerically ambiguous diffuse innovation")
expect_error(diffuse(c(1, 1), noise_loading = matrix(0)), "deterministic|positive innovation variance")
expect_error(sarima(1:20, d = 1, mean = 1), "mean|drift")
expect_error(sarima(1:20, d = 3), "0|two|2")
expect_error(sarima(1:20, seasonal_d = 3), "0|two|2")
expect_error(sarima(1:20, d = 1.5), "integer")
expect_error(sarima(1:20, d = 1, period = 6), "4|12|quarterly|monthly")
expect_error(sarima(1:50, ma = .2, sma = -.4, d = 2, seasonal_d = 2), "32|bound")
expect_error(sarima(1:20, ar = 1, d = 1), "stationary|rank")
for (invalid in list(1 + 1i, "1", TRUE, factor("1"), matrix(1))) {
  expect_error(diffuse(invalid), "real numeric|vector")
  expect_error(diffuse(1:5, initial_state = invalid), "real numeric|vector")
  expect_error(sarima(1:20, d = invalid), "real numeric|scalar|vector")
}
for (invalid in list(1 + 1i, "1", TRUE, c(1, 2), matrix(1))) {
  expect_error(diffuse(1:5, observation_variance = invalid), "real numeric|scalar|vector")
  expect_error(diffuse(1:5, mean = invalid), "real numeric|scalar|vector")
  expect_error(diffuse(1:5, variance = invalid), "real numeric|scalar|vector")
}
for (invalid in c(0, -1, NA_real_, Inf)) expect_error(diffuse(1:5, variance = invalid), "variance")
expect_equal(unserialize(serialize(walk, NULL)), walk)
expect_equal(diffuse(y, variance = 1.7), walk)

# Independent rank oracle for the canonical SARIMA history design. Its columns
# are the explicit homogeneous solutions, and rank is computed using base R's
# SVD, not the production per-phase counting/divided-difference certificate.
homogeneous_design <- function(n, period, d, seasonal_d) {
  time <- seq_len(n) / n
  phase <- (seq_len(n) - 1L) %% period
  columns <- list()
  if (seasonal_d) for (season in 0:(period - 1L)) {
    for (power in 0:(seasonal_d - 1L)) {
      columns[[length(columns) + 1L]] <- as.numeric(phase == season) * time^power
    }
  }
  if (d) for (power in seasonal_d + 0:(d - 1L)) {
    columns[[length(columns) + 1L]] <- time^power
  }
  if (!length(columns)) matrix(numeric(), n, 0) else do.call(cbind, columns)
}
homogeneous_rank <- function(x) {
  if (!nrow(x) || !ncol(x)) return(0L)
  singular <- svd(x, nu = 0, nv = 0)$d
  relative <- singular / max(singular)
  # Require a clear numerical rank gap in the independent oracle itself.
  if (any(relative > 1e-12 & relative < 1e-10)) {
    stop("Independent homogeneous-design rank has no clear singular-value gap.")
  }
  sum(relative >= 1e-10)
}
rank_oracle <- function(values, period, d, seasonal_d) {
  design <- homogeneous_design(length(values), period, d, seasonal_d)
  initial <- ncol(design)
  before <- after <- integer(length(values))
  kind <- integer(length(values))
  used <- integer()
  rank <- 0L
  for (i in seq_along(values)) {
    before[i] <- initial - rank
    if (!is.na(values[i])) {
      used <- c(used, i)
      next_rank <- homogeneous_rank(design[used, , drop = FALSE])
      stopifnot((next_rank - rank) %in% 0:1)
      kind[i] <- if (next_rank > rank) 1L else 2L
      rank <- next_rank
    }
    after[i] <- initial - rank
  }
  list(kind = kind, before = before, after = after, remaining = initial - rank)
}

# An independent likelihood oracle for white-noise integrated models. Applying
# base R's differences to I forms the innovation operator A. The full-data
# flat-history density is proportional to exp(-||A y||^2 / (2 variance)); its
# initial-coordinate Jacobian is one. Integrating missing y coordinates by QR
# yields a determinant of A_missing, without a state recursion or large prior.
missing_difference_oracle <- function(values, period, d, seasonal_d, variance = 1) {
  operator <- diag(length(values))
  if (d) operator <- diff(operator, differences = d)
  if (seasonal_d) operator <- diff(operator, lag = period, differences = seasonal_d)
  missing <- which(is.na(values))
  observed <- which(!is.na(values))
  signal <- drop(operator[, observed, drop = FALSE] %*% values[observed])
  degrees <- nrow(operator) - length(missing)
  logdet <- 0
  if (length(missing)) {
    design <- operator[, missing, drop = FALSE]
    singular <- svd(design, nu = 0, nv = 0)$d
    stopifnot(length(singular) == length(missing), min(singular) > max(singular) * 1e-10)
    decomposition <- qr(design, LAPACK = TRUE)
    logdet <- 2 * sum(log(abs(diag(qr.R(decomposition)))))
    complement <- qr.Q(decomposition, complete = TRUE)[,
      length(missing) + seq_len(degrees), drop = FALSE]
    errors <- drop(crossprod(complement, signal))
  } else errors <- signal
  sse <- sum(errors^2)
  scale <- if (degrees > 0) sse / degrees else NA_real_
  list(fixed = -.5 * (degrees * log(2 * pi * variance) + logdet + sse / variance),
       profile = if (degrees > 0) -.5 * (degrees * (log(2 * pi * scale) + 1) + logdet) else NA_real_,
       scale = scale, sse = sse, degrees = degrees)
}
local({
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(73261)
  n <- 90L
  masks <- list(complete = integer(), registered = c(1, 3, 5, 6, 16, 21, 23, 86),
    internal = c(20, 21, 44), edges = c(1:6, 86:90),
    periodic = seq(3, n, by = 3), blocks = c(1:20, 44:65),
    sparse = setdiff(seq_len(n), c(1, 21, 41, 61, 81)),
    all_missing = seq_len(n), random = sample(seq_len(n), 36L))
  for (period in c(4, 12)) for (d in 0:2) for (seasonal_d in 0:2) {
    for (mask_name in names(masks)) {
      mask <- masks[[mask_name]]
      values <- sin(seq_len(n) * sqrt(2)) + cos(seq_len(n) * sqrt(3)) / 2
      values[mask] <- NA_real_
      expected <- rank_oracle(values, period, d, seasonal_d)
      if (period == 4 && d == 2 && seasonal_d == 1 && mask_name == "sparse") {
        # This valid but intentionally unidentifiable design still exceeds the
        # floating propagated-basis residual bound. Preserve its precise limit;
        # the exact certificate must not weaken the numerical safeguard.
        expect_equal(expected$remaining, 3L)
        expect_error(sarima(values, period = period, d = d, seasonal_d = seasonal_d),
                     "certified diffuse observation-null identity failed its residual check")
        next
      }
      actual <- tryCatch(sarima(values, period = period, d = d, seasonal_d = seasonal_d),
        error = function(error) stop("Rank stress s=", period, ", d=", d,
          ", D=", seasonal_d, ", mask=", mask_name, ": ", conditionMessage(error)))
      expect_equal(actual$update_kind, expected$kind)
      expect_equal(actual$rank_before, expected$before)
      expect_equal(actual$rank_after, expected$after)
      expect_equal(actual$remaining_diffuse_rank, expected$remaining)
      expect_equal(actual$diffuse_count, sum(expected$kind == 1L))
      expect_equal(actual$finite_count, sum(expected$kind == 2L))
      expect_equal(actual$numerics$exact_history_certification, d + period * seasonal_d > 0)
      expect_equal(actual$numerics$exact_history_pivot_count, sum(expected$kind == 1L))
      expect_equal(actual$numerics$exact_history_zero_count,
                   sum(expected$kind == 2L & expected$before > 0L))
      if (expected$remaining) {
        expect_equal(actual$likelihood_status, "unresolved_diffuse")
        expect_true(is.na(actual$fixed_log_likelihood))
      } else {
        expect_true(is.finite(actual$fixed_log_likelihood))
        integrated <- missing_difference_oracle(values, period, d, seasonal_d)
        expect_equal(actual$fixed_log_likelihood, integrated$fixed, tolerance = 1e-7)
        expect_equal(actual$profiled_log_likelihood, integrated$profile, tolerance = 1e-7)
        expect_equal(actual$profiled_variance, integrated$scale, tolerance = 1e-7)
      }
    }
  }
})

# The same long sparse prefix can fail its numerical residual check before
# later observations would identify all coordinates. Final design rank alone
# does not guarantee that floating propagation stays within supported bounds.
late_identified <- rep(NA_real_, 90)
late_rows <- c(1, 21, 41, 61, 81, 87:90)
late_identified[late_rows] <- sin(late_rows * sqrt(2)) + cos(late_rows * sqrt(3)) / 2
expect_equal(rank_oracle(late_identified, 4, 2, 1)$remaining, 0L)
expect_error(sarima(late_identified, period = 4, d = 2, seasonal_d = 1),
             "certified diffuse observation-null identity failed its residual check")

# Equal divided-difference knot sums across distinct seasonal phases must not
# identify the second global polynomial coefficient prematurely. The resulting
# dependent observation is a finite update while one diffuse direction remains.
special_patterns <- list(
  list(period = 4, seasonal_d = 1, n = 20L, observed = c(1:4, 7, 9, 13),
       finite_at = 9L, final_pivot = 13L, difference = c(1, -2, 1, 0, -1, 2, -1)),
  list(period = 12, seasonal_d = 2, n = 60L, observed = c(1:24, 29, 37, 49),
       finite_at = 37L, final_pivot = 49L,
       difference = c(1, -2, 1, rep(0, 9), -2, 4, -2, rep(0, 9), 1, -2, 1))
)
for (settings in special_patterns) {
  values <- rep(NA_real_, settings$n)
  values[settings$observed] <- sin(settings$observed / 3) + settings$observed / 20
  actual <- sarima(values, period = settings$period, d = 2, seasonal_d = settings$seasonal_d)
  expected_rank <- rank_oracle(values, settings$period, 2, settings$seasonal_d)
  expect_equal(actual$update_kind, expected_rank$kind)
  expect_equal(actual$update_kind[settings$finite_at], 2L)
  expect_equal(actual$rank_after[settings$finite_at], 1L)
  expect_equal(actual$update_kind[settings$final_pivot], 1L)
  expect_equal(actual$rank_after[settings$final_pivot], 0L)
  joint <- integrated_joint(settings$n, numeric(), numeric(), settings$difference)
  expected <- flat_prior_oracle(values, joint)
  expect_equal(actual$fixed_log_likelihood, expected$fixed, tolerance = 1e-7)
  expect_equal(actual$profiled_log_likelihood, expected$profile, tolerance = 1e-7)
}
