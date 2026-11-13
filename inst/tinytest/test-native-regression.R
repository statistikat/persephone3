# Regression expectations come from dense Gaussian integration and base R QR,
# never from a native transition, gain, innovation or coefficient estimate.
local({
  evaluate <- getFromNamespace("native_regression_likelihood", "persephone3")
  fixed_likelihood <- getFromNamespace("native_fixed_likelihood", "persephone3")
  core <- getFromNamespace(".native_regression_likelihood", "persephone3")

  gaussian_regression <- function(y, x, fixed = rep(NA_real_, ncol(x)),
      period = 4L, d = 0L, D = 0L, ar = 0, ma = 0, mean = 0, variance = 1,
      covariance = NULL) {
    x <- unname(x)
    n <- length(y)
    operator <- diag(n)
    if (d) operator <- diff(operator, differences = d)
    if (D) operator <- diff(operator, lag = period, differences = D)
    m <- nrow(operator)
    if (is.null(covariance)) {
      gamma0 <- (1 + ma^2 + 2 * ar * ma) / (1 - ar^2)
      gamma1 <- ar * gamma0 + ma
      covariance <- toeplitz(c(gamma0, ar^(seq_len(m - 1L) - 1L) * gamma1))
    }
    stopifnot(identical(dim(covariance), c(m, m)))
    root <- chol(covariance)
    white <- forwardsolve(t(root), operator)
    observed <- which(!is.na(y))
    missing <- which(is.na(y))
    finite <- m - length(missing)
    logdet <- 2 * sum(log(diag(root)))
    if (length(missing)) {
      nuisance <- white[, missing, drop = FALSE]
      singular <- svd(nuisance, nu = 0L, nv = 0L)$d
      stopifnot(length(singular) == length(missing),
                min(singular) > max(singular) * 1e-10)
      decomposition <- qr(nuisance, LAPACK = TRUE)
      logdet <- logdet + 2 * sum(log(abs(diag(qr.R(decomposition)))))
      complement <- qr.Q(decomposition, complete = TRUE)[,
        length(missing) + seq_len(finite), drop = FALSE]
      map <- crossprod(complement, white[, observed, drop = FALSE])
    } else map <- white
    known <- which(!is.na(fixed))
    free <- which(is.na(fixed))
    fixed_effect <- if (length(known)) drop(x[, known, drop = FALSE] %*% fixed[known]) else rep(0, n)
    response <- drop(map %*% (y[observed] - fixed_effect[observed] - mean))
    design <- map %*% x[observed, free, drop = FALSE]
    coefficients <- fixed
    unscaled <- matrix(numeric(), 0L, 0L)
    residual <- response
    if (length(free)) {
      singular <- svd(design, nu = 0L, nv = 0L)$d
      stopifnot(length(singular) == length(free), min(singular) > max(singular) * 1e-10)
      coefficients[free] <- qr.solve(design, response)
      residual <- drop(response - design %*% coefficients[free])
      unscaled <- solve(crossprod(design))
    }
    q <- sum(residual^2)
    scale <- q / finite
    list(coefficients = coefficients, unscaled = unscaled, q = q,
      finite = finite, logdet = logdet, scale = scale, response = response,
      design = design, fixed_effect = fixed_effect,
      fitted = if (ncol(x)) drop(x %*% coefficients) else rep(0, n),
      fixed = -.5 * (finite * log(2 * pi * variance) + logdet + q / variance),
      profile = -.5 * (finite * (log(2 * pi) + 1 + log(scale)) + logdet),
      residual_df = finite - length(free))
  }

  check_fit <- function(actual, expected, variance = 1, label = "") {
    expect_inherits(actual, "persephone_native_regression_likelihood", info = label)
    expect_equal(actual$regression$status, "available", info = label)
    expect_equal(unname(actual$regression$coefficients), expected$coefficients, tolerance = 1e-7, info = label)
    expect_equal(actual$regression$free_count, ncol(expected$design), info = label)
    expect_equal(actual$regression$rank, ncol(expected$design), info = label)
    expect_equal(actual$regression$residual_df, expected$residual_df, info = label)
    expect_equal(actual$likelihood$finite_count, expected$finite, info = label)
    expect_equal(actual$likelihood$transformed_fixed, expected$fixed, tolerance = 1e-7, info = label)
    expect_equal(actual$likelihood$transformed_profiled, expected$profile, tolerance = 1e-7, info = label)
    expect_equal(actual$likelihood$profiled_variance, expected$scale, tolerance = 1e-7, info = label)
    expect_equal(unname(actual$regression$unscaled_covariance), expected$unscaled, tolerance = 1e-7, info = label)
    expect_equal(unname(actual$regression$covariance_fixed_scale), expected$unscaled * variance, tolerance = 1e-7, info = label)
    expect_equal(unname(actual$regression$covariance_ml), expected$unscaled * expected$scale, tolerance = 1e-7, info = label)
    expect_equal(unname(actual$regression$covariance_residual),
      expected$unscaled * expected$q / expected$residual_df, tolerance = 1e-7, info = label)
    # Innovation and batch-whitening coordinates can differ orthogonally;
    # their normal equations and likelihood norm must coincide.
    expect_equal(unname(crossprod(actual$regression$whitened_design)),
      crossprod(expected$design), tolerance = 1e-7, info = label)
    expect_equal(unname(crossprod(actual$regression$whitened_design,
                                  actual$regression$whitened_response)),
      crossprod(expected$design, expected$response), tolerance = 1e-7, info = label)
    expect_equal(sum(actual$regression$whitened_response^2), sum(expected$response^2), tolerance = 1e-7, info = label)
    expect_equal(as.numeric(actual$effects$fixed), expected$fixed_effect, tolerance = 1e-7, info = label)
    expect_equal(as.numeric(actual$effects$fitted), expected$fitted, tolerance = 1e-7, info = label)
    expect_equal(actual$likelihood$objective, "concentrated_ml", info = label)
    expect_equal(actual$likelihood$nparams_profiled, ncol(expected$design) + 1L, info = label)
    expect_equal(actual$trace_uncertainty, "conditional_on_coefficients", info = label)
  }

  # White-noise intercept/slope: independent OLS, plus a fixed zero coefficient
  # that must remain in the design/specification and out of free covariance.
  t <- seq_len(40)
  x <- cbind(level = 1, slope = (t - 20) / 20, disabled = cos(t / 4))
  y <- ts(1.2 - .7 * x[, "slope"] + sin(t * sqrt(2)) / 5,
          start = c(2000, 2), frequency = 4)
  columns <- lapply(seq_len(ncol(x)), function(i) ts(x[, i], start = start(y), frequency = 4))
  names(columns) <- colnames(x)
  value <- evaluate(y, columns, coefficients = c(disabled = 0), variance = 2)
  oracle <- gaussian_regression(as.numeric(y), x, c(NA, NA, 0), variance = 2)
  check_fit(value, oracle, 2, "white OLS")
  expect_identical(evaluate(y, columns,
    coefficients = c(disabled = 0, slope = NA_real_, level = NA_real_), variance = 2), value)
  expect_identical(names(value$regression$coefficients), colnames(x))
  expect_identical(unname(value$regression$coefficient_states), c("estimated", "estimated", "fixed"))
  expect_identical(dimnames(value$regression$unscaled_covariance), list(c("level", "slope"), c("level", "slope")))
  expect_equal(as.numeric(value$traces$predictions), oracle$fitted, tolerance = 1e-7)
  expect_equal(as.numeric(value$effects$residual_series), as.numeric(y) - oracle$fitted, tolerance = 1e-7)
  expect_identical(unserialize(serialize(value, NULL)), value)

  for (period in c(4L, 12L)) for (orders in list(c(0L, 0L), c(1L, 0L), c(0L, 1L),
      c(1L, 1L), c(2L, 1L), c(1L, 2L), c(2L, 2L))) {
    for (correlated in c(FALSE, TRUE)) {
      n <- 84L
      t <- seq_len(n)
      x <- cbind(first = sin(t / 5) + t / n / 10,
                 second = ((t %% 7) - 3) / 4, known = cos(t / 11))
      y <- .9 * x[, 1] - .3 * x[, 2] + .2 * x[, 3] +
        .15 * sin(t * sqrt(2)) + .08 * cos(t * sqrt(3))
      y[c(1L, 15L, 16L, 43L, 84L)] <- NA_real_
      level <- if (sum(orders) == 0L) .4 else 0
      y <- ts(y + level, start = c(1998, 3), frequency = period)
      regressors <- ts(x, start = start(y), frequency = period)
      ar <- if (correlated) .3 else 0
      ma <- if (correlated) -.2 else 0
      before <- serialize(list(y, regressors), NULL)
      actual <- evaluate(y, regressors, coefficients = c(known = .2),
        ar = ar, ma = ma, d = orders[1], seasonal_d = orders[2], mean = level, variance = .7)
      expected <- gaussian_regression(as.numeric(y), x, c(NA, NA, .2), period,
        orders[1], orders[2], ar, ma, level, .7)
      label <- paste("Gaussian s", period, "d", orders[1], "D", orders[2], "ARMA", correlated)
      check_fit(actual, expected, .7, label)
      expect_identical(serialize(list(y, regressors), NULL), before, info = label)
      expect_equal(actual$regression$finite_source_rows,
                   actual$span$source_rows[actual$regression$finite_rows], info = label)
      expect_equal(tsp(actual$effects$fitted), tsp(y), info = label)
      expect_equal(is.na(actual$effects$residual_series), is.na(y), info = label)
    }
  }

  # Multiplicative regular/seasonal MA covariance is a finite convolution:
  # psi(B)=(1+theta*B)(1+Theta*B^s), gamma_h=sum_j psi_j psi_(j+h).
  # This tests seasonal covariance jointly with regression and two differences.
  for (period in c(4L, 12L)) {
    n <- 84L
    t <- seq_len(n)
    x <- cbind(a = sin(t / 5), b = cos(t / 11), known = (t %% 5) / 4)
    y <- .8 * x[, 1L] - .4 * x[, 2L] + .1 * x[, 3L] + sin(t * sqrt(2)) / 7
    y[c(1L, 15L, 16L, 43L, 84L)] <- NA_real_
    theta <- .2
    seasonal_theta <- -.3
    psi <- numeric(period + 2L)
    psi[c(1L, 2L, period + 1L, period + 2L)] <-
      c(1, theta, seasonal_theta, theta * seasonal_theta)
    m <- n - 1L - period
    gamma <- vapply(0:(m - 1L), function(lag) {
      if (lag >= length(psi)) return(0)
      j <- seq_len(length(psi) - lag)
      sum(psi[j] * psi[j + lag])
    }, numeric(1))
    expected <- gaussian_regression(y, x, c(NA, NA, .1), period = period,
      d = 1L, D = 1L, variance = .8, covariance = toeplitz(gamma))
    actual <- evaluate(ts(y, frequency = period), ts(x, frequency = period),
      coefficients = c(known = .1), ma = theta, seasonal_ma = seasonal_theta,
      d = 1, seasonal_d = 1, variance = .8)
    check_fit(actual, expected, .8, paste("seasonal MA", period))
  }

  # No design and all-fixed designs retain the fixed-model likelihood; free
  # regression coefficients do not introduce a REML determinant or n-p divisor.
  y <- ts(sin(seq_len(40) / 5), start = c(2000, 1), frequency = 4)
  no_design <- evaluate(y, ar = .2, d = 1)
  direct <- fixed_likelihood(y, ar = .2, d = 1)
  expect_equal(no_design$likelihood$transformed_fixed, direct$likelihood$transformed_fixed)
  expect_equal(no_design$likelihood$transformed_profiled, direct$likelihood$transformed_profiled)
  expect_equal(dim(no_design$regression$unscaled_covariance), c(0L, 0L))
  expect_equal(length(no_design$regression$coefficients), 0L)
  known <- list(x = ts(cos(seq_len(40) / 7), start = start(y), frequency = 4))
  all_fixed <- evaluate(y, known, coefficients = c(x = .8), d = 1)
  fixed_direct <- fixed_likelihood(y - .8 * known$x, d = 1)
  expect_equal(all_fixed$likelihood$transformed_fixed, fixed_direct$likelihood$transformed_fixed)
  expect_equal(all_fixed$likelihood$transformed_profiled, fixed_direct$likelihood$transformed_profiled)
  expect_equal(all_fixed$regression$free_count, 0L)
  expect_identical(all_fixed$regression$coefficient_states, c(x = "fixed"))

  # Generated mean and drift columns have explicit and separately checked units.
  y <- ts(2 + sin(seq_len(40) * sqrt(2)), start = c(2000, 1), frequency = 4)
  mean_fit <- evaluate(y, include_mean = TRUE)
  expect_equal(unname(mean_fit$regression$coefficients), mean(y))
  expect_equal(as.numeric(mean_fit$effects$fitted), rep(mean(y), length(y)))
  expect_equal(mean_fit$regression$generated_columns, "mean")
  for (period in c(4L, 12L)) for (orders in list(c(1L, 0L), c(0L, 1L), c(1L, 1L), c(2L, 2L))) {
    t <- seq_len(84)
    degree <- sum(orders)
    drift <- t^degree / (factorial(degree) * period^orders[2L])
    delta <- drift
    if (orders[1L]) delta <- diff(delta, differences = orders[1L])
    if (orders[2L]) delta <- diff(delta, lag = period, differences = orders[2L])
    expect_equal(delta, rep(1, length(delta)), tolerance = 1e-8)
    y <- ts(.6 * drift + sin(t * sqrt(2)) / 5, start = c(2000, 1), frequency = period)
    fit <- evaluate(y, d = orders[1], seasonal_d = orders[2], include_drift = TRUE)
    expected <- gaussian_regression(as.numeric(y), matrix(drift, ncol = 1),
      period = period, d = orders[1], D = orders[2])
    check_fit(fit, expected, label = paste("generated drift", period, paste(orders, collapse = ",")))
    expect_equal(as.numeric(fit$inputs$regressors[, "drift"]), drift)
    expect_equal(fit$regression$drift_policy, "unit_differenced_mean_one_based_source_index")
  }

  # Windows select responses and regressors consistently. Future/past values
  # enter only the deterministic context effect, not the coefficient objective.
  for (period in c(4L, 12L)) {
    n <- 120L
    t <- seq_len(n)
    x <- sin(t / 9) + t / 40
    positive <- ts(exp(2 + .15 * x + sin(t * sqrt(2)) / 20), start = c(2000, 1), frequency = period)
    rows <- (2L * period + 1L):(8L * period)
    positive[c(rows[1L], rows[2L], rows[8L])] <- NA_real_
    reg <- list(x = ts(x, start = start(positive), frequency = period))
    fit <- evaluate(positive, reg, d = 1, transformation = "log",
      estimation_start = c(2002, 1), estimation_end = c(2007, period),
      forecast_horizon = 2, backcast_horizon = 1)
    selected <- window(positive, start = c(2002, 1), end = c(2007, period))
    expected <- gaussian_regression(log(as.numeric(selected)), matrix(x[rows], ncol = 1), period = period, d = 1)
    check_fit(fit, expected, label = paste("log window", period))
    observed <- which(!is.na(selected))
    jacobian_rows <- observed[observed >= observed[1L] + 1L]
    jacobian <- -sum(log(selected[jacobian_rows]))
    expect_equal(fit$adjustment$log_jacobian, jacobian)
    expect_equal(fit$likelihood$adjusted_fixed, expected$fixed + jacobian)
    expect_equal(fit$likelihood$adjusted_profiled, expected$profile + jacobian)
    expect_equal(fit$adjustment$local_rows, jacobian_rows)
    expect_equal(fit$adjustment$source_rows, rows[jacobian_rows])
    expect_equal(fit$span$source_rows, rows)
    expect_equal(fit$inputs$observed_rows, seq_along(rows) + 1L)
    context_rows <- (min(rows) - 1L):(max(rows) + 2L)
    expect_equal(as.numeric(fit$effects$context), x[context_rows] * expected$coefficients)
    expect_equal(tsp(fit$inputs$original), tsp(selected))
    changed <- reg
    changed$x[setdiff(context_rows, rows)] <- changed$x[setdiff(context_rows, rows)] + 7
    again <- evaluate(positive, changed, d = 1, transformation = "log",
      estimation_start = c(2002, 1), estimation_end = c(2007, period),
      forecast_horizon = 2, backcast_horizon = 1)
    expect_identical(again$likelihood, fit$likelihood)
    expect_identical(again$regression$coefficients, fit$regression$coefficients)
    expect_true(any(again$effects$context != fit$effects$context))
    expect_error(evaluate(positive, list(x = window(reg$x, start = c(2002, 1), end = c(2007, period))),
      estimation_start = c(2002, 1), estimation_end = c(2007, period), forecast_horizon = 1), "cover")
    drift <- evaluate(positive, d = 1, include_drift = TRUE, transformation = "log",
      estimation_start = c(2002, 1), estimation_end = c(2007, period),
      forecast_horizon = 2, backcast_horizon = 1)
    expect_equal(as.numeric(drift$inputs$regressors[, "drift"]), context_rows)
    expect_identical(unserialize(serialize(fit, NULL)), fit)
  }

  # Identification failures are explicit, rather than silently choosing one
  # solution of an aliased model or pretending the objective is available.
  y <- ts(sin(seq_len(40) / 7), frequency = 4)
  one <- ts(rep(1, 40), frequency = 4)
  trend <- ts(seq_len(40), frequency = 4)
  for (design in list(list(a = trend, b = trend), list(zero = 0 * trend))) {
    aliased <- evaluate(y, design)
    expect_equal(aliased$regression$status, "rank_deficient")
    expect_true(all(is.na(aliased$regression$coefficients)))
    expect_true(is.na(aliased$likelihood$transformed_fixed))
    expect_true(all(is.na(aliased$regression$unscaled_covariance)))
    expect_true(is.null(aliased$traces))
  }
  homogeneous <- evaluate(y, list(level = one), d = 1)
  expect_equal(homogeneous$regression$status, "rank_deficient")
  expect_equal(homogeneous$regression$rank, 0L)
  expect_true(is.na(homogeneous$likelihood$transformed_profiled))
  mixed <- evaluate(y, list(a = trend, b = trend, level = one), coefficients = c(level = 2))
  expect_equal(unname(mixed$regression$coefficients["level"]), 2)
  expect_equal(unname(mixed$regression$coefficient_states["level"]), "fixed")
  short <- evaluate(ts(c(1, NA, 3), frequency = 12),
                    list(x = ts(1:3, frequency = 12)), seasonal_d = 1)
  expect_equal(short$regression$status, "unresolved_diffuse")
  expect_true(is.na(short$likelihood$transformed_fixed))
  fixed_short <- evaluate(ts(c(1, NA, 3), frequency = 12),
    list(x = ts(1:3, frequency = 12)), coefficients = c(x = 2), seasonal_d = 1)
  expect_equal(fixed_short$regression$status, "unresolved_diffuse")
  expect_identical(fixed_short$regression$coefficient_states, c(x = "fixed"))
  expect_equal(fixed_short$regression$coefficients, c(x = 2))
  expect_equal(as.numeric(fixed_short$effects$fixed), c(2, 4, 6))
  expect_equal(as.numeric(fixed_short$effects$fitted), c(2, 4, 6))
  expect_equal(as.numeric(fixed_short$effects$residual_series), c(-1, NA, -3))
  expect_equal(as.numeric(fixed_short$effects$context), c(2, 4, 6))
  expect_true(is.na(fixed_short$likelihood$transformed_fixed))
  no_finite <- evaluate(ts(2, frequency = 4), list(x = ts(1, frequency = 4)), d = 1)
  expect_equal(no_finite$regression$status, "no_finite_innovations")
  expect_true(is.na(no_finite$likelihood$transformed_profiled))
  saturated <- evaluate(ts(2, frequency = 4), list(x = ts(1, frequency = 4)), variance = 2)
  expect_equal(unname(saturated$regression$coefficients), 2)
  expect_equal(saturated$regression$residual_df, 0L)
  expect_equal(saturated$likelihood$profile_status, "zero_scale_boundary")
  expect_equal(saturated$likelihood$profiled_variance, 0)
  expect_equal(saturated$likelihood$transformed_profiled, Inf)
  expect_equal(saturated$likelihood$transformed_fixed, dnorm(0, sd = sqrt(2), log = TRUE))
  expect_equal(unname(saturated$regression$covariance_fixed_scale), matrix(2))
  expect_equal(unname(saturated$regression$covariance_ml), matrix(0))
  expect_true(all(is.na(saturated$regression$covariance_residual)))

  # Substantial integrated whitening can run independent response/design
  # channels on workers; the model and output arithmetic must remain unchanged.
  old_threads <- Sys.getenv("PERSEPHONE_NUM_THREADS", unset = NA_character_)
  on.exit(if (is.na(old_threads)) Sys.unsetenv("PERSEPHONE_NUM_THREADS") else
    Sys.setenv(PERSEPHONE_NUM_THREADS = old_threads), add = TRUE)
  y <- ts(sin(seq_len(96) * sqrt(2)), frequency = 12)
  regressors <- list(a = ts(cos(seq_len(96) / 4), frequency = 12),
                    b = ts(sin(seq_len(96) / 7), frequency = 12))
  Sys.setenv(PERSEPHONE_NUM_THREADS = "1")
  serial <- evaluate(y, regressors, d = 2, seasonal_d = 2)
  Sys.setenv(PERSEPHONE_NUM_THREADS = "4")
  parallel <- evaluate(y, regressors, d = 2, seasonal_d = 2)
  expect_identical(parallel, serial)

  # Rcpp's accepted NULL scalar defaults must also be explicit in R metadata.
  defaults <- evaluate(y, regressors, mean = NULL, variance = NULL,
                       rank_tolerance = NULL)
  expect_identical(defaults, evaluate(y, regressors))

  expect_error(evaluate(1:40), "univariate numeric ts")
  expect_error(evaluate(y, include_mean = 1), "logical")
  expect_error(evaluate(y, include_mean = matrix(TRUE)), "logical")
  expect_error(evaluate(y, include_drift = NA), "logical")
  expect_error(evaluate(y, include_mean = TRUE, mean = 1), "two level")
  expect_error(evaluate(y, include_mean = TRUE, d = 1), "stationary")
  expect_error(evaluate(y, include_drift = TRUE), "requires differencing")
  expect_error(evaluate(y, list(mean = ts(rep(1, length(y)), frequency = 12)), include_mean = TRUE), "conflict")
  for (bad in list(c(a = NaN), c(a = Inf), c(a = 1i), c(1), c(unknown = 1),
                   c(a = 1, a = 2), matrix(1))) {
    expect_error(evaluate(y, regressors, coefficients = bad), "coefficients")
  }
  for (bad in list(TRUE, NA_real_, .5, -1, 3, 1i, matrix(1))) {
    expect_error(evaluate(y, d = bad), "whole number")
  }
  for (bad in list(0, -1, Inf, NA_real_, 1i, matrix(1))) expect_error(evaluate(y, variance = bad))
  for (bad in list(0, 1, NA_real_, 1i, matrix(.1))) expect_error(evaluate(y, rank_tolerance = bad))
  expect_error(evaluate(y, regressors, forecast_horizon = 1), "cover")
  expect_error(evaluate(y, regressors, backcast_horizon = 1), "cover")
  invalid_reg <- regressors
  invalid_reg$a[2L] <- NA_real_
  y[2L] <- NA_real_
  expect_error(evaluate(y, invalid_reg), "missing or nonfinite")
  overflow_y <- ts(rep(1, 40), frequency = 4)
  overflow_x <- list(x = ts(c(rep(1, 40), .Machine$double.xmax), frequency = 4))
  expect_error(evaluate(overflow_y, overflow_x, coefficients = c(x = 2),
    forecast_horizon = 1), "finite numeric range.*context")
  raw <- function(values = 1:12, design = matrix(1, 12, 1), coefficients = NA_real_) {
    core(values, design, coefficients, numeric(), numeric(), numeric(), numeric(),
         12, 0, 0, 0, 1, 1e-10)
  }
  expect_error(raw(design = matrix(1, 11, 1)), "dimension|match")
  expect_error(raw(design = matrix(NA_real_, 12, 1)), "finite")
  expect_error(raw(design = matrix(TRUE, 12, 1)), "numeric")
  expect_error(raw(design = matrix(1i, 12, 1)), "numeric")
  expect_error(raw(design = array(1, c(12, 1, 1))), "matrix|dimension")
  expect_error(raw(coefficients = matrix(NA_real_)), "vector|dimension")
  expect_error(raw(coefficients = NaN), "finite|NaN|NA")
  expect_error(raw(coefficients = Inf), "finite")
  expect_error(raw(coefficients = c(NA_real_, NA_real_)), "match|coefficient")
})
