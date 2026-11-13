# Fixed-parameter preprocessing contract: window selection, dates and explicit
# pinned-JD+ Jacobian policy. Numerical kernels have their independent suites.
evaluate <- getFromNamespace("native_fixed_likelihood", "persephone3")
sarima_kernel <- getFromNamespace(".native_sarima_likelihood", "persephone3")

# Proper stationary log-Gaussian likelihood has the usual complete Jacobian.
y <- ts(exp(c(.2, -.4, NA, .8, 1.1, NaN, -.2)), start = c(2003, 4), frequency = 4)
proper <- evaluate(y, transformation = "log", mean = .3, variance = 1.7)
seen <- which(!is.na(y))
expected_raw <- sum(dnorm(log(y[seen]), .3, sqrt(1.7), log = TRUE))
expected_scale <- mean((log(y[seen]) - .3)^2)
expect_inherits(proper, "persephone_native_likelihood")
expect_equal(proper$likelihood$transformed_fixed, expected_raw)
expect_equal(proper$likelihood$adjusted_fixed, expected_raw - sum(log(y[seen])))
expect_equal(proper$likelihood$profiled_variance, expected_scale)
expect_equal(proper$likelihood$adjusted_profiled,
  sum(dnorm(log(y[seen]), .3, sqrt(expected_scale), log = TRUE)) - sum(log(y[seen])))
expect_identical(proper$adjustment$policy, "jdplus_3.9.0_calendar_window")
expect_equal(proper$adjustment$local_rows, seen)
expect_equal(proper$span$source_rows, seq_along(y))
expect_equal(proper$parameters$mean, .3)

# Observed log random-walk increments have variance equal to their time gaps.
y <- ts(exp(c(NA, .2, NA, .7, -.1, NA, .4, NA)), start = c(1999, 11), frequency = 12)
walk <- evaluate(y, d = 1, transformation = "log", variance = 2)
seen <- which(!is.na(y))
increments <- diff(log(y[seen]))
gaps <- diff(seen)
raw <- sum(dnorm(increments, sd = sqrt(2 * gaps), log = TRUE))
expect_equal(walk$likelihood$transformed_fixed, raw)
expect_equal(walk$likelihood$adjusted_fixed, raw - sum(log(y[seen[-1L]])))
expect_equal(walk$likelihood$profiled_variance, mean(increments^2 / gaps))
expect_equal(walk$adjustment$local_rows, seen[-1L])
expect_equal(walk$kernel$update_kind, c(0L, 1L, 0L, 2L, 2L, 0L, 2L, 0L))

for (frequency in c(4, 12)) {
  original <- ts(exp(2 + sin(seq_len(120) / 5) + cos(seq_len(120) / 13) / 3),
                 start = c(2000, 3), frequency = frequency)
  from <- 2 * frequency - 1L # 2002 period 1, counted from 2000 period 3.
  through <- 8 * frequency - 2L # 2007's final period, inclusive.
  original[unique(c(1, 2, from, from + 1L, from + 7L, 44, 119, 120))] <- NA_real_
  before <- serialize(original, NULL)
  ranges <- list(list(start = NULL, end = NULL, rows = 1:120),
                 list(start = c(2002, 1), end = c(2007, frequency), rows = from:through))
  for (range in ranges) for (transformation in c("none", "log")) {
    for (orders in list(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(2, 1), c(1, 2), c(2, 2))) {
      value <- evaluate(original, ar = .2, ma = -.1,
        d = orders[1], seasonal_d = orders[2], transformation = transformation,
        estimation_start = range$start, estimation_end = range$end, variance = 1.4)
      selected <- if (is.null(range$start)) original else
        window(original, start = range$start, end = range$end)
      transformed <- if (transformation == "log") log(selected) else selected
      direct <- sarima_kernel(as.numeric(transformed), .2, -.1, numeric(), numeric(),
                              frequency, orders[1], orders[2], 0, 1.4)
      observed <- which(!is.na(selected))
      degree <- orders[1] + frequency * orders[2]
      jacobian_rows <- if (transformation == "log")
        observed[observed - min(observed) >= degree] else observed
      jacobian <- if (transformation == "log") -sum(log(selected[jacobian_rows])) else 0
      expect_equal(value$span$source_rows, range$rows)
      expect_equal(value$inputs$original, selected)
      expect_equal(value$inputs$transformed, transformed)
      expect_equal(value$inputs$missing, as.logical(is.na(selected)))
      expect_equal(value$likelihood$transformed_fixed, direct$fixed_log_likelihood)
      expect_equal(value$likelihood$transformed_profiled, direct$profiled_log_likelihood)
      expect_equal(value$likelihood$adjusted_fixed, direct$fixed_log_likelihood + jacobian)
      expect_equal(value$likelihood$adjusted_profiled, direct$profiled_log_likelihood + jacobian)
      expect_equal(value$adjustment$local_rows, jacobian_rows)
      expect_equal(value$adjustment$source_rows, range$rows[jacobian_rows])
      expect_equal(value$adjustment$log_jacobian, jacobian)
      expect_equal(sum(value$adjustment$terms), jacobian)
      expect_true(all(vapply(value$traces, function(x) identical(tsp(x), tsp(selected)), logical(1))))
      expect_equal(tsp(value$adjustment$terms), tsp(selected))
      expect_equal(value$likelihood$finite_count + value$likelihood$diffuse_count, length(observed))
      expect_equal(value$likelihood$normalization, "fixed_initial_diffuse_coordinates")
      expect_equal(value$initialization, "fresh_prior_at_span_start")
    }
  }
  expect_identical(serialize(original, NULL), before)
  roundtrip <- evaluate(original, d = 1)
  expect_identical(unserialize(serialize(roundtrip, NULL)), roundtrip)

  # Values outside an explicit likelihood window are neither transformed nor
  # used to condition its prior; its dates and missing periods stay intact.
  excluded <- original
  excluded[1] <- 0
  excluded[length(excluded)] <- Inf
  windowed <- evaluate(excluded, d = 1, transformation = "log",
                        estimation_start = c(2002, 1), estimation_end = c(2007, frequency))
  expected <- evaluate(window(original, start = c(2002, 1), end = c(2007, frequency)),
                        d = 1, transformation = "log")
  expect_equal(windowed$likelihood, expected$likelihood)
  expect_equal(windowed$traces, expected$traces)
  expect_error(evaluate(excluded, transformation = "log"), "positive|finite")
  expect_error(evaluate(original, estimation_start = c(1999, 1)), "inside")
  expect_error(evaluate(original, estimation_end = c(2040, 1)), "inside")
  expect_error(evaluate(original, estimation_start = c(2005, 1),
                        estimation_end = c(2004, 1)), "must not follow")
  for (invalid in list(NA_real_, c(2000, NA), c(2000.1, 1), c(2000, 0),
                       c(2000, frequency + 1), "2000-01", 2000, c(2000, 1, 2),
                       c(2000, 1) + 1i, matrix(c(2000, 1)), TRUE)) {
    expect_error(evaluate(original, estimation_start = invalid), "c\\(year, period\\)")
    expect_error(evaluate(original, estimation_end = invalid), "c\\(year, period\\)")
  }
}

# A calendar-prefix correction deliberately differs from finite-pivot selection
# with missing observations in the diffuse portion of a seasonal I(2) model.
series <- ts(exp(2 + sin(seq_len(120) / 6)), start = c(2000, 2), frequency = 12)
series[c(1, 3, 5, 6, 16, 21, 23, 86)] <- NA_real_
seasonal <- evaluate(series, seasonal_d = 2, transformation = "log")
expect_equal(seasonal$adjustment$local_rows, setdiff(26:120, 86))
expect_true(length(seasonal$adjustment$local_rows) != seasonal$likelihood$finite_count)
expect_equal(seasonal$adjustment$log_jacobian, -sum(log(series[setdiff(26:120, 86)])))

# Unresolved/no-data objectives remain unavailable; a Jacobian never turns a
# failed or unidentified kernel into an optimizer value.
short <- evaluate(ts(exp(c(.1, NA, .3)), frequency = 12), seasonal_d = 1,
                   transformation = "log")
expect_equal(short$likelihood$status, "unresolved_diffuse")
expect_true(is.na(short$likelihood$adjusted_fixed))
expect_true(is.na(short$likelihood$adjusted_profiled))
expect_equal(short$adjustment$log_jacobian, 0)
empty <- evaluate(ts(rep(NA_real_, 5), frequency = 4), transformation = "log")
expect_equal(empty$likelihood$status, "no_observations")
expect_equal(empty$likelihood$adjusted_fixed, 0)
expect_true(is.na(empty$likelihood$adjusted_profiled))
expect_equal(empty$adjustment$local_rows, integer())
one <- evaluate(ts(exp(.2), start = c(-1, 4), frequency = 4), d = 1,
                transformation = "log", estimation_start = c(-1, 4), estimation_end = c(-1, 4))
expect_equal(one$span$source_rows, 1L)
expect_equal(one$span$estimation_domain$start, c(-1, 4))
expect_equal(one$likelihood$profile_status, "no_finite_innovations")
expect_error(evaluate(1:40), "univariate numeric ts")
expect_error(evaluate(ts(1:40, frequency = 2)), "monthly or quarterly")
expect_error(evaluate(series, transformation = "auto"), "transformation")
expect_error(evaluate(series, transformation = matrix("log")), "transformation")
expect_error(evaluate(series, transformation = array("none", c(1, 1, 1))), "transformation")
for (year in c(-6e14, 6e14)) {
  extreme <- ts(1:13, start = c(year, 1), frequency = 12)
  expect_error(evaluate(extreme, estimation_start = c(year, 2),
                        estimation_end = c(year, 2)), "represented exactly")
}
expect_error(evaluate(series, d = 1, mean = 1), "drift|mean zero")
expect_error(evaluate(series, ar = 1i), "real numeric")
expect_error(evaluate(series, regressors = list(x = series)), "unused argument")
