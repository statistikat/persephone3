# Transformation and regular-domain contracts are independent of Java.
transform <- getFromNamespace(".native_transform", "persephone3")
inverse <- getFromNamespace(".native_inverse_transform", "persephone3")
prepare <- getFromNamespace("native_prepare_inputs", "persephone3")

original <- c(1, exp(1), NA_real_, 100, NaN, .Machine$double.xmin)
logged <- transform(original, "log")
expect_equal(logged$values, log(original))
expect_equal(logged$log_jacobian, -log(original))
expect_identical(logged$missing, is.na(original))
expect_equal(inverse(logged$values, "log"), original, tolerance = 1e-12)
expect_equal(logged$log_jacobian[1:2], c(0, -1))
unchanged <- transform(c(-2, 0, NA_real_, 5), "none")
expect_equal(unchanged$values, c(-2, 0, NA_real_, 5))
expect_equal(unchanged$log_jacobian, c(0, 0, NA_real_, 0))
expect_equal(inverse(unchanged$values), unchanged$values)
expect_equal(transform(numeric())$values, numeric())
expect_equal(inverse(numeric(), "log"), numeric())
expect_equal(transform(c(NA_real_, NaN), "log")$missing, c(TRUE, TRUE))
expect_error(transform(c(0, 1), "log"), "positive")
expect_error(transform(c(-1, 1), "log"), "positive")
expect_error(transform(Inf), "finite")
expect_error(transform(-Inf, "log"), "finite")
expect_error(transform(1, "auto"), "'none' or 'log'")
expect_error(inverse(1, "auto"), "'none' or 'log'")
expect_error(inverse(Inf, "log"), "finite")
expect_error(inverse(1000, "log"), "overflowed or underflowed")
expect_error(inverse(-1000, "log"), "overflowed or underflowed")
for (bad in list(1+2i, "1", TRUE, factor("a"), list(1), matrix(1:4, 2), array(1:8, c(2, 2, 2)))) {
  expect_error(transform(bad), "real numeric vector")
  expect_error(inverse(bad), "real numeric vector")
}
for (bad in list(NA_character_, c("log", "none"), 1, TRUE)) {
  expect_error(transform(1, bad), "'none' or 'log'")
  expect_error(inverse(1, bad), "'none' or 'log'")
}
# No state survives a failed conversion; no implicit lognormal bias correction.
expect_equal(inverse(c(0, 1), "log"), c(1, exp(1)))
expect_equal(original[1:2], c(1, exp(1)))

for (frequency in c(4, 12)) {
  n <- 3 * frequency
  series <- ts(101:(100 + n), start = c(2000, 2), frequency = frequency)
  series[5] <- NA_real_
  backcast <- 2L
  forecast <- frequency
  # Deliberately supply one extra period at both ends to test actual cropping.
  reg_start <- stats::tsp(series)[1] - (backcast + 1) / frequency
  supplied <- ts(seq_len(n + backcast + forecast + 2), start = reg_start,
                 frequency = frequency)
  supplied[c(1, length(supplied))] <- NA_real_
  regressors <- list(calendar = supplied, activity = supplied * 10)
  before <- serialize(list(series, regressors), NULL)
  prepared <- prepare(series, regressors, "log", forecast, backcast)
  expect_inherits(prepared, "persephone_native_inputs")
  expect_identical(prepared$schema_version, 1L)
  expect_identical(prepared$original, series)
  expect_equal(prepared$transformed, log(series))
  expect_equal(prepared$log_jacobian, -log(series))
  expect_equal(prepared$missing, as.logical(is.na(series)))
  expect_equal(prepared$observed_rows, backcast + seq_len(n))
  expect_equal(prepared$required_domain$length, n + backcast + forecast)
  expect_equal(prepared$required_domain$start, c(1999, frequency))
  expect_equal(prepared$observed_domain$start, c(2000, 2))
  expect_equal(prepared$required_domain$end, c(2004, 1))
  expect_identical(colnames(prepared$regressors), c("calendar", "activity"))
  used <- 2:(length(supplied) - 1)
  expect_equal(prepared$regressors, cbind(calendar = used, activity = 10 * used))
  expect_identical(serialize(list(series, regressors), NULL), before)
  expect_identical(unserialize(serialize(prepared, NULL)), prepared)
  expect_equal(prepare(series, cbind(calendar = supplied, activity = supplied * 10),
                       "log", forecast, backcast), prepared)

  no_regressors <- prepare(series, forecast_horizon = forecast)
  expect_equal(dim(no_regressors$regressors), c(n + forecast, 0L))
  expect_equal(no_regressors$transformed, series)
  expect_equal(no_regressors$log_jacobian[!is.na(series)], rep(0, n - 1))
  expect_equal(no_regressors$missing, as.logical(is.na(series)))
  # Neither a missing response nor an absent future horizon excuses missing
  # regressor data within the explicitly requested domain.
  interior_missing <- supplied
  interior_missing[backcast + 6] <- NA_real_
  expect_error(prepare(series, list(x = interior_missing), "none", forecast, backcast),
               "missing or nonfinite")
  interior_infinite <- supplied
  interior_infinite[3] <- Inf
  expect_error(prepare(series, list(x = interior_infinite), "none", forecast, backcast),
               "missing or nonfinite")
  expect_error(prepare(series, list(x = window(supplied, end = stats::end(series))),
                       forecast_horizon = forecast), "complete.*forecast domain")
  expect_error(prepare(series, list(x = window(supplied, start = stats::start(series))),
                       backcast_horizon = backcast), "complete.*forecast domain")
  expect_error(prepare(series, list(x = ts(1:100, start = 1999,
                                          frequency = if (frequency == 12) 4 else 12))),
               "frequency does not match")
  expect_error(prepare(series, list(supplied)), "distinct nonempty names")
  expect_error(prepare(series, list(x = supplied, x = supplied)), "distinct nonempty names")
  expect_error(prepare(series, supplied), "named list")
  expect_error(prepare(series, list(x = as.numeric(supplied))), "univariate numeric ts")
  expect_error(prepare(series, list(x = supplied + 1i)), "univariate numeric ts")
  expect_error(prepare(series, data.frame(x = as.numeric(supplied))), "named list")
  for (bad in list(-1, .5, NA_real_, Inf, "1", c(1, 2), 1i)) {
    expect_error(prepare(series, forecast_horizon = bad), "forecast_horizon")
    expect_error(prepare(series, backcast_horizon = bad), "backcast_horizon")
  }
  expect_error(prepare(series, forecast_horizon = .Machine$integer.max), "matrix row range")
  expect_error(prepare(series, transformation = "auto"), "transformation")
  expect_error(prepare(series, transformation = NA_character_), "transformation")
  expect_error(prepare(series, transformation = c("none", "log")), "transformation")
}
expect_error(prepare(1:24), "univariate numeric ts")
expect_error(prepare(ts(1:24, frequency = 2)), "monthly or quarterly")
expect_error(prepare(ts(1:24, start = 2000.01, frequency = 12)), "exact monthly")
expect_error(prepare(ts(c(1, Inf), start = 2000, frequency = 12)), "finite")
expect_error(prepare(ts(c(1, 0), start = 2000, frequency = 12), transformation = "log"), "positive")
single <- prepare(ts(1, start = c(2000, 4), frequency = 4),
                   list(x = ts(2, start = c(2000, 4), frequency = 4)))
expect_equal(dim(single$regressors), c(1L, 1L))
expect_equal(unname(single$regressors[1, 1]), 2)
expect_equal(single$observed_rows, 1L)
