make_forecasts <- getFromNamespace("new_forecast_contract", "persephone3")
aggregate_forecasts <- getFromNamespace("aggregate_forecast_contracts", "persephone3")
components <- c("y", "t", "sa", "s", "i", "cal")

for (freq in c(12, 4)) {
  observed <- ts(seq_len(2 * freq), start = 2020, frequency = freq)
  future <- ts(100 + seq_len(freq), start = 2022, frequency = freq)
  points <- setNames(lapply(seq_along(components), function(i) future + i), components)
  errors <- list(y = future * 0 + 2)
  one <- make_forecasts(points, errors, observed, mode = "Additive")
  two <- make_forecasts(lapply(points, function(x) x + 20), errors, observed, mode = "Additive")
  expect_inherits(one, "persephone_forecasts")
  expect_identical(one$points, points)
  expect_identical(one$standard_errors$y, errors$y)
  expect_identical(names(one$points), components)
  expect_identical(one$domains$y$length, length(future))
  expect_equal(one$domains$y$start, c(2022, 1))
  expect_identical(unserialize(serialize(one, NULL)), one)
  expect_equal(make_forecasts(list()), NULL)
  expect_equal(make_forecasts(list(y = numeric())), NULL)
  expect_error(make_forecasts(list(y = as.numeric(future))), "univariate numeric ts")
  expect_error(make_forecasts(list(y = future), list(y = errors$y * -1)), "standard errors")
  expect_error(make_forecasts(list(y = future), list(y = ts(rep(1, 2 * freq),
                 start = 2022, frequency = freq))), "aligned prefix")
  expect_error(make_forecasts(list(y = future), list(y = ts(rep(1, freq),
                 start = 2023, frequency = freq))), "aligned prefix")
  expect_error(make_forecasts(list(y = ts(1:freq, start = 2023, frequency = freq)),
                 observed = observed), "immediately after")
  expect_error(make_forecasts(list(y = ts(1:freq, start = 2022, frequency = 2)),
                 observed = observed), "frequency")

  # Per-field horizons are deliberate: actual TRAMO outputs can return shorter
  # calendar forecasts and error forecasts than the original-series points.
  short <- window(future, end = time(future)[freq / 2])
  partial <- make_forecasts(list(y = future, cal = short), list(y = short * 0 + 2),
                            observed, mode = "Additive")
  expect_identical(partial$points$cal, short)
  expect_equal(partial$domains$cal$length, freq / 2)
  expect_equal(partial$standard_error_domains$y$length, freq / 2)
  expect_identical(partial$availability$standard_errors[1], "partial_horizon")
  expect_true(grepl("first", partial$availability$uncertainty_reason[1]))

  added <- aggregate_forecasts(list(a = one, b = two))
  for (name in components) expect_equal(added$points[[name]], one$points[[name]] + two$points[[name]])
  expect_true(all(vapply(added$standard_errors, is.null, logical(1))))
  expect_true(all(grepl("covariance", added$availability$uncertainty_reason)))
  expect_identical(added$aggregation, "selected_children")
  expect_equal(aggregate_forecasts(list(a = one, b = two), c(b = 3, a = 1))$points$y,
               (one$points$y + 3 * two$points$y) / 4)

  weights <- list(a = ts(seq_len(3 * freq), start = 2020, frequency = freq), b = 1)
  future_weights <- 2 * freq + seq_len(freq)
  weighted <- aggregate_forecasts(list(a = one, b = two), weights)
  expect_equal(weighted$points$y,
               ts((as.numeric(one$points$y) * future_weights + as.numeric(two$points$y)) /
                   (future_weights + 1), start = 2022, frequency = freq))
  weights$a <- window(weights$a, end = c(2021, freq))
  carried <- aggregate_forecasts(list(a = one, b = two), weights)
  expect_equal(carried$points$y, (one$points$y * (2 * freq) + two$points$y) / (2 * freq + 1))

  multiplicative <- make_forecasts(points, errors, observed, mode = "Multiplicative")
  expect_identical(unname(multiplicative$scales), c(rep("level", 3), rep("factor", 3)))
  factors <- aggregate_forecasts(list(a = multiplicative, b = multiplicative))
  expect_equal(factors$points$y, points$y * 2)
  expect_equal(factors$points$t, points$t * 2)
  expect_equal(factors$points$sa, points$sa * 2)
  expect_true(all(vapply(factors$points[c("s", "i", "cal")], is.null, logical(1))))
  expect_true(all(grepl("scales", tail(factors$availability$point_reason, 3))))
  mixed <- aggregate_forecasts(list(a = one, b = multiplicative))
  expect_equal(mixed$points$sa, one$points$sa + multiplicative$points$sa)
  expect_equal(mixed$points$s, NULL)
  expect_equal(aggregate_forecasts(list(a = multiplicative))$standard_errors$y, errors$y)
  expect_equal(aggregate_forecasts(list(a = multiplicative))$points$s, points$s)

  missing_sa <- make_forecasts(list(y = future), observed = observed, mode = "Additive")
  incomplete <- aggregate_forecasts(list(a = one, b = missing_sa))
  expect_equal(incomplete$points$y, points$y + future)
  expect_equal(incomplete$points$sa, NULL)
  expect_true(grepl("b", incomplete$availability$point_reason[3]))
  expect_equal(aggregate_forecasts(list(a = NULL, b = NULL)), NULL)
  expect_equal(aggregate_forecasts(list(a = one, b = NULL)), NULL)
  different <- make_forecasts(list(y = short), observed = observed)
  expect_error(aggregate_forecasts(list(a = one, b = different)), "time instances")
  other_frequency <- make_forecasts(list(y = ts(1:freq, start = 2022, frequency = 2)))
  expect_error(aggregate_forecasts(list(a = one, b = other_frequency)), "frequency")
  expect_error(aggregate_forecasts(list(a = one, b = two), c(a = 1, b = -1)), "nonzero")
}

if (all(vapply(c("rjd3toolkit", "rjd3x13", "rjd3tramoseats"),
               requireNamespace, logical(1), quietly = TRUE))) {
  message("Test public forecasts against actual backend values")
  for (freq in c(12, 4)) {
    series <- if (freq == 12) AirPassengers else stats::aggregate(AirPassengers, nfrequency = 4)
    for (method in c("x13", "tramoseats")) {
      constructor <- if (method == "x13") perX13 else perTramo
      x <- constructor(series, "rsa0", backend = "rjd3")
      expect_equal(x$forecasts, NULL)
      expect_equal(x$forecastsDirect, NULL)
      x$run()
      expect_identical(x$forecasts, x$result$forecasts)
      expect_identical(x$forecastsDirect, x$result$forecasts)
      for (name in components) {
        expect_identical(x$forecasts$points[[name]], x$output$user_defined[[paste0(name, "_f")]])
      }
      expect_identical(x$forecasts$standard_errors$y, x$output$user_defined$y_ef)
      expect_equal(tsp(x$forecasts$points$y)[1], tsp(series)[2] + 1 / freq)
      batch <- perBatch(a = x, b = x, method = method, backend = "rjd3")
      expect_identical(batch$forecasts$a, x$forecasts)

      # Keep the same engine/template throughout to compare matching horizons.
      inner <- perHts(a = series, b = series * 1.1 + 10 * sin(seq_along(series)),
                      method = method, template = "rsa0", backend = "rjd3")
      hierarchy <- perHts(group = inner, c = series / 2, method = method,
                          template = "rsa0", backend = "rjd3")
      expect_equal(hierarchy$forecasts, NULL)
      expect_equal(hierarchy$forecastsIndirect, NULL)
      hierarchy$run()
      hierarchy$indirect <- TRUE
      hierarchy$components$group$indirect <- FALSE
      chosen <- hierarchy$components$group$forecastsDirect$points$y +
        hierarchy$components$c$forecasts$points$y
      expect_equal(hierarchy$forecasts$points$y, chosen)
      full <- hierarchy$components$group$components$a$forecasts$points$y +
        hierarchy$components$group$components$b$forecasts$points$y +
        hierarchy$components$c$forecasts$points$y
      expect_equal(hierarchy$forecastsIndirect$points$y, full)
      expect_true(max(abs(chosen - full)) > 1e-6)
      expect_true(all(vapply(hierarchy$forecastsIndirect$standard_errors, is.null, logical(1))))
      expect_identical(hierarchy$forecastsDirect, hierarchy$result$forecasts)
    }
  }
}
if (requireNamespace("rjd3x13", quietly = TRUE)) local({
  # JDemetra supplies twelve preprocessing errors and calendar forecasts
  # independently of the final X-11 horizon.
  x <- perX13(AirPassengers, "rsa3", speclist = list(x11.fcasts = 0L), backend = "rjd3")
  x$run()
  expect_identical(x$result$status$code, "success")
  expect_null(x$forecasts)
  expect_null(x$result$forecasts)
  expect_identical(length(x$output$user_defined$y_ef), 12L)
  expect_identical(length(x$output$user_defined$cal_f), 12L)
  available <- setNames(x$result$availability$state, x$result$availability$field)
  expect_true(all(available[c("y_f", "cal_f", "y_ef")] == "not_provided"))
  expect_false("Forecasts" %in% x$plot()$x$attrs$labels)
  x$updateParams(x11.fcasts = 3L)
  expect_true(x$is_stale)
  expect_null(x$forecasts)
  x$run()
  fc <- x$forecasts
  expect_identical(fc$points$y, x$output$user_defined$y_f)
  expect_identical(fc$points$cal, x$output$user_defined$cal_f)
  expect_identical(fc$standard_errors$y,
                   window(x$output$user_defined$y_ef, end = end(fc$points$y)))
  expect_identical(length(fc$standard_errors$y), 3L)
  expect_identical(fc$availability$standard_errors[1], "available")
  hc <- x$hchart()$x$hc_opts$series
  interval <- hc[[match("confidence interval", vapply(hc, "[[", character(1), "name"))]]
  expect_identical(length(interval$data), 3L)
})
