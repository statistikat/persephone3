# Read the Kalos-owned JVM corpus from the pinned R package.
helper <- new.env(parent = baseenv())
sys.source(system.file("tinytest/helpers/fixtures.R", package = "rkalos", mustWork = TRUE), helper)
path <- system.file("tinytest/fixtures/native/tramo-seats-bridge/rsa0-x86_64.txt",
                    package = "rkalos", mustWork = TRUE)
records <- readLines(path)
cases <- split(records, cumsum(startsWith(records, "case ")))
keys <- c(Series = "y", Trend = "t", SeasonallyAdjusted = "sa", Seasonal = "s", Irregular = "i")
domain <- function(value) c(start(value), end(value), frequency(value))
count <- 0L
for (case in cases) {
  input <- strsplit(case[2L], " ", fixed = TRUE)[[1L]]
  series <- ts(helper$fixture_hex_doubles(input[-seq_len(4L)]),
               start = as.integer(input[2:3]), frequency = 12)
  x <- perTramo(series, "rsa0", backend = "native")
  x$run()
  expect_identical(x$result$status$code, "success")
  expect_identical(x$result$method, "tramoseats")
  for (line in case[startsWith(case, "merged ")]) {
    row <- strsplit(line, " ", fixed = TRUE)[[1L]]
    key <- keys[[row[2L]]]
    value <- if (row[3L] == "Value") x$result$components[[key]] else x$forecasts$points[[key]]
    expect_identical(as.integer(start(value)), as.integer(row[4:5]))
    expect_identical(as.numeric(value), helper$fixture_hex_doubles(row[-seq_len(6L)]), info = case[1L])
    count <- count + length(value)
  }
}
expect_identical(c(length(cases), count), c(6L, 3960L))

x <- perTramo(AirPassengers, "rsa0", backend = "native")
x$run()
saved <- x$result
expect_identical(x$adjusted, x$output$finalSA)
expect_identical(diagnosticsTable(x)$arimaMdl, "(0 1 1)(0 1 1)")
expect_false(diagnosticsTable(x)$logTransform)
expect_identical(x$getRegressors()$value, x$output$model$mean_coefficient)
expect_identical(nrow(x$getOutliers()), 0L)
expect_true(any(grepl("TRAMO / SEATS", capture.output(summary(x)), fixed = TRUE)))
expect_null(x$forecasts$standard_errors$y)
expect_identical(x$forecasts$availability$standard_errors[1L], "unavailable")
expect_identical(length(x$result$seats_forecast_standard_errors$y), 24L)
expect_identical(unserialize(serialize(x$result, NULL)), x$result)
expect_error(x$updateParams(forecast = 1.25), "whole number")
expect_error(x$updateParams(log = TRUE), "tramo_seats_fit.templates")
expect_false(x$is_stale)
expect_identical(x$result, saved)
expect_error(perTramo(AirPassengers, "rsa3", backend = "native"), "tramo_seats_fit.templates")
expect_error(perTramo(UKgas, "rsa0", backend = "native"), "monthly_only")
x$updateParams(forecast = 0L, backcast = 3L)
expect_true(x$is_stale)
expect_identical(x$result, saved)
x$run()
expect_null(x$forecasts)
expect_true(any(x$output$merged$information == "Backcast"))
expect_false(x$is_stale)
expect_false(identical(x$result$provenance$request_fingerprint, saved$provenance$request_fingerprint))

h <- perHts(a = AirPassengers, b = AirPassengers, method = "tramoseats",
            template = "rsa0", backend = "native")
h$run()
expect_identical(h$result$components$sa, h$adjustedDirect)
expect_identical(as.numeric(h$adjustedIndirect), 2 * as.numeric(saved$components$sa))
expect_identical(domain(h$adjustedIndirect), domain(saved$components$sa))
expect_equal(h$forecastsIndirect$points$y, 2 * saved$forecasts$points$y, tolerance = 0)
expect_true(all(vapply(h$forecastsIndirect$standard_errors, is.null, logical(1))))

if (requireNamespace("rjd3tramoseats", quietly = TRUE)) {
  reference <- perTramo(AirPassengers, "rsa0", backend = "rjd3")
  reference$run()
  for (key in unname(keys)) {
    expect_identical(as.numeric(saved$components[[key]]), as.numeric(reference$result$components[[key]]))
    # rjd3 reconstructs tsp from its start and length. The R input's end
    # can round one ULP differently while identifying the same final month.
    expect_identical(domain(saved$components[[key]]), domain(reference$result$components[[key]]))
    expect_identical(as.numeric(saved$forecasts$points[[key]]), as.numeric(reference$forecasts$points[[key]]))
    expect_identical(tsp(saved$forecasts$points[[key]]), tsp(reference$forecasts$points[[key]]))
    column <- if (key == "y") "series" else key
    target <- reference$output$decomposition$stochastics[[column]]$fcasts.stde
    expect_identical(as.numeric(saved$seats_forecast_standard_errors[[key]]), as.numeric(target))
    expect_identical(tsp(saved$seats_forecast_standard_errors[[key]]), tsp(target))
  }
}
