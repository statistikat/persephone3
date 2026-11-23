rsa0 <- perTramo(AirPassengers, "rsa0", backend = "native")
expect_warning(rsa0$fixModel(), "not run")
rsa0$run()
spec <- rsa0$params
result <- rsa0$result
expect_identical(rsa0$fixModel(), spec)
expect_identical(rsa0$fixOutlier(timespan = 0), rsa0)
expect_identical(rsa0$params, spec)
expect_identical(rsa0$result, result)
expect_false(rsa0$is_stale)

fixed <- perX13(AirPassengers, "restricted", backend = "native",
  speclist = list(trend_length = 13L, seasonal = rep("s3x3", 12L)))
fixed$run()
expect_error(fixed$fixModel(), "no estimated ARIMA orders")
expect_identical(fixed$fixOutlier(timespan = 0), fixed)
expect_false(fixed$is_stale)

input <- AirPassengers
input[70L] <- input[70L] * 3
rsa3 <- perX13(input, "rsa3", backend = "native")
rsa3$run()
expect_true(nrow(rsa3$getOutliers()) > 0L)
before <- list(params = rsa3$params, result = rsa3$result)
expect_error(rsa3$fixModel(), "automatic_spec_pinned")
expect_error(rsa3$fixOutlier(timespan = 0), "automatic_spec_pinned")
expect_error(rsa3$fixOutlier(timespan = 12), "automatic_spec_pinned")
expect_identical(rsa3$fixOutlier(timespan = length(input)), rsa3)
expect_identical(list(params = rsa3$params, result = rsa3$result), before)
expect_false(rsa3$is_stale)
