# Both public fit routes expose the same getter schemas. Reports and rejected
# updates use the last successful fit, including when the object is stale.
local({
  fixed <- list(seasonal = rep("s3x3", 12), trend_length = 13L)
  a <- perX13(AirPassengers, "restricted", speclist = fixed, backend = "native")
  a$run()
  expect_identical(a$getRegressors(), NULL)
  expect_equal(nrow(a$getOutliers()), 0L)
  reg <- list(class = "outlier", effect = "irregular", coefficient = 3,
              columns = list(pulse = as.numeric(seq_along(AirPassengers) == 25)))
  a$updateParams(variables = list(reg))
  a$run()
  expect_identical(a$getRegressors()$value, 3)
  out <- a$getOutliers()
  expect_identical(out$name, "pulse")
  expect_identical(out$autoout, FALSE)
  expect_true(is.na(out$stde) && is.na(out$date))
  expect_true(any(grepl("X-11", capture.output(summary(a)))))
  expect_true(length(capture.output(print(a))) > 0L)

  y <- AirPassengers
  y[70] <- y[70] * 3
  b <- perX13(y, backend = "native")
  b$run()
  v <- b$result$model$variables
  expect_true(length(v$name) > 0)
  expect_identical(b$getRegressors()$value, v$coefficient)
  expect_identical(b$getOutliers()$type, v$type)
  expect_false(anyNA(b$getOutliers()$date))
  expect_true(all(b$getOutliers()$autoout))
  expect_equal(diagnosticsTable(b)$nOutliers, length(v$name))
  expect_true(any(grepl(b$result$provenance$mode_effective, capture.output(summary(b)))))
  expect_true(length(capture.output(print(b))) > 0L)

  b$updateParams(spec = b$specification)
  for (stale in c(FALSE, TRUE)) {
    if (stale) b$updateParams(sigma_policy = "accurate")
    old <- b$params
    fit <- b$result
    wrong <- b$specification
    wrong$parameters$template <- "restricted"
    expect_error(b$updateParams(spec = wrong), "template")
    expect_error(b$updateParams(spec = list(bad = TRUE)), "specification")
    expect_error(b$updateParams(sigma_policy = "invalid"), "sigma_policy")
    expect_identical(b$params, old)
    expect_identical(b$result, fit)
    expect_identical(b$is_stale, stale)
  }

  h <- perHts(a = a, b = b, method = "x13", template = "restricted",
              speclist = fixed, iterate = FALSE, backend = "native")
  previous <- list(h$paramsDirect, h$components$a$params, h$components$b$params)
  expect_error(h$updateParams(log = TRUE, iterate = TRUE), "automatic_spec_pinned")
  expect_identical(list(h$paramsDirect, h$components$a$params, h$components$b$params),
                   previous)
})
