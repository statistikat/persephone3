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

local({
  filters <- list(seasonal = rep(c("s3x3", "s3x5"), 6L), trend_length = 13L)
  fixed <- perX13(AirPassengers, "restricted", speclist = filters, backend = "native")
  fixed$run()
  qr <- fixed$generateQrTable()
  expect_identical(qr$Final.Henderson.Filter, "H13")
  expect_identical(qr$Seasonal.Filter, "FILTER_S3X3, FILTER_S3X5")
  expect_identical(qr$MovingHoliday, FALSE)
  expect_identical(qr$LeapYear, FALSE)
  expect_equal(qr$NbTD, 0)

  var <- function(class, effect) {
    list(class = class, effect = effect, coefficient = 1,
         columns = list(effect = as.numeric(seq_along(AirPassengers) == 50L)))
  }
  fixed$updateParams(variables = list(var("days_related", "calendar_effect"),
                                      var("moving_holiday", "calendar_effect")))
  fixed$run()
  qr <- fixed$generateQrTable()
  expect_true(qr$MovingHoliday)
  expect_true(is.na(qr$LeapYear) && is.na(qr$NbTD))
  fixed$updateParams(variables = list(var("user", "calendar_effect")))
  fixed$run()
  qr <- fixed$generateQrTable()
  expect_true(is.na(qr$MovingHoliday) && is.na(qr$LeapYear) && is.na(qr$NbTD))

  for (method in c("x13", "tramoseats")) {
    constructor <- if (method == "x13") perX13 else perTramo
    tpl <- if (method == "x13") "rsa3" else "rsa0"
    model <- constructor(AirPassengers, tpl, backend = "native")
    model$run()
    qr <- model$generateQrTable()
    expect_identical(qr$LeapYear, FALSE)
    expect_identical(qr$MovingHoliday, FALSE)
    expect_equal(qr$NbTD, 0)
    measure <- function(adjusted) paste0(round(100 * max(abs(
      as.numeric(AirPassengers) - as.numeric(adjusted)) / as.numeric(AirPassengers))), "%")
    expect_identical(qr$Max.Adj, measure(model$adjusted))
    if (requireNamespace(paste0("rjd3", method), quietly = TRUE)) {
      spec <- if (method == "x13") list(x11.fcasts = 0L) else NULL
      reference <- constructor(AirPassengers, tpl, speclist = spec, backend = "rjd3")
      reference$run()
      ref <- reference$generateQrTable()
      for (key in c("Final.Henderson.Filter", "Seasonal.Filter", "LeapYear",
                    "MovingHoliday", "NbTD", "Max.Adj"))
        expect_equal(qr[[key]], ref[[key]], info = paste(method, key))
    }
  }
})
