# Test summary() for single objects

suppressMessages(library(rjd3toolkit))

data(AirPassengers, package = "datasets")

AT <- national_calendar(list(
  special_day("NEWYEAR"),
  fixed_day(1, 6),
  special_day("EASTERMONDAY"),
  special_day("MAYDAY"),
  fixed_day(12, 24, weight = 0.5),
  special_day("CHRISTMAS")
))

message("Test summary - plain x13")

obj <- perX13(AirPassengers, "rsa3")
expect_error(summary(obj))  # not yet run
obj$run()

out <- capture.output(res <- summary(obj))
# returns the object invisibly
expect_true(identical(res, obj))
hdr <- paste(out, collapse = "\n")
# header: frequency, observations, span
expect_true(grepl("Frequency: 12", hdr, fixed = TRUE))
expect_true(grepl("Observations: 144", hdr, fixed = TRUE))
expect_true(grepl("Span: 1949.1 - 1960.12", hdr, fixed = TRUE))
# model definition
expect_true(grepl("(0 1 1)(0 1 1)", hdr, fixed = TRUE))
expect_true(grepl("Transformation:   Log", hdr, fixed = TRUE))
expect_true(grepl("Trading day:     0 regressors", hdr, fixed = TRUE))
# no regression tables without regressors, but the decomposition section
expect_true(!any(grepl("Trading day regressors", out, fixed = TRUE)))
expect_true(any(grepl("Seasonal adjustment: multiplicative", out, fixed = TRUE)))
expect_true(any(grepl("3x3", out, fixed = TRUE)))
expect_true(any(grepl("Henderson", out, fixed = TRUE)))
# the ARIMA parameters of (0 1 1)(0 1 1): one row per estimated parameter
expect_true(any(grepl("-- ARIMA parameters (0 1 1)(0 1 1)", out, fixed = TRUE)))
expect_true(any(grepl("^ar.ma.1 ", out)))
expect_true(any(grepl("^ar.ma.b1 ", out)))
expect_equal(sum(grepl("^ar\\.(ar|ma)(\\.b)?[0-9]+ ", out)), 2)
# quality: the M-statistics and the total quality indicator
expect_true(any(grepl("-- Quality", out, fixed = TRUE)))
expect_true(any(grepl("M-statistics:", out, fixed = TRUE)))
expect_true(any(grepl("Q-M2 =", out, fixed = TRUE)))
expect_true(any(grepl("Total quality:", out, fixed = TRUE)))
expect_true(any(grepl("Total quality:        Good", out, fixed = TRUE)))
# the R6 method gives the same output
expect_equal(capture.output(obj$summary()), out)

message("Test summary - x13 with trading days and outliers")

obj2 <- perX13(AirPassengers, "rsa3", td = AT, easter.enabled = TRUE)
obj2$run()
out2 <- capture.output(summary(obj2))
hdr2 <- paste(out2, collapse = "\n")
expect_true(grepl("Trading day:     6 regressors", hdr2, fixed = TRUE))
expect_true(grepl("Easter effect:   Yes", hdr2, fixed = TRUE))
# the section titles of the three coefficient tables
expect_true(any(grepl("-- Outliers", out2, fixed = TRUE)))
expect_true(any(grepl("-- Trading day regressors", out2, fixed = TRUE)))
expect_true(any(grepl("-- Mean / Leap year", out2, fixed = TRUE)))
# one row per trading day regressor, labelled by the regressor names
nTd <- sum(grepl("^group_[0-9]+ ", out2))
expect_true(nTd == obj2$output$user_defined[["regression.ntd"]])
# joint F tests, trading days and trading days + outliers
expect_true(any(grepl("Trading days: ", out2, fixed = TRUE)))
expect_true(grepl("Trading days + outliers: ", hdr2, fixed = TRUE))

message("Test summary - x13 with leap year")

obj3 <- perX13(AirPassengers, "rsa3", td.option = "TradingDays",
               td.leapyear = "LeapYear", transform.fun = "None")
obj3$run()
out3 <- capture.output(summary(obj3))
hdr3 <- paste(out3, collapse = "\n")
expect_true(grepl("Transformation:   None", hdr3, fixed = TRUE))
expect_true(grepl("Leap year:       Yes", hdr3, fixed = TRUE))
expect_true(any(grepl("^leap year", out3)))
# x11 filters are shown
expect_true(any(grepl("Final MA filter", out3, fixed = TRUE)))
expect_true(any(grepl("Final trend-cycle", out3, fixed = TRUE)))

message("Test summary - tramoseats")

obj4 <- perTramo(AirPassengers, "rsa3", td = AT)
obj4$run()
out4 <- capture.output(summary(obj4))
hdr4 <- paste(out4, collapse = "\n")
expect_true(grepl("Trading day:     6 regressors", hdr4, fixed = TRUE))
# the mean of the regression appears in the Mean / Leap year table
expect_true(any(grepl("^const", out4)))
# the ARIMA parameters of the estimated model
expect_true(any(grepl("^ar.ar.1 ", out4)))
expect_true(any(grepl("^ar.ma.b1 ", out4)))
# SEATS has no X-11 filters
expect_true(any(grepl("SEATS (no X-11 filters)", out4, fixed = TRUE)))
expect_true(!any(grepl("Final MA filter", out4, fixed = TRUE)))
# the quality indicator is available for tramoseats as well
expect_true(any(grepl("Total quality:", out4, fixed = TRUE)))

message("Test summary - model without ARIMA parameters")

obj5 <- perX13(AirPassengers, "rsa3", automdl.enabled = FALSE,
               arima.p = 0, arima.d = 1, arima.q = 0,
               arima.bp = 0, arima.bd = 1, arima.bq = 0)
obj5$run()
out5 <- capture.output(summary(obj5))
expect_true(grepl("ARIMA model:     (0 1 0)(0 1 0)",
                  paste(out5, collapse = "\n"), fixed = TRUE))
expect_true(!any(grepl("ARIMA parameters", out5, fixed = TRUE)))

message("Test summary - hierarchical objects")

hts <- perHts(a = perX13(AirPassengers, "rsa3"), b = perX13(AirPassengers, "rsa1"))
expect_error(summary(hts))
