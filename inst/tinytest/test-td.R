# Test perTd - seasonal adjustment with user-defined trading day regressors

library(rjd3toolkit)

data(AirPassengers, package = "datasets")

AT <- national_calendar(list(
  special_day("NEWYEAR"),
  fixed_day(1, 6),
  special_day("EASTERMONDAY"),
  special_day("ASCENSION"),
  special_day("WHITMONDAY"),
  special_day("CORPUSCHRISTI"),
  special_day("MAYDAY"),
  special_day("ASSUMPTION"),
  fixed_day(10, 26),
  special_day("ALLSAINTSDAY"),
  fixed_day(12, 8),
  fixed_day(12, 24, weight = 0.5),
  special_day("CHRISTMAS"),
  fixed_day(12, 26),
  fixed_day(12, 31, weight = 0.5)
))

message("Test perTd - x13 with calendar regressors")

obj <- perTd(AirPassengers, calendar = AT, method = "x13")
expect_inherits(obj, "x13Single")
obj$run()
expect_true(!is.null(obj$output))
# the td regressors enter the regression model
vars <- obj$output$preprocessing$description$variables
expect_true("TD" %in% unlist(lapply(vars, function(v) as.character(v$type))))
# residual trading day diagnostics are available
expect_true(all(c("td.ftest.i", "td.ftest.sa") %in%
  names(obj$output$diagnostics)))
# print works although the td variable has several coefficients
out <- capture.output(print(obj))
expect_true(length(out) > 0)

message("Test perTd - tramoseats with calendar regressors")

obj_ts <- perTd(AirPassengers, calendar = AT, method = "tramoseats")
expect_inherits(obj_ts, "tramoseatsSingle")
obj_ts$run()
expect_true(!is.null(obj_ts$output))

message("Test perTd - precomputed regressors from genTd")

tdAT <- genTd(
  freq = 12, firstYear = 1949, lastYear = 1961,
  hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39",
            "easter+50", "easter+60", "08-15", "10-26", "11-01",
            "12-08", "12-24", "12-25", "12-26", "12-31"),
  weight = c(rep(1, 11), 0.5, rep(1, 2), 0.5)
)[[3]][, 1:6]
obj_g <- perTd(AirPassengers, td = tdAT, method = "x13")
obj_g$run()
expect_true(!is.null(obj_g$output))
# outlier extraction (used by print) handles multi-coefficient td variables
out_g <- capture.output(print(obj_g))
expect_true(length(out_g) > 0)
otl <- persephone3:::getOutliers(obj_g)
expect_true(nrow(otl) > 0)
expect_true(all(!is.na(otl$stde)))

message("Test perTd - td_context building blocks")

tdc <- persephone3:::td_context(AirPassengers, calendar = AT, method = "x13")
expect_equal(names(tdc), c("td", "context", "speclist"))
expect_equal(colnames(tdc$td), paste0("group_", 0:6))
expect_equal(names(tdc$context$variables), "REGS_TD")
expect_equal(tdc$speclist$td.option, "UserDefined")
expect_equal(
  tdc$speclist$td.uservariable,
  paste0("REGS_TD.", colnames(tdc$td))
)
# method-specific default for the tests
expect_equal(tdc$speclist$td.test, c("Add", "Joint_F"))
tdc_ts <- persephone3:::td_context(AirPassengers, calendar = AT, method = "tramoseats")
expect_equal(tdc_ts$speclist$td.test, "Joint_F")

message("Test perTd - input checks")

expect_error(perTd(AirPassengers, method = "x13"))
expect_error(perTd(AirPassengers, calendar = AT, td = tdAT, method = "x13"))
expect_error(perTd(AirPassengers, calendar = "AT", method = "x13"))
expect_error(perTd(AirPassengers, td = 1:144, method = "x13"))
expect_error(perTd(AirPassengers, td = window(tdAT, start = c(1955, 1)), method = "x13"))

message("Test perTd - combining with other specifications")

obj2 <- perTd(
  AirPassengers, calendar = AT, method = "x13",
  transform.fun = "Log", easter.enabled = FALSE,
  automdl.enabled = FALSE,
  arima.p = 1, arima.d = 2, arima.q = 0,
  arima.bp = 1, arima.bd = 1, arima.bq = 0
)
obj2$run()
spec <- unclass(obj2$params)$regarima
expect_equal(spec$transform$fn, "LOG")
expect_equal(spec$regression$easter$type, "UNUSED")
expect_equal(length(spec$regression$td$users), 6)

message("Test perTd - merging with an existing modelling context")

extra <- ts(matrix(rnorm(2 * length(AirPassengers)),
                   ncol = 2, dimnames = list(NULL, c("E1", "E2"))),
            start = start(AirPassengers), frequency = frequency(AirPassengers))
ctx <- modelling_context(
  calendars = list(cal = AT),
  variables = list(EXTRA = extra)
)
obj3 <- perTd(AirPassengers, calendar = AT, method = "x13", context = ctx)
expect_true(all(c("EXTRA", "REGS_TD") %in% names(obj3$context$variables)))
expect_equal(names(obj3$context$calendars), "cal")
obj3$run()
expect_true(!is.null(obj3$output))

message("Test perTd - user td.* arguments take precedence")

obj4 <- perTd(AirPassengers, calendar = AT, method = "x13",
              speclist = list(td.test = "Add"))
expect_equal(unclass(obj4$params)$regarima$regression$td$test, "ADD")
obj4$run()
expect_true(!is.null(obj4$output))

message("Test td argument of perX13 and perTramo")

# calendar object
obj5 <- perX13(AirPassengers, "rsa3", td = AT)
obj5$run()
vars5 <- obj5$output$preprocessing$description$variables
expect_true("TD" %in% unlist(lapply(vars5, function(v) as.character(v$type))))
expect_true(all(c("td.ftest.i", "td.ftest.sa") %in%
  names(obj5$output$diagnostics)))

# precomputed regressors, tramoseats
obj6 <- perTramo(AirPassengers, "rsa3", td = tdAT)
obj6$run()
expect_true(!is.null(obj6$output))
expect_true(length(unclass(obj6$params)$tramo$regression$users) == ncol(tdAT))

# td_context list
obj7 <- perX13(AirPassengers, "rsa3",
               td = persephone3:::td_context(AirPassengers, calendar = AT,
                                             method = "x13"))
obj7$run()
expect_true(!is.null(obj7$output))

# additional spec arguments in ... are not dropped
obj8 <- perX13(AirPassengers, "rsa3", td = AT, transform.fun = "Log",
               easter.enabled = FALSE)
expect_equal(unclass(obj8$params)$regarima$transform$fn, "LOG")
expect_equal(unclass(obj8$params)$regarima$regression$easter$type, "UNUSED")
# user td.* arguments override the generated ones
obj9 <- perX13(AirPassengers, "rsa3", td = AT, td.test = "Add")
expect_equal(unclass(obj9$params)$regarima$regression$td$test, "ADD")
# user context is preserved
obj10 <- perX13(AirPassengers, "rsa3", context = ctx, td = AT)
expect_true(all(c("EXTRA", "REGS_TD") %in% names(obj10$context$variables)))
# invalid td input
expect_error(perX13(AirPassengers, "rsa3", td = "AT"))

message("Test td argument of updateParams")

obj11 <- perX13(AirPassengers, "rsa3")
obj11$updateParams(td = AT)
expect_true("REGS_TD" %in% names(obj11$context$variables))
expect_equal(length(unclass(obj11$params)$regarima$regression$td$users), 6)
obj11$run()
expect_true(!is.null(obj11$output))

message("Test perTd - centered regressors and series levels")

# the calendar regressors are centered by default (contrasts = TRUE, as in
# rjd3toolkit::calendar_td() and genTd()); otherwise they are nearly
# collinear with the regression constant and x13 may estimate the model in
# logs, so that the trend and s.a. series are returned on the log scale and
# no longer overlay the original series in plot()
obj12 <- perTd(AirPassengers, calendar = AT, method = "x13")
obj12$run()
tdv <- rjd3toolkit::calendar_td(calendar = AT, s = AirPassengers)
expect_equal(ncol(tdv), 6)
expect_true(all(abs(colMeans(tdv)) < 0.1))
yr <- range(obj12$output$user_defined$y, na.rm = TRUE)
tr_ <- range(obj12$output$user_defined$t, na.rm = TRUE)
sar <- range(obj12$output$user_defined$sa, na.rm = TRUE)
expect_true(tr_[1] < yr[2] && tr_[2] > yr[1])
expect_true(sar[1] < yr[2] && sar[2] > yr[1])
