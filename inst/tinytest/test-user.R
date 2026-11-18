# Test the `user` argument of perX13()/perTramo() - user-defined regressors
# (e.g. the leap year regressor of genTd()) next to the trading days

library(rjd3toolkit)

data(AirPassengers, package = "datasets")

tdGen <- genTd(
  freq = 12, firstYear = 1949, lastYear = 1961,
  hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39"),
  weight = rep(1, 5)
)
td6 <- tdGen[[3]][, 1:6, drop = FALSE]
lpY <- tdGen[[3]][, 7, drop = FALSE]
set.seed(42)
noise <- ts(stats::rnorm(144), start = 1949, frequency = 12)

# the types and names of the variables of the regression model; `obj` can be a
# persephone object or the rjd3 output of one
regVars <- function(obj) {
  out <- if (inherits(obj, "persephone")) obj$output else obj
  v <- out$preprocessing$description$variables
  stats::setNames(
    vapply(v, function(z) as.character(z$type), ""),
    vapply(v, function(z) z$name, "")
  )
}

message("Test user regressors - trading days and further regressors together")

obj <- perX13(AirPassengers, "rsa5c", td = td6, user = lpY)
expect_inherits(obj, "x13Single")
obj$run()
expect_true(!is.null(obj$output))
tp <- regVars(obj)
# both are estimated: the trading days of td and the regressor of user
expect_true("TD" %in% tp)
expect_true("lpYear" %in% names(tp))
expect_equal(unname(tp[["lpYear"]]), "UNSPECIFIED")
# the regressor keeps up with the series and is the 7th column of genTd()
expect_equal(frequency(lpY), frequency(AirPassengers))
expect_true(any(grepl("leap|lpyear", names(tp), ignore.case = TRUE)))

sumOut <- capture.output(summary(obj))
# a user-defined leap year regressor counts as leap year adjustment
expect_true(any(grepl("Leap year: *Yes", sumOut)))
expect_true(any(grepl("Trading day: *6 regressors", sumOut)))
# and it is shown in the coefficient table
expect_true(any(grepl("Mean / Leap year", sumOut)))
expect_true(any(grepl("leap year", sumOut)))
# its coefficient is the one of the regression
coef <- persephone3:::sumCoefTable(obj, "UNSPECIFIED")
expect_true(!is.null(coef))
expect_equal(nrow(coef), 1L)
expect_true(all(c("coef", "stderr", "t", "p") %in% names(coef)))

message("Test user regressors - several regressors and their names")

# a list of regressors, one named regressor per column; a name that the java
# side does not allow is cleaned up, the label keeps the original name
obj2 <- perX13(
  AirPassengers, "rsa5c",
  user = list(leapYear = lpY, "my regressor" = noise)
)
obj2$run()
tp2 <- regVars(obj2)
expect_true(all(c("lpYear", "my regressor") %in% names(tp2)))
expect_true(all(tp2[c("lpYear", "my regressor")] == "UNSPECIFIED"))
sum2 <- capture.output(summary(obj2))
expect_true(any(grepl("User regressors: *1", sum2)))
expect_true(any(grepl("User-defined regressors", sum2)))
expect_true(any(grepl("my regressor", sum2)))
# a leap year regressor is not counted twice (it belongs to the leap year line)
expect_true(any(grepl("Leap year: *Yes", sum2)))

# several columns become one regressor each
obj3 <- perX13(AirPassengers, "rsa5c", user = tdGen[[3]])
obj3$run()
tp3 <- regVars(obj3)
expect_equal(
  names(tp3)[tp3 == "UNSPECIFIED"],
  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
    "lpYear")
)

# a regressor without column names gets one
unnamed <- lpY
colnames(unnamed) <- NULL
obj4 <- perX13(AirPassengers, "rsa5c", user = unnamed)
obj4$run()
expect_true("user1" %in% names(regVars(obj4)))

# a short name for the leap year regressor is understood as well
shortY <- lpY
colnames(shortY) <- NULL
obj4b <- perX13(AirPassengers, "rsa5c", user = list(lpY = shortY))
obj4b$run()
expect_true("lpY" %in% names(regVars(obj4b)))
expect_true(any(grepl("Leap year: *Yes",
                      capture.output(summary(obj4b)))))

message("Test user regressors - the modelling context")

# the names of a modelling context: the names of its variables and, one level
# below, the names of the individual regressors
ctxNames <- function(ctx) {
  unique(unlist(c(
    names(ctx$variables),
    lapply(ctx$variables, function(v) {
      if (is.list(v)) names(v) else colnames(v)
    })
  ), use.names = FALSE))
}

# the regressors of `user` and of an own context are kept together
ctx <- modelling_context(variables = list(noise = noise))
obj5 <- perX13(AirPassengers, "rsa5c", context = ctx, user = lpY)
expect_true(all(c("noise", "lpYear") %in% ctxNames(obj5$context)))
# the same is true for the trading day regressors of `td`
obj6 <- perX13(AirPassengers, "rsa5c", context = ctx, td = td6, user = lpY)
expect_true(all(c("noise", "REGS_TD", "lpYear") %in% ctxNames(obj6$context)))
obj6$run()
expect_true(all(c("TD", "UNSPECIFIED") %in% regVars(obj6)))

# updateParams() adds a regressor only once, even when it is given again
ids <- function(obj) sum(persephone3:::spec_usrdefvar_ids(obj$params) == "r.lpYear")
obj5$updateParams(user = lpY)
expect_equal(ids(obj5), 1L)
obj5$updateParams(user = lpY, td = td6)
expect_equal(ids(obj5), 1L)
expect_true(all(c("REGS_TD", "lpYear") %in% ctxNames(obj5$context)))

message("Test user regressors - tramoseats")

obj7 <- perTramo(AirPassengers, "rsa3", user = lpY)
expect_inherits(obj7, "tramoseatsSingle")
obj7$run()
expect_true("lpYear" %in% names(regVars(obj7)))
# tramoseats does not estimate both, so it says so
expect_warning(
  obj8 <- perTramo(AirPassengers, "rsa3", td = td6, user = lpY),
  "does not estimate the user-defined trading day"
)

message("Test user regressors - false input")

expect_error(perX13(AirPassengers, "rsa3", user = 1:144), "'user' must be")
expect_error(perX13(AirPassengers, "rsa3", user = list()), "'user' must be")
expect_error(
  perX13(AirPassengers, "rsa3", user = list(a = lpY, b = lpY)),
  "twice"
)
# a regressor that does not cover the series
lp_short <- window(lpY, end = c(1955, 12))
expect_error(
  perX13(AirPassengers, "rsa3", user = lp_short),
  "cover its full span"
)
# a regressor of another frequency
lp_q <- ts(as.numeric(lpY[, 1])[seq(1, 144, by = 3)],
           start = 1949, frequency = 4)
expect_error(
  perX13(AirPassengers, "rsa3", user = lp_q),
  "cover its full span"
)

message("Test user regressors - hierarchical time series")

ap <- as.numeric(stats::window(AirPassengers, end = c(1960, 12)))
mk <- function(k) ts(ap * k, start = 1949, frequency = 12)
comps <- list(C20 = mk(1), C21 = mk(1.1), C22 = mk(0.9), C20_21 = mk(2.1))
# the series of the groups mode: the sub-series and the aggregate of groups
inGroups <- comps[c("C20", "C21", "C20_21")]
groups <- list(C20_21 = c("C20", "C21"))
# a series estimated with the trading days and with the regressor of `user`
hasTdAndUser <- function(node) {
  tp <- regVars(node)
  "TD" %in% tp && "lpYear" %in% names(tp)
}

# one set of regressors for every series of the structure
ht <- perHts(
  list = inGroups, method = "x13", template = "rsa5c", groups = groups,
  td = td6, user = lpY
)
ht$run()
# the sub-series and the aggregates are estimated with both
expect_true(all(vapply(ht$components, hasTdAndUser, TRUE)))
# which can be read in the summary of one of the series
sumOut <- capture.output(summary(ht$components$C20, td = FALSE))
expect_true(any(grepl("Leap year: *Yes", sumOut)))
expect_true(any(grepl("leap year", sumOut)))
# adding the same regressors again does not double them
expect_silent(ht$updateParams(user = lpY))
expect_silent(ht$updateParams(td = td6, user = lpY))
ht$run()
expect_true(all(vapply(ht$components, function(node) {
  sum(names(regVars(node)) == "lpYear") == 1
}, TRUE)))

message("Test user regressors - hierarchical time series, one series only")

# a named list gives the regressors only to the series it names
ht2 <- perHts(
  list = inGroups, method = "x13", template = "rsa5c", groups = groups,
  user = list(C21 = lpY)
)
ht2$run()
expect_true("lpYear" %in% names(regVars(ht2$components$C21)))
expect_false("lpYear" %in% names(regVars(ht2$components$C20)))
expect_false("lpYear" %in% names(regVars(ht2$components$C20_21)))
# updateParams() with one set of regressors adds them to every series
expect_silent(ht2$updateParams(user = lpY))
ht2$run()
expect_true(all(vapply(ht2$components, function(node) {
  "lpYear" %in% names(regVars(node))
}, TRUE)))

message("Test user regressors - hierarchical time series, direct aggregate")

# in the classic mode (without groups) the aggregate series itself is estimated
# with the regressors too
ht3 <- perHts(
  list = comps, method = "x13", template = "rsa5c", td = td6, user = lpY
)
expect_true(is.null(ht3$adjustedDirect))
ht3$run()
expect_true("lpYear" %in% names(regVars(ht3$output)))
expect_true(all(vapply(ht3$components, hasTdAndUser, TRUE)))
# a named list is for the components only, the aggregate keeps its own model
ht4 <- perHts(list = comps, method = "x13", user = list(C21 = lpY))
ht4$run()
expect_true("lpYear" %in% names(regVars(ht4$components$C21)))
expect_false("lpYear" %in% names(regVars(ht4$components$C20)))
expect_false("lpYear" %in% names(regVars(ht4$output)))
# updateParams() works the same way
ht5 <- perHts(list = comps, method = "x13")
expect_silent(ht5$updateParams(user = lpY))
ht5$run()
expect_true("lpYear" %in% names(regVars(ht5$output)))
expect_true(all(vapply(ht5$components, function(node) {
  "lpYear" %in% names(regVars(node))
}, TRUE)))
ht6 <- perHts(list = comps, method = "x13")
expect_silent(ht6$updateParams(component = "C22", user = lpY))
ht6$run()
expect_true("lpYear" %in% names(regVars(ht6$components$C22)))
expect_false("lpYear" %in% names(regVars(ht6$components$C20)))

message("Test user regressors - hierarchical time series, false input")

expect_error(
  perHts(
    list = inGroups, method = "x13", groups = groups,
    user = list(C21 = lpY, CX = lpY)
  ),
  "not provided"
)
expect_error(
  perHts(
    list = inGroups, method = "x13", groups = groups,
    user = list(C21 = 1:144)
  ),
  "ts/mts"
)
expect_error(
  perHts(list = comps, method = "x13", user = 1:144),
  "'user' must be"
)
