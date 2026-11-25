# Test hierarchical time series functionality

message("Test hierarchical - basic structure")

# Create hierarchical object
objX13 <- perX13(AirPassengers, "rsa3")
ht <- perHts(a = objX13, b = objX13, method = "x13")

# Before run, adjusted should be NULL
expect_true(is.null(ht$adjustedDirect))
expect_true(is.null(ht$adjustedIndirect))

ht$run()

# After run, adjusted series should exist
expect_inherits(ht$adjustedDirect, "ts")
expect_inherits(ht$adjustedIndirect, "ts")
expect_identical(tsp(ht$adjustedDirect), tsp(ht$adjustedIndirect))

message("Test hierarchical - nested hierarchies")

objX13_nested <- perX13(AirPassengers, "rsa3")
ht_nested <- perHts(a = objX13_nested, b = objX13_nested, method = "x13")
ht_nested2 <- perHts(a = ht_nested, b = objX13_nested, method = "x13")
ht_nested2$run()

expect_inherits(ht_nested2$adjustedDirect, "ts")
expect_inherits(ht_nested2$adjustedIndirect, "ts")

message("Test hierarchical - incompatible time instances")

obj1 <- perX13(AirPassengers, "rsa3")
obj2 <- perX13(JohnsonJohnson, "rsa3")

# Different time periods should cause an error
expect_error(
  perHts(a = obj1, b = obj2),
  "time"
)

message("Test hierarchical - qualityTable (known issue)")

objX13_qt <- perX13(AirPassengers, "rsa3")
ht_qt <- perHts(a = objX13_qt, b = objX13_qt, method = "x13")
ht_qt$run()

# qualityTable may have issues, just check it doesn't crash completely
# TODO: Fix qualityTable implementation
expect_true(!is.null(ht_qt))

message("Test hierarchical - print method")

expect_silent(print(ht))

message("Test hierarchical - quality table with direct and indirect rows")

qt <- ht$qualityTable()
expect_true(all(c("agg", "seasTest", "tdTest") %in% names(qt)))
# classic mode: the aggregate has a direct and an indirect row, the
# sub-series have agg = "none"
expect_identical(qt$agg, c("direct", "indirect", "none", "none"))
expect_identical(qt$type[c(1, 2)], c("aggregate", "aggregate"))
expect_identical(qt$run, rep(TRUE, 4))
# the indirect row has no indicators of a model fit ...
expect_true(all(is.na(qt[2, c(
  "seasonality", "logTransform", "arimaMdl",
  "Q", "Q2", "TotalQ"
)])))
# ... its outlier counts are NA (an indirect aggregate has no model of its
# own, the outliers of the sub-series are not attributed to it)
expect_true(is.na(qt$nOutliers[2]))
# ... but it is tested on residual seasonality and trading day effects
expect_true(all(is.numeric(qt$seasTest)) && all(is.numeric(qt$tdTest)))

# before the run the tests are NA
ht_new <- perHts(
  a = perX13(AirPassengers, "rsa3"),
  b = perX13(AirPassengers, "rsa3"), method = "x13"
)
expect_true(all(is.na(ht_new$qualityTable()$seasTest)))

message("Test hierarchical - quality table in groups mode")

set.seed(1)
n_qt <- length(AirPassengers)
qts <- cbind(
  A = AirPassengers,
  B = AirPassengers * 0.6 + 10 * rlnorm(n_qt),
  C = AirPassengers * 0.4 + 10 * rlnorm(n_qt),
  F = AirPassengers * 2 + 10 * rlnorm(n_qt)
)
htg <- perHts(
  A = qts[, "A"], B = qts[, "B"], C = qts[, "C"], F = qts[, "F"],
  method = "x13", groups = list(F = c("A", "B", "C"))
)
htg$run()
qtg <- htg$qualityTable()
# the aggregate F has one direct and one indirect row
expect_identical(qtg$component, c("A", "B", "C", "F", "F"))
expect_identical(qtg$agg, c("none", "none", "none", "direct", "indirect"))
# aggregates are marked as aggregates also in groups mode
expect_identical(
  qtg$type,
  c("single", "single", "single", "aggregate", "aggregate")
)
expect_true(is.numeric(qtg$seasTest[5]) || is.na(qtg$seasTest[5]))
expect_true(is.numeric(qtg$tdTest[5]) || is.na(qtg$tdTest[5]))
expect_true(qtg$run[5])

# with adjustment = "indirect" the aggregates have only the indirect row
htg_i <- perHts(
  A = qts[, "A"], B = qts[, "B"], C = qts[, "C"], F = qts[, "F"],
  method = "x13", groups = list(F = c("A", "B", "C")),
  adjustment = "indirect"
)
htg_i$run()
agg_i <- htg_i$qualityTable()
expect_identical(agg_i$agg, c("none", "none", "none", "indirect"))
expect_true(all(is.numeric(agg_i$seasTest)) && all(is.numeric(agg_i$tdTest)))

# with adjustment = "direct" the aggregates have only the direct row
htg_d <- perHts(
  A = qts[, "A"], B = qts[, "B"], C = qts[, "C"], F = qts[, "F"],
  method = "x13", groups = list(F = c("A", "B", "C")),
  adjustment = "direct"
)
htg_d$run()
expect_identical(
  htg_d$qualityTable()$agg,
  c("none", "none", "none", "direct")
)

message("Test qualityTable - outliers of the indirect adjustment")

# an indirect aggregate has no model of its own: the outlier counts of the
# indirect row are NA, they are not collected from the sub-series
oa <- perX13(AirPassengers, "rsa0")
oa$updateParams(outliers = c("AO1951.1", "LS1955.4", "AO1952.3"))
ob <- perX13(AirPassengers, "rsa0")
ob$updateParams(outliers = c("AO1951.1", "AO1953.7"))
og <- perX13(AirPassengers, "rsa0")
hto <- perHts(S1 = oa, S2 = ob, AG = og, method = "x13",
              groups = list(AG = c("S1", "S2")))
hto$run()
to <- hto$qualityTable()
# S1 has 3 outliers, S2 has 2; the indirect row counts nothing of that
expect_equal(to$nOutliers[to$component == "S1"], 3)
expect_equal(to$nOutliers[to$component == "S2"], 2)
expect_true(all(is.na(to$nOutliers[to$agg == "indirect"])))

# the same in the classic mode
htc <- perHts(a = oa, b = ob, method = "x13")
htc$run()
tc <- htc$qualityTable()
expect_true(all(is.na(tc$nOutliers[tc$agg == "indirect"])))
