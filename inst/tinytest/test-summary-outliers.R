# Test the "autoout" column of the summary() Outliers table (TRUE for the
# automatically detected outliers, FALSE for the ones pre-specified in the
# specification, as in getOutliers())

message("Test summary - outliers autoout column")

y <- datasets::AirPassengers
y[141] <- 260  # massive spike, 1960.09, found by the automatic detection
obj <- perX13(y, "rsa3", outliers = c("AO1953.7"))
obj$run()
s <- capture.output(summary(obj))

# header of the Outliers table carries the new column
head_i <- grep("^-- Outliers", s)
expect_identical(length(head_i), 1L)
expect_true(grepl("autoout", s[head_i + 1]))

# the pre-specified outlier is FALSE, the detected one is TRUE; the p-value
# of the spike is tiny and must be shown as "< 0.0001"
rows <- grep("^AO[0-9]{4}\\.", s, value = TRUE)
expect_true(any(grepl("^AO1953\\.07", rows) & grepl("FALSE\\s*$", rows)))
expect_true(any(grepl("^AO1960\\.09", rows) & grepl("TRUE\\s*$", rows)))
expect_true(any(grepl("^AO1960\\.09", rows) & grepl("< 0.0001", rows)))

# the classification must agree with getOutliers()
g <- persephone3:::getOutliers(obj)
expect_identical(sum(grepl("FALSE\\s*$", rows)), sum(!g$autoout))
expect_identical(sum(grepl("TRUE\\s*$", rows)), sum(g$autoout))

# the joint F test line drops the equals sign below the threshold:
# "P < 0.0001" instead of "P = < 0.0001"
td_obj <- perTd(datasets::AirPassengers, calendar = rjd3toolkit::national_calendar(
  list(rjd3toolkit::special_day("NEWYEAR"), rjd3toolkit::fixed_day(1, 6),
       rjd3toolkit::special_day("EASTERMONDAY"))), method = "x13")
td_obj$run()
s_td <- capture.output(summary(td_obj))
expect_true(any(grepl("Trading days:.*P < 0\\.0001", s_td)))

# a run without outliers has no table and no column (AirPassengers with the
# default rsa3 settings detects none; a too short series would break the
# rjd3 diagnostics)
clean <- perX13(datasets::AirPassengers, "rsa3")
clean$run()
s_clean <- capture.output(summary(clean))
expect_false(any(grepl("^-- Outliers", s_clean)))
expect_false(any(grepl("autoout", s_clean)))
