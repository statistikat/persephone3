# Test genTd function for trading days

message("Test genTd - basic holidays")

hdAT <- genTd(hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39",
                        "easter+50", "easter+60",
                        "08-15", "10-26", "11-01", "12-08", "12-24", "12-25",
                        "12-26", "12-31"))
expect_true(length(hdAT) == 3)

message("Test genTd - with weights")

hdAT1 <- genTd(hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39",
                         "easter+50", "easter+60",
                         "08-15", "10-26", "11-01", "12-08", "12-24", "12-25",
                         "12-26", "12-31"),
               weight = c(rep(1, 11), 0.5, rep(1, 2), 0.5))
expect_true(length(hdAT1) == 3)

message("Test genTd - no easter")

hdAT_no_easter <- genTd(hd = list("01-01", "01-06", "05-01"))
expect_true(length(hdAT_no_easter) == 3)

message("Test genTd - only easter")

hdAT_only_easter <- genTd(hd = list("easter+1", "easter+39",
                                    "easter+50", "easter+60"))
expect_true(length(hdAT_only_easter) == 3)

message("Test genTd - single holiday")

hdAT_single <- genTd(hd = list("01-01"))
expect_true(length(hdAT_single) == 3)

hdAT_easter_single <- genTd(hd = list("easter+12"))
expect_true(length(hdAT_easter_single) == 3)

message("Test genTd - approximate easter")

hdAT_approx <- genTd(hd = list("01-01", "easter+1"),
                     weight = c(5, 4), firstYear = 1999, lastYear = 2100,
                     adjustEaster = "approximate")
expect_true(length(hdAT_approx) == 3)

message("Test genTd - quarterly frequency")

td7 <- genTd(freq = 4, hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39",
                                 "easter+50", "easter+60", "08-15", "10-26", "11-01",
                                 "12-08", "12-24", "12-25", "12-26", "12-31"),
             weight = c(rep(1, 11), 0.5, rep(1, 2), 0.5))
expect_true(length(td7) == 3)

message("Test genTd - rjd3 calendar holidays")

sd <- rjd3toolkit::special_day
fd <- rjd3toolkit::fixed_day
ed <- rjd3toolkit::easter_day

# names and holiday objects generate the same regressors as the strings
expect_identical(
  genTd(hd = list(sd("ASCENSION")), firstYear = 2000, lastYear = 2020),
  genTd(hd = list("easter+39"), firstYear = 2000, lastYear = 2020)
)
expect_identical(
  genTd(hd = list("whitmonday"), firstYear = 2000, lastYear = 2020),
  genTd(hd = list("easter+50"), firstYear = 2000, lastYear = 2020)
)
expect_identical(
  genTd(hd = list(sd("CHRISTMAS"), fd(1, 6), ed(39)),
        firstYear = 2000, lastYear = 2020),
  genTd(hd = list("12-25", "01-06", "easter+39"),
        firstYear = 2000, lastYear = 2020)
)

# the weight of a holiday object multiplies into the weight argument, an
# offset shifts the date of the holiday
expect_identical(
  genTd(hd = list(fd(12, 24, weight = 0.5)), weight = 0.5,
        firstYear = 2000, lastYear = 2020),
  genTd(hd = list("12-24"), weight = 0.25, firstYear = 2000, lastYear = 2020)
)
expect_identical(
  genTd(hd = list(sd("CHRISTMAS", offset = 1)),
        firstYear = 2000, lastYear = 2020),
  genTd(hd = list("12-26"), firstYear = 2000, lastYear = 2020)
)

# a whole national calendar can be the holiday list
AT <- rjd3toolkit::national_calendar(list(
  sd("NEWYEAR"), fd(1, 6), sd("EASTERMONDAY"), sd("ASCENSION"),
  sd("WHITMONDAY"), sd("CORPUSCHRISTI"), sd("MAYDAY"), sd("ASSUMPTION"),
  fd(10, 26), sd("ALLSAINTSDAY"), fd(12, 8), fd(12, 24, weight = 0.5),
  sd("CHRISTMAS"), fd(12, 26), fd(12, 31, weight = 0.5)
))
expect_identical(
  genTd(hd = AT, firstYear = 2000, lastYear = 2020),
  genTd(hd = list("01-01", "01-06", "easter+1", "easter+39", "easter+50",
                  "easter+60", "05-01", "08-15", "10-26", "11-01", "12-08",
                  "12-24", "12-25", "12-26", "12-31"),
        weight = c(rep(1, 11), 0.5, 1, 1, 0.5),
        firstYear = 2000, lastYear = 2020)
)

# validity periods restrict the dates on which a holiday is subtracted
expect_identical(
  genTd(hd = list(fd(1, 6, validity = list(start = "2010-01-01"))),
        firstYear = 2010, lastYear = 2020),
  genTd(hd = list("01-06"), firstYear = 2010, lastYear = 2020)
)

# holidays that always fall on a sunday leave the regressors unchanged
expect_identical(
  genTd(hd = character(0), firstYear = 2000, lastYear = 2020),
  genTd(hd = list("EASTER", "JULIANEASTER"), firstYear = 2000, lastYear = 2020)
)

# the definitions of the pre-specified holidays must be the dates that
# rjd3toolkit generates for them
drift <- character(0)
for (event in names(persephone3:::p3_specialdays)) {
  def <- persephone3:::p3_specialdays[[event]]
  if (is.na(def)) {
    next
  }
  dates <- if (grepl("^easter", def)) {
    as.Date(timeDate::Easter(year = 2000:2040)) +
      persephone3:::genTd_easter_off(def)
  } else {
    as.Date(paste0(2000:2040, "-", def))
  }
  hol <- rjd3toolkit::holidays(
    calendar = rjd3toolkit::national_calendar(list(sd(event))),
    start = "2000-01-01",
    length = as.integer(as.Date("2040-12-31") - as.Date("2000-01-01")) + 1L,
    type = "All"
  )
  jd <- as.Date(rownames(hol)[which(hol[, 1] != 0)])
  if (!identical(sort(dates), sort(jd))) {
    drift <- c(drift, event)
  }
}
expect_identical(drift, character(0))

# unknown events are rejected, the message lists the known ones
expect_error(genTd(hd = list("FASCHING")), "FASCHING")
expect_error(genTd(hd = list("LABORDAY")), "LABORDAY")
expect_error(genTd(hd = list("01-06", "01-01"), weight = 1), "weight")
