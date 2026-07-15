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
