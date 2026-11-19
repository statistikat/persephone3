# Test the dashboard tables (getTradingDaysTable, getOutlierTable,
# getQualityTable) and the tabbed dashboard_single

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

coef_cols <- c("Variable", "Coef", "Std.Error", "t.value",
               "P.value", "Test")

message("Test dashboard tables - input validation")

obj0 <- perX13(AirPassengers, "rsa3")
expect_error(getTradingDaysTable(obj0))   # not yet run
expect_error(getOutlierTable(obj0))
expect_error(getQualityTable(obj0))
expect_error(getTradingDaysTable(AirPassengers))

message("Test dashboard tables - plain x13 without regressors")

obj <- perX13(AirPassengers, "rsa3")
obj$run()
empty <- getTradingDaysTable(obj)
expect_true(is.data.frame(empty))
expect_equal(names(empty), coef_cols)
expect_equal(nrow(empty), 0)
expect_equal(nrow(getOutlierTable(obj)), 0)

q <- getQualityTable(obj)
expect_equal(names(q), c("Item", "Value", "Status"))
expect_true("Total quality" %in% q$Item)
expect_equal(q$Value[q$Item == "Total quality"], "Good")
expect_true(all(q$Status[q$Status != ""] %in%
                  c("Good", "Uncertain", "Bad", "Severe", "Undefined")))
# the classification of the normality test agrees with diagnosticsTable()
d <- diagnosticsTable(obj)
expect_equal(
  q$Status[q$Item == "Normality"], as.character(d$normality)
)
# the combined calendar tests of JDemetra+ are shown with their p-value
comb_items <- c(
  "Combined test on SA", "Combined test on SA (last y.)",
  "Combined test on irregular", "Combined test on irregular (last y.)"
)
expect_true(all(comb_items %in% q$Item))
expect_false(any(c("Calendar in SA (all)", "Calendar in Irregular (all)") %in%
                   q$Item))
p_comb <- suppressWarnings(as.numeric(q$Value[q$Item == comb_items[1]]))
expect_true(!is.na(p_comb) && p_comb >= 0 && p_comb <= 1)
expect_true(q$Status[q$Item == comb_items[1]] %in% c("Good", "Bad"))

message("Test dashboard tables - x13 with trading days")

obj_td <- perX13(AirPassengers, "rsa3", td = AT)
obj_td$run()
td <- getTradingDaysTable(obj_td)
expect_equal(names(td), coef_cols)
# one row per trading day regressor plus the derived Sunday
n_days <- obj_td$output$user_defined[["regression.ntd"]]
t_rows <- td$Test == "t"
expect_equal(sum(t_rows), n_days + 1)
expect_true("Sunday (derived)" %in% td$Variable)
expect_false(any(grepl("^group_[0-9]+$", td$Variable)))
# exactly one joint F test, the one of the trading days, with the F(q, df)
# label and a name that says what it tests
f_rows <- grepl("^F\\(", td$Test)
expect_equal(sum(f_rows), 1)
f_row <- td[f_rows, ]
expect_equal(f_row$Variable, "Trading days - FTest")
expect_true(grepl("^F\\([0-9]+, [0-9]+\\)$", f_row$Test))
expect_true(f_row$Coef > 0)
expect_true(f_row$P.value >= 0 && f_row$P.value <= 1)
# the joint F test of the trading days and the outliers together is not
# part of the table any more
expect_false(any(grepl("outlier", td$Variable)))
# Sunday (derived) is minus the sum of the six day effects
days <- td[t_rows & td$Variable != "Sunday (derived)", ]
expect_equal(
  sum(days$Coef) + td$Coef[td$Variable == "Sunday (derived)"], 0,
  tolerance = 1e-8
)

message("Test dashboard tables - tramoseats outliers and leap year")

obj_ts <- perTramo(AirPassengers, "rsafull")
obj_ts$run()
otl <- getOutlierTable(obj_ts)
expect_equal(names(otl), coef_cols)
expect_true(any(grepl("^(AO|LS|TC)[0-9]{4}\\.[0-9]+$", otl$Variable)))
expect_true("leap year" %in% otl$Variable)
# t value and p value are consistent
expect_equal(otl$t.value, otl$Coef / otl$Std.Error, tolerance = 1e-8)

message("Test dashboard_single - tabbed layout")

file <- tempfile(fileext = ".html")
ui <- dashboard_single(obj_td, file = file, title = "Dashboard Test")
expect_true(inherits(ui, "shiny.tag.list"))
txt <- paste(readLines(file, warn = FALSE), collapse = "\n")
unlink(file)
# the tab bar and all five panes
for (tab in c("Overview", "Plots", "Trading days", "Outliers", "Quality")) {
  expect_true(grepl(tab, txt, fixed = TRUE))
}
expect_true(grepl("p3-tab-btn", txt, fixed = TRUE))
# the tables of the tabs and the plotly plots
expect_true(grepl("Sunday (derived)", txt, fixed = TRUE))
expect_true(grepl("Trading days", txt, fixed = TRUE))
expect_true(grepl("Total quality", txt, fixed = TRUE))
expect_true(grepl("plotly", txt, fixed = TRUE))
