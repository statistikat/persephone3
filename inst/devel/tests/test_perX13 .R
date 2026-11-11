
library(rjd3x13)
library(rjd3toolkit)

# Trading days with rjd3tools ....................
AT <- national_calendar(list(
  special_day('NEWYEAR'),
  fixed_day(1, 6),
  special_day('EASTERMONDAY'),
  special_day('ASCENSION'),
  special_day('WHITMONDAY'),
  special_day('CORPUSCHRISTI'),
  special_day('MAYDAY'),
  special_day('ASSUMPTION'),
  fixed_day(10, 26),
  special_day('ALLSAINTSDAY'),
  fixed_day(12, 8),
  fixed_day(12, 24, weight = 0.5),
  special_day('CHRISTMAS'),
  fixed_day(12, 26),
  fixed_day(12, 31, weight = 0.5)
))

td7at <- calendar_td(AT, 12, c(2000,1), 480, groups=c(1,2,3,4,5,6,0),
                     holiday = 7, contrasts = TRUE)

calendar_td(AT, 12, c(2005,1), 480, groups=c(1,1,1,1,1,0,0),
                     holiday = 7, contrasts = TRUE)

td_at <- calendar_td(
  calendar = AT,
  s = pi_unadj[,3],
  holiday = 7,
  groups = c(1, 2, 3, 4, 5, 6, 0),
  contrasts = FALSE
)


# Alternativ: aus Persephone ..........................
td7lpY <- genTd(freq = 12, firstYear = 2000,
                 hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39",
                           "easter+50", "easter+60", "08-15", "10-26", "11-01",
                           "12-08", "12-24", "12-25", "12-26", "12-31"),
                 weight = c(rep(1,11), 0.5, rep(1,2), 0.5))
tdAT7 <- td7lpY[[3]][,1:6]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(highcharter)
mlauf <- mountSTAT::mountMeth(folder = "Froehlich")
ui <- fread(paste0(mlauf, "/J2023/SB/PI/UI.csv"), dec = ",")
g00 <- ts(as.numeric(ui[1,2:ncol(ui)]), start = c(2005,1), freq = 12)
hchart(g00)

spec_init <- rjd3x13::x13_spec("RSA3")
#sa_x13_v3 <- rjd3x13::x13(y_raw, spec = "RSA5")
#rjd3x13::userdefined_variables_x13()

my_context <- rjd3toolkit::modelling_context(calendars = list(cal = AT))
my_context <- modelling_context(variables = list(REGS_TD = td_at))
spec_new <- set_tradingdays(
  x = spec_init,
  option = "UserDefined",
  uservariable = c("REGS_TD.group_0", "REGS_TD.group_1", "REGS_TD.group_2", "REGS_TD.group_3",
                   "REGS_TD.group_4", "REGS_TD.group_5", "REGS_TD.group_6"),
  test = c("Add", "Joint_F")
)
serAdj <- x13(g00, spec_new, context = my_context)
serAdj

#my_context <- rjd3toolkit::modelling_context(variables = list(cal = tdAT7))
spec_g00 <- rjd3toolkit::set_tradingdays(spec_init,
                                         option = "TradingDays",
                                         calendar.name="cal",
                                         test = c("Add", "Joint_F"))
spec_g00 <- rjd3toolkit::set_transform(spec_g00, fun = "Log", adjust = "LeapYear")
spec_g00 <- rjd3toolkit::add_outlier(spec_g00,
                                     type = c("AO", "TC", "TC"),
                                     date = c("2023-02-01", "2020-03-01", "2020-04_01"))
spec_g00 <- rjd3toolkit::set_automodel(spec_g00, enable = FALSE)
spec_g00 <- rjd3toolkit::set_arima(spec_g00, mean = 0,
                                   p  = 1, d =  1, q =  0,
                                   bp = 0, bd = 1, bq = 1)

g00_adj <- x13(g00, spec_g00, context = my_context)
(g00_adj$result$diagnostics$td.ftest.sa)
g00_adj$estimation_spec$x11
sa <- g00_adj$result$final$d11final
t <- g00_adj$result$final$d12final
unlist(g00_adj$result$mstats)
g00_adj$result$diagnostics















ser <- AirPassengers
BE <- national_calendar(list(
  fixed_day(7, 21),
  special_day("NEWYEAR"),
  special_day("CHRISTMAS"),
  special_day("MAYDAY"),
  special_day("EASTERMONDAY"),
  special_day("ASCENSION"),
  special_day("WHITMONDAY"),
  special_day("ASSUMPTION"),
  special_day("ALLSAINTSDAY"),
  special_day("ARMISTICE")
))
td_be <- calendar_td(
  calendar = BE,
  s = ser,
  holiday = 7,
  groups = c(1, 1, 1, 2, 2, 3, 0),
  contrasts = FALSE
)

my_context <- modelling_context(variables = list(REGS_TD = td_be))

spec_init <- x13_spec("RSA3")
spec_new <- set_tradingdays(
  x = spec_init,
  option = "UserDefined",
  uservariable = c("REGS_TD.group_0", "REGS_TD.group_1", "REGS_TD.group_2", "REGS_TD.group_3"),
  test = c("Add", "Joint_F")
)
serAdj <- x13(ser, spec_new, context = my_context)
serAdj




################################################################
################################################################
pkgload::load_all("/data/home/froeh/persephone3")
ts1 <- pi_unadj[, "AT"]

library(persephone3)
library(rjd3toolkit)

#data(AirPassengers, package = "datasets")

## ---- 1) from a national calendar -----------------------------------------
# Austrian calendar with holidays weighted for trading day estimation
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

obj <- perX13(ts1, "rsa3", td = AT)
obj$run()

# regression variables used in the model
print(obj$output$preprocessing$description$variables)

# trading day F-tests (irregular and s.a. series)
obj$output$diagnostics$td.ftest.i
obj$output$diagnostics$td.ftest.sa

# standard output as usual
plot(obj)          # smoothed lines, no range selector
getCoefs(obj)      # TD coefficients are included


## ---- 2) from precomputed regressors (genTd) ------------------------------
tdAT <- genTd(
  freq = 12, firstYear = 1949, lastYear = 1961,
  hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39",
            "easter+50", "easter+60", "08-15", "10-26", "11-01",
            "12-08", "12-24", "12-25", "12-26", "12-31"),
  weight = c(rep(1, 11), 0.5, rep(1, 2), 0.5)
)[[3]][, 1:6]   # the 6 centered day regressors

obj2 <- perX13(ts1, "rsa3", td = tdAT)
obj2$run()
plot(obj2)


## ---- 3) combining with an existing context / extra spec args -------------
# extra regressors in a context are kept, td variables are merged in
ctx <- modelling_context(variables = list(
  MYVAR = ts(rnorm(144), start = 1949, frequency = 12)
))
obj3 <- perX13(AirPassengers, "rsa3", context = ctx, td = AT,
               transform.fun = "Log",      # ordinary spec args still work
               td.test = "Joint_F")        # td.* args override the defaults
obj3$run()

# or set the trading days later on an existing object
obj4 <- perX13(AirPassengers, "rsa3")
obj4$updateParams(td = AT)
obj4$run()
