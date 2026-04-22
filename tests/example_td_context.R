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

tdAT7 <- calendar_td(AT, 12, c(2000,1), 480, groups=c(1,2,3,4,5,6,0),

                     holiday = 7, contrasts = TRUE)

tdAT5 <- calendar_td(AT, 12, c(2005,1), 480, groups=c(1,1,1,1,1,0,0),

                     holiday = 7, contrasts = TRUE)

# Alternativ: aus Persephone ..........................

td7lpY <- genTd(freq = 12, firstYear = 2000,

                hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39",

                          "easter+50", "easter+60", "08-15", "10-26", "11-01",

                          "12-08", "12-24", "12-25", "12-26", "12-31"),

                weight = c(rep(1,11), 0.5, rep(1,2), 0.5))

tdAT7 <- td7lpY[[3]][,1:6]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(highcharter)
require(data.table)
mlauf <- mountSTAT::mountMeth(folder = "Froehlich")

ui <- fread(paste0(mlauf, "/J2023/SB/PI/UI.csv"), dec = ",")

g00 <- ts(as.numeric(ui[1,2:ncol(ui)]), start = c(2005,1), freq = 12)

hchart(g00)

spec_init <- rjd3x13::x13_spec("RSA3")

#sa_x13_v3 <- rjd3x13::x13(y_raw, spec = "RSA5")

#rjd3x13::userdefined_variables_x13()

my_context <- rjd3toolkit::modelling_context(calendars = list(cal = AT))

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

obj <- perX13(
  g00,
  "rsa3",
  context = my_context,
  td.option = "TradingDays",
  td.calendar.name="cal",
  td.test = c("Add", "Joint_F"),
  transform.fun = "Log",
  transform.adjust = "LeapYear",
  outliers = c("AO2020.3", "AO2020.4","TC2023.2"),
  automdl.enable = FALSE,
  arima.mean = 0,
  arima.p = 1,
  arima.d = 1,
  arima.q = 0,
  arima.bp = 0,
  arima.bd = 1,
  arima.bq = 1
)
obj$run()
obj
