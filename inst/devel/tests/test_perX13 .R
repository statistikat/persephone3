
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
  freq = 12, firstYear = 2000, lastYear = 2100,
  hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39",
            "easter+50", "easter+60", "08-15", "10-26", "11-01",
            "12-08", "12-24", "12-25", "12-26", "12-31"),
  weight = c(rep(1, 11), 0.5, rep(1, 2), 0.5)
)[[3]][, 1:6]   # the 6 centered day regressors

obj2 <- perX13(ts1, "rsa3", td = tdAT, group = c(1,1,1,1,1,2,2))
obj2$run()
plot(obj2)


## ---- 3) combining with an existing context / extra spec args -------------
# extra regressors in a context are kept, td variables are merged in
ctx <- modelling_context(variables = list(
  MYVAR = ts(rnorm(144), start = 2000, frequency = 12)
))
obj3 <- perX13(ts1, "rsa3", context = ctx, td = AT,
               transform.fun = "Log", td.test = c("Add", "Joint_F"))        # td.* args override the defaults
obj3$run()

# or set the trading days later on an existing object
obj4 <- perX13(AirPassengers, "rsa3")
obj4$updateParams(td = AT)
obj4$run()



#################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ---- simulated chain linked data: leaves a, b, c and the aggregates -------
# n <- 12 * 20; tt <- seq_len(n)
# ul <- list(a = 100 + .5 * tt + 5 * sin(2 * pi * tt / 12),
#            b =  60 + .3 * tt + 3 * sin(2 * pi * tt / 12 + 1),
#            c =  40 + .2 * tt + 2 * sin(2 * pi * tt / 12 + 2))
# lf <- list(a = .50 + .0020 * tt, b = .60 + .0010 * tt, c = .55 + .0015 * tt)
# w   <- list(agg = c(.5, .3, .2), ab = c(.6, .4))          # weights per aggregate
# mk  <- function(lv, ww) {                                  # linked series + factor
#   u <- Reduce("+", Map("*", ul[lv], as.list(ww)))
#   f <- Reduce("+", Map(function(k, wi) ul[[k]] * lf[[k]] * wi, lv, as.list(ww))) / u
#   list(y = u * f, f = f)
# }
# A <- mk(c("a", "b", "c"), w$agg); B <- mk(c("a", "b"), w$ab)
# mts <- ts(cbind(do.call("cbind", Map("*", ul, lf)), agg = A$y, ab = B$y),
#           start = c(2000, 1), frequency = 12)
# lfm <- ts(cbind(do.call("cbind", lf), agg = A$f, ab = B$f),
#           start = c(2000, 1), frequency = 12)
# ## ---- trading day regressors, per series -----------------------------------
# td_a <- td_context(ts = window(mts[, "a"]), method = "x13",
#   td = genTd(freq = 12, firstYear = 2000, lastYear = 2020,
#              hd = list("01-01", "01-06", "05-01", "easter+1"))[[3]][, 1:6])

#

# Liste der relevanten Aggegate --------------------------
aggList <- list(
    F = c("F41", "F42", "F43"),
    F41_43 = c("F41", "F43"),
    D_E36 = c("D", "E36"),
    AT00 = c("B", "C", "D", "E", "F41", "F42", "F43"),
    AT01 = c("B", "C", "F41", "F42", "F43"),
    AT02 = c("B", "C", "D352", "D353", "E", "F41", "F42", "F43"),
    AT03 = c("B", "C", "D", "E"),
    EU07 = c("B", "C", "D351", "D352", "F41", "F42", "F43"),
    EU08 = c("B", "C", "D351", "D352"),
    C13_14 = c("C13", "C14"),
    C17_18 = c("C17", "C18"),
    C20_21 = c("C20", "C21"),
    C26_27 = c("C26", "C27"),
    C29_30 = c("C29", "C30"),
    C31_32 = c("C31", "C32"),
    D35_E36 = c("D351", "D352")
)
names(aggList)
gew_nam <- as.character(unlist(aggList))
pi_unverkettet <- ts(fread(file = paste0(mlauf, "/J2026/COSA/PI_unlinked.csv")), start = c(2000, 1),
                      freq = 12)
pi_kett <- ts(fread(file = paste0(mlauf, "/J2026/COSA/PI_verkettet.csv")), start = c(2000, 1),
            freq = 12)
linkFakt <- fread(file = paste0(mlauf, "/J2026/COSA/linkFaktoren.csv"))
gew <- fread(file = paste0(mlauf, "/J2026/COSA/gew.csv"))[, -1]
names(gew) <- gew_nam
lapply(aggList, )

######################################
aggs   <- names(aggList)                        # the 16 aggregate series
series <- union(unique(unlist(aggList)), aggs)  # all 39 series perHts needs
start0 <- c(2000, 1)
freq   <- 12

# helper: pick columns by name from an mts / data.table / data.frame
pick <- function(x, cols) {
  x <- as.data.frame(x)
  miss <- setdiff(cols, names(x))
  if (length(miss)) stop("columns missing: ", paste(miss, collapse = ", "))
  as.matrix(x[, cols, drop = FALSE])
}

## ---- 1) pi_kett -> the series (mts, one column per series) -------------------
# aggregates must be supplied as series as well (direct SA + their cl factors);
# extra columns (V1, other NACE codes) are dropped
mts <- ts(pick(pi_kett, series), start = start0, frequency = freq)

## ---- 2) aggList -> groups (already the right form) ---------------------------
# groups = aggList

## ---- 3) gew -> one weight block per aggregate ---------------------------------
# gew has one column per (aggregate, sub-series) pair, in the order of
# unlist(aggList) -> split by position, name the columns by the sub-series
gw_mat <- as.matrix(as.data.frame(gew))         # purely numeric weight matrix
stopifnot(ncol(gw_mat) == length(unlist(aggList)))
len  <- lengths(aggList)
from <- cumsum(c(1, head(len, -1)))

weights <- lapply(seq_along(aggList), function(i) {
  m <- gw_mat[, seq.int(from[i], length.out = len[i]), drop = FALSE]
  rs <- rowSums(m)                              # production weights sum to 0.97-1.01
  w  <- ts(m[rs > 0, , drop = FALSE] / rs[rs > 0],  # normalize rows to sum 1
           start = start0, frequency = freq)
  colnames(w) <- aggList[[i]]
  w
})
names(weights) <- aggs

## ---- 4) linkFakt -> cl_weights (chain linking factors) -------------------------
# the factors themselves (linked = unchained * factor), one column per series;
# the V1 index column is dropped by the name-based pick
cl_weights <- ts(pick(linkFakt, series), start = start0, frequency = freq)

## ---- 5) align the spans (pi ends 2026.09, factors end 2026.01) -----------------
# keep only months for which chain linking factors exist; the weights keep
# their full span (later values are used for the indirect forecasts)
dec  <- function(e) e[1] + (e[2] - 1) / freq
endc <- if (dec(stats::end(mts)) <= dec(stats::end(cl_weights)))
          stats::end(mts) else stats::end(cl_weights)
if (dec(endc) < dec(stats::end(mts)))
  message("pi_kett windowed to ", paste(endc, collapse = "."),
          " - the chain linking factors end there")
mts        <- window(mts, end = endc)
cl_weights <- window(cl_weights, end = endc)

## ---- 6) the perHts object -------------------------------------------------------
hts <- perHts(
  list       = mts,             # leaves + aggregates, one column each
  groups     = aggList,         # which sub-series build which aggregate
  weights    = weights,         # one weight mts per aggregate
  cl_weights = cl_weights,      # chain linking factors per series
  ref_year   = 2021,            # base year of the indirect aggregates (2015 = 100)
  td = tdAT,
  method     = "x13", template = "rsa3",
  adjustment = "both"           # or "indirect": skips the direct SA of all 16
)                               # aggregates -> much faster
# optional trading day regressors for single series:
#   td = list(C = tdAT, "C20_21" = tdAT)

hts$run()

## ---- 7) results -----------------------------------------------------------------
hts$getSeries(aggregates = TRUE)
hts$getAdjustedIndirect("AT00")     # 5-step indirect, re-referenced to 2015 = 100
hts$getAdjustedDirect("AT00")
hts$adjustedIndirect                # named list over all 16 aggregates
print(hts); diagnosticsTable(hts)
plot(hts, series = "AT00")

#####################################


## ---- 1) groups + 2) weights + 3) cl_weights/ref_year + 5) td -------------
hts <- perHts(
  list = mts, method = "x13", template = "rsa3",
  #groups   = list(agg = c("a", "b", "c"), ab = c("a", "b")),
  groups = aggList,
  weights  = w,                                    # one weight set per aggregate
  cl_weights = lfm, ref_year = 2015,               # chain linking + base year
  td = list(a = td_a),                             # only "a" gets td regressors
  adjustment = "both"                              # "both" | "direct" | "indirect"
)
hts$run()

## ---- 6) accessors ---------------------------------------------------------
hts$getSeries()                        # "a" "b" "c" "agg" "ab"  (all series)
hts$getSeries(aggregates = TRUE)       # "agg" "ab"
hts$getAdjustedIndirect("agg")         # 5-step indirect, re-referenced to 2021
hts$getAdjustedDirect("ab")            # direct adjustment of that aggregate
hts$getUnchainedIndirect("agg")        # weighted sum before it is re-chained
hts$getForecastsIndirect("agg")        # indirect forecasts
hts$adjustedIndirect                   # named list over all aggregates
hts$adjusted                           # indirect (direct if adjustment="direct")
hts$getComponent("a")$adjusted         # one series, a full persephone object

## ---- fast mode: no direct adjustment of the aggregates at all -------------
hts_i <- perHts(list = mts, method = "x13", template = "rsa3",
  groups = list(agg = c("a", "b", "c")), weights = w,
  cl_weights = lfm, ref_year = 2021, adjustment = "indirect")
hts_i$run(); hts_i$adjustedIndirect$agg

## ---- time varying / annual weights (repeated within their periods) --------
perHts(list = mts, method = "x13", template = "rsa3",
  groups = list(agg = c("a", "b", "c")),
  weights = list(agg = ts(cbind(a = .5, b = .3, c = .2), 2000, frequency = 1)))

## ---- 7) plotting one aggregate of many ------------------------------------
plot(hts, series = "agg")
plot(hts, series = "agg", plotEngine = "plotly")

## ---- update parameters later (td for one series, spec for all) ------------
hts_i$updateParams(component = "b", td = td_a)
hts_i$updateParams(speclist = list(transform.fun = "Log"), iterate = TRUE)

## ---- diagnostics ----------------------------------------------------------
print(hts); diagnosticsTable(hts)
Compatibility notes:
# unchanged classic behaviour (one aggregate, nested levels still work)
ht <- perHts(a = perX13(AirPassengers), b = perX13(AirPassengers * 2), method = "x13")
ht$run(); ht$adjustedDirect; ht$adjustedIndirect   # plain ts, as before
