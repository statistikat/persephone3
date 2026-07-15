library(persephone3)

library(rjd3x13)
library(rjd3toolkit)

source("R/persephone.R")

source("R/perX13.R")
source("R/printDiagnostics.R")
source("tests/perHtsCL.R")
source("tests/perHts1.R")
source("tests/dashboardBuilder.R")
source("tests/dashboardShiny.R")

# Daten laden
library(data.table)
ilauf <- mountSTAT::mountWinShare(server = "DatenU", share = "U_KIN")
mlauf <- mountSTAT::mountMeth(folder = "Froehlich")
dir_prod <- paste0(ilauf, "/PRODUKTION/INDEX_2021")
kett <- fread(file.path(dir_prod, "2_OUTPUT/PRODUKTIONSINDEX/UNBEREINIGT/pi_kett.csv"))
nac <- kett$NACE
kett <- t(kett[, 3:(ncol(kett)-8)])
colnames(kett) <- nac
kett <- ts(kett, start=c(2000,1), freq = 12)
#write.csv2(kett, file = paste0(mlauf, "/J2026/COSA/PI_verkettet.csv"))

# Replace AirPassengers by the exact series you used in JD+
#y <- AirPassengers
y <- kett[,1]
m <- x13(ts20, "rsa5")
m <- perX13(ts20, "rsa5")
m$run()

str(m, max.level = 3)
# or, for a lower-level inspection:
jm <- jx13(ts20, "rsa5")
# dictionary(jm)   # if you want to inspect all available outputs

sa  <- m$result$final$d11final
irr <- m$result$final$d13final

res <- result(jm, "full_res")
sa <- result(jm, "sa")
irr <- result(jm, "i")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m$output$preprocessing$diagnostics$doornikhansen
m$output$preprocessing$diagnostics$lb

# RegArima residuals
normality      <- result(jm, "residuals.doornikhansen")
independence   <- result(jm, "residuals.lb")

# Residual seasonality tests
qs_sa          <- result(jm, "diagnostics.seas-sa-qs")
f_sa           <- result(jm, "diagnostics.seas-sa-f")

# Combined seasonality tests
comb_sa        <- result(jm, "diagnostics.seas-sa-combined")
comb_sa_last   <- result(jm, "diagnostics.seas-sa-combined3")
comb_i         <- result(jm, "diagnostics.seas-i-combined")
comb_i_last    <- result(jm, "diagnostics.seas-i-combined3")

# Residual trading days tests
td_sa_last     <- result(jm, "diagnostics.td-sa-last")
td_sa_all      <- result(jm, "diagnostics.td-sa-all")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# F-Test für Trading days
result(jm, "regression.td-ftest")
result(jm, "regression.td-derived")

result(jm, "diagnostics.td-sa-all")
result(jm, "diagnostics.td-sa-last")
result(jm, "diagnostics.td-i-all")
result(jm, "diagnostics.td-i-last")


# Q1:
result(jm, "m-statistics.q")
# Q2:
result(jm, "m-statistics.q-m2")

# Overall Summary
result(jm, "quality.summary")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

user_defined_output <- c("regression.td-ftest", "residuals.doornikhansen",
                         "residuals.lb", "regression.td-derived",
                         "diagnostics.seas-sa-f", "diagnostics.seas-sa-qs",
                         "diagnostics.td-sa-all", "diagnostics.td-sa-last",
                         "diagnostics.td-i-all", "diagnostics.td-i-last",
                         "m-statistics.q", "m-statistics.q-m2", "quality.summary")
m <- x13(y,"rsa5", userdefined=user_defined_output)
m$user_defined
qual <- diagnostics(m)
qual$preprocessing$P.value
m <- x13(t20, "rsa5")
ts20
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
spec_list <- list(transform.fun = "Log", transform.adjust = NA,
                  easter.enabled = TRUE,
                  td.option = "TD4", td.test = "None",
                  td.coef = c(0.7, NA, 0.5),
                  td.coef.type = c("Fixed", "Estimated", "Fixed"),
                  td.leapyear = "LengthOfPeriod",
                  td.leapyear.coef = 0.6,
                  outliers = c("AO2000.7", "LS2008.11"),
                  automdl.enabled = FALSE,
                  arima.p = 1, arima.d = 2, arima.q = 0,
                  arima.bp = 1, arima.bd = 1, arima.bq = 0)
 
obj <- perX13(ts20, "rsa3")
obj$updateParams(speclist=spec_list)
obj$run()
obj$output
summary(rjd3x13::x13_dictionary())


class_resid_norm_indep <- function(p) {
  if (is.na(p)) "Undefined"
  else if (p < 0.01) "Bad"
  else if (p < 0.10) "Uncertain"
  else "Good"
}

class_resid_spectral <- function(p) {
  if (is.na(p)) "Undefined"
  else if (p < 0.001) "Severe"
  else if (p < 0.010) "Bad"
  else if (p < 0.100) "Uncertain"
  else "Good"
}

class_resid_seasonality <- function(p) {
  if (is.na(p)) "Undefined"
  else if (p < 0.01) "Severe"
  else if (p < 0.05) "Bad"
  else if (p < 0.10) "Uncertain"
  else "Good"
}

class_resid_norm_indep(normality$pvalue)
class_resid_norm_indep(independence$pvalue)

#########################################################
files <- list.files("R", full.names = TRUE)
files

for (f in files) {
  txt <- readLines(f, warn = FALSE)
  if (any(grepl("hierarchicalTimeSeries|adjustedIndirect|R6Class\\(|weights|run = function", txt))) {
    cat("\n---", f, "---\n")
    cat(grep("hierarchicalTimeSeries|adjustedIndirect|R6Class\\(|weights|run = function", txt,
             value = TRUE), sep = "\n")
  }
}

