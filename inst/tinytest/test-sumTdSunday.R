# Test the derived Sunday effect of sumTdSunday() and its parameterisation
# detection: with regressors given as contrasts against Sunday (the default
# of the trading day modules of JDemetra+ and of genTd()) the Sunday effect
# is -sum(b); with regressors centered with respect to the mean day or given
# as day counts it is -(1/7) sum(b).

suppressMessages(library(rjd3toolkit))

data(AirPassengers, package = "datasets")

message("Test Sunday effect - the two parameterisations")

# counts of the days of the week and days per month of 1949-01 .. 1961-12
dates <- seq(as.Date("1949-01-01"), as.Date("1961-12-31"), by = "day")
midx <- match(format(dates, "%Y-%m"),
              format(seq(as.Date("1949-01-01"), as.Date("1961-12-31"),
                         by = "month"), "%Y-%m"))
mf <- factor(midx, levels = 1:144)
dow <- match(substr(weekdays(dates), 1, 3),
             c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
N <- matrix(0, 144, 7)
for (j in 1:7) N[, j] <- as.vector(tapply(dow == j, mf, sum))
D <- rowSums(N)

# true day effects (Monday to Sunday) and the Sunday deviation from the
# mean day effect, which is what the "Sunday (derived)" row must show
set.seed(42)
f <- c(10, 0, 0, 0, 0, 0, -4)
true_sun <- f[7] - sum(f) / 7
y <- as.vector(N %*% f) + stats::rnorm(144, 0, 0.5)

# regressors as contrasts against Sunday and as centered regressors
Xc <- sweep(N[, 1:6, drop = FALSE], 1, N[, 7], "-")
Xz <- sweep(N[, 1:6, drop = FALSE], 1, D / 7, "-")
bc <- coef(stats::lm(y ~ cbind(1, Xc) - 1))[-1]
bz <- coef(stats::lm(y ~ cbind(1, Xz) - 1))[-1]

# a minimal fit object with the given design matrix and coefficients, with
# six estimated trading day regressors
fakeFit <- function(X6, b) {
  vars <- lapply(seq_len(6), function(i) {
    list(name = paste0("d", i), type = "TD",
         coef = list(type = "ESTIMATED"))
  })
  structure(
    list(output = list(preprocessing = list(
      description = list(log = FALSE, variables = vars),
      estimation = list(
        b = unname(b), bvar = diag(1e-3, 6), X = unname(X6),
        likelihood = list(neffectiveobs = 140, nparams = 8)
      )
    ))),
    class = c("x13Single", "persephoneSingle", "persephone")
  )
}

sun_c <- persephone3:::sumTdSunday(fakeFit(Xc, bc))
sun_z <- persephone3:::sumTdSunday(fakeFit(Xz, bz))
# the contrasts are recognised as such (-sum(b)), the centered regressors
# get the factor 1/7; both must give the true Sunday effect
expect_equal(rownames(sun_c), "Sunday (derived)")
expect_equal(sun_c$coef, true_sun, tolerance = 0.1)
expect_equal(sun_c$coef, -sum(bc), tolerance = 1e-8)
expect_equal(sun_z$coef, true_sun, tolerance = 0.1)
expect_equal(sun_z$coef, -sum(bz) / 7, tolerance = 1e-8)
# without the design matrix (X-13 with the built-in trading day module) the
# contrast parameterisation is assumed
noX <- fakeFit(Xc, bc)
noX$output$preprocessing$estimation$X <- NULL
expect_equal(persephone3:::sumTdSunday(noX)$coef, -sum(bc), tolerance = 1e-8)

message("Test Sunday effect - real fit with trading day regressors")

# in a fit with the trading day calendar the detected parameterisation must
# be consistent with the sum of the displayed coefficients: Sunday (derived)
# and the six day effects add up to zero
AT <- national_calendar(list(
  special_day("NEWYEAR"), fixed_day(1, 6), special_day("EASTERMONDAY"),
  special_day("MAYDAY"), fixed_day(12, 24, weight = 0.5), special_day("CHRISTMAS")
))
obj <- perX13(AirPassengers, "rsa3", td = AT)
obj$run()
td <- getTradingDaysTable(obj)
days <- td[td$Test == "t" & td$Variable != "Sunday (derived)", ]
expect_equal(nrow(days), 6)
expect_equal(sum(days$Coef) + td$Coef[td$Variable == "Sunday (derived)"],
             0, tolerance = 1e-8)
# the trading day regressors of the demetra calendar are contrasts against
# Sunday, so the Sunday effect is minus the sum of the six coefficients
X <- obj$output$preprocessing$estimation$X
if (!is.null(X) && ncol(X) == 6) {
  expect_true(any(abs(rowSums(unclass(X))) > 1))
  expect_equal(td$Coef[td$Variable == "Sunday (derived)"], -sum(days$Coef),
               tolerance = 1e-8)
}
