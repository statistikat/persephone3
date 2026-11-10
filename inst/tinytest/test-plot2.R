# Test plot changes: smoothed lines, optional range selector, overlaid spectra

data(AirPassengers, package = "datasets")

message("Test plot - smoothed lines and range selector")

obj <- perX13(AirPassengers, "rsa1")

# pre-run plot is smoothed by default (finer time grid than the observations)
g0 <- plot(obj)
expect_true(length(g0$x$data[[1]]) > length(AirPassengers))

# the range selector is off by default and can be switched on
g_no <- plot(obj)
expect_true(!isTRUE(g_no$x$attrs$showRangeSelector))
g_yes <- plot(obj, rangeSelector = TRUE)
expect_true(isTRUE(g_yes$x$attrs$showRangeSelector))

obj$run()

n <- length(obj$output$user_defined$y)

# smoothed plot: 8 segments per observation interval
g <- plot(obj, forecasts = FALSE)
expect_equal(length(g$x$data[[1]]), (n - 1) * 8 + 1)

# the spline passes through the observations
vals <- vapply(g$x$data[[2]], function(z) z[1], 0)
grid <- 1 + 8 * seq_len(n) - 8
expect_equal(as.numeric(vals[grid]),
             as.numeric(obj$output$user_defined$y),
             tolerance = 1e-6)

# smooth = FALSE keeps the original observations
g2 <- plot(obj, smooth = FALSE, forecasts = FALSE)
expect_equal(length(g2$x$data[[1]]), n)
vals2 <- vapply(g2$x$data[[2]], function(z) z[1], 0)
expect_equal(as.numeric(vals2), as.numeric(obj$output$user_defined$y),
             tolerance = 1e-6)

# plots with outliers and forecasts still work (unsmoothed overlay series)
obj$updateParams(addout.type = "AO", addout.date = "1950-01-01")
obj$run()
expect_error(print(plot(obj)), NA)

message("Test plot - overlaid spectra")

x <- perX13(AirPassengers, "rsa5c")
x$run()

# one spectrum per engine, all plot types
for (pt in c("arSpec", "arSpecBars", "periodogram")) {
  expect_error(plotSpectrum(x, plotType = pt), NA)
  expect_error(plotSpectrum(x, plotEngine = "ggplot", plotType = pt), NA)
}

# overlaid spectra of the three series types, highcharter engine
g3 <- plotSpectrum(x, tsType = c("sa", "irregular", "residuals"))
expect_equal(length(g3$x$hc_opts$series), 3)
expect_equal(
  vapply(g3$x$hc_opts$series, function(s) s$name, ""),
  c("sa", "irregular", "residuals")
)

# overlaid spectra, ggplot engine (one coloured line per series type)
g4 <- plotSpectrum(x, plotEngine = "ggplot",
                   tsType = c("sa", "irregular", "residuals"))
d4 <- ggplot2::ggplot_build(g4)$data[[1]]
expect_equal(length(unique(d4$group)), 3)
expect_equal(length(unique(d4$colour)), 3)

# numeric codes work as well
g5 <- plotSpectrum(x, tsType = c(2, 3))
expect_equal(length(g5$x$hc_opts$series), 2)

# overlay is not available for the bar plot type
expect_error(plotSpectrum(x, tsType = c("sa", "irregular"),
                          plotType = "arSpecBars"))
expect_error(plotSpectrum(x, plotEngine = "ggplot", tsType = c("sa", "irregular"),
                          plotType = "arSpecBars"))

# invalid tsType
expect_error(plotSpectrum(x, tsType = "nonsense"))
