x <- perTramo(AirPassengers, "rsa0", backend = "native")
expect_error(x$plotResiduals(), "No results")
x$run()
r <- x$result$diagnostics$residuals
expect_identical(r, x$output$residuals)
expect_identical(as.integer(start(r$tsres)), c(1950L, 2L))
expect_identical(as.integer(end(r$tsres)), c(1960L, 12L))
expect_identical(length(r$tsres), length(AirPassengers) - 13L)

dy <- x$plotResiduals(which = "res")
expect_identical(as.Date(dy$x$data[[1L]]), zoo::as.Date(r$tsres))
expect_identical(as.numeric(dy$x$data[[2L]]), as.numeric(r$tsres))
for (key in c("acf", "acf2", "pacf")) {
  chart <- x$plotResiduals(which = key, interactive = FALSE)$data
  corr <- switch(key, acf = stats::acf(r$tsres, plot = FALSE),
    acf2 = stats::acf(r$tsres^2, plot = FALSE), pacf = stats::pacf(r$tsres, plot = FALSE))
  index <- if (key == "pacf") seq_along(corr$lag) else seq_along(corr$lag)[-1]
  expect_identical(as.numeric(chart$lag), as.numeric(corr$lag)[index] * frequency(r$tsres))
  expect_identical(as.numeric(chart$acf), as.numeric(corr$acf)[index])
}
z <- as.numeric(r$tsres) / r$ser
expect_identical(x$plotResiduals(which = "sreshist", interactive = FALSE)$data$x, z)
qq <- stats::qqnorm(z, plot.it = FALSE)
plot_qq <- x$plotResiduals(which = "nqq", interactive = FALSE)$data
expect_identical(plot_qq$x, qq$x)
expect_identical(plot_qq$y, qq$y)
spectrum <- plotSpectrum(x, "ggplot", "residuals", "periodogram")$data
expect_identical(spectrum$spec, as.numeric(stats::spec.pgram(r$tsres, plot = FALSE)$spec))
dashboard <- as.character(dashboard_single(x))
expect_false(grepl("Residual diagnostics are unavailable", dashboard, fixed = TRUE))
expect_false(grepl("Plot error:", dashboard, fixed = TRUE))
expect_true(grepl("Residual diagnostics", dashboard, fixed = TRUE))

if (requireNamespace("rjd3tramoseats", quietly = TRUE)) {
  jd <- perTramo(AirPassengers, "rsa0", backend = "rjd3")
  jd$run()
  reference <- jd$output$user_defined
  expect_identical(as.numeric(r$tsres), as.numeric(reference$residuals.tsres))
  expect_identical(c(start(r$tsres), end(r$tsres), frequency(r$tsres)),
    c(start(reference$residuals.tsres), end(reference$residuals.tsres), frequency(reference$residuals.tsres)))
  expect_identical(r$ser, reference$residuals.ser)
  for (key in c("acf", "acf2", "pacf", "nqq", "sreshist"))
    expect_identical(x$plotResiduals(key, interactive = FALSE)$data,
                     jd$plotResiduals(key, interactive = FALSE)$data)
}
