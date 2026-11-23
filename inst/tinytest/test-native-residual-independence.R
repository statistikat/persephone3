x <- perTramo(AirPassengers, "rsa0", backend = "native")
x$run()
lb <- x$result$diagnostics$residuals$lb
expect_identical(x$output$residuals$lb, lb)
# Frozen public JD+ 3.9.0 RSA0 residuals.lb for the AirPassengers observations.
expect_identical(lb$value, 0x1.47ef8d88a3c46p+5)
expect_identical(lb$pvalue, 0x1.0ea36b3475900p-7)
expect_identical(lb$df, 22)
expect_identical(lb$lags, 24L)

metric <- persephone3:::diagnostic_metrics(x)$independence
expect_identical(metric$kind, "test")
expect_identical(metric$source, "residuals/lb")
expect_identical(metric$statistic, lb$value)
expect_identical(metric$pvalue, lb$pvalue)
expect_identical(metric$status, "Bad")
expect_identical(metric$availability, "available")
expect_identical(persephone3:::diagnostic_dashboard_values(x)$p_indep, metric$pvalue)
expect_identical(persephone3:::diagnostic_dashboard_values(x)$st_indep, metric$status)
expect_identical(x$diagnosticsTable()$independence, metric$status)
expect_identical(x$generateQrTable()[["Ljung.Box.p.value"]], metric$pvalue)

short <- perTramo(stats::ts(as.numeric(AirPassengers)[1:37], frequency = 12), "rsa0", backend = "native")
short$run()
undefined <- persephone3:::diagnostic_metrics(short)$independence
expect_true(is.nan(short$output$residuals$lb$pvalue))
expect_identical(undefined$availability, "missing")
expect_identical(undefined$status, "Undefined")
expect_identical(short$diagnosticsTable()$independence, "Undefined")
expect_identical(short$generateQrTable()[["Ljung.Box.p.value"]], NA_real_)

if (requireNamespace("rjd3tramoseats", quietly = TRUE)) {
  jd <- perTramo(AirPassengers, "rsa0", backend = "rjd3")
  jd$run()
  reference <- jd$output$user_defined$residuals.lb
  expect_identical(metric$statistic, reference$value)
  expect_identical(metric$pvalue, reference$pvalue)
  expect_identical(jd$diagnosticsTable()$independence, x$diagnosticsTable()$independence)
  expect_identical(jd$generateQrTable()[["Ljung.Box.p.value"]], metric$pvalue)
}
