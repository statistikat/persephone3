x <- perTramo(AirPassengers, "rsa0", backend = "native")
x$run()
dh <- x$result$diagnostics$residuals$doornikhansen
expect_identical(x$output$residuals$doornikhansen, dh)
# Public JD+ 3.9.0 RSA0 residuals.doornikhansen on the original AirPassengers series.
expect_identical(dh$value, 0x1.f150b9ee08657p+2)
expect_identical(dh$pvalue, 0x1.50900d01a97e0p-6)
expect_identical(dh$df, 2)

metric <- persephone3:::diagnostic_metrics(x)$normality
expect_identical(metric$kind, "test")
expect_identical(metric$source, "residuals/doornikhansen")
expect_identical(metric$statistic, dh$value)
expect_identical(metric$pvalue, dh$pvalue)
expect_identical(metric$availability, "available")
expect_identical(metric$status, "Uncertain")
expect_identical(persephone3:::diagnostic_dashboard_values(x)$p_norm, metric$pvalue)
expect_identical(persephone3:::diagnostic_dashboard_values(x)$st_norm, metric$status)
expect_identical(x$diagnosticsTable()$normality, metric$status)
expect_identical(x$generateQrTable()[["Doornik.Hansen.p.value"]], dh$pvalue)

short <- perTramo(stats::ts(as.numeric(AirPassengers)[1:37], frequency = 12), "rsa0", backend = "native")
short$run()
short_dh <- persephone3:::diagnostic_metrics(short)$normality
expect_identical(short_dh$pvalue, 0x1.59cfc26963f40p-3)
expect_identical(short$diagnosticsTable()$normality, "Good")
expect_identical(short$generateQrTable()[["Doornik.Hansen.p.value"]], short_dh$pvalue)
expect_identical(short$diagnosticsTable()$independence, "Undefined")

if (requireNamespace("rjd3tramoseats", quietly = TRUE)) {
  jd <- perTramo(AirPassengers, "rsa0", backend = "rjd3")
  jd$run()
  ref <- jd$output$user_defined$residuals.doornikhansen
  expect_identical(ref$value, metric$statistic)
  expect_identical(ref$pvalue, metric$pvalue)
  expect_identical(jd$diagnosticsTable()$normality, x$diagnosticsTable()$normality)
  expect_identical(jd$generateQrTable()[["Doornik.Hansen.p.value"]], ref$pvalue)
}
