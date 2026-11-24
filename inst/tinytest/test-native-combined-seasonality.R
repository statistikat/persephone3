helper <- new.env(parent = environment())
sys.source(system.file("tinytest/helpers/fixtures.R", package = "rkalos", mustWork = TRUE), helper)
directory <- system.file("tinytest/fixtures/native", package = "rkalos", mustWork = TRUE)
panel <- function(path, name) helper$fixture_named_rsa0_cases(file.path(directory, path, paste0(name, "-x86_64.txt")))
inputs <- c(panel("tramo-seats-bridge", "rsa3"), panel("tramo-seats-bridge", "rsa3-ref-retry"))
cases <- c(panel("combined-seasonality", "public"), panel("combined-seasonality", "fixed-x13"))
count <- 0L
for (header in names(cases)) {
  fields <- strsplit(header, " ", fixed = TRUE)[[1L]]
  id <- fields[1L]
  kind <- fields[2L]
  if (!startsWith(kind, "wk_") && kind != "fixed_d8") next
  lines <- inputs[[substring(id, regexpr("fit_", id, fixed = TRUE))]]
  input <- strsplit(lines[startsWith(lines, "input ")], " ", fixed = TRUE)[[1L]]
  series <- stats::ts(helper$fixture_hex_doubles(input[-seq_len(4L)]), frequency = 12, start = as.integer(input[2:3]))
  if (kind == "fixed_d8") {
    public_mode <- strsplit(id, "_", fixed = TRUE)[[1L]][2L]
    mode <- c(Additive = "additive", Multiplicative = "multiplicative", LogAdditive = "log_additive",
              PseudoAdditive = "pseudo_additive")[[public_mode]]
    obj <- perX13(series, "restricted", backend = "native",
                  speclist = list(log = fields[3L] == "true", seasonal = rep("s3x5", 12L), trend_length = 13L, mode = mode))
  } else obj <- perTramo(series, sub("_.*", "", id), backend = "native")
  obj$run()
  lines <- cases[[header]]
  expected <- sub("^summary ", "", lines[startsWith(lines, "summary ")])
  metric <- persephone3:::diagnostic_metrics(obj)$seasonality
  expect_identical(obj$result$diagnostics$combined_seasonality$summary, expected, info = id)
  expect_identical(metric$value, expected, info = id)
  expect_identical(metric$source, "diagnostics/combined_seasonality/summary", info = id)
  expect_identical(metric$availability, "available", info = id)
  expect_identical(obj$diagnosticsTable()$seasonality, expected, info = id)
  expect_identical(obj$generateQrTable()$CombinedTest_SI, expected, info = id)
  expect_identical(persephone3:::diagnostic_dashboard_values(obj)$seasonality, expected, info = id)
  if (startsWith(kind, "wk_") && requireNamespace("rjd3tramoseats", quietly = TRUE)) {
    jd <- perTramo(series, sub("_.*", "", id), backend = "rjd3")
    jd$run()
    expect_identical(jd$output$user_defined[["diagnostics.seas-si-combined"]], expected, info = id)
    expect_identical(jd$diagnosticsTable()$seasonality, metric$value, info = id)
  }
  count <- count + 1L
}
expect_identical(count, 24L)
short <- perTramo(stats::ts(as.numeric(AirPassengers)[1:35], frequency = 12), "rsa0", backend = "native")
short$run()
expect_identical(persephone3:::diagnostic_metrics(short)$seasonality$availability, "missing")
expect_identical(short$diagnosticsTable()$seasonality, NA_character_)

# The product's RSA3 decoder fixes its preprocessing settings and accepts
# explicit forecast counts. Compare both observed-domain verdicts to JD+.
for (id in c("air", "ao", "seas")) {
  rows <- readLines(file.path(directory, "x13-refusal-jvm-profiles", paste0(id, ".csv")))
  series <- stats::ts(helper$fixture_doubles(rows[-1L]),
                      start = as.integer(strsplit(rows[1L], ",", fixed = TRUE)[[1L]]), frequency = 12)
  for (horizon in c(0L, 12L)) {
    obj <- perX13(series, "rsa3", backend = "native", speclist = list(forecast = horizon))
    obj$run()
    expect_identical(obj$result$diagnostics$combined_seasonality$summary, "Present", info = id)
    expect_identical(obj$diagnosticsTable()$seasonality, "Present", info = id)
    expect_identical(obj$generateQrTable()$CombinedTest_SI, "Present", info = id)
    expect_identical(persephone3:::diagnostic_dashboard_values(obj)$seasonality, "Present", info = id)
    if (requireNamespace("rjd3x13", quietly = TRUE)) {
      jd <- perX13(series, "rsa3", backend = "rjd3", speclist = list(x11.fcasts = horizon))
      jd$run()
      expect_identical(jd$output$user_defined[["diagnostics.seas-si-combined"]],
                       obj$result$diagnostics$combined_seasonality$summary, info = id)
    }
  }
}
