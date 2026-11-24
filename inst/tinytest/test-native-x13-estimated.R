# Capture domain: bridge (PORTABLE). The live-JVM parity block below compares
# against rjd3 fits whose native counterpart is pinned by the x13-estimated
# JVM captures, qualified on macOS ARM64 with HotSpot _dexp/_dlog intrinsics
# disabled (kalos/docs/contracts/tramo-seats-bridge-contract.md; evidence
# tmp_opencode/m6-c-verify/rebaseline-ledger-2026-11-24.md, scratch). On x86-64
# it runs in the bridge pass (see R/helpers.R); the guard defers it with a
# documented message or refuses a mismatched single pass. On aarch64 the
# domains coincide.
helper <- new.env(parent = environment())
sys.source(system.file("tinytest/helpers/fixtures.R", package = "rkalos", mustWork = TRUE), helper)
directory <- system.file("tinytest/fixtures/native", package = "rkalos", mustWork = TRUE)
cases <- helper$fixture_named_rsa0_cases(file.path(directory, "x13-estimated-templates/templates-x86_64.txt"))
diagnostics <- helper$fixture_named_rsa0_cases(file.path(directory, "x13-estimated-templates/diagnostics-x86_64.txt"))
si_panel <- c(helper$fixture_named_rsa0_cases(file.path(directory, "combined-seasonality/stock-x86_64.txt")),
              helper$fixture_named_rsa0_cases(file.path(directory, "combined-seasonality/stock-rsa2c-x86_64.txt")))
tokens <- function(line) strsplit(line, " ", fixed = TRUE)[[1L]]
near <- function(actual, reference) all(abs(as.numeric(actual) - as.numeric(reference)) <= 1e-8 * pmax(1, abs(as.numeric(reference))))
domain <- function(x) c(start(x), end(x), frequency(x))
for (header in names(cases)) {
  parts <- tokens(header)
  rows <- readLines(file.path(directory, "x13-refusal-jvm-profiles", paste0(parts[2L], ".csv")))
  series <- stats::ts(helper$fixture_doubles(rows[-1L]), frequency = 12, start = as.integer(strsplit(rows[1L], ",")[[1L]]))
  lines <- cases[[header]]
  field <- function(key) tokens(lines[startsWith(lines, paste0(key, " "))])
  obj <- perX13(series, parts[1L], backend = "native")
  obj$run()
  result <- obj$result
  expect_true(inherits(result, "persephone_native_estimated_fit"), info = header)
  expect_identical(result$status$code, "success", info = header)
  expect_identical(result$provenance$decode_template, parts[1L], info = header)
  expect_identical(result$domain$frequency, frequency(series), info = header)
  expect_identical(result$model$log, field("model")[2L] == "true", info = header)
  expect_identical(obj$fixModel(), obj$params, info = header)
  expect_identical(result$likelihood, obj$output$likelihood, info = header)
  res <- field("residuals")
  tsres <- result$diagnostics$residuals$tsres
  expect_identical(as.integer(start(tsres)), as.integer(res[2:3]), info = header)
  expect_identical(as.numeric(tsres), helper$fixture_hex_doubles(res[-seq_len(4L)]), info = header)
  tables <- list(final_sa = result$components$sa, final_trend = result$components$t, final_irregular = result$components$i,
                 y_f = obj$forecasts$points$y, forecast_sa = obj$forecasts$points$sa, forecast_trend = obj$forecasts$points$t)
  for (key in names(tables)) {
    row <- field(key)
    actual <- tables[[key]]
    expect_identical(as.integer(start(actual)), as.integer(row[2:3]), info = paste(header, key))
    expect_identical(length(actual), as.integer(row[4L]), info = paste(header, key))
    expect_true(near(actual, helper$fixture_hex_doubles(row[-seq_len(4L)])), info = paste(header, key))
  }
  expect_identical(obj$forecasts$availability$points[1L], "available", info = header)
  expect_identical(obj$forecasts$availability$standard_errors[1L], "available", info = header)
  expect_true(all(obj$forecasts$availability$standard_errors[-1L] == "unavailable"), info = header)
  diag <- diagnostics[[header]]
  item <- function(name) tokens(diag[startsWith(diag, paste0(name, " "))])
  ref <- item("y_ef_12")
  expected_errors <- helper$fixture_hex_doubles(ref[-seq_len(4L)])
  errors <- obj$forecasts$standard_errors$y
  expect_identical(domain(errors), domain(obj$forecasts$points$y), info = header)
  expect_true(all(abs(as.numeric(errors) - expected_errors) <= 2e-8 * (1 + abs(expected_errors))), info = header)
  stats <- persephone3:::diagnostic_metrics(obj)
  expect_identical(result$diagnostics$residuals$ser, helper$fixture_hex_doubles(item("residuals.ser")[2L]), info = header)
  for (name in c("independence", "normality")) {
    key <- if (name == "independence") "lb" else "doornikhansen"
    observed <- stats[[name]]
    ref <- helper$fixture_hex_doubles(item(paste0("residuals.", key))[2:3])
    expect_identical(c(observed$statistic, observed$pvalue), ref, info = paste(header, name))
    expect_identical(obj$diagnosticsTable()[[name]], observed$status, info = paste(header, name))
  }
  variables <- lapply(lines[startsWith(lines, "variable ")], tokens)
  labels <- unname(vapply(variables, `[[`, character(1), 2L))
  if (any(labels %in% c("AO", "LS", "TC"))) {
    expect_error(obj$fixOutlier(timespan = 0L), "estimated_x13_fit.templates", info = header)
  } else {
    expect_identical(obj$fixOutlier(timespan = 0L), obj, info = header)
  }
  si_id <- paste("x13", parts[1L], parts[2L], sep = "_")
  si_case <- si_panel[[which(startsWith(names(si_panel), paste0(si_id, " ")))]]
  verdict <- sub("^summary ", "", si_case[startsWith(si_case, "summary ")])
  qr <- obj$generateQrTable()
  expect_identical(result$diagnostics$combined_seasonality$summary, verdict, info = header)
  expect_identical(obj$diagnosticsTable()$seasonality, verdict, info = header)
  expect_identical(qr$CombinedTest_SI, verdict, info = header)
  expect_identical(qr$Ljung.Box.p.value, stats$independence$pvalue, info = header)
  expect_identical(qr$Doornik.Hansen.p.value, stats$normality$pvalue, info = header)
  expect_identical(persephone3:::diagnostic_dashboard_values(obj)$seasonality, verdict, info = header)
  expect_equal(qr$NbTD, sum(labels == "td"), info = header)
  expect_identical(qr$LeapYear, "lp" %in% labels, info = header)
  expect_identical(qr$MovingHoliday, "easter" %in% labels, info = header)
  expect_identical(qr$Noutliers, sum(labels %in% c("AO", "LS", "TC")), info = header)
  if (length(variables)) {
    regs <- obj$getRegressors()
    coefs <- helper$fixture_hex_doubles(tokens(lines[grepl("^coefficients( |$)", lines)])[-1L])
    expect_true(near(regs$value, coefs), info = header)
    expect_identical(regs$type, ifelse(labels == "AO", "outlier", toupper(labels)), info = header)
  }
  if (requireNamespace("rjd3x13", quietly = TRUE) &&
      persephone3:::oracle_domain_allows("bridge", "test-native-x13-estimated.R")) {
    jd <- perX13(series, parts[1L], backend = "rjd3", userdefined = c("cal_f(12)", "y_f(12)", "y_ef(12)"))
    jd$run()
    for (name in c("y", "sa", "t", "s", "i", "cal")) {
      expected <- jd$result$components[[name]]
      expect_identical(domain(result$components[[name]]), domain(expected), info = paste(header, name))
      expect_true(near(result$components[[name]], expected), info = paste(header, name))
    }
    for (name in c("y", "sa", "t", "s", "cal")) {
      expected <- if (name == "cal") jd$output$user_defined[["cal_f(12)"]] else jd$result$forecasts$points[[name]]
      expect_identical(domain(result$forecasts$points[[name]]), domain(expected), info = paste(header, name, "forecast"))
      expect_true(near(result$forecasts$points[[name]], expected), info = paste(header, name, "forecast"))
    }
    expect_identical(jd$output$user_defined[["diagnostics.seas-si-combined"]], verdict, info = header)
    expected_errors <- jd$output$user_defined[["y_ef(12)"]]
    expect_identical(domain(errors), domain(expected_errors), info = header)
    expect_true(all(abs(as.numeric(errors) - as.numeric(expected_errors)) <= 2e-8 * (1 + abs(as.numeric(expected_errors)))), info = header)
    expected <- jd$generateQrTable()
    for (key in c("Doornik.Hansen.p.value", "Ljung.Box.p.value"))
      expect_equal(qr[[key]], expected[[key]], tolerance = 1e-12, info = paste(header, key))
    for (key in c("NbTD", "LeapYear", "MovingHoliday", "Noutliers"))
      expect_equal(qr[[key]], expected[[key]], info = paste(header, key))
  }
}
obj <- perX13(AirPassengers, "rsa1", backend = "native")
expect_error(obj$updateParams(forecast = 3L), "twelve forecasts")
expect_error(obj$updateParams(log = FALSE), "estimated_x13_fit.templates")
expect_error(perX13(UKgas, "rsa0", backend = "native"), "monthly_only")
