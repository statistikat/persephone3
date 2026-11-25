# Product consumption of the rkalos residual + residual trading-day F-test
# surfaces (M7 cutover gates, item 27). rkalos >= 0.1.0.9028 attaches a dated
# preprocessing residual bundle to the automatic fit
# (capabilities()$automatic_fit$residuals) and a td.ftest.sa / td.ftest.i pair
# to all four product fit routes via native_residual_trading_days_tests
# (capabilities()$td_tests), FAIL-SOFT: a degenerate zero-composition fit
# (the quick/noout pseudo_additive D8 requests documented in the rkalos
# ledger) leaves the td fields absent. This file asserts what the product
# does with both surfaces: the envelope carries them, the accessors, quality
# report, diagnostics table and dashboard serve them, absent means Undefined
# and nothing is fabricated, and where the native fit lacks residuals the
# honest plotResiduals() refusal stays.
#
# Capture domain: default. The live-JVM block below compares in-session
# native fits against in-session backend = "rjd3" fits on THIS host's
# default-intrinsics JVM. The inputs are tolerance-qualified product
# components (the component-level claims are pinned elsewhere), so every
# CROSS-BACKEND scalar check below is TOLERANCE parity at the scaled
# 2e-8 * (1 + |ref|) convention -- deliberately NOT a bit claim, unlike the
# bridge-domain fixture comparisons (test-native-x13-estimated.R et al.). The
# definitional bridge checks are a different layer: they compare a live field
# with the pinned machine applied to that SAME live fit's own published
# series, so no cross-implementation composition is involved and they are
# pinned at rel 1e-12 (X-11) / 1e-10 (TRAMO-SEATS). No stored JVM reference is
# consumed here, so the file is not bridge-guarded; the helpers.R
# oracle_domain_allows("default", ...) guard defers the block out of the
# bridge pass and fails a mismatched single pass, matching the test-parity.R
# demand-dispatch convention. On aarch64 the domains coincide and the guard is
# inert.
#
# Definition note (root-caused 2026-11-24; dossier
# tmp_opencode/m7-c-persephone/td-ftest-classification-2026-11-24.md). Both
# sides run the SAME machine -- TradingDaysTest.maTest(data, false): the six
# GenericTradingDays TD7 contrast columns without a mean column over an
# ARIMA(0,1,1)[12], the pinned JointTest F(6, n-7), capture option flast = 0
# (so -all and -last share one fit). That machine is bit-exact against its
# 24-case/192-double dashboard oracle (rkalos
# fixtures/native/residual-trading-days/public-x86_64.txt; replayed in the
# rkalos suite and independently here: every word bit-identical). What
# differs is the INPUT SERIES the machine is handed:
#   * rjd3 diagnostics.td-sa-all/-last and td-i-all/-last are extracted by
#     GenericSaTestsExtractor from GenericSaTests.residualTradingDaysTests(),
#     which reads the lsa/lirr fields -- the SA and irregularity ON THE
#     MODEL'S WORKING (ESTIMATION) SCALE: X13Diagnostics.of() wires
#     TsData.fitToDomain(X11Results.getD11()/getD13(), x11.getActualDomain())
#     and TramoSeatsDiagnostics.of() wires SeatsResults.getInitialComponents()
#     .getSeries(SeasonallyAdjusted|Irregular, Value), neither of them
#     back-transformed (verified against the pinned jdplus 3.9.0 bytecode);
#   * the rkalos attach (.attach_td_tests, by design -- see its header
#     comment) computes the test from the fit's OWN decorated finalSA /
#     finalIrregular on their published observed domain, i.e. on the REPORTED
#     scale.
# Because maTest estimates a RegSarima model internally, its F statistic is
# invariant to a linear rescaling but NOT to a nonlinear change of scale. So
# the two definitions coincide whenever the model is estimated on the
# reported scale -- measured here at rel 3.9e-09 (x13 rsa0) and rel 0.0e+00 /
# 1.3e-13 (tramo rsa0) -- and differ for every log-transformed template:
# rsa3 by 21.9% (x11.fcasts = 0; 29.8% plain) and tramo rsafull by 57.5%.
# That is a definitional difference in the input representation, not a
# formula or test difference, and this file pins all three layers of it: the
# attach's own definition bit-for-bit (ungated), the reference field's input
# convention through an exact bridge on the live fit's own published series
# (rel 1e-10), and the tolerance parity of the non-transformed fits
# cross-backend. The divergence itself is pinned too, so neither convention
# can drift silently. Note that R/diagnostic_rules.R maps
# "diagnostics.td-sa-all" and "diagnostics/td.ftest.sa" onto one metric, so
# for log-transformed templates the metric value is backend-dependent by
# definition; reference parity for those routes is a follow-up item (the
# native envelope already carries the pieces: output$preadjustment$log and
# the working-scale output$initial block).
rel <- function(actual, reference) abs(actual / reference - 1)
near <- function(actual, reference) {
  ok <- is.finite(actual) && is.finite(reference) &&
    abs(actual - reference) <= 2e-8 * (1 + abs(reference))
  isTRUE(ok)
}

# --- envelope flow: every route's diagnostics block carries td.ftest.sa/i ---
auto <- perX13(AirPassengers, "rsa3", backend = "native")
auto$run()
expect_identical(auto$result$diagnostics[c("td.ftest.sa", "td.ftest.i")],
                 auto$output$diagnostics[c("td.ftest.sa", "td.ftest.i")])
stock <- perX13(AirPassengers, "rsa0", backend = "native")
stock$run()
expect_identical(stock$result$diagnostics[c("td.ftest.sa", "td.ftest.i")],
                 stock$output$diagnostics[c("td.ftest.sa", "td.ftest.i")])
tramo <- perTramo(AirPassengers, "rsafull", backend = "native")
tramo$run()
expect_identical(tramo$result$diagnostics[c("td.ftest.sa", "td.ftest.i")],
                 tramo$output$diagnostics[c("td.ftest.sa", "td.ftest.i")])
# the tramo envelope builder is a pass-through, not a field whitelist: the
# raw diagnostics arrive untouched and the residual bundle keeps its slot.
expect_identical(tramo$result$diagnostics$residuals, tramo$output$residuals)

# --- the attach's own definition, pinned bit-for-bit on the native side ------
# The product's td scalars are exactly the primitive applied to the delivered
# decorated finalSA / finalIrregular on their published domain (the rkalos
# convention, quoted in the header): no JVM, no tolerance, no restatement.
for (obj in list(auto, stock, tramo)) {
  sa <- obj$result$components$sa
  ir <- obj$result$components$i
  ref <- rkalos::native_residual_trading_days_tests(sa, ir,
          as.integer(stats::start(sa)[1L]), as.integer(stats::start(sa)[2L]))
  expect_identical(obj$output$diagnostics$td.ftest.sa, ref[["td-sa-all"]])
  expect_identical(obj$output$diagnostics$td.ftest.i, ref[["td-i-all"]])
}

# --- metric resolution and presentation for the published td fields -------
for (obj in list(auto, stock, tramo)) {
  m <- persephone3:::diagnostic_metrics(obj)
  expect_identical(m$td_sa_f$source, "diagnostics/td.ftest.sa")
  expect_identical(m$td_sa_all$source, "diagnostics/td.ftest.sa")
  expect_identical(m$td_i_all$source, "diagnostics/td.ftest.i")
  expect_identical(m$td_sa_f$availability, "available")
  for (key in c("td_sa_all", "td_sa_f", "td_i_all")) {
    expect_true(is.finite(m[[key]]$statistic) && is.finite(m[[key]]$pvalue))
    expect_true(m[[key]]$status %in% c("Severe", "Bad", "Uncertain", "Good"))
  }
  # the -last variants stay honestly missing (the primitive publishes the
  # shared -all/-last value under the -all keys only).
  expect_identical(m$td_sa_last$availability, "missing")
  expect_identical(m$td_i_last$availability, "missing")
  tbl <- obj$diagnosticsTable()
  expect_true(all(tbl[c("fTestSA_td", "combSA_all", "combI_all")] %in%
                    c("Severe", "Bad", "Uncertain", "Good")))
  expect_identical(tbl$combSA_last, "Undefined")
  expect_identical(tbl$combI_last, "Undefined")
  expect_identical(tbl$fTestSA_td, m$td_sa_f$status)
  qr <- obj$generateQrTable()
  expect_true(qr[["Residual.TD.Effect"]] %in% c("Yes", "No"))
  expect_identical(qr[["Residual.TD.Effect"]], if (m$td_sa_f$pvalue < 0.05) "Yes" else "No")
  vals <- persephone3:::diagnostic_dashboard_values(obj)
  expect_identical(vals$p_residual_td, m$td_sa_f$pvalue)
  expect_identical(vals$st_residual_td, m$td_sa_f$status)
}

# --- residual ungate: the automatic envelope carries the bundle ------------
r <- auto$result$diagnostics$residuals
expect_identical(r, auto$output$residuals) # same field as the estimated route
expect_true(stats::is.ts(r$tsres))
expect_identical(as.integer(c(stats::start(r$tsres), stats::end(r$tsres),
                              stats::frequency(r$tsres))),
                 c(1950L, 2L, 1960L, 12L, 12L))
expect_identical(length(r$tsres), 131L)
expect_true(is.finite(r$ser) && r$ser > 0)
expect_identical(persephone3:::fitted_residuals(auto), r)
dy <- auto$plotResiduals(which = "res")
expect_true(inherits(dy, "dygraphs"))
expect_identical(as.numeric(dy$x$data[[2L]]), as.numeric(r$tsres))
for (key in c("acf", "acf2", "pacf")) {
  chart <- auto$plotResiduals(which = key, interactive = FALSE)$data
  corr <- switch(key, acf = stats::acf(r$tsres, plot = FALSE),
                 acf2 = stats::acf(r$tsres^2, plot = FALSE),
                 pacf = stats::pacf(r$tsres, plot = FALSE))
  index <- if (key == "pacf") seq_along(corr$lag) else seq_along(corr$lag)[-1]
  expect_identical(as.numeric(chart$acf), as.numeric(corr$acf)[index])
}
expect_identical(auto$plotResiduals(which = "sreshist", interactive = FALSE)$data$x,
                 as.numeric(r$tsres) / r$ser)
qq <- stats::qqnorm(as.numeric(r$tsres) / r$ser, plot.it = FALSE)
plot_qq <- auto$plotResiduals(which = "nqq", interactive = FALSE)$data
expect_identical(plot_qq$x, qq$x)
expect_identical(plot_qq$y, qq$y)
html <- as.character(persephone3:::dashboard_single(auto))
expect_true(grepl("Residual diagnostics", html, fixed = TRUE))
expect_false(grepl("Residual diagnostics are unavailable", html, fixed = TRUE))
expect_false(grepl("Plot error:", html, fixed = TRUE))
# the table normality/independence cells carry the bundle's own tests
m_auto <- persephone3:::diagnostic_metrics(auto)
expect_identical(m_auto$normality$source, "residuals/doornikhansen")
expect_identical(m_auto$independence$source, "residuals/lb")
expect_true(m_auto$normality$status %in% c("Severe", "Bad", "Uncertain", "Good"))
expect_true(m_auto$independence$status %in% c("Severe", "Bad", "Uncertain", "Good"))

# --- honest edges ----------------------------------------------------------
# Fail-soft route: the restricted fit in pseudo_additive mode drives the
# quick/noout pseudo_additive D8 request whose zero-composition outcome
# legitimately lacks the td fields (documented in the rkalos ledger). The
# columns stay Undefined -- no crash, no fabricated value.
deg <- perX13(AirPassengers, "restricted", backend = "native",
              speclist = list(mode = "pseudo_additive", log = FALSE,
                              seasonal = rep("s3x3", 12L), trend_length = 13L))
deg$run()
expect_false("td.ftest.sa" %in% names(deg$output$diagnostics))
expect_false("td.ftest.sa" %in% names(deg$result$diagnostics))
m_deg <- persephone3:::diagnostic_metrics(deg)
expect_identical(m_deg$td_sa_f$availability, "missing")
expect_true(is.na(m_deg$td_sa_f$pvalue) && is.na(m_deg$td_sa_f$statistic))
expect_identical(m_deg$td_sa_f$status, "Undefined")
expect_identical(deg$diagnosticsTable()$fTestSA_td, "Undefined")
expect_identical(deg$generateQrTable()[["Residual.TD.Effect"]], NA_character_)
expect_true(is.na(persephone3:::diagnostic_dashboard_values(deg)$p_residual_td))
# refusal semantics: a native fit that lacks residuals still refuses.
fixed <- perX13(AirPassengers, "restricted", backend = "native",
                speclist = list(seasonal = rep("s3x3", 12L), trend_length = 13L))
fixed$run()
expect_error(fixed$plotResiduals(), "The fit does not supply dated preprocessing residuals")
expect_null(fixed$result$diagnostics$residuals)

# --- live-JVM tolerance parity (default capture domain) --------------------
if (requireNamespace("rjd3x13", quietly = TRUE) &&
    persephone3:::oracle_domain_allows("default", "test-native-td-parity.R")) {
  # rsa0: the estimated-route TD tests and the dated residual scalars match the
  # live JVM within the scaled tolerance. This is the non-transformed case: the
  # attach's reported-scale definition and the reference field's working-scale
  # definition are the same representation here (measured rel 3.9e-09), so the
  # parity is numerical, not just classificatory.
  jd <- perX13(AirPassengers, "rsa0", backend = "rjd3")
  jd$run()
  mj <- persephone3:::diagnostic_metrics(jd)
  mn <- persephone3:::diagnostic_metrics(stock)
  for (key in c("td_sa_all", "td_sa_f", "td_i_all")) {
    expect_true(near(mn[[key]]$statistic, mj[[key]]$statistic),
                info = paste0("rsa0 ", key, " statistic"))
    expect_true(near(mn[[key]]$pvalue, mj[[key]]$pvalue),
                info = paste0("rsa0 ", key, " pvalue"))
  }
  expect_identical(stock$diagnosticsTable()$fTestSA_td, jd$diagnosticsTable()$fTestSA_td)
  expect_identical(stock$diagnosticsTable()$combSA_all, jd$diagnosticsTable()$combSA_all)
  expect_identical(stock$diagnosticsTable()$combI_all, jd$diagnosticsTable()$combI_all)
  expect_identical(stock$generateQrTable()[["Residual.TD.Effect"]],
                   jd$generateQrTable()[["Residual.TD.Effect"]])
  # rsa0 residual bundle vs the live preprocessing diagnostics
  ud <- jd$output$user_defined
  expect_identical(c(stats::start(stock$result$diagnostics$residuals$tsres),
                     stats::frequency(stock$result$diagnostics$residuals$tsres)),
                   c(stats::start(ud$residuals.tsres), stats::frequency(ud$residuals.tsres)))
  expect_true(near(stock$result$diagnostics$residuals$ser, ud$residuals.ser))
  for (pair in list(c("lb", "residuals.lb"), c("doornikhansen", "residuals.doornikhansen"))) {
    expect_true(near(stock$result$diagnostics$residuals[[pair[1]]]$value,
                     ud[[pair[2]]]$value), info = pair[2])
    expect_true(near(stock$result$diagnostics$residuals[[pair[1]]]$pvalue,
                     ud[[pair[2]]]$pvalue), info = pair[2])
  }
  # rsa3: the residual diagnostics scalars the ungate now serves are
  # tolerance parity against the live preprocessing diagnostics.
  jd3 <- perX13(AirPassengers, "rsa3", backend = "rjd3", speclist = list(x11.fcasts = 0L))
  jd3$run()
  ud3 <- jd3$output$user_defined
  expect_true(near(r$ser, ud3$residuals.ser), info = "rsa3 residuals.ser")
  expect_true(near(r$lb$value, ud3$residuals.lb$value), info = "rsa3 lb value")
  expect_true(near(r$lb$pvalue, ud3$residuals.lb$pvalue), info = "rsa3 lb pvalue")
  expect_true(near(r$doornikhansen$value, ud3$residuals.doornikhansen$value),
              info = "rsa3 doornikhansen value")
  expect_true(near(r$doornikhansen$pvalue, ud3$residuals.doornikhansen$pvalue),
              info = "rsa3 doornikhansen pvalue")
  expect_true(all(abs(as.numeric(r$tsres) - as.numeric(ud3$residuals.tsres)) <=
                    2e-8 * (1 + abs(as.numeric(ud3$residuals.tsres)))))
  expect_identical(auto$diagnosticsTable()$normality, jd3$diagnosticsTable()$normality)
  expect_identical(auto$diagnosticsTable()$independence, jd3$diagnosticsTable()$independence)
  # --- the reference field's input convention, asserted (definitional bridge) --
  # X-11, log-transformed template: the live diagnostics.td-* field equals the
  # same primitive applied to the live fit's OWN published components on the
  # model's working scale. Pinned at rel 1e-12 (measured 0.0e+00 for both F
  # values and for the td-sa-all p-value, 8.9e-16 for the td-i-all p-value). The
  # transform is the one the product itself reports: the native rsa3 envelope
  # carries preadjustment$log = TRUE, the native rsa0 envelope FALSE.
  expect_true(isTRUE(auto$output$preadjustment$log),
              info = "rsa3 is estimated on the log scale")
  expect_false(isTRUE(stock$output$preadjustment$log),
              info = "rsa0 is estimated on the reported scale")
  working <- function(x) stats::ts(log(as.numeric(x)), start = stats::start(x),
                                   frequency = stats::frequency(x))
  sa3 <- jd3$result$components$sa
  i3 <- jd3$result$components$i
  bridge <- rkalos::native_residual_trading_days_tests(working(sa3), working(i3),
            as.integer(stats::start(sa3)[1L]), as.integer(stats::start(sa3)[2L]))
  for (nm in c("td-sa-all", "td-i-all")) {
    live <- ud3[[paste0("diagnostics.", nm)]]
    expect_true(rel(live$value, bridge[[nm]]$value) < 1e-12, info = paste(nm, "F bridge"))
    expect_true(rel(live$pvalue, bridge[[nm]]$pvalue) < 1e-12, info = paste(nm, "p bridge"))
  }
  # The other half of the definition: the shipped attach is the levels variant
  # (pinned bit-for-bit above), so on a log-transformed template the two
  # conventions differ materially -- measured 2.19e-01 relative here. Pinning the
  # divergence means a silent change of either convention fails this file.
  expect_true(abs(auto$output$diagnostics$td.ftest.sa$value -
                    ud3[["diagnostics.td-sa-all"]]$value) > 0.05,
              info = "levels-vs-working-scale divergence is still the definition")
  # Both fits classify the TD effect the same way at 0.05, the level the quality
  # report column and the dashboard actually serve.
  expect_identical(auto$generateQrTable()[["Residual.TD.Effect"]],
                   jd3$generateQrTable()[["Residual.TD.Effect"]])
  expect_identical(auto$generateQrTable()[["Residual.TD.Effect"]], "Yes")

  # TRAMO-SEATS: the reference field reads
  # SeatsResults.getInitialComponents() (SA and Irregular, Value), which rjd3
  # publishes on the working scale as output$decomposition$stochastics. Bridge
  # pinned at rel 1e-10 (measured <= 2.8e-14).
  jt <- perTramo(AirPassengers, "rsafull", backend = "rjd3")
  jt$run()
  udj <- jt$output$user_defined
  st <- jt$output$decomposition$stochastics
  bridge_t <- rkalos::native_residual_trading_days_tests(st$sa$data, st$i$data,
               as.integer(stats::start(st$sa$data)[1L]),
               as.integer(stats::start(st$sa$data)[2L]))
  for (nm in c("td-sa-all", "td-i-all")) {
    live <- udj[[paste0("diagnostics.", nm)]]
    expect_true(rel(live$value, bridge_t[[nm]]$value) < 1e-10, info = paste("tramo", nm, "F bridge"))
    expect_true(rel(live$pvalue, bridge_t[[nm]]$pvalue) < 1e-10, info = paste("tramo", nm, "p bridge"))
  }
  # The additive control that keeps the parity claim above non-vacuous: with no
  # transform in force (tramo rsa0) the initial and final published blocks
  # coincide and the native attach reaches tolerance parity with the live field
  # on the same route -- measured rel 0.0e+00 / 1.3e-13 for the F values,
  # 0.0e+00 / 1.1e-12 for the p-values, well inside the 2e-8 * (1 + |ref|) band.
  tv <- perTramo(AirPassengers, "rsa0", backend = "native")
  tv$run()
  tj <- perTramo(AirPassengers, "rsa0", backend = "rjd3")
  tj$run()
  expect_identical(as.character(tv$output$mode), "additive")
  st0 <- tj$output$decomposition$stochastics
  fin0 <- tj$output$final
  expect_identical(max(abs(as.numeric(st0$sa$data) - as.numeric(fin0$sa$data))), 0)
  expect_true(max(abs(as.numeric(st0$i$data) - as.numeric(fin0$i$data))) < 1e-9)
  mt <- persephone3:::diagnostic_metrics(tv)
  mj0 <- persephone3:::diagnostic_metrics(tj)
  for (key in c("td_sa_all", "td_i_all")) {
    expect_true(near(mt[[key]]$statistic, mj0[[key]]$statistic),
                info = paste0("tramo rsa0 ", key, " statistic"))
    expect_true(near(mt[[key]]$pvalue, mj0[[key]]$pvalue),
                info = paste0("tramo rsa0 ", key, " pvalue"))
  }
  expect_identical(tv$generateQrTable()[["Residual.TD.Effect"]],
                   tj$generateQrTable()[["Residual.TD.Effect"]])
}
