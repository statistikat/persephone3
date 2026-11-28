# M15 (rkalos 0.1.0.9035, core 1e2e3c8): the published X-13 irregular-component
# forecast i_f, consumed from the native envelopes. The reference backend shows
# i_f to users as the user_defined series "i_f" — the constant placeholder the
# JVM X13Extractor manufactures on the finals trend-forecast (D12a) domain —
# which the forecast contract serves as points$i (live-verified 2026-11-28,
# tmp_opencode/m15-p3if/m15-p3if-feature-rjd3-ref.log: rjd3 rsa3 publishes
# user_defined$i_f as 12 points at 1961-1962, every word 3ff0000000000000 =
# 1.0; the additive rsa0 counterpart answers +0.0). The native route carries
# the same product as the envelope pair "irregular_forecast" under the core
# entry's own member names (status, values), reachable verbatim on the fit
# envelope (x$output) and the result's raw echo (result$raw); the wrapper
# product slot points$i stays honestly unprovided on the native route (the
# consumption is a separate future landing — the 9035 core ledger: "consumers
# read it by name and nothing existing changes"; the reference-lane
# availability shape is i points "available" / field i_f "available"). The
# ENGINE's claim against
# the JVM lives in the core: the thirteen-case double-JVM panel with BIT
# claims for all 144 published words (rkalos ledger, 9035 entry). The words
# pinned below are the core entry's output RECORDED LIVE from this consumer
# lane (serial lane, Java-free); no numerics are hand-set.
hexof <- function(x) vapply(as.numeric(x), function(d)
  paste(rev(as.character(writeBin(d, raw()))), collapse = ""), character(1))
hex_doubles <- function(h) vapply(h, function(s)
  readBin(as.raw(rev(strtoi(substring(s, seq(1L, 16L, 2L),
                  seq(2L, 16L, 2L)), 16L))), "double"), numeric(1))
shape <- c("status", "values")
ONE <- "3ff0000000000000"   # the reference's multiplicative-domain constant
ZERO <- "0000000000000000"  # +0.0 byte pattern (a negative zero reads 8000...)

# --- Automatic lane: default perX13(rsa3) carries the pair end to end -------
auto <- perX13(AirPassengers, "rsa3", backend = "native")
auto$run()
fa <- auto$output$irregular_forecast
expect_true(!is.null(fa), info = "automatic pair present")
expect_identical(names(fa), shape, info = "pair members by name (status, values)")
expect_identical(fa$status, "available", info = "status verbatim")
recorded_auto <- rep(ONE, 12L)
expect_identical(hexof(fa$values), recorded_auto,
                 info = "automatic words are the core entry, live-recorded")
expect_identical(as.numeric(fa$values), unname(hex_doubles(recorded_auto)),
                 info = "automatic i_f values bit-identical to the recording")
d12a <- auto$output$finals$tables$d12a
expect_identical(length(fa$values), length(d12a),
                 info = "count equals the D12a-domain count")
expect_identical(length(fa$values), 12L)
expect_identical(as.numeric(stats::start(d12a)), c(1961, 1),
                 info = "the pair lands on the D12a domain")
expect_identical(auto$result$raw$irregular_forecast, fa,
                 info = "result$raw carries the pair verbatim")
expect_true(auto$result$model$log, info = "rsa3 AirPassengers is a log fit")
expect_identical(auto$result$provenance$mode_effective, "multiplicative")
expect_identical(as.numeric(fa$values), rep(1, 12L),
                 info = "the log fit answers the constant 1.0 verbatim (no back-transform)")
expect_identical(unserialize(serialize(auto$result, NULL)), auto$result)

# --- Estimated-template lane: the additive answer, sign pinned ---------------
est <- perX13(AirPassengers, "rsa0", backend = "native")
est$run()
fe <- est$output$irregular_forecast
expect_true(!is.null(fe), info = "estimated pair present")
expect_identical(names(fe), shape)
expect_identical(fe$status, "available")
expect_identical(est$result$provenance$mode_effective, "additive")
recorded_rsa0 <- rep(ZERO, 12L)
expect_identical(hexof(fe$values), recorded_rsa0,
                 info = "estimated words are the core entry, live-recorded (+0.0 sign)")
expect_identical(as.numeric(fe$values), unname(hex_doubles(recorded_rsa0)))

# --- The wrapper product arm, honestly recorded (retired narrative fixed) ---
# The i_f-unavailable pins retired with the 9035 landing asserted a MISSING
# CORE SURFACE. That premise is gone: the core publishes the pair and it lands
# on both envelopes (pinned above). What remains unprovided is the WRAPPER
# product slot — the native route has not yet wired points$i (the reference
# parity slot served by user_defined$i_f on the rjd3 backend, header read);
# this is a consumer gap, recorded honestly, never fabricated:
expect_null(auto$forecasts$points$i, info = "wrapper product slot stays unprovided")
avail <- stats::setNames(auto$result$availability$state, auto$result$availability$field)
expect_identical(avail[["i_f"]], "unavailable",
                 info = "the product availability string tracks the product slot")

# --- Constant semantics by mode, recorded live on the automatic lane --------
rsa3_direct <- function(...) {
  a <- list(series = stats::ts(as.numeric(AirPassengers), start = c(1949, 1),
    frequency = 12),
    transform = list(aicc = -2),
    automodel = list(max_d = 2L, max_bd = 1L, ub1 = 1 / 1.0309278350515465,
      ub2 = 1 / 1.1363636363636365, cancel = 0.1, precision = 1e-7,
      models_count = 5L, max_p = 2L, max_q = 2L, max_bp = 1L, max_bq = 1L,
      balanced = FALSE, mixed = TRUE, accept_white_noise = FALSE, eps = 1e-7),
    outliers = list(used = TRUE, types = c("AO", "LS", "TC"), span = NULL,
      max_round = 30L, max_outliers = 30L, tcrate = 0.7),
    ami = list(precision = 1e-7, mean_precision = 1e-5, va = 0,
      reduce_va = 0.14286, ljung_box_limit = 0.95, check_mu = TRUE,
      accept_airline = FALSE, ur_limit = 0.95),
    regression = list(mean_test_initial = 1.96, mean_test_final = 1, tsig = 1),
    sigma = "legacy", lower_sigma = 1.5, upper_sigma = 2.5,
    msr_policy = "jdplus", mode = "additive", backcast = 0L, forecast = -1L,
    frequency = 12L, start_period = 1L, first_year = 1949L)
  ov <- list(...); a[names(ov)] <- NULL
  do.call(rkalos::native_x13_fit_automatic, c(a, ov))
}
# The JVM DecompositionMode predicate (Multiplicative OR LogAdditive answers
# 1.0; Additive and PseudoAdditive answer +0.0) — unlike the X11 context's
# isMultiplicative(), which counts PseudoAdditive; measured to separate at
# exactly this boundary (rkalos 9035 golds, cross-verified live here).
loga <- rsa3_direct(mode = "log_additive")
expect_identical(loga$mode_effective, "log_additive")
expect_identical(hexof(loga$irregular_forecast$values), rep(ONE, 12L),
                 info = "log_additive answers 1.0 (the JVM predicate counts LogAdditive)")
padd <- rsa3_direct(mode = "pseudo_additive")
expect_identical(padd$mode_effective, "pseudo_additive")
expect_identical(hexof(padd$irregular_forecast$values), rep(ZERO, 12L),
                 info = "pseudo_additive answers +0.0 (the fit lands; only i_f refuses the mode)")
level <- rsa3_direct(transform = list(aicc = 1e9))
expect_false(level$model$log)
expect_identical(level$mode_effective, "additive")
expect_identical(hexof(level$irregular_forecast$values), rep(ZERO, 12L),
                 info = "the level arm answers +0.0 on the automatic lane")

# --- The zero-horizon arm passes through verbatim ----------------------------
zero <- rsa3_direct(forecast = 0L)
expect_identical(zero$status, "available")
expect_identical(zero$irregular_forecast$status, "no_requested_predictions")
expect_identical(zero$irregular_forecast$values, numeric())
# ... and stays verbatim through the wrapper (the product contract honestly
# carries no forecasts for a zero horizon; the pair answer is the engine's).
zw <- perX13(AirPassengers, "rsa3", backend = "native")
zw$run()
zw$updateParams(forecast = 0L)
zw$run()
expect_identical(zw$output$irregular_forecast$status, "no_requested_predictions")
expect_identical(zw$output$irregular_forecast$values, numeric())
expect_null(zw$forecasts, info = "zero horizon keeps the honest empty product")

# --- Refusal absence: a documented non-available status carries no pair ------
refusal <- rsa3_direct(calendar = TRUE)
expect_identical(refusal$status,
  "unsupported:x13 automatic fit defers calendar/easter/TD/user detection (21-I)")
expect_null(refusal$irregular_forecast, info = "absent on refusal")
expect_error(perX13(AirPassengers, "rsa4", backend = "native")$run(), "21-I")

# --- Cross-backend note vs the reference constants (static, bit-tier) --------
# The reference publishes its i_f as a CONSTANT (the extractor manufactures
# 1.0/0.0 — no transcendental math), so cross-backend fidelity is decidable
# word-exactly without a JVM call in this pass: the native answers above are
# the same double words as the constants the live rjd3 read served (log read
# quoted in the header; rsa3 12/12 words 3ff0000000000000, rsa0 +0.0).
expect_identical(hexof(fa$values), rep(hexof(1), 12L),
                 info = "same word as the reference's constant 1.0")
expect_identical(hexof(fe$values), rep(hexof(0), 12L),
                 info = "same word as the reference's constant +0.0")
