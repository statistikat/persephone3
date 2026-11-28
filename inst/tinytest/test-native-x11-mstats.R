# M14 (rkalos 0.1.0.9034, core c3706ea): the X-11 diagnostic battery --
# m1..m11 through the pinned getM clamp, q, qm2 and the two MDiagnostics
# grades -- consumed from the native envelopes. The reference backend shows
# its battery to users as the output component "mstats" (live-verified member
# names m1..m11, q, qm2) feeding the Q/Q2/M-statistic-count diagnostic columns;
# the wrapper serves the native battery under the SAME component name
# (result$mstats, verbatim echo of the envelope field x11_mstats) and feeds the
# same columns from it (R/diagnostic_rules.R, R/diagnosticsTable.R). The
# ENGINE's claim against the JVM is the core's twelve-case double-JVM replay
# (bit for m4 and the grades, tier max(1e-11, 5e-12*|want|), measured worst
# 3.226e-13 at the rkalos boundary -- rkalos ledger, 9034 entry). The words
# pinned below are the core entry's output RECORDED LIVE from this consumer
# lane (serial lane, Java-free), cross-identical to the rkalos 9034 gold
# recordings; no numerics are hand-set.
# Capture domain: bridge (PORTABLE). The live-JVM block at the end compares the
# native battery against a live rjd3 battery on the same call; that comparison
# is qualified on a JVM with HotSpot _dexp/_dlog intrinsics disabled (see
# R/helpers.R). On x86-64 it runs in the bridge pass
# (tools/run-oracle-bridge-pass.R); the guard defers it with a documented
# message in pass A. On aarch64 the domains coincide.
hexof <- function(x) vapply(as.numeric(x), function(d)
  paste(rev(as.character(writeBin(d, raw()))), collapse = ""), character(1))
hex_doubles <- function(h) vapply(h, function(s)
  readBin(as.raw(rev(strtoi(substring(s, seq(1L, 16L, 2L),
                  seq(2L, 16L, 2L)), 16L))), "double"), numeric(1))
fields <- c(paste0("m", 1:11), "q", "qm2")
shape <- c("status", fields, "q_grade", "qm2_grade")

# --- Automatic lane: default perX13(rsa3) carries the battery end to end ----
auto <- perX13(AirPassengers, "rsa3", backend = "native")
auto$run()
b <- auto$output$x11_mstats
expect_true(!is.null(b), info = "automatic battery present")
expect_identical(names(b), shape, info = "battery names (status, m1..m11, q, qm2, grades)")
expect_identical(b$status, "available", info = "battery status verbatim")
recorded_auto <- c(
  "3fa40f77cf735d97", "3fa3c7911fde7d48", "0000000000000000", "3febfdc8d332eebf",
  "3fd106a55fbbe161", "3fe528e3c9a30fcd", "3fc9e73ec5e0b286", "3fdbfbfb4b0e4c0b",
  "3fd691e71be655f5", "3fdd0eeec1efe956", "3fda07b032b9ccf1", "3fd09bd347baae56",
  "3fd293b84cb4da4e")
expect_identical(hexof(b[fields]), recorded_auto,
                 info = "automatic words are the core entry, live-recorded")
expect_true(all(is.finite(as.numeric(b[fields]))), info = "automatic members finite")
expect_identical(c(b$q_grade, b$qm2_grade), c("Good", "Good"), info = "grade words")

# Same-name mirror + consumption (users see one shape on both backends).
expect_true(identical(auto$result$mstats, b), info = "result$mstats is the verbatim mirror")
expect_identical(unserialize(serialize(auto$result, NULL)), auto$result)
metrics <- persephone3:::diagnostic_metrics(auto)
expect_identical(metrics$q$value, b$q)
expect_identical(metrics$q2$value, b$qm2)
expect_identical(metrics$q$source, "x11_mstats/q")
expect_identical(metrics$q2$source, "x11_mstats/qm2")
expect_identical(metrics$q$status, "Good")   # round-trip of the core's own grade
expect_identical(metrics$q2$status, "Good")
tab <- diagnosticsTable(auto)
expect_identical(tab$Q, round(b$q, 2))
expect_identical(tab$Q2, round(b$qm2, 2))
expect_identical(tab$nMstatOutside,
                 sum(as.numeric(b[paste0("m", 1:11)]) > 1, na.rm = TRUE))
dv <- persephone3:::diagnostic_dashboard_values(auto)
expect_identical(dv$q1, b$q)
expect_identical(dv$q2, b$qm2)
# The native summary block keeps its compact honest form (the JDemetra
# quality.summary aggregate stays honestly unavailable); the battery serves
# the shared diagnostic surfaces asserted above.
expect_true(any(grepl("X-13 / X-11", capture.output(summary(auto)), fixed = TRUE)))

# --- Estimated-template lane: the same wiring on the stock route -------------
est <- perX13(AirPassengers, "rsa0", backend = "native")
est$run()
be <- est$output$x11_mstats
expect_true(!is.null(be), info = "estimated battery present")
expect_identical(names(be), shape)
expect_identical(be$status, "available")
recorded_rsa0 <- c(
  "3fb0300a7c5c82da", "3fa937adce853070", "3fada5bb222383e0", "3fd567b7b0905c38",
  "3fd4560fa4c660b3", "3ff2cf7f57e301c7", "3fe925f965812fb6", "3fec80f44c98af71",
  "3feaae7c10d613c9", "3ff316fd746ccbbb", "3ff2b976f6015174", "3fdf004c07d196e9",
  "3fe1707ea81ff2e8")
expect_identical(hexof(be[fields]), recorded_rsa0,
                 info = "estimated words are the core entry, live-recorded")
expect_true(all(is.finite(as.numeric(be[fields]))), info = "estimated members finite")
expect_identical(c(be$q_grade, be$qm2_grade), c("Good", "Good"))
expect_true(identical(est$result$mstats, be), info = "estimated mirror")
me <- persephone3:::diagnostic_metrics(est)
expect_identical(me$q$value, be$q)
expect_identical(me$q2$value, be$qm2)
te <- diagnosticsTable(est)
expect_identical(te$Q, round(be$q, 2))
expect_identical(te$Q2, round(be$qm2, 2))

# --- Absence arms: lanes and builds without a battery stay honestly empty ---
fixed <- perX13(AirPassengers, "restricted", backend = "native",
                speclist = list(seasonal = rep("s3x3", 12), trend_length = 13L))
fixed$run()
expect_null(fixed$output$x11_mstats,
            info = "restricted route publishes no battery (rkalos domain)")
expect_null(fixed$result$mstats)
mf <- persephone3:::diagnostic_metrics(fixed)
expect_true(is.na(mf$q$value) && identical(mf$q$availability, "missing"))
expect_true(is.na(mf$q2$value))

# --- Refusal arms verbatim (real engine answers; nothing fabricated) --------
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
# Pseudo-additive refuse-answer: explicit status, every word at the untouched
# default (m-words -1, q/qm2 NaN, grades empty) -- never a partial battery;
# and no wrapper surface interprets it (Q/Q2 stay missing, never a number).
padd <- rsa3_direct(mode = "pseudo_additive")
expect_identical(padd$status, "available")
expect_identical(padd$x11_mstats$status, "unsupported:pseudo_additive")
expect_identical(as.numeric(padd$x11_mstats[fields]), c(rep(-1, 11), NaN, NaN),
                 info = "refused battery stays untouched (rkalos ledger words)")
expect_identical(padd$x11_mstats$q_grade, "")
expect_identical(padd$x11_mstats$qm2_grade, "")
x_padd <- list(result = list(backend = "native"), output = padd)
mp <- persephone3:::diagnostic_metrics(x_padd)
expect_true(is.na(mp$q$value) && identical(mp$q$availability, "missing"))
expect_true(is.na(mp$q2$value) && identical(mp$q2$availability, "missing"))
expect_identical(mp$q$status, "Undefined")
# 21-I refusal: the fit itself refuses and NO battery travels with it.
refusal <- rsa3_direct(calendar = TRUE)
expect_identical(refusal$status,
  "unsupported:x13 automatic fit defers calendar/easter/TD/user detection (21-I)")
expect_null(refusal$x11_mstats, info = "no battery outside the available domain")
# The wrapper lane keeps its established refusal shape (rsa4 detects TD/Easter):
expect_error(perX13(AirPassengers, "rsa4", backend = "native")$run(), "21-I")

# --- Cross-backend parity vs the LIVE JVM battery on the same call ----------
if (requireNamespace("rjd3x13", quietly = TRUE) &&
    persephone3:::oracle_domain_allows("bridge", "test-native-x11-mstats.R")) {
  ref <- perX13(AirPassengers, "rsa3", backend = "rjd3")
  ref$run()
  jvm <- ref$output$mstats
  # Member naming: the reference block carries the thirteen words by the very
  # names the native battery uses; the status and the grade pair are the core's
  # honest additions (the reference aggregate category is quality.summary).
  expect_identical(names(jvm), fields)
  # Core certified tiers (core c3706ea twelve-case replay, rkalos 9034 entry):
  # m4 and the grade words BIT. Achieved on this lane: m4 bit-identical; the
  # grade pair "Good"/"Good" with the reference's aggregate "Good".
  expect_identical(hexof(b$m4), hexof(jvm$m4), info = "m4 bit anchor vs the live JVM")
  expect_identical(c(b$q_grade, b$qm2_grade), c("Good", "Good"))
  expect_identical(ref$result$diagnostics[["quality.summary"]]$value, "Good")
  # The remaining twelve words at the core's certified tier -- its own measured
  # worst 3.226e-13 at this boundary (stock_rsa0_ao m6, rkalos ledger). This
  # call's ACHIEVED values (tmp_opencode/m14-p3consumer/parity-x11-mstats-table
  # .csv, live recordings on both sides): m1 7.63e-17, m2 6.94e-17, m3 0 (bit),
  # m5 3.89e-16, m6 3.44e-15, m7 2.78e-16, m8 1.61e-15, m9 3.05e-15,
  # m10 5.16e-15, m11 5.16e-15, q 7.77e-16, qm2 8.88e-16 -- worst 5.163e-15,
  # 63x inside the pinned tier; the pin stays at the certified value.
  tier <- 3.226e-13
  diffs <- unname(abs(as.numeric(b[fields]) - as.numeric(jvm[fields])))
  expect_true(all(diffs[-4] <= tier), info = "twelve words within the core tier")
  expect_identical(diffs[4], 0)
  # The wrapper-served Q/Q2 are the JVM's own table values at the table's
  # rounding (two decimals).
  expect_identical(diagnosticsTable(auto)$Q, diagnosticsTable(ref)$Q)
  expect_identical(diagnosticsTable(auto)$Q2, diagnosticsTable(ref)$Q2)
}
