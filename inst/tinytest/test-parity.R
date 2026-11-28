# Backend parity harness (item 14). Fits representative specifications through
# the rjd3 reference backend once each, derives canonical result summaries
# (component names, time spans, key values with explicit numeric tolerances,
# structural invariants) and defines the comparison contract that the native
# backend must satisfy. It deliberately stores no whole-object snapshots: only
# metric-level values, so Java-side structure changes cannot create brittle
# failures, while numerical drift is caught.
#
# Native status (honest, asserted, not assumed): since rkalos schema_version 2
# the native backend qualifies two fits: the restricted fixed-coefficient
# X-13 fit (gate capabilities()$native_x13_fit_restricted) and the
# automatic-spec X-13 fit for the pinned rsa3 decode table (gate
# capabilities()$native_x13_fit_automatic) -- never capabilities()$fit, which
# keeps its meaning that UNRESTRICTED native fitting is unimplemented. For the
# pinned fixed specification below the native lane runs through the real
# product path (perX13 + $run()) and is compared against the rjd3 reference —
# first native-vs-rjd3 numeric agreement measurement in the family, recorded
# 2026-11-16 as bit-exact (all final-series max diffs 0.000e+00, far inside
# the 1e-8 policy). Everything outside the qualified domains (the automatic
# templates other than rsa3, other methods or frequencies) must still raise
# the explicit "no Java fallback" error, asserted below. Since the maintainer
# decision of 2026-11-17 the sigma policy defaults to "legacy" (= pinned
# JDemetra behavior; "accurate" stays selectable): the dual-lane
# specifications state no sigma policy, so the default path is exercised
# end-to-end, and bit-exactness must persist because the default maps to the
# same pinned rule the explicit policy used.
#
# Automatic-lane parity contract (item 21-H/M4, 2026-11-21): the rsa3 native
# lane is compared against pinned JVM captures (fixtures native/parity-m4/,
# byte-identical across independent JVM processes) at the tiers of the
# rkalos x13-automatic-pipeline contract (docs/contracts, table after the
# 21-H.3 note): decision bits BIT-EXACT (log, d/bd/p/q/bp/bq, mean, outlier
# variables, neffective, nparams, final Henderson, final seasonal filter);
# landing coefficients 1e-2 rel (contract worst 9.8e-3; measured worst
# 4.1e-4); loglik/AICC 2e-3 rel (measured worst 2.0e-8); downstream tables
# (out/d11final family: sa) 1e-6 rel (measured 1.7e-7); split finals (d12: t)
# 1e-5 (measured 8.2e-8); the d13 class (irregular i) 2e-4 rel -- the
# contract's own measured class "~2e-4", driven by the landing-basin drift
# seam the contract names (G2); measured worst 1.97e-4 on the 'seas' fixture,
# deterministic across processes. These bands are contract values, never
# widened here; a violation must STOP with diagnostics like the 1e-8 policy
# above.
#
# Structure: the native lane (gate contract, refusals, restricted fit through
# the product path with pinned baseline metrics) must pass WITHOUT Java or
# rjd3 installed — verified by running this file in a JVM-free Rscript. Only
# the rjd3 reference sections (specification matrix, dual-lane comparison,
# hierarchy, known issue) are gated on the reference packages.
#
# Numeric tolerances come from the 2026-11-16 Linux stability study
# (IMPLEMENTATION_STATUS.md, M3 section): 98 metrics across 9 reference fits
# compared between two separate R sessions on the qualified stack
# (rjd3 3.9.0/3.9.1, jars 0.0.6, JVM 21.0.11) were bit-identical (max diff 0).
# Tolerances below are therefore pure safety margins for JVM minor-version or
# platform drift, five orders of magnitude below the reporting precision (0.1):
#   key values (last 6, mean, sd, sum of y/t/sa): 1e-8 absolute
#   hierarchy aggregates (direct/indirect):       1e-8 absolute
#   names, time spans, lengths, counts, NA masks: exact (integer/character)
# Any rjd3 stack upgrade that changes digits at 1e-8 must re-baseline this
# file with a recorded reason; silent tolerance widening is not allowed. If a
# native-vs-rjd3 metric violates 1e-8 the harness must STOP with full
# diagnostics (it prints max diffs before the expectation), never widen.
#
# Known issue (difficult-model lane, backend-side, NOT a persephone3 defect):
# a deliberately degenerate 48-month series (linear trend plus a strict
# 3-period pattern) makes rjd3toolkit's protobuf-to-R conversion throw
# "missing value where TRUE/FALSE needed" at `if (pval < 0)` inside
# .jd2r_test -> statisticaltest while computing an NA test statistic. This is
# encoded as regression protection, not as a desired behavior:
#   - fit succeeds  -> upstream fixed the conversion: assert the fit is sane
#     (green, known issue retires itself);
#   - fit fails with the pinned message -> documented current state (green);
#   - other failures must fail the check in every locale. Generate the
#     interpreter's exact missing-condition message in the active R session.
#
# Capture domain: default (x86_64-default). The demand-run dispatch against
# fixtures/native/refusals-m4gap/demand-jvm-profiles.tsv pins THIS host's
# default-intrinsics JVM captures; the JVM-agnostic proto/spec walks and the
# native fixtures are domain-independent. On x86-64 the demand dispatch runs
# in the default pass (see R/helpers.R); the same guard that defers the five
# bridge-parity files defers these checks out of the bridge pass, and a naive
# single-pass JVM mismatch fails loudly (evidence tmp_opencode/m6-c-verify/
# rebaseline-ledger-2026-11-24.md, scratch). On aarch64 the domains coincide.
has_rjd3 <- all(vapply(c("rjd3toolkit", "rjd3x13", "rjd3tramoseats"),
                       requireNamespace, logical(1), quietly = TRUE))

series_m <- AirPassengers
series_q <- stats::aggregate(AirPassengers, nfrequency = 4)
TOL_KEY <- 1e-8
TOL_HTS <- 1e-8

# Automatic-lane comparison helpers (item M4). Both lanes are reduced to the
# same canonical key/value lines as the pinned JVM captures so decision
# strings compare byte-identically; numeric tiers are applied separately at
# the contract bands (see the header). Fixture format: "key=value" lines.
parity_auto_dir <- function() {
  path <- system.file("tinytest", "fixtures", "native", "parity-m4",
                      package = "persephone3")
  stopifnot(nzchar(path))
  path
}

parity_auto_key_lines <- function(path) {
  ls <- readLines(path)
  stats::setNames(sub("^[^=]*=", "", ls), sub("=.*", "", ls))
}

# The captured finite values are round-trip binary64 decimals. Parse the input
# once with a correctly rounded conversion on platforms with 64-bit long double.
read_series_fixture <- function(id) {
  k <- parity_auto_key_lines(file.path(parity_auto_dir(), paste0("series-", id, ".txt")))
  values <- jsonlite::fromJSON(paste0("[", chartr(";", ",", k[["values"]]), "]"))
  stats::ts(values, start = c(as.integer(k[["start_year"]]), as.integer(k[["start_period"]])),
            frequency = as.integer(k[["frequency"]]))
}

parity_auto_g <- function(v) {
  if (is.null(v) || !length(v)) return("NULL")
  paste(sprintf("%.17g", as.numeric(v)), collapse = ";")
}

# Canonical lines from the rkalos automatic fit (product output envelope).
parity_auto_native_lines <- function(fit) {
  m <- fit$model
  s <- fit$selection
  c(template = "rsa3",
    log = as.character(m$log),
    d = paste0(m$d, " bd=", m$bd),
    p = as.character(m$p), q = as.character(m$q),
    bp = as.character(m$bp), bq = as.character(m$bq),
    mean = as.character(m$mean),
    nvars = as.character(length(m$variables$name)),
    vars = paste(m$variables$name, collapse = "|"),
    neff = as.character(as.integer(m$effective_observations)),
    nparams = as.character(m$n_coefficients),
    theta = parity_auto_g(m$coefficients$ma),
    btheta = parity_auto_g(m$coefficients$sma),
    phi = parity_auto_g(m$coefficients$ar),
    bphi = parity_auto_g(m$coefficients$sar),
    ll = parity_auto_g(m$log_likelihood),
    aicc = parity_auto_g(m$aicc),
    final_henderson = parity_auto_g(s$d$final_henderson_length),
    final_seasonal = parity_auto_g(as.integer(s$d$seas_filter)),
    ud_sa_full = parity_auto_g(as.numeric(fit$finalSA)),
    ud_t_full = parity_auto_g(as.numeric(fit$finalTrend)),
    ud_i_full = parity_auto_g(as.numeric(fit$finalIrregular)))
}

# Canonical lines from a raw rjd3 x13_fast result (rsa3-shaped request).
parity_auto_jvm_lines <- function(f) {
  d <- f$preprocessing$description
  pick <- function(x) if (is.null(x) || !length(x)) NULL else
    unlist(x[seq(1, length(x), 2)])
  names_ <- vapply(d$variables, function(v) v$name, character(1))
  nonconst <- names_ != "const"
  fs <- f$decomposition$final_seasonal
  code <- if (length(fs) == 1L && is.character(fs))
    suppressWarnings(as.numeric(sub("FILTER_S3X", "", fs))) else as.numeric(fs)[1L]
  seasonal <- if (is.na(code)) as.character(fs) else
    paste(rep(sprintf("%.17g", code), stats::frequency(f$final$d11final)),
          collapse = ";")
  c(template = "rsa3",
    log = as.character(d$log),
    d = paste0(d$arima$d, " bd=", d$arima$bd),
    p = as.character(length(d$arima$phi) %/% 2L),
    q = as.character(length(d$arima$theta) %/% 2L),
    bp = as.character(length(d$arima$bphi) %/% 2L),
    bq = as.character(length(d$arima$btheta) %/% 2L),
    mean = as.character(any(names_ == "const")),
    nvars = as.character(sum(nonconst)),
    vars = paste(names_[nonconst], collapse = "|"),
    neff = as.character(as.integer(
      f$preprocessing$estimation$likelihood$neffectiveobs)),
    nparams = as.character(length(f$preprocessing$estimation$parameters$val)),
    theta = parity_auto_g(pick(d$arima$theta)),
    btheta = parity_auto_g(pick(d$arima$btheta)),
    phi = parity_auto_g(pick(d$arima$phi)),
    bphi = parity_auto_g(pick(d$arima$bphi)),
    ll = parity_auto_g(f$preprocessing$estimation$likelihood$ll),
    aicc = parity_auto_g(f$preprocessing$estimation$likelihood$aicc),
    final_henderson = parity_auto_g(f$decomposition$final_henderson),
    final_seasonal = seasonal,
    ud_sa_full = parity_auto_g(as.numeric(f$user_defined$sa)),
    ud_t_full = parity_auto_g(as.numeric(f$user_defined$t)),
    ud_i_full = parity_auto_g(as.numeric(f$user_defined$i)))
}

# Decision-line keys (bit-exact tier) and numeric tier bands (contract
# x13-automatic-pipeline-contract.md, 21-H.3 table and horizon-envelope note).
PARITY_AUTO_DECISIONS <- c("log", "d", "p", "q", "bp", "bq", "mean",
                           "nvars", "vars", "neff", "nparams",
                           "final_henderson", "final_seasonal")
PARITY_AUTO_BANDS <- c(theta = 1e-2, btheta = 1e-2, phi = 1e-2, bphi = 1e-2,
                       ll = 2e-3, aicc = 2e-3,
                       ud_sa_full = 1e-6, ud_t_full = 1e-5, ud_i_full = 2e-4)

parity_auto_compare <- function(native, jvm, prefix = "") {
  problems <- character(0)
  for (key in PARITY_AUTO_DECISIONS) {
    if (!identical(native[[key]], jvm[[key]])) {
      problems <- c(problems, sprintf("decision %s: native '%s' vs jvm '%s'",
                                      key, native[[key]], jvm[[key]]))
    }
  }
  for (key in names(PARITY_AUTO_BANDS)) {
    if (identical(native[[key]], "NULL") && identical(jvm[[key]], "NULL")) next
    a <- as.numeric(strsplit(native[[key]], ";", fixed = TRUE)[[1]])
    b <- as.numeric(strsplit(jvm[[key]], ";", fixed = TRUE)[[1]])
    if (length(a) != length(b)) {
      problems <- c(problems, sprintf("tier %s: length %d vs %d", key, length(a), length(b)))
      next
    }
    worst <- suppressWarnings(max(abs(a - b) / pmax(abs(b), 1e-300)))
    if (!is.finite(worst) || worst > PARITY_AUTO_BANDS[[key]]) {
      problems <- c(problems, sprintf("tier %s: rel %.3g exceeds band %.3g",
                                      key, worst, PARITY_AUTO_BANDS[[key]]))
    }
    message(prefix, "parity ", key, " worst rel ", format(worst, digits = 3),
            " (band ", PARITY_AUTO_BANDS[[key]], ")")
  }
  problems
}

# The pinned restricted fixed specification both lanes must reproduce:
# fixed s3x15 seasonal filter in every period, fixed Henderson length 13,
# additive mode, no regression variables, zero horizons, monthly frequency.
# The sigma policy is deliberately OMITTED: the maintainer decision of
# 2026-11-17 makes "legacy" the product default (restricted_fit schema v2,
# sigma_policy_default), so this exercises the default path end-to-end; the
# default maps to the same pinned rule the explicit "legacy" used, so the
# dual-lane diffs must stay exactly zero.
native_fixed_speclist <- list(seasonal = rep("s3x15", 12L),
                              trend_length = 13L)

# Time domain as exact integers (year, period, frequency): the backend stores
# the decimal end, whose floating-point representation is not canonical, so
# spans are pinned through (start year, start period, frequency) + length.
time_domain <- function(t) c(floor(t[1]),
                             round((t[1] - floor(t[1])) * t[3]) + 1, t[3])

# ---- Native lane: runs with Java absent (asserted at the end) --------------

local({
  cap <- backend_capabilities("native")
  # The gating contract itself (schema_version 2, verbatim rkalos passthrough).
  expect_identical(cap$restricted_fit, rkalos::capabilities()$restricted_fit)
  expect_identical(cap$schema_version, 2L)
  expect_identical(cap$native_x13_fit_restricted, TRUE)
  expect_false(cap$fit)
  expect_identical(cap$status, "unimplemented")
  rf <- cap$restricted_fit
  expect_true(all(c("fixed_coefficients_only", "fixed_seasonal_requests_only",
                    "fixed_trend_length_only", "sigma_policy_default",
                    "zero_horizon_only", "monthly_only") %in% names(rf)))
  # restricted_fit schema v2 (2026-11-17): sigma_policy_required replaced by
  # the maintainer's legacy sigma default; the Persephone gate is exactly this
  # identity.
  expect_identical(rf$schema_version, 2L)
  expect_false("sigma_policy_required" %in% names(rf))
  expect_identical(rf$sigma_policy_default, "legacy")
  expect_identical(rf$frequency, 12L)
  expect_true(all(c("additive", "multiplicative", "log_additive",
                    "pseudo_additive") %in% rf$modes))
})

# The stock and decoded templates have distinct native specification classes.
local({
  constructed <- perX13(series_m, "rsa3", backend = "native")
  expect_true(inherits(constructed$specification$parameters,
                       "persephone_native_auto_spec"))
  refuse_tpl <- function(template, method = "x13") {
    series <- if (method == "x13") series_m else series_q
    tryCatch({
      if (method == "x13") perX13(series, template, backend = "native")
      else perTramo(series, template, backend = "native")
      "no error"
    }, error = function(e) conditionMessage(e))
  }
  for (template in c("rsa0", "rsa1", "rsa2c")) {
    expect_identical(refuse_tpl(template), "no error")
    stock <- perX13(series_m, template, backend = "native")
    expect_true(inherits(stock$specification$parameters, "persephone_native_estimated_spec"))
    expect_identical(stock$specification$parameters$forecast, -1L)
  }
  e4 <- refuse_tpl("rsa4")
  expect_match(e4, "automatic_fit.deferred")
  expect_match(e4, "TD2")
  e5 <- refuse_tpl("rsa5c")
  expect_match(e5, "automatic_fit.deferred")
  expect_match(e5, "TD7")
  # The automatic template on a frequency outside the qualified domain.
  # The automatic X-13 template and TRAMO RSA0 have separate gates.
  eq <- refuse_tpl("rsa2", method = "tramoseats")
  expect_match(eq, "does not yet implement specification construction")
  expect_match(eq, "tramo_seats_fit.templates", fixed = TRUE)
  expect_match(tryCatch({perX13(series_q, "rsa3", backend = "native"); "no error"},
                        error = function(e) conditionMessage(e)),
               "monthly_only")
})

# Out-of-domain native requests: explicit refusal naming the violated scope
# field(s), no Java fallback, mirroring the matrix grid.
spec_matrix <- expand.grid(method = c("x13", "tramoseats"),
                           template = c("rsa0", "rsa3"),
                           freq = c(12L, 4L),
                           stringsAsFactors = FALSE)

# The caller-supplied fixed-coefficient entry retains its own scope checks.
local({
  core_statuses <- c(
    msr = "unsupported:x13 fit fixed seasonal requests only",
    henderson = "unsupported:x13 fit fixed Henderson trend length",
    horizon = "unsupported:x13 fit requires zero horizons")
  fit_fun <- rkalos::native_x13_fit
  raw_fun <- get0(".native_x13_fit", envir = asNamespace("rkalos"))
  expect_true(is.function(fit_fun) && is.function(raw_fun))
  y <- as.numeric(AirPassengers)
  st <- fit_fun(y, seasonal = rep("msr", 12L), trend_length = 13L,
                frequency = 12L, start_period = 1L)
  expect_identical(st$status, core_statuses[["msr"]])
  st <- fit_fun(y, seasonal = rep("s3x3", 12L), trend_length = 0L,
                frequency = 12L, start_period = 1L)
  expect_identical(st$status, core_statuses[["henderson"]])
  st <- fit_fun(y, seasonal = rep("s3x3", 12L), trend_length = 13L,
                frequency = 12L, start_period = 1L, forecast = 12L)
  expect_identical(st$status, core_statuses[["horizon"]])
  # Template-faithful -1 (the core normalizes |h| to annual frequencies):
  st <- fit_fun(y, seasonal = rep("s3x3", 12L), trend_length = 13L,
                frequency = 12L, start_period = 1L, forecast = -1L)
  expect_identical(st$status, core_statuses[["horizon"]])
  # Estimated coefficients have no carrier: a non-finite coefficient throws at
  # the pinned preadjust shape guard (never an estimation seam).
  thrown <- tryCatch({
    raw_fun(y, list(list(class = "user", effect = "undefined",
                         coefficient = NA_real_, columns = list(seq_along(y)))),
            FALSE, 12L, 1L, rep("s3x3", 12L), 13L, "legacy", 1.5, 2.5,
            "jdplus", "additive", 0L, 0L)
    NA_character_
  }, error = function(e) conditionMessage(e))
  expect_match(thrown, "x13 preadjust coefficients are finite", fixed = TRUE)
  # Contrast: the in-envelope fixed request runs (the envelope itself works --
  # what the templates request is not in it).
  st <- fit_fun(y, seasonal = rep("s3x3", 12L), trend_length = 13L,
                frequency = 12L, start_period = 1L)
  expect_identical(st$status, "available")
})

for (i in seq_len(nrow(spec_matrix))) {
  spec <- spec_matrix[i, ]
  native_series <- if (spec$freq == 12L) series_m else series_q
  err <- tryCatch({
    if (spec$method == "x13") perX13(native_series, spec$template, backend = "native")
    else perTramo(native_series, spec$template, backend = "native")
    "no error"
  }, error = function(e) conditionMessage(e))
  qualified <- spec$freq == 12L && (spec$method == "tramoseats" || spec$template %in% c("rsa0", "rsa1", "rsa2c", "rsa3"))
  if (qualified) expect_identical(err, "no error")
  else expect_match(err, "native backend does not yet implement|No Java fallback", fixed = FALSE)
}

# Per-scope-field refusals inside the fit path (constructed, then refused at
# the gate in backend_native_request()).
local({
  mod <- function(base, ...) {
    out <- base
    vals <- list(...)
    for (name in names(vals)) out[[name]] <- vals[[name]]  # NULL removes
    out
  }
  refuse <- function(speclist, series = series_m, method = "x13") {
    tryCatch({
      obj <- if (method == "x13") {
        perX13(series, template = "restricted", speclist = speclist,
               backend = "native")
      } else {
        perTramo(series, template = "restricted", speclist = speclist,
                 backend = "native")
      }
      obj$run()
      "no error"
    }, error = function(e) conditionMessage(e))
  }
  fixed <- native_fixed_speclist
  # Sigma: an omitted policy is NO LONGER a refusal (2026-11-17 decision) —
  # see the default-applied proof below. A STATED policy outside the pinned
  # pair is refused naming the contract field.
  e <- refuse(mod(fixed, sigma_policy = "jdemetra"))
  expect_match(e, "sigma_policy_default")
  expect_match(e, "No Java fallback")
  expect_match(refuse(mod(fixed, trend_length = 0L)), "fixed_trend_length_only")
  expect_match(refuse(mod(fixed, seasonal = rep("msr", 12L))),
               "fixed_seasonal_requests_only")
  expect_match(refuse(fixed, series = series_q), "monthly_only")
  expect_match(refuse(mod(fixed, forecast = 6)),
               "not part of the restricted native specification")
  badvar <- list(list(class = "user", effect = "series", coefficient = NA_real_,
                      columns = list(as.numeric(series_m))))
  expect_match(refuse(mod(fixed, variables = badvar)),
               "fixed_coefficients_only")
  # The fixed X-13 specification is outside the TRAMO RSA0 envelope.
  expect_match(refuse(fixed, method = "tramoseats"),
               "does not yet implement specification construction")
})

# The restricted fit through the product path: honest envelope, provenance
# echoes, is_stale semantics, and the pinned Java-free baseline metrics
# (measured 2026-11-16 on rkalos 10afd3e / core 2b93ab0 and re-measured
# 2026-11-17 on rkalos 20af662 through the default-sigma path — bit-identical,
# the default applies the same pinned rule — and bit-exact against the rjd3
# reference in the dual-lane section below; the pin proves cross-session
# determinism of the native lane in JVM-free environments).
native_fit <- local({
  obj <- perX13(series_m, template = "restricted",
                speclist = native_fixed_speclist, backend = "native")
  obj$run()
  obj
})
local({
  res <- native_fit$result
  expect_true(inherits(res, "persephone_native_restricted_fit"))
  expect_false(inherits(res, "persephone_result"))
  expect_identical(res$status$code, "success")
  # Honest envelope: raw rkalos result kept as-is, no faked rjd3 shape.
  expect_true(inherits(native_fit$output, "persephone_native_x13_fit"))
  expect_null(native_fit$output$user_defined)
  expect_null(native_fit$forecasts)   # zero_horizon_only
  sa <- res$components$sa
  present <- names(res$components)[vapply(res$components, function(x) !is.null(x),
                                          logical(1))]
  expect_true(all(c("t", "sa", "i") %in% present))
  expect_identical(as.integer(stats::frequency(sa)), 12L)
  expect_identical(time_domain(stats::tsp(sa)), time_domain(stats::tsp(series_m)))
  expect_identical(as.numeric(native_fit$adjusted), as.numeric(native_fit$output$finalSA))
  expect_identical(res$provenance$scope, "restricted_fixed_model_x13_fit_only")
  expect_identical(res$provenance$trend_length, 13L)
  # Sigma provenance: the spec states no policy, so the compound string must
  # record the applied maintainer default while the core echo stays verbatim.
  expect_identical(res$provenance$sigma, "legacy")
  expect_identical(res$provenance$sigma_policy, "legacy (default)")
  expect_identical(res$provenance$mode_requested, "additive")
  expect_identical(res$provenance$mode_effective, "additive")
  expect_match(res$provenance$request_fingerprint, "^[0-9a-f]{8}-[0-9a-f]{8}$")
  expect_false(anyNA(res$components$sa))
  expect_false(native_fit$is_stale)
  native_fit$updateParams(speclist = list(trend_length = 15L))
  expect_true(native_fit$is_stale)
  native_fit$updateParams(speclist = list(trend_length = 13L))
  expect_false(native_fit$is_stale)
  # Availability table: finals available, zero-horizon outputs not provided.
  avail <- stats::setNames(res$availability$state, res$availability$field)
  expect_true(all(avail[c("t", "sa", "i")] == "available"))
  expect_true(all(avail[c("y_f", "sa_f", "y_ef")] == "not_provided"))
})

# Determinism of the request fingerprint: identical request -> identical
# fingerprint; a scope-visible change (the stated sigma policy is effective
# in the request) -> different fingerprint.
local({
  refit <- perX13(series_m, template = "restricted",
                  speclist = native_fixed_speclist, backend = "native")
  refit$run()
  expect_identical(refit$result$provenance$request_fingerprint,
                   native_fit$result$provenance$request_fingerprint)
  other_speclist <- native_fixed_speclist
  other_speclist$sigma_policy <- "accurate"
  other <- perX13(series_m, template = "restricted",
                  speclist = other_speclist, backend = "native")
  other$run()
  expect_false(identical(other$result$provenance$request_fingerprint,
                         native_fit$result$provenance$request_fingerprint))
})

# Default-applied proof (maintainer decision 2026-11-17): an omitted sigma
# policy must be bit-identical to the explicit "legacy" run — the default is
# the same pinned rule, so also the request fingerprint matches — while the
# provenance distinguishes default from explicit.
local({
  explicit_speclist <- native_fixed_speclist
  explicit_speclist$sigma_policy <- "legacy"
  explicit <- perX13(series_m, template = "restricted",
                     speclist = explicit_speclist, backend = "native")
  explicit$run()
  expect_identical(explicit$result$status$code, "success")
  expect_identical(explicit$result$components, native_fit$result$components)
  expect_identical(explicit$result$provenance$request_fingerprint,
                   native_fit$result$provenance$request_fingerprint)
  expect_identical(explicit$result$provenance$sigma_policy, "legacy (explicit)")
  expect_identical(explicit$output$sigma, "legacy")
  expect_true(explicit$output$sigma_set)
})

# Explicit "accurate" smoke: selectable, honored end-to-end, honestly echoed.
# On this pinned series (and on every rkalos fit fixture) legacy and accurate
# produce the same finals — the fixtures' documented divergence is at the
# sigma-stage level, where the stage records carry the policy echo and the
# accurate policy never inherits the legacy cutoff state; the product lane
# surfaces exactly those echoes. Values are not pinned here.
local({
  acc_speclist <- native_fixed_speclist
  acc_speclist$sigma_policy <- "accurate"
  acc <- perX13(series_m, template = "restricted",
                speclist = acc_speclist, backend = "native")
  acc$run()
  expect_identical(acc$result$status$code, "success")
  expect_identical(acc$result$provenance$sigma_policy, "accurate (explicit)")
  expect_identical(acc$output$sigma, "accurate")
  expect_identical(acc$result$selection$b$policy, "accurate")
  expect_identical(acc$result$selection$c$policy, "accurate")
  expect_false(identical(acc$result$selection$b$policy,
                         native_fit$result$selection$b$policy))
  # Finals coincide on this series (measured 0.000e+00 on the qualified
  # stack); kept as a sanity bound, not as a pinned reference value.
  expect_true(max(abs(as.numeric(acc$result$components$sa) -
                        as.numeric(native_fit$result$components$sa))) <= TOL_KEY)
})

# Pinned native baseline (see above): 1e-8 session-stability metrics.
local({
  sa <- as.numeric(native_fit$result$components$sa)
  expect_true(all(abs(rev(utils::tail(sa, 6)) -
    c(462.280289945309, 445.550991459852, 482.81782395023,
      491.988183136371, 550.711751295872, 544.871905828635)) <= TOL_KEY))
  expect_true(abs(mean(sa) - 280.298611111111) <= TOL_KEY)
  expect_true(abs(stats::sd(sa) - 113.79689072143) <= TOL_KEY)
  expect_true(abs(sum(sa) - 40363) <= TOL_KEY)
})

# ---- Native automatic lane (rsa3 decode table, item M4) ---------------------
# Everything in this section runs WITHOUT Java or rjd3: the automatic gate
# contract, the update/fail-closed matrix, the product fit with pinned
# bit-level metrics and the decision-bit matrix against the pinned JVM
# captures in fixtures/native/parity-m4/ (each capture double-produced
# byte-identical in independent JVM processes; evidence recorded in
# IMPLEMENTATION_STATUS.md).
local({
  cap <- backend_capabilities("native")
  af <- cap$automatic_fit
  expect_identical(cap$native_x13_fit_automatic, TRUE)
  # Re-baselined 2026-11-21 at rkalos b90d367 (automatic_fit schema 3L):
  # x11$zero_horizon_only was REPLACED by x11$horizon_envelope (the 21-H.4 R
  # horizon qualification). Since the M12 horizon alignment (2026-11-27) the
  # Persephone automatic request sends the decode table's own template
  # horizons (forecast = -1 -> effective 12, backcast = 0, the measured
  # x11.nfcasts/-1 and x11.nbcasts/0 of the JVM rsa3 template), inside that
  # envelope; the RESTRICTED fit keeps zero_horizon_only = TRUE (asserted in
  # the restricted block above).
  expect_identical(af$schema_version, 3L)
  expect_identical(af$native_x13_fit_automatic, TRUE)
  expect_identical(af$frequency, 12L)
  expect_true(af$monthly_only)
  expect_true(af$complete_finite_input_only)
  expect_identical(af$transform$auto_log_test, TRUE)
  expect_null(af$x11$zero_horizon_only)
  expect_true(is.character(af$x11$horizon_envelope) &&
                length(af$x11$horizon_envelope) == 1L &&
                grepl("backcast/forecast", af$x11$horizon_envelope, fixed = TRUE))
  expect_identical(cap$restricted_fit$zero_horizon_only, TRUE)
  expect_identical(af$x11$sigma_policy_default, "legacy")
  expect_identical(af$x11$msr_requests_pinned, TRUE)
  expect_identical(af$x11$automatic_henderson, TRUE)
  expect_identical(af$entry_points, "native_x13_fit_automatic()")
})

auto_fit_m4 <- local({
  obj <- perX13(series_m, "rsa3", backend = "native")
  obj$run()
  obj
})
local({
  res <- auto_fit_m4$result
  expect_true(inherits(res, "persephone_native_automatic_fit"))
  expect_false(inherits(res, "persephone_result"))
  expect_true(inherits(auto_fit_m4$output, "persephone_native_x13_automatic_fit"))
  expect_null(auto_fit_m4$output$user_defined)
  expect_identical(res$status$code, "success")
  expect_identical(res$status$convergence, "converged")
  expect_identical(res$provenance$backend, "native")
  expect_true(res$provenance$automatic)
  expect_identical(res$provenance$decode_template, "rsa3")
  expect_identical(res$provenance$scope, "automatic_spec_x13_fit_only")
  # Sigma provenance (automatic surface): no policy stated -> the qualified
  # default is applied at the rkalos interface and echoed as such.
  expect_identical(res$provenance$sigma_provenance, "legacy (default)")
  # mode "additive" is the neutral (JVM UNKNOWN) request; the effective mode
  # is X13Kernel.forceMode's log-driven derivation -- multiplicative here.
  expect_identical(res$provenance$mode_requested, "additive")
  expect_identical(res$provenance$mode_effective, "multiplicative")
  expect_match(res$provenance$request_fingerprint, "^[0-9a-f]{8}-[0-9a-f]{8}$")
  # Template-default horizons (M12 alignment, 2026-11-27): the JVM rsa3
  # template measures x11.nfcasts = -1 / x11.nbcasts = 0 and the native
  # request reproduces them, so the default fit publishes one year of
  # forecast points (no backcasts). y_ef became a live envelope surface with
  # the M13 P5 landing (rkalos 0.1.0.9033, core 69f83be); i_f stays honestly
  # unprovided (no core surface yet -- P4 slice brief).
  expect_identical(res$provenance$effective_backcast, 0L)
  expect_identical(res$provenance$effective_forecast, 12L)
  expect_identical(res$forecasts, auto_fit_m4$forecasts)
  fc_auto <- auto_fit_m4$forecasts
  expect_true(inherits(fc_auto, "persephone_forecasts"))
  expect_identical(vapply(fc_auto$points[c("y", "t", "sa", "s", "cal")], length,
                          integer(1)),
                   stats::setNames(rep(12L, 5L), c("y", "t", "sa", "s", "cal")))
  expect_null(fc_auto$points$i)
  # M13 P5 (2026-11-28): the automatic lane's y SE is available. Presence
  # pins only — no SE numerics hand-pinned here: the core entry replays 144
  # predictions against the dual-JVM evidence at <= 1e-12 relative (worst
  # 1.86e-14, rkalos 0cbcb2e lane) and the live bridge-domain parity check
  # reaches worst 3.13e-13 abs over the 12 horizons
  # (tmp_opencode/m13-p3consumer/parity-yef-table.csv).
  expect_identical(fc_auto$availability$standard_errors[fc_auto$availability$component == "y"],
                   "available")
  expect_true(!is.null(fc_auto$standard_errors$y))
  expect_identical(length(fc_auto$standard_errors$y), 12L)
  expect_identical(as.numeric(stats::start(fc_auto$standard_errors$y)), c(1961, 1))
  expect_true(all(fc_auto$availability$standard_errors[fc_auto$availability$component != "y"] ==
                    "unavailable"))
  avail <- stats::setNames(res$availability$state, res$availability$field)
  expect_true(all(avail[c("y", "t", "sa", "i", "s", "cal", "y_f", "t_f",
                          "sa_f", "s_f", "cal_f")] == "available"))
  expect_identical(avail[["y_ef"]], "available")
  expect_identical(avail[["i_f"]], "unavailable")
  sa <- res$components$sa
  expect_identical(as.integer(stats::frequency(sa)), 12L)
  expect_identical(time_domain(stats::tsp(sa)), time_domain(stats::tsp(series_m)))
  expect_identical(auto_fit_m4$adjusted, auto_fit_m4$output$finalSA)
  expect_false(auto_fit_m4$is_stale)
  expect_identical(unserialize(serialize(res, NULL)), res)
  m <- res$provenance$model
  expect_true(m$log)
  expect_identical(c(m$p, m$d, m$q, m$bp, m$bd, m$bq), c(0L, 1L, 1L, 0L, 1L, 1L))
  expect_identical(as.integer(m$effective_observations), 131L)
  # The pinned JDemetra+ decisions on this series (rsa3; the request horizons
  # do not enter the selection: identical under forecast = 0 and forecast = -1).
  expect_identical(as.integer(auto_fit_m4$output$selection$d$final_henderson_length), 9L)
  expect_true(all(auto_fit_m4$output$selection$d$seas_filter == 3L))
  # is_stale semantics for the single adjustable field.
  auto_fit_m4$updateParams(speclist = list(sigma_policy = "legacy"))
  expect_true(auto_fit_m4$is_stale)
})

# Sigma semantics on the automatic lane: the omitted policy is the effective
# "legacy" (identical fingerprint and components, distinct provenance);
# "accurate" is selectable and fingerprint-visible; an invalid stated policy
# is refused at run() naming the contract field. Module-state fields are
# refused at construction: the template's decode table is pinned data.
local({
  refit <- perX13(series_m, "rsa3", backend = "native")
  refit$run()
  expect_identical(refit$result$provenance$request_fingerprint,
                   auto_fit_m4$result$provenance$request_fingerprint)
  explicit <- perX13(series_m, "rsa3", speclist = list(sigma_policy = "legacy"),
                     backend = "native")
  explicit$run()
  expect_identical(explicit$result$provenance$sigma_provenance, "legacy (explicit)")
  expect_identical(explicit$result$provenance$request_fingerprint,
                   auto_fit_m4$result$provenance$request_fingerprint)
  expect_identical(explicit$result$components, auto_fit_m4$result$components)
  accurate <- perX13(series_m, "rsa3", speclist = list(sigma_policy = "accurate"),
                     backend = "native")
  accurate$run()
  expect_identical(accurate$result$status$code, "success")
  expect_identical(accurate$result$provenance$sigma_provenance, "accurate (explicit)")
  expect_false(identical(accurate$result$provenance$request_fingerprint,
                         auto_fit_m4$result$provenance$request_fingerprint))
  expect_error(perX13(series_m, "rsa3", speclist = list(sigma_policy = "jdemetra"),
                      backend = "native"), "sigma_policy_default")
  for (change in list(list(transform.fun = "Log"), list(arima.p = 1L),
                      list(x11.henderson.filter = 13L), list(lower_sigma = 2),
                      list(mode = "multiplicative"))) {
    expect_match(tryCatch({perX13(series_m, "rsa3", speclist = change,
                                  backend = "native"); "no error"},
                          error = conditionMessage),
                 "not part of the automatic native specification")
  }
  # Contexts and user-defined outputs stay refused (rjd3 features).
  expect_match(tryCatch({perX13(series_m, "rsa3", context = list(a = series_m),
                                backend = "native")$run(); "no error"},
                        error = conditionMessage),
               "No Java fallback")
  expect_match(tryCatch({perX13(series_m, "rsa3", userdefined = "weird",
                                backend = "native")$run(); "no error"},
                        error = conditionMessage),
               "userdefined")
})

# Fail-closed matrix (verbatim core statuses through the product path): the
# automatic lane answers the pinned unsupported: status for an incomplete
# series, with the request fingerprint and no Java fallback. Requests retain
# the original observed calendar in their components (horizon extensions live
# only in the dated forecast tables).
local({
  broken <- series_m
  broken[5L] <- NA_real_
  obj <- perX13(broken, "rsa3", backend = "native")
  err <- tryCatch({obj$run(); "no error"}, error = conditionMessage)
  expect_match(err, "verbatim core status: unsupported:x13 automatic fit requires a complete finite series")
  expect_match(err, "No Java fallback")
})

# Pinned native automatic baseline: bit-level values (the native lane is
# byte-deterministic across processes on the qualified stack, proven by the
# capture pairs; text identity of the %.17g renderings is the pin), recorded
# 2026-11-21 for rkalos 00c3a88 / core f500162 through the product path.
# Re-recorded 2026-11-27 for the template-horizon default (forecast = -1,
# the JVM rsa3 template's measured x11.nfcasts): the previous renderings were
# the zero-horizon request and stay bit-reproducible through
# speclist = list(forecast = 0L); against the JVM rsa3 DEFAULT these values
# close the as-called final-series distance from max |delta| 5.141 (rel 1.2%)
# to the sub-1e-12 ulp class (see the dated IMPLEMENTATION_STATUS.md entry).
local({
  sa <- as.numeric(auto_fit_m4$result$components$sa)
  t <- as.numeric(auto_fit_m4$result$components$t)
  i <- as.numeric(auto_fit_m4$result$components$i)
  g <- function(v) paste(sprintf("%.17g", v), collapse = ";")
  expect_identical(g(rev(utils::tail(sa, 4))),
                   "488.93033644682504;487.57552858287698;496.77291366958877;483.25182595065911")
  expect_identical(g(rev(utils::tail(t, 4))),
                   "491.8304339022078;488.69781286657042;486.12817857487835;484.3665220273993")
  expect_identical(g(rev(utils::tail(i, 4))),
                   "0.99410346075501421;0.99770352096091774;1.0218969719589519;0.9976986516903058")
  # R's sum/mean intermediates depend on the platform's long double.
  # Accumulate rounding residuals so identical binary64 component vectors
  # give the same 17-digit checksum on arm64 and x86.
  compensated_sum <- function(values) {
    total <- correction <- 0
    for (value in values) {
      next_total <- total + value
      lost <- if (abs(total) >= abs(value)) (total - next_total) + value else
        (value - next_total) + total
      correction <- correction + lost
      total <- next_total
    }
    total + correction
  }
  corrected_mean <- function(values) {
    estimate <- compensated_sum(values) / length(values)
    estimate + compensated_sum(values - estimate) / length(values)
  }
  expect_identical(sprintf("%.17g", corrected_mean(sa)), "280.05744581956469")
  expect_identical(sprintf("%.17g", corrected_mean(t)), "280.10282649118341")
  expect_identical(sprintf("%.17g", compensated_sum(sa)), "40328.272198017316")
  expect_identical(sprintf("%.17g", compensated_sum(t)), "40334.80701473041")
  expect_identical(sprintf("%.17g", compensated_sum(i)), "143.99536843374648")
})

# Decision-bit matrix against the pinned JVM captures (JVM-free here): the
# product lane's decisions must be byte-identical to the pinned JDemetra+
# choices on all three corpus series; tier families must stay inside the
# contract bands (violations print their magnitudes and fail). The captures
# are zero-horizon JVM fits (fixtures/native/parity-m4/, 2026-11-21), and
# X-11 final tables are horizon-sensitive, so the compared lane requests
# the SAME horizons the captures carry: forecast = 0L stays an explicitly
# supported request (the default is now the template's forecast = -1L; the
# default path is qualified by the default-horizon block above and by the
# live dual-lane section below, which runs both backends at the default).
local({
  fixture_dir <- parity_auto_dir()
  corpus_fit <- function(series) {
    obj <- perX13(series, "rsa3", backend = "native",
                  speclist = list(backcast = 0L, forecast = 0L))
    obj$run()
    expect_identical(obj$result$status$code, "success")
    obj
  }
  fits <- list(air = corpus_fit(series_m))
  for (id in c("ao", "seas")) fits[[id]] <- corpus_fit(read_series_fixture(id))
  for (id in names(fits)) {
    pinned <- parity_auto_key_lines(file.path(fixture_dir, paste0("jvm-rsa3-", id, ".txt")))
    native <- parity_auto_native_lines(fits[[id]]$output)
    for (key in PARITY_AUTO_DECISIONS) {
      expect_identical(native[[key]], pinned[[key]],
                       info = paste(id, "decision bit", key))
    }
    problems <- parity_auto_compare(native, pinned, prefix = paste0(id, "/"))
    if (length(problems)) message("AUTOMATIC TIER VIOLATIONS (", id, "):\n",
                                  paste(problems, collapse = "\n"))
    expect_identical(problems, character(0))
  }
})

# Java-free proof for this section: if the reference packages are absent the
# restricted fit above must have run without ever loading a Java package.
if (!has_rjd3) {
  expect_false(any(c("rJava", "rjd3jars", "rjd3toolkit", "rjd3x13",
                     "rjd3tramoseats", "RProtoBuf") %in% loadedNamespaces()))
}

# ---- rjd3 reference lane (needs the reference stack) ------------------------
if (has_rjd3) {

# One cached reference fit per specification, reused by every check below.
spec_cache <- new.env(parent = emptyenv())
reference_fit <- function(method, template, freq) {
  key <- paste(method, template, freq)
  if (!is.null(spec_cache[[key]])) return(spec_cache[[key]])
  series <- if (freq == 12) series_m else series_q
  fit <- if (method == "x13") {
    perX13(series, template, backend = "rjd3")
  } else {
    perTramo(series, template, backend = "rjd3")
  }
  fit$run()
  spec_cache[[key]] <- fit
  spec_cache[[key]]
}

# Canonical summary builder: metric-level only (see header). Both lanes are
# reduced through the same builder so the comparison is shape-identical.
parity_build <- function(components, forecast_n) {
  keys <- names(components)
  numeric_components <- lapply(components, as.numeric)
  list(keys = keys,
       present = keys[vapply(keys, function(k) !is.null(components[[k]]), logical(1))],
       lengths = vapply(numeric_components, length, integer(1)),
       tsp = stats::tsp(components[[keys[1L]]]),
       last6 = lapply(numeric_components, function(v) rev(utils::tail(v, 6))),
       mean = vapply(numeric_components, mean, numeric(1)),
       sd = vapply(numeric_components, stats::sd, numeric(1)),
       sum = vapply(numeric_components, sum, numeric(1)),
       na = lapply(numeric_components, is.na),
       forecast_n = forecast_n)
}

parity_summary <- function(fit) {
  ud <- fit$output$user_defined
  keys <- c("y", "t", "sa")
  parity_build(stats::setNames(lapply(keys, function(k) ud[[k]]), keys),
               length(ud$y_f))
}

# Parity comparison contract. Exact for structural fields, absolute tolerance
# for numeric metrics. Returns a character vector of violations ("" = parity).
parity_compare <- function(reference, candidate, tol = c(key = TOL_KEY)) {
  problems <- character(0)
  if (!identical(reference$keys, candidate$keys)) problems <- c(problems, "keys")
  if (!identical(reference$present, candidate$present)) problems <- c(problems, "present")
  if (!identical(reference$lengths, candidate$lengths)) problems <- c(problems, "lengths")
  if (!isTRUE(all.equal(reference$tsp, candidate$tsp))) problems <- c(problems, "tsp")
  if (!identical(reference$na, candidate$na)) problems <- c(problems, "na-mask")
  if (!identical(reference$forecast_n, candidate$forecast_n)) problems <- c(problems, "forecast_n")
  for (key in reference$present) {
    for (metric in c("last6", "mean", "sd", "sum")) {
      value <- reference[[metric]][[key]]
      other <- candidate[[metric]][[key]]
      if (length(value) != length(other) ||
          any(abs(value - other) > tol[["key"]])) {
        problems <- c(problems, paste(key, metric))
      }
    }
  }
  problems
}

# Native fit hook (flipped 2026-11-16 from the always-NULL TODO placeholder):
# when the gate and the scope accept the request, run the native fit through
# the product path and return its candidate summary; requests outside the
# restricted domain (automatic templates that estimate coefficients, other
# methods, other frequencies) honestly have no candidate and return NULL.
parity_native_fit <- function(method, template, freq) {
  if (!identical(method, "x13") || !identical(template, "restricted") ||
      !identical(as.integer(freq), 12L)) return(NULL)
  cap <- rkalos::capabilities()
  if (!identical(cap$native_x13_fit_restricted, TRUE)) return(NULL)
  fit_obj <- perX13(series_m, template = "restricted",
                    speclist = native_fixed_speclist, backend = "native")
  fit_obj$run()
  parity_build(fit_obj$result$components[c("sa", "t", "i")], forecast_n = 0L)
}

for (i in seq_len(nrow(spec_matrix))) {
  spec <- spec_matrix[i, ]
  fit <- reference_fit(spec$method, spec$template, spec$freq)
  ref <- parity_summary(fit)

  # Canonical rjd3 summary: structural invariants.
  expect_true(all(c("y", "t", "sa") %in% ref$present))
  expect_identical(unname(ref$lengths[c("y", "t", "sa")]),
                   rep(length(if (spec$freq == 12) series_m else series_q), 3L))
  expect_identical(time_domain(ref$tsp),
                   time_domain(stats::tsp(if (spec$freq == 12) series_m else series_q)))
  expect_false(any(unlist(ref$na)))
  expect_true(all(vapply(ref$last6, function(v) all(is.finite(unlist(v))), logical(1))))
  expect_true(ref$forecast_n >= 1L)

  # Self-parity proves the summary extraction is deterministic.
  expect_identical(parity_compare(ref, parity_summary(fit)), character(0))

  # This hook carries the RESTRICTED 1e-8 absolute-parity contract only; the
  # automatic template is qualified solely at the contract tiers (see the
  # automatic dual-lane section below), never through absolute parity here.
  expect_null(parity_native_fit(spec$method, spec$template, spec$freq))
}

# Both public backends run the fixed specification with zero forecast months.
# Retain the direct x13_fast call as the independent numerical reference.
dual_lane <- local({
  spec <- rjd3x13::x13_spec("rsa0")
  spec <- rjd3toolkit::set_transform(spec, fun = "none")
  spec <- rjd3toolkit::set_tradingdays(spec, option = "None")
  spec <- rjd3x13::set_x11(spec, seasonal.filter = "S3X15", henderson.filter = 13,
                           mode = "Additive", fcasts = 0, bcasts = 0,
                           lsigma = 1.5, usigma = 2.5)
  ud <- rjd3x13::x13_fast(series_m, spec = spec,
                          userdefined = c("sa", "t", "i", "y", "s", "cal"))$user_defined
  keys <- c("sa", "t", "i")
  product <- perX13(series_m, "rsa0", backend = "rjd3")
  product$updateParams(spec = spec)
  product$run()
  expect_null(product$forecasts)
  for (key in keys) expect_identical(product$result$components[[key]], ud[[key]])
  for (key in c("y", "s", "cal")) {
    expect_identical(as.numeric(native_fit$result$components[[key]]), as.numeric(ud[[key]]))
    expect_identical(time_domain(tsp(native_fit$result$components[[key]])), time_domain(tsp(ud[[key]])))
  }
  reference <- parity_build(stats::setNames(lapply(keys, function(k) ud[[k]]), keys),
                            forecast_n = length(ud$y_f))
  candidate <- parity_native_fit("x13", "restricted", 12L)
  list(reference = reference, candidate = candidate, ud = ud)
})
local({
  ref <- dual_lane$reference
  cand <- dual_lane$candidate
  expect_false(is.null(cand))
  violations <- parity_compare(ref, cand)
  if (length(violations)) {
    # STOP-with-diagnostics policy: show the offending magnitudes before the
    # failing expectation so a red run is immediately interpretable.
    for (key in ref$present) {
      message("parity violation ", key, ": max abs diff ",
              format(max(abs(as.numeric(dual_lane$ud[[key]]) -
                             as.numeric(native_fit$result$components[[key]]))),
                     digits = 6))
    }
  }
  expect_identical(violations, character(0))
  # Time domain exact (integer tuples) on both lanes, matching the series.
  expect_identical(time_domain(cand$tsp), time_domain(ref$tsp))
  expect_identical(time_domain(ref$tsp), time_domain(stats::tsp(series_m)))
  # Full-series agreement, stronger than the pinned metrics (diagnostic value:
  # 2026-11-16 measured all max diffs at exactly 0.000e+00, bit-exact).
  for (key in c("sa", "t", "i")) {
    maxdiff <- max(abs(as.numeric(dual_lane$ud[[key]]) -
                         as.numeric(native_fit$result$components[[key]])))
    expect_true(maxdiff <= TOL_KEY)
  }
})

# Dual-lane automatic template (rsa3, item M4). Since the M12 horizon
# alignment (2026-11-27) the reference is the pinned rsa3 TEMPLATE DEFAULT
# (x11.nfcasts = -1 / x11.nbcasts = 0) -- what both product lanes now request
# by default. The fixtures/native/parity-m4/ captures stay the zero-horizon
# JVM records they were taken as; decision bits are contract-proven
# horizon-invariant, so the stack pins below are unaffected, and the tier
# families now measure the as-called default-vs-default margins. Three checks
# per corpus series: the LIVE JVM fit
# must reproduce the PINNED capture (stack-pin drift check -- the captures
# were byte-identical across independent JVM processes), the NATIVE product
# fit must reproduce the pinned DECISION BITS, and the tier families must stay
# inside the contract bands (STOP-with-diagnostics, never widen).
local({
  fixture_dir <- parity_auto_dir()
  xspec <- rjd3x13::x13_spec("rsa3")   # template default horizons (M12 alignment)
  native_objects <- list(air = auto_fit_m4)
  for (id in c("ao", "seas")) {
    obj <- perX13(read_series_fixture(id), "rsa3", backend = "native")
    obj$run()
    native_objects[[id]] <- obj
  }
  for (id in names(native_objects)) {
    series <- if (identical(id, "air")) series_m else read_series_fixture(id)
    reference <- rjd3x13::x13_fast(series, spec = xspec,
                                   userdefined = c("sa", "t", "i"))
    jvm_live <- parity_auto_jvm_lines(reference)
    jvm_pinned <- parity_auto_key_lines(file.path(fixture_dir, paste0("jvm-rsa3-", id, ".txt")))
    native <- parity_auto_native_lines(native_objects[[id]]$output)
    # Stack pin: live JVM decisions must equal the pinned choices exactly.
    for (key in PARITY_AUTO_DECISIONS) {
      expect_identical(jvm_live[[key]], jvm_pinned[[key]],
                       info = paste(id, "jvm live vs pinned", key))
    }
    # Native decisions bit-exact against the pinned choices.
    for (key in PARITY_AUTO_DECISIONS) {
      expect_identical(native[[key]], jvm_pinned[[key]],
                       info = paste(id, "native vs pinned", key))
    }
    # Tiers against the LIVE reference (measured worsts printed).
    problems <- parity_auto_compare(native, jvm_live, prefix = paste0(id, "/live "))
    if (length(problems)) message("AUTOMATIC DUAL-LANE VIOLATIONS (", id, "):\n",
                                  paste(problems, collapse = "\n"))
    expect_identical(problems, character(0))
  }
})

# M4-gap stack pin (2026-11-21): the pinned decode and demand captures under
# fixtures/native/refusals-m4gap/ (double-JVM byte-identical at capture; see
# the refusal-table header in R/backend_native.R for probe hashes) must be
# reproduced BYTE-IDENTICALLY by the live stack: the jar proto text and the
# rjd3 spec walk for all six templates, and the product-lane demand runs
# (x13_fast with the template defaults intact, nfcasts = -1 included) for the
# three automodel-OFF templates on the standard series. The refusals for
# rsa0/rsa1/rsa2c cite exactly these facts, so the refusal contract is
# re-verified against the live JVM here -- no native run exists to compare.
if (has_rjd3) local({
  ref_dir <- file.path(parity_auto_dir(), "..", "refusals-m4gap")
  ref_dir <- normalizePath(ref_dir, mustWork = TRUE)
  read_bytes <- function(path) {
    con <- file(path, "rb"); on.exit(close(con))
    readBin(con, what = "raw", n = file.size(path))
  }
  g <- function(v) if (is.null(v) || !length(v)) "NULL" else
    paste(sprintf("%.17g", as.numeric(v)), collapse = ";")
  pickv <- function(x) if (is.null(x) || !length(x)) NULL else
    unlist(x[seq(1, length(x), 2)])
  proto_capture <- function(t) {
    jobj <- rJava::.jcall("jdplus/x13/base/api/x13/X13Spec",
                          "Ljdplus/x13/base/api/x13/X13Spec;", "fromString", t)
    buf <- rJava::.jcall("jdplus/x13/base/r/X13", "[B", "toBuffer", jobj)
    msg <- rJava::.jcall("jdplus/x13/base/protobuf/Spec",
                         "Ljdplus/x13/base/protobuf/Spec;", "parseFrom", buf)
    c(sprintf("### TEMPLATE %s", t), sprintf("### PROTO_BYTES %d", length(buf)),
      paste(sprintf("%02x", as.integer(buf)), collapse = ""),
      "### PROTO_TEXT", rJava::.jcall(msg, "S", "toString"))
  }
  rspec_capture <- function(t) {
    out <- character(0)
    walk <- function(x, prefix) {
      if (is.list(x)) {
        for (n in names(x)) walk(x[[n]], paste0(prefix, ".", n))
      } else {
        out[[length(out) + 1L]] <<- sprintf(
          "%s = %s", prefix,
          paste(format(x, trim = TRUE, digits = 17), collapse = "|"))
      }
    }
    walk(rjd3x13::x13_spec(t), t)
    c(sprintf("### TEMPLATE %s", t), out)
  }
  demand_capture <- function(tpl, id, series) {
    f <- rjd3x13::x13_fast(series, spec = rjd3x13::x13_spec(tpl),
                           userdefined = c("sa", "t", "i", "y_f"))
    d <- f$preprocessing$description
    names_ <- vapply(d$variables, function(v) v$name, character(1))
    nonconst <- names_ != "const"
    fs <- f$decomposition$final_seasonal
    c(paste0("template=", tpl),
      paste0("series=", id),
      paste0("log=", d$log),
      paste0("d=", d$arima$d, " bd=", d$arima$bd),
      paste0("p=", length(d$arima$phi) %/% 2L),
      paste0("q=", length(d$arima$theta) %/% 2L),
      paste0("bp=", length(d$arima$bphi) %/% 2L),
      paste0("bq=", length(d$arima$btheta) %/% 2L),
      paste0("theta=", g(pickv(d$arima$theta))),
      paste0("btheta=", g(pickv(d$arima$btheta))),
      paste0("phi=", g(pickv(d$arima$phi))),
      paste0("bphi=", g(pickv(d$arima$bphi))),
      paste0("nvars=", sum(nonconst)),
      paste0("vars=", paste(names_[nonconst], collapse = "|")),
      paste0("lsr_coef=", g(unlist(lapply(d$variables[nonconst],
                                          function(v) v$coef$value)))),
      paste0("coef_types=", paste(vapply(d$variables[nonconst],
                                         function(v) v$coef$type[1], character(1)),
                                  collapse = "|")),
      paste0("nforecast=", length(na.omit(as.numeric(f$user_defined$y_f)))),
      paste0("final_henderson=", g(f$decomposition$final_henderson)),
      paste0("final_seasonal=", paste(as.character(fs), collapse = "|")),
      paste0("final_seasonal_n=", length(fs)))
  }
  write_tmp <- function(lines) {
    path <- tempfile(fileext = ".txt")
    con <- file(path, "w"); writeLines(lines, con); close(con)
    path
  }
  for (t in c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c")) {
    expect_identical(read_bytes(write_tmp(proto_capture(t))),
                     read_bytes(file.path(ref_dir, paste0("proto-", t, ".txt"))),
                     info = paste(t, "live jar proto vs pinned capture"))
    expect_identical(read_bytes(write_tmp(rspec_capture(t))),
                     read_bytes(file.path(ref_dir, paste0("rspec-", t, ".txt"))),
                     info = paste(t, "live rjd3 spec walk vs pinned capture"))
  }
  ref_series <- list(air = series_m)
  for (id in c("ao", "seas")) {
    ref_series[[id]] <- read_series_fixture(id)
  }
  # Kalos captured complete records with native ARM and x86 JVMs. Disabling
  # the six HotSpot math intrinsics on x86 independently reproduced ARM.
  arch <- rJava::.jcall("java/lang/System", "S", "getProperty", "os.arch")
  portable <- arch %in% c("aarch64", "arm64")
  profile <- if (portable) "portable" else "x86_64-default"
  profiles <- read.delim(file.path(ref_dir, "demand-jvm-profiles.tsv"),
                         colClasses = "character")
  # Capture-domain guard (see file header): on x86-64 this dispatch belongs to
  # the default JVM math-flag pass and is deferred out of the bridge pass with
  # a documented message; a mismatched single pass fails loudly. The guard is
  # inert on aarch64, where the demand captures run the portable rows.
  if (persephone3:::oracle_domain_allows("default", "test-parity.R (x86_64-default demand dispatch)"))
  for (tpl in c("rsa0", "rsa1", "rsa2c")) {
    for (id in names(ref_series)) {
      live <- demand_capture(tpl, id, ref_series[[id]])
      path <- file.path(ref_dir, sprintf("demand-%s-%s.txt", tpl, id))
      expected <- read_bytes(path)
      row <- profiles[profiles$profile == profile & profiles$template == tpl &
                        profiles$series == id, , drop = FALSE]
      stopifnot(nrow(row) == 1L)
      if (portable) {
        lines <- readLines(path, warn = FALSE)
        for (field in c("theta", "btheta", "lsr_coef")) {
          pos <- which(startsWith(lines, paste0(field, "=")))
          stopifnot(length(pos) == 1L)
          lines[pos] <- paste0(field, "=", row[[field]])
        }
        expected <- read_bytes(write_tmp(lines))
      }
      label <- paste("demand run", tpl, id, profile)
      expect_identical(read_bytes(write_tmp(live)), expected, info = label)
      # Hash canonical LF bytes so the same reference works on Windows.
      sha <- rJava::.jcall("java/security/MessageDigest",
                           "Ljava/security/MessageDigest;", "getInstance", "SHA-256")
      bytes <- charToRaw(paste0(paste(live, collapse = "\n"), "\n"))
      checksum <- rJava::.jcall(sha, "[B", "digest", bytes)
      expect_identical(paste(sprintf("%02x", as.integer(checksum)), collapse = ""),
                       row$record_sha256, info = label)
    }
  }
})

# Hierarchical case: aggregates over fitted single-series components.
hts_fit <- local({
  h <- perHts(a = series_m, b = series_m * 1.1 + 20 * sin(seq_along(series_m)),
              method = "x13", backend = "rjd3")
  h$run()
  h
})
hts_ref <- list(direct = as.numeric(hts_fit$adjustedDirect),
                indirect = as.numeric(hts_fit$adjustedIndirect),
                tsp_direct = stats::tsp(hts_fit$adjustedDirect))
expect_identical(time_domain(hts_ref$tsp_direct), time_domain(stats::tsp(series_m)))
expect_identical(length(hts_ref$direct), length(series_m))
expect_true(all(is.finite(hts_ref$direct)))
expect_true(all(is.finite(hts_ref$indirect)))
# Decision semantics: the direct aggregate and the fully indirect aggregate are
# distinct results (re-verified in M2); equality would hide a routing defect.
expect_true(max(abs(hts_ref$direct - hts_ref$indirect)) > TOL_HTS)

# Known issue: degenerate series, rjd3toolkit NA test-statistic conversion
# (policy and rationale in the file header).
degenerate <- ts(10 + 0.5 * seq_len(48) + rep(c(0, 1, 5), 16),
                 start = c(2016, 1), frequency = 12)
outcome <- tryCatch({
  bad <- perX13(degenerate, backend = "rjd3")
  bad$run()
  bad
}, error = function(e) conditionMessage(e))
if (is.persephone(outcome)) {
  # Upstream fixed the conversion: require a sane fit, retiring the pin.
  expect_false(is.null(outcome$adjusted))
  expect_true(all(is.finite(stats::na.omit(as.numeric(outcome$adjusted)))))
} else {
  missing_condition <- tryCatch(if (NA) TRUE, error = conditionMessage)
  expect_match(outcome, missing_condition, fixed = TRUE)
}

}
