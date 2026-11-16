# Backend parity harness (item 14). Fits representative specifications through
# the rjd3 reference backend once each, derives canonical result summaries
# (component names, time spans, key values with explicit numeric tolerances,
# structural invariants) and defines the comparison contract that the native
# backend must satisfy. It deliberately stores no whole-object snapshots: only
# metric-level values, so Java-side structure changes cannot create brittle
# failures, while numerical drift is caught.
#
# Native status (honest, asserted, not assumed): since rkalos schema_version 2
# the native backend qualifies exactly one fit, the restricted fixed-
# coefficient X-13 fit. The gate is capabilities()$native_x13_fit_restricted
# (never capabilities()$fit, which keeps its meaning that UNRESTRICTED native
# fitting is unimplemented). For the pinned fixed specification below the
# native lane runs through the real product path (perX13 + $run()) and is
# compared against the rjd3 reference — first native-vs-rjd3 numeric agreement
# measurement in the family, recorded 2026-11-16 as bit-exact (all final-series
# max diffs 0.000e+00, far inside the 1e-8 policy). Everything outside the
# restricted domain (automatic templates that estimate coefficients, other
# methods or frequencies) must still raise the explicit "no Java fallback"
# error, asserted below. Since the maintainer decision of 2026-11-17 the
# sigma policy defaults to "legacy" (= pinned JDemetra behavior; "accurate"
# stays selectable): the dual-lane specification states no sigma policy, so
# the default path is exercised end-to-end, and bit-exactness must persist
# because the default maps to the same pinned rule the explicit policy used.
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
#   - fit fails with a DIFFERENT message on an English/C message locale ->
#     RED on purpose, because the failure contract changed and this file's
#     header must be revisited;
#   - non-English LC_MESSAGES -> skip, because the pinned signature is an R
#     interpreter string and would be falsely localized.
has_rjd3 <- all(vapply(c("rjd3toolkit", "rjd3x13", "rjd3tramoseats"),
                       requireNamespace, logical(1), quietly = TRUE))

series_m <- AirPassengers
series_q <- stats::aggregate(AirPassengers, nfrequency = 4)
TOL_KEY <- 1e-8
TOL_HTS <- 1e-8

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
  expect_identical(cap, rkalos::capabilities())
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

# Out-of-domain native requests: explicit refusal naming the violated scope
# field(s), no Java fallback. First the per-specification template contract
# (automatic templates estimate coefficients), mirroring the matrix grid.
spec_matrix <- expand.grid(method = c("x13", "tramoseats"),
                           template = c("rsa0", "rsa3"),
                           freq = c(12L, 4L),
                           stringsAsFactors = FALSE)
for (i in seq_len(nrow(spec_matrix))) {
  spec <- spec_matrix[i, ]
  native_series <- if (spec$freq == 12) series_m else series_q
  err <- tryCatch({
    if (spec$method == "x13") perX13(native_series, backend = "native")
    else perTramo(native_series, backend = "native")
    "no error"
  }, error = function(e) conditionMessage(e))
  expect_match(err, "native backend does not yet implement|No Java fallback",
               fixed = FALSE)
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
  # The qualified fit is x13-only; native tramoseats names the entry points.
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

  # Automatic templates estimate coefficients: outside the restricted native
  # domain, so the hook must report absence rather than fake parity.
  expect_null(parity_native_fit(spec$method, spec$template, spec$freq))
}

# Dual-lane fixed specification: the milestone headline. The rjd3 reference is
# the raw x13_fast output for the pinned fixed specification (the same data
# the reference lane feeds its normalizer; the product-level rjd3 path cannot
# itself carry zero-horizon outputs because the pre-existing forecast contract
# rejects rjd3's residual cal_f/y_ef series against zero point forecasts — a
# pre-existing rjd3-lane limitation, untouched here). The native candidate
# runs through perX13(backend = "native")$run().
dual_lane <- local({
  spec <- rjd3x13::x13_spec("rsa0")
  spec <- rjd3toolkit::set_transform(spec, fun = "none")
  spec <- rjd3toolkit::set_tradingdays(spec, option = "None")
  spec <- rjd3x13::set_x11(spec, seasonal.filter = "S3X15", henderson.filter = 13,
                           mode = "Additive", fcasts = 0, bcasts = 0,
                           lsigma = 1.5, usigma = 2.5)
  ud <- rjd3x13::x13_fast(series_m, spec = spec,
                          userdefined = c("sa", "t", "i"))$user_defined
  keys <- c("sa", "t", "i")
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
lc_messages <- suppressWarnings(Sys.getlocale("LC_MESSAGES"))
english_locale <- isTRUE(lc_messages %in% c("C", "C.UTF-8")) ||
  grepl("^en", if (is.na(lc_messages)) "" else lc_messages)
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
} else if (!english_locale) {
  skip("Known-issue signature is locale-sensitive; LC_MESSAGES is not English")
} else {
  expect_match(outcome, "missing value where TRUE/FALSE needed")
}

}
