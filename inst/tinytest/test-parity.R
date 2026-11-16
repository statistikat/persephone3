# Backend parity harness (item 14). Fits representative specifications through
# the rjd3 reference backend once each, derives canonical result summaries
# (component names, time spans, key values with explicit numeric tolerances,
# structural invariants) and defines the comparison contract that the native
# backend must satisfy once rkalos exports fitting. It deliberately stores no
# whole-object snapshots: only metric-level values, so Java-side structure
# changes cannot create brittle failures, while numerical drift is caught.
#
# Native status (honest, asserted, not assumed): rkalos 0.1.0 reports
# capabilities()$fit = FALSE and no fitting export exists, so the native lane
# is recorded as unavailable and every native construction route must raise the
# explicit "no Java fallback" error. parity_native_fit() below is the single
# TODO hook where the native comparison plugs in; today it always returns NULL
# and the harness asserts that fact instead of silently skipping.
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
# file with a recorded reason; silent tolerance widening is not allowed.
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
if (!all(vapply(c("rjd3toolkit", "rjd3x13", "rjd3tramoseats"), requireNamespace,
                logical(1), quietly = TRUE))) {
  skip("Reference backend packages unavailable; parity harness needs rjd3 fits")
}

series_m <- AirPassengers
series_q <- stats::aggregate(AirPassengers, nfrequency = 4)
TOL_KEY <- 1e-8
TOL_HTS <- 1e-8

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

# Canonical summary: metric-level only (see header). Candidate (future native)
# results must expose the same shape.
parity_summary <- function(fit) {
  ud <- fit$output$user_defined
  keys <- c("y", "t", "sa")
  component <- lapply(keys, function(k) as.numeric(ud[[k]]))
  names(component) <- keys
  list(keys = keys,
       present = keys[vapply(keys, function(k) !is.null(ud[[k]]), logical(1))],
       lengths = vapply(component, length, integer(1)),
       tsp = stats::tsp(ud$sa),
       last6 = lapply(component, function(v) rev(utils::tail(v, 6))),
       mean = vapply(component, mean, numeric(1)),
       sd = vapply(component, stats::sd, numeric(1)),
       sum = vapply(component, sum, numeric(1)),
       na = lapply(component, is.na),
       forecast_n = length(ud$y_f))
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

# TODO(item 22b/27 - native fit hook): when rkalos exports a native seasonal-
# adjustment fitting entry point (rkalos::capabilities()$fit becomes TRUE),
# build the candidate summary here from the native result of the same
# specification and return it; parity_compare() below then becomes the real
# cross-backend check. Until then this must return NULL.
parity_native_fit <- function(method, template, freq) NULL

spec_matrix <- expand.grid(method = c("x13", "tramoseats"),
                           template = c("rsa0", "rsa3"),
                           freq = c(12L, 4L),
                           stringsAsFactors = FALSE)

# Time domain as exact integers (year, period, frequency): the backend stores
# the decimal end, whose floating-point representation is not canonical, so
# spans are pinned through (start year, start period, frequency) + length.
time_domain <- function(t) c(floor(t[1]),
                             round((t[1] - floor(t[1])) * t[3]) + 1, t[3])

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

  # Self-parity proves the summary extraction is deterministic (the same
  # contract a future native candidate must satisfy without violation).
  expect_identical(parity_compare(ref, parity_summary(fit)), character(0))

  # Honest native status: unavailable capability plus explicit no-fallback.
  cap <- backend_capabilities("native", spec$method)
  expect_false(cap$fit)
  expect_identical(cap$status, "unimplemented")
  native_series <- if (spec$freq == 12) series_m else series_q
  err <- tryCatch({
    if (spec$method == "x13") perX13(native_series, backend = "native")
    else perTramo(native_series, backend = "native")
    "no error"
  }, error = function(e) conditionMessage(e))
  expect_match(err, "native backend does not yet implement|No Java fallback",
               fixed = FALSE)

  # Native lane: no fitting exists, so the hook must be empty and comparing
  # against the reference must report the absence rather than fake parity.
  expect_null(parity_native_fit(spec$method, spec$template, spec$freq))
}

# One native capability query per backend method records the stack state.
cap_native <- backend_capabilities("native")
expect_false(cap_native$fit)
expect_identical(cap_native$status, "unimplemented")
expect_identical(cap_native, rkalos::capabilities())

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
