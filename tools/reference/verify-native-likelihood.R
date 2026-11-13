# Rscript tools/reference/verify-native-likelihood.R [--native [--source]]
# Pinned direct preprocessing comparisons. No fixtures or tolerance rules change.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][[1L]])
tooling <- dirname(normalizePath(script))
options <- commandArgs(trailingOnly = TRUE)
if (any(!options %in% c("--native", "--source")) ||
    ("--source" %in% options && !"--native" %in% options))
  stop("Usage: verify-native-likelihood.R [--native [--source]]")
source(file.path(tooling, "common.R"))
source(file.path(tooling, "extended-fixtures.R"))
root <- reference_root()
base_dir <- reference_fixture_dir(root)
parent_before <- extended_parent_hashes(base_dir)
extended_assert_oracle(base_dir)

checks <- direct_fits <- 0L
maximum_jacobian_error <- 0
assert <- function(value, label) {
  if (!isTRUE(value)) stop("Fixed-likelihood probe failed: ", label, call. = FALSE)
  checks <<- checks + 1L
}
close <- function(actual, expected, label, native_check = FALSE) {
  # Same bounds as verify-diffuse.R, fixed before observing new native output.
  tolerance <- if (native_check) 2e-8 else 1e-10
  assert(length(actual) == length(expected) && all(is.finite(actual)) &&
    all(is.finite(expected)) && all(abs(actual - expected) <= tolerance * (1 + abs(expected))), label)
}

fixed_spec <- function(method, period, d, D, coef, transformation) {
  spec <- if (method == "x13") rjd3x13::regarima_spec("rg0") else rjd3tramoseats::tramo_spec("tr0")
  # JD+ names the AR polynomial coefficients; native/R names the coefficients
  # subtracted by that polynomial. MA signs coincide. Keep this bridge explicit.
  values <- c(-as.numeric(coef$phi), coef$theta, -as.numeric(coef$bphi), coef$btheta)
  spec <- rjd3toolkit::set_arima(spec, mean = NULL, p = length(coef$phi), d = d,
    q = length(coef$theta), bp = length(coef$bphi), bd = D, bq = length(coef$btheta),
    coef = if (length(values)) values else NULL,
    coef.type = if (length(values)) rep("Fixed", length(values)) else NULL)
  spec <- rjd3toolkit::set_transform(spec, fun = if (transformation == "log") "Log" else "None")
  spec <- rjd3toolkit::set_automodel(spec, enabled = FALSE)
  if (method == "tramoseats") spec <- rjd3toolkit::set_estimate(spec, exact.ml = TRUE)
  spec
}

assert_fixed <- function(result, period, d, D, coef, transformation, label) {
  assert(is.list(result) && length(result$estimation$likelihood) == 10L, paste(label, "direct result"))
  assert(length(result$description$variables) == 0L && length(result$estimation$b) == 0L &&
    length(result$estimation$parameters$val) == 0L, paste(label, "no estimated or regression parameters"))
  arima <- result$description$arima
  assert(arima$period == period && arima$d == d && arima$bd == D, paste(label, "fixed orders"))
  for (name in c("phi", "theta", "bphi", "btheta")) {
    if (length(coef[[name]])) {
      expected <- if (name %in% c("phi", "bphi")) -coef[[name]] else coef[[name]]
      assert(identical(as.numeric(arima[[name]]["value", ]), expected) &&
        all(as.character(arima[[name]]["type", ]) == "FIXED"), paste(label, name, "fixed values"))
    } else assert(is.null(arima[[name]]), paste(label, name, "absent"))
  }
  assert(identical(result$description$log, transformation == "log") &&
    identical(result$description$preadjustment, "NONE"), paste(label, "transform without preadjustment"))
  assert(result$estimation$likelihood$nparams == 1L, paste(label, "variance-only parameter count"))
}

coefficients <- list(white = list(), arma = list(phi = 0.3, theta = 0.2),
                     airline = list(theta = 0.2, btheta = -0.4))
ar_acf <- rjd3toolkit::sarima_properties(rjd3toolkit::sarima_model(period = 4L, phi = -0.3), nacf = 4L)$acf
seasonal_acf <- rjd3toolkit::sarima_properties(rjd3toolkit::sarima_model(period = 4L, bphi = -0.25), nacf = 4L)$acf
ma_acf <- rjd3toolkit::sarima_properties(rjd3toolkit::sarima_model(period = 4L, theta = 0.2), nacf = 4L)$acf
close(ar_acf[2L] / ar_acf[1L], 0.3, "public JD+ regular AR sign convention")
close(seasonal_acf[5L] / seasonal_acf[1L], 0.25, "public JD+ seasonal AR sign convention")
close(ma_acf[2L] / ma_acf[1L], 0.2 / 1.04, "public JD+ MA sign convention")
orders <- expand.grid(d = 0:2, D = 0:2)
cases <- list()
for (period in c(4L, 12L)) for (row in seq_len(nrow(orders))) for (model in names(coefficients)) {
  order <- orders[row, ]
  coef <- coefficients[[model]]
  k <- order$d + period * order$D
  states <- max(1L, length(coef$phi) + period * length(coef$bphi),
                1L + length(coef$theta) + period * length(coef$btheta)) + k
  if (states > 32L) next
  for (span in c("all", "middle")) for (mask in c("complete", "leading", "internal", "edges")) {
    t <- seq_len(120L)
    y <- ts(exp(2 + 0.15 * sin(t / 9) + 0.02 * cos(t * sqrt(2)) + 0.001 * t),
            start = c(2000L, 1L), frequency = period)
    first <- if (span == "all") 1L else 2L * period + 1L
    last <- if (span == "all") length(y) else 8L * period
    n <- last - first + 1L
    missing <- switch(mask, complete = integer(), leading = 1:3,
      internal = unique(c(period + 3L, period + 4L, 2L * period + 1L)),
      edges = c(1:3, n - 1L, n))
    y[first - 1L + missing] <- NA_real_
    # Outside-window missing values must not affect an explicitly selected
    # likelihood window. The direct oracle receives only that selected window.
    if (span == "middle") y[c(first - 1L, last + 1L)] <- NA_real_
    selected <- seq.int(first, last)
    input <- ts(as.numeric(y[selected]), start = if (span == "all") c(2000L, 1L) else c(2002L, 1L), frequency = period)
    observed <- which(!is.na(input))
    jacobian_local <- observed[observed >= min(observed) + k]
    id <- paste(period, paste0("d", order$d), paste0("D", order$D), model, span, mask, sep = "-")
    cases[[id]] <- list(id = id, input = y, selected = input, original_indices = selected,
      jacobian_local = jacobian_local, jacobian_original = selected[jacobian_local],
      estimation_start = if (span == "all") NULL else c(2002L, 1L),
      estimation_end = if (span == "all") NULL else c(2007L, period),
      period = period, d = order$d, D = order$D, coef = coef, transformation = "log")
    # A bounded identity-transform control for every d/D/frequency combination.
    if (model == "white" && span == "all" && mask == "complete") {
      identity_case <- cases[[id]]
      identity_case$id <- paste0(id, "-none")
      identity_case$transformation <- "none"
      identity_case$jacobian_local <- which(!is.na(identity_case$selected))
      identity_case$jacobian_original <- identity_case$original_indices[identity_case$jacobian_local]
      cases[[identity_case$id]] <- identity_case
    }
  }
}
assert(length(cases) == 426L, "fixed case matrix dimensions")

oracle <- list()
for (case in cases) {
  observed <- sum(!is.na(case$selected))
  expected_jacobian <- if (case$transformation == "log") -sum(log(case$selected[case$jacobian_local])) else 0
  for (method in c("x13", "tramoseats")) {
    label <- paste(method, case$id)
    spec <- fixed_spec(method, case$period, case$d, case$D, case$coef, case$transformation)
    result <- if (method == "x13") rjd3x13::regarima_fast(case$selected, spec) else rjd3tramoseats::tramo_fast(case$selected, spec)
    assert_fixed(result, case$period, case$d, case$D, case$coef, case$transformation, label)
    value <- result$estimation$likelihood
    assert(value$nobs == observed && value$neffectiveobs == observed - case$d - case$period * case$D,
           paste(label, "observed and finite counts"))
    correction <- value$adjustedll - value$ll
    close(correction, expected_jacobian, paste(label, "calendar-window Jacobian"))
    maximum_jacobian_error <- max(maximum_jacobian_error, abs(correction - expected_jacobian))
    oracle[[label]] <- value
    direct_fits <- direct_fits + 1L
  }
}
assert(identical(parent_before, extended_parent_hashes(base_dir)), "original reference corpus unchanged")
cat("PASS:", direct_fits, "direct fixed fits;", checks, "checks; maximum Jacobian error", format(maximum_jacobian_error, digits = 8), "\n")

before_selector_checks <- checks
selector_fits <- 0L
fit <- function(method, y, spec) {
  selector_fits <<- selector_fits + 1L
  if (method == "x13") rjd3x13::regarima_fast(y, spec) else rjd3tramoseats::tramo_fast(y, spec)
}
# Perturb individual observed log values across the proposed cutoff. This checks
# membership rather than only a coincidentally matching aggregate Jacobian sum.
perturbations <- list("12-d0-D2-white-all-internal" = c(23L, 24L, 26L, 27L),
                      "12-d1-D0-white-middle-leading" = c(4L, 5L))
for (id in names(perturbations)) for (method in c("x13", "tramoseats")) {
  case <- cases[[id]]
  base <- oracle[[paste(method, id)]]
  spec <- fixed_spec(method, case$period, case$d, case$D, case$coef, "log")
  for (index in perturbations[[id]]) {
    y <- case$selected
    assert(!is.na(y[index]), paste(id, "perturbation observes a value"))
    y[index] <- y[index] * exp(0.125)
    result <- fit(method, y, spec)
    assert_fixed(result, case$period, case$d, case$D, case$coef, "log", paste(method, id, index))
    value <- result$estimation$likelihood
    change <- (value$adjustedll - value$ll) - (base$adjustedll - base$ll)
    close(change, if (index %in% case$jacobian_local) -0.125 else 0,
          paste(method, id, index, "individual Jacobian membership"))
  }
}

# Explicit JD+ estimate selectors have different leading-missing Jacobian
# semantics. Freeze that distinction rather than treating setters and direct
# window inputs as interchangeable. d1 here is the exclusive boundary 2008-01-01.
for (period in c(4L, 12L)) for (method in c("x13", "tramoseats")) for (mask in c("complete", "leading", "internal", "edges")) {
  id <- paste(period, "d1-D0-white-middle", mask, sep = "-")
  case <- cases[[id]]
  t <- seq_len(120L)
  full <- ts(exp(2 + 0.15 * sin(t / 9) + 0.02 * cos(t * sqrt(2)) + 0.001 * t),
             start = c(2000L, 1L), frequency = period)
  full[case$original_indices] <- case$selected
  spec <- fixed_spec(method, period, 1L, 0L, list(), "log")
  spec <- rjd3toolkit::set_estimate(spec, type = "Between", d0 = "2002-01-01", d1 = "2008-01-01")
  result <- fit(method, full, spec)
  assert_fixed(result, period, 1L, 0L, list(), "log", paste(method, id, "explicit selector"))
  value <- result$estimation$likelihood
  base <- oracle[[paste(method, id)]]
  close(value$ll, base$ll, paste(method, id, "selector/direct-window raw likelihood"))
  selector_indices <- which(!is.na(case$selected) & seq_along(case$selected) >= 2L)
  expected <- -sum(log(case$selected[selector_indices]))
  close(value$adjustedll - value$ll, expected, paste(method, id, "selector anchors cutoff before trimming window NAs"))
  if (mask %in% c("leading", "edges")) {
    assert(abs(value$adjustedll - base$adjustedll) > 1, paste(method, id, "selector differs from direct trimmed-window adjustment"))
  }
  outside <- tryCatch(fit(method, case$input, spec), error = identity)
  assert(inherits(outside, "error") && grepl("Missing values outside the estimation domain", conditionMessage(outside), fixed = TRUE),
         paste(method, id, "explicit selector rejects outside-window missing values"))
}
assert(identical(parent_before, extended_parent_hashes(base_dir)), "original corpus unchanged after selector probes")
cat("PASS:", selector_fits, "additional perturbation/selector calls;", checks - before_selector_checks, "checks\n")

if ("--native" %in% options) {
  if ("--source" %in% options) {
    if (!requireNamespace("pkgload", quietly = TRUE)) stop("--source requires pkgload and an already compiled development tree")
    pkgload::load_all(root, quiet = TRUE, compile = FALSE)
  }
  if (!requireNamespace("persephone3", quietly = TRUE)) stop("--native requires the development persephone3 installation")
  native <- getFromNamespace("native_fixed_likelihood", "persephone3")
  before_native_checks <- checks
  maximum_error <- c(raw_profile = 0, adjusted_profile = 0, profiled_variance = 0, jacobian = 0)
  for (case in cases) {
    value <- tryCatch(native(case$input, ar = as.numeric(case$coef$phi), ma = as.numeric(case$coef$theta),
      seasonal_ar = as.numeric(case$coef$bphi), seasonal_ma = as.numeric(case$coef$btheta),
      d = case$d, seasonal_d = case$D, transformation = case$transformation,
      estimation_start = case$estimation_start, estimation_end = case$estimation_end),
      error = function(e) stop(case$id, ": ", conditionMessage(e), call. = FALSE))
    assert(inherits(value, "persephone_native_likelihood") && value$schema_version == 1L &&
      identical(value$backend, "native") && identical(value$operation, "fixed_sarima_likelihood") &&
      identical(value$initialization, "fresh_prior_at_span_start"), paste(case$id, "native operation contract"))
    assert(identical(value$inputs$original, case$selected) &&
      identical(as.integer(value$span$source_rows), case$original_indices), paste(case$id, "selected values/domain and original index map"))
    assert(identical(as.integer(value$adjustment$local_rows), case$jacobian_local) &&
      identical(as.integer(value$adjustment$source_rows), case$jacobian_original), paste(case$id, "exact Jacobian index membership"))
    assert(identical(value$adjustment$policy, if (case$transformation == "log") "jdplus_3.9.0_calendar_window" else "identity") &&
      value$adjustment$difference_degree == case$d + case$period * case$D, paste(case$id, "named adjustment policy"))
    terms <- numeric(length(case$selected))
    if (case$transformation == "log") terms[case$jacobian_local] <- -log(case$selected[case$jacobian_local])
    assert(identical(tsp(value$adjustment$terms), tsp(case$selected)), paste(case$id, "Jacobian term domain"))
    close(as.numeric(value$adjustment$terms), terms, paste(case$id, "per-observation Jacobian terms"), TRUE)
    close(value$adjustment$log_jacobian, sum(terms), paste(case$id, "Jacobian sum"), TRUE)
    assert(identical(value$likelihood$status, "available") && identical(value$likelihood$profile_status, "available"),
           paste(case$id, "native likelihood availability"))
    close(value$likelihood$adjusted_fixed, value$likelihood$transformed_fixed + sum(terms),
          paste(case$id, "fixed-variance adjustment identity"), TRUE)
    for (method in c("x13", "tramoseats")) {
      expected <- oracle[[paste(method, case$id)]]
      label <- paste(method, case$id)
      assert(value$likelihood$observed_count == expected$nobs &&
        value$likelihood$finite_count == expected$neffectiveobs &&
        value$likelihood$diffuse_count == case$d + case$period * case$D,
        paste(label, "native counts"))
      close(value$likelihood$transformed_profiled, expected$ll, paste(label, "native raw profile"), TRUE)
      close(value$likelihood$adjusted_profiled, expected$adjustedll, paste(label, "native adjusted profile"), TRUE)
      close(value$likelihood$profiled_variance, expected$ssq / expected$neffectiveobs,
            paste(label, "native profiled variance"), TRUE)
      maximum_error <- pmax(maximum_error, abs(c(
        raw_profile = value$likelihood$transformed_profiled - expected$ll,
        adjusted_profile = value$likelihood$adjusted_profiled - expected$adjustedll,
        profiled_variance = value$likelihood$profiled_variance - expected$ssq / expected$neffectiveobs,
        jacobian = value$adjustment$log_jacobian - (expected$adjustedll - expected$ll))))
    }
  }
  assert(identical(parent_before, extended_parent_hashes(base_dir)), "original corpus unchanged after native comparisons")
  cat("PASS:", length(cases), "native fixed-window cases;", checks - before_native_checks, "checks\n")
  print(maximum_error, digits = 8)
}
