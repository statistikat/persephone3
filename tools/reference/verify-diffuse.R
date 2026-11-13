# Rscript tools/reference/verify-diffuse.R [--native [--source] [--second-order]]
# Read-only probes. --native additionally requires an installed development
# persephone3 containing .native_sarima_likelihood; choose its library with R_LIBS.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][[1]])
tooling <- dirname(normalizePath(script))
options <- commandArgs(trailingOnly = TRUE)
if (any(!options %in% c("--native", "--source", "--second-order")) ||
    (any(c("--source", "--second-order") %in% options) && !"--native" %in% options))
  stop("Usage: verify-diffuse.R [--native [--source] [--second-order]]")
source(file.path(tooling, "common.R"))
source(file.path(tooling, "extended-fixtures.R"))
root <- reference_root()
base_dir <- reference_fixture_dir(root)
parent_before <- extended_parent_hashes(base_dir)
extended_assert_oracle(base_dir)
native <- NULL
if ("--native" %in% options) {
  if ("--source" %in% options) {
    if (!requireNamespace("pkgload", quietly = TRUE)) stop("--source requires pkgload and an already compiled development tree")
    pkgload::load_all(root, quiet = TRUE, compile = FALSE)
  }
  if (!requireNamespace("persephone3", quietly = TRUE)) stop("--native requires the development persephone3 installation")
  native <- getFromNamespace(".native_sarima_likelihood", "persephone3")
}

checks <- 0L
maximum_error <- c(jd_profile = 0, jd_fixed = 0, native_profile = 0, native_fixed = 0)
assert <- function(value, label) {
  if (!isTRUE(value)) stop("Diffuse probe failed: ", label, call. = FALSE)
  checks <<- checks + 1L
}
# Bounds declared before native comparison: absolute-plus-relative roundoff
# checks for this small, well-conditioned synthetic suite, not general model
# acceptance tolerances. Neither stored fixtures nor tolerance rules are changed.
close <- function(actual, expected, label, native_check = FALSE) {
  tolerance <- if (native_check) 2e-8 else 1e-10
  assert(length(actual) == length(expected) && all(is.finite(actual)) &&
    all(is.finite(expected)) && all(abs(actual - expected) <= tolerance * (1 + abs(expected))), label)
}

probe_data <- function(kind, period) {
  n <- 120L
  t <- seq_len(n)
  z <- sin(t * sqrt(2)) + 0.5 * cos(t * sqrt(3))
  if (kind %in% c("seasonal", "seasonal-gaps", "combined")) {
    y <- numeric(n)
    y[seq_len(period)] <- 100 + seq_len(period)
    for (i in seq.int(period + 1L, n)) y[i] <- y[i - period] + z[i]
    if (kind == "combined") y <- cumsum(y)
  } else y <- 100 + cumsum(z)
  if (kind == "ima1") y <- 100 + cumsum(z + 0.4 * c(0, head(z, -1L)))
  if (kind == "log-rw") y <- exp(2 + cumsum(z) / 50)
  if (kind %in% c("rw-gaps", "seasonal-gaps")) y[c(20L, 21L, 44L)] <- NA_real_
  if (kind == "rw-edges") y[c(1L, 2L, 119L, 120L)] <- NA_real_
  list(y = ts(y, start = c(2000L, 1L), frequency = period),
       d = as.integer(!kind %in% c("seasonal", "seasonal-gaps")),
       bd = as.integer(kind %in% c("seasonal", "seasonal-gaps", "combined")),
       theta = if (kind == "ima1") 0.4 else NULL,
       log = identical(kind, "log-rw"))
}

likelihood <- function(m, q, logdet) {
  c(m = m, q = q, logdet = logdet,
    fixed = -0.5 * (m * log(2 * pi) + q + logdet),
    profile = -0.5 * (m * (log(2 * pi) + 1 + log(q / m)) + logdet))
}

# Independent probability identities: observed increments of independent random
# walks have variance equal to their time gap. Each seasonal phase is one walk.
# Combined complete data have iid doubly differenced innovations. For IMA(1),
# first differences have the known tridiagonal MA(1) covariance, evaluated using
# R's public dense linear algebra rather than any Kalman implementation.
analytic <- function(data, kind) {
  y <- if (data$log) log(data$y) else data$y
  if (kind == "ima1") {
    w <- as.numeric(diff(y))
    covariance <- toeplitz(c(1 + 0.4^2, 0.4, rep(0, length(w) - 2L)))
    return(likelihood(length(w), drop(crossprod(w, solve(covariance, w))),
                      2 * sum(log(diag(chol(covariance))))))
  }
  if (kind == "combined") {
    w <- diff(diff(y, lag = frequency(y)))
    return(likelihood(length(w), sum(w^2), 0))
  }
  period <- if (data$bd) frequency(y) else 1L
  m <- q <- logdet <- 0
  for (phase in seq_len(period)) {
    indices <- seq.int(phase, length(y), by = period)
    observed <- indices[!is.na(y[indices])]
    gaps <- diff(observed) / period
    if (length(gaps)) {
      m <- m + length(gaps)
      q <- q + sum(diff(y[observed])^2 / gaps)
      logdet <- logdet + sum(log(gaps))
    }
  }
  likelihood(m, q, logdet)
}

fixed_spec <- function(method, data) {
  spec <- if (method == "x13") rjd3x13::regarima_spec("rg0") else rjd3tramoseats::tramo_spec("tr0")
  # mean=NULL removes drift. In the pinned wrappers mean=FALSE instead creates
  # an undefined parameter. Orders and coefficient values must be set together.
  spec <- rjd3toolkit::set_arima(spec, mean = NULL, p = 0L, d = data$d,
    q = length(data$theta), bp = 0L, bd = data$bd, bq = 0L,
    coef = data$theta, coef.type = if (length(data$theta)) "Fixed" else NULL)
  spec <- rjd3toolkit::set_transform(spec, fun = if (data$log) "Log" else "None")
  spec <- rjd3toolkit::set_automodel(spec, enabled = FALSE)
  if (method == "tramoseats") spec <- rjd3toolkit::set_estimate(spec, exact.ml = TRUE)
  spec
}

# Only public Java entry points are invoked. javap -public was used to inspect
# signatures; no Java numerical method bodies were copied or translated.
dk_likelihood <- function(data, profile) {
  model <- rjd3toolkit::sarima_model(period = frequency(data$y), d = data$d,
    bd = data$bd, theta = data$theta)
  java_model <- rjd3toolkit::.r2jd_sarima(model)
  ssf <- rJava::.jcall("jdplus/toolkit/base/core/ssf/arima/SsfArima",
    "Ljdplus/toolkit/base/core/ssf/univariate/Ssf;", "ssf",
    rJava::.jcast(java_model, "jdplus/toolkit/base/core/arima/IArimaModel"))
  y <- if (data$log) log(data$y) else data$y
  observations <- rJava::.jnew("jdplus/toolkit/base/core/ssf/univariate/SsfData",
    rJava::.jarray(as.numeric(y)))
  value <- rJava::.jcall("jdplus/toolkit/base/core/ssf/dk/DkToolkit",
    "Ljdplus/toolkit/base/core/ssf/likelihood/DiffuseLikelihood;", "likelihood",
    rJava::.jcast(ssf, "jdplus/toolkit/base/core/ssf/univariate/ISsf"),
    rJava::.jcast(observations, "jdplus/toolkit/base/core/ssf/univariate/ISsfData"),
    profile, FALSE)
  c(ll = rJava::.jcall(value, "D", "logLikelihood"),
    q = rJava::.jcall(value, "D", "ssq"),
    observed = rJava::.jcall(value, "I", "dim"),
    diffuse = rJava::.jcall(value, "I", "getD"),
    finite_logdet = rJava::.jcall(value, "D", "logDeterminant"),
    diffuse_logdet = rJava::.jcall(value, "D", "getDiffuseCorrection"),
    legacy = rJava::.jcall(value, "Z", "isLegacy"))
}

kinds <- c("rw", "seasonal", "combined", "ima1", "rw-gaps", "seasonal-gaps", "rw-edges", "log-rw")
cases <- native_cases <- 0L
for (period in c(4L, 12L)) for (kind in kinds) {
  data <- probe_data(kind, period)
  expected <- analytic(data, kind)
  id <- paste(period, kind, sep = "-")
  observed <- sum(!is.na(data$y))
  diffuse <- data$d + period * data$bd
  assert(length(data$y) == 120L && frequency(data$y) == period, paste(id, "input domain"))
  assert(observed - diffuse == expected[["m"]], paste(id, "analytic finite observation count"))
  dk_profile <- dk_likelihood(data, TRUE)
  dk_fixed <- dk_likelihood(data, FALSE)
  close(dk_profile[["ll"]], expected[["profile"]], paste(id, "DK profiled likelihood"))
  close(dk_fixed[["ll"]], expected[["fixed"]], paste(id, "DK unit-variance likelihood"))
  for (dk in list(dk_profile, dk_fixed)) {
    close(dk[["q"]], expected[["q"]], paste(id, "DK sum of squares"))
    close(dk[["finite_logdet"]], expected[["logdet"]], paste(id, "DK finite log determinant"))
    close(dk[["diffuse_logdet"]], 0, paste(id, "DK canonical diffuse correction"))
    assert(dk[["observed"]] == observed && dk[["diffuse"]] == diffuse && dk[["legacy"]] == 0,
           paste(id, "DK observation counts and nonlegacy convention"))
  }
  maximum_error[["jd_fixed"]] <- max(maximum_error[["jd_fixed"]], abs(dk_fixed[["ll"]] - expected[["fixed"]]))
  for (method in c("x13", "tramoseats")) {
    spec <- fixed_spec(method, data)
    result <- if (method == "x13") rjd3x13::regarima_fast(data$y, spec) else rjd3tramoseats::tramo_fast(data$y, spec)
    label <- paste(method, id)
    assert(is.list(result) && length(result$estimation$likelihood) == 10L, paste(label, "direct result"))
    assert(length(result$description$variables) == 0L && length(result$estimation$b) == 0L,
           paste(label, "no drift, regressors or outlier coefficients"))
    assert(length(result$estimation$parameters$val) == 0L, paste(label, "no estimated ARIMA coefficients"))
    arima <- result$description$arima
    assert(arima$d == data$d && arima$bd == data$bd && arima$period == period,
           paste(label, "fixed integration orders"))
    assert(is.null(arima$phi) && is.null(arima$bphi) && is.null(arima$btheta), paste(label, "absent AR/seasonal coefficients"))
    if (length(data$theta)) {
      assert(identical(as.numeric(arima$theta["value", ]), data$theta) &&
             identical(as.character(arima$theta["type", ]), "FIXED"), paste(label, "fixed MA coefficient"))
    } else assert(is.null(arima$theta), paste(label, "absent MA coefficients"))
    assert(identical(result$description$log, data$log) && identical(result$description$preadjustment, "NONE"),
           paste(label, "transform scope"))
    value <- result$estimation$likelihood
    assert(value$nobs == observed && value$neffectiveobs == expected[["m"]] && value$nparams == 1L,
           paste(label, "sample sizes and variance-only parameter count"))
    close(value$ssq, expected[["q"]], paste(label, "sum of squares"))
    close(value$ll, expected[["profile"]], paste(label, "profiled raw likelihood"))
    close(value$ll, dk_profile[["ll"]], paste(label, "direct DK profile agreement"))
    # Bounded empirical convention: complete d=1 log-walk only. Do not extend
    # this selection rule to missing data, regressions or automatic transforms.
    correction <- if (data$log) -sum(log(data$y[-1L])) else 0
    close(value$adjustedll - value$ll, correction, paste(label, "adjusted likelihood Jacobian"))
    maximum_error[["jd_profile"]] <- max(maximum_error[["jd_profile"]], abs(value$ll - expected[["profile"]]))
    cases <- cases + 1L
  }
  # Compare original untransformed observations. Log-Jacobian integration belongs
  # to the later regression/transform pipeline and is deliberately not asserted
  # as native adjustedll here.
  if (!is.null(native) && !data$log) {
    value <- native(as.numeric(data$y), ar = numeric(), ma = if (is.null(data$theta)) numeric() else data$theta,
      seasonal_ar = numeric(), seasonal_ma = numeric(), period = period,
      d = data$d, seasonal_d = data$bd, mean = 0, variance = 1)
    assert(identical(value$likelihood_status, "available") && identical(value$profile_status, "available"),
           paste(id, "native likelihood status"))
    assert(value$observed_count == observed && value$diffuse_count == diffuse &&
      value$finite_count == expected[["m"]] && value$initial_diffuse_rank == diffuse &&
      value$remaining_diffuse_rank == 0, paste(id, "native observation counts and exhausted diffuse rank"))
    close(value$sum_squared_standardized, dk_profile[["q"]], paste(id, "native sum of squares"), TRUE)
    close(value$sum_log_finite_variances, dk_profile[["finite_logdet"]], paste(id, "native finite determinant"), TRUE)
    close(value$sum_log_diffuse_variances, dk_profile[["diffuse_logdet"]], paste(id, "native diffuse determinant"), TRUE)
    close(value$profiled_log_likelihood, dk_profile[["ll"]], paste(id, "native profiled likelihood"), TRUE)
    close(value$fixed_log_likelihood, dk_fixed[["ll"]], paste(id, "native unit-variance likelihood"), TRUE)
    maximum_error[["native_profile"]] <- max(maximum_error[["native_profile"]], abs(value$profiled_log_likelihood - dk_profile[["ll"]]))
    maximum_error[["native_fixed"]] <- max(maximum_error[["native_fixed"]], abs(value$fixed_log_likelihood - dk_fixed[["ll"]]))
    native_cases <- native_cases + 1L
  }
  cat("PASS", id, "both direct wrappers, public DK and independent probability identities\n")
}
assert(identical(parent_before, extended_parent_hashes(base_dir)), "original reference corpus unchanged")
cat("PASS:", cases, "direct fixed-specification fits;", native_cases, "native comparisons;", checks, "checks\n")
print(maximum_error[if (is.null(native)) c("jd_profile", "jd_fixed") else names(maximum_error)], digits = 8)

if ("--second-order" %in% options) {
  # Additional differential checks against public DK, separate from the closed-
  # form identities above. They exercise nonzero diffuse corrections and the
  # structural zero certificates. No new numerical tolerance is introduced.
  before_checks <- checks
  accepted <- rejected <- nonzero_corrections <- 0L
  second_errors <- c(profile = 0, fixed = 0, finite_logdet = 0, diffuse_logdet = 0)
  orders <- list(c(d = 0L, D = 2L), c(d = 2L, D = 0L), c(d = 2L, D = 1L))
  masks <- list(complete = integer(), internal = c(20L, 21L, 44L),
    edges = c(1L, 2L, 119L, 120L), early = c(1L, 3L, 5L, 6L, 16L, 21L, 23L, 86L))
  for (period in c(4L, 12L)) for (order in orders) for (mask in names(masks)) {
    t <- seq_len(120L)
    y <- sin(t * sqrt(2)) + 0.5 * cos(t * sqrt(3))
    # Generate inputs by repeated seasonal accumulation and ordinary cumulative
    # sums, with zero prehistory. No fitted output determines the input values.
    for (j in seq_len(order[["D"]])) {
      for (i in seq.int(period + 1L, length(y))) y[i] <- y[i] + y[i - period]
    }
    for (j in seq_len(order[["d"]])) y <- cumsum(y)
    y[masks[[mask]]] <- NA_real_
    data <- list(y = ts(y, start = c(2000L, 1L), frequency = period),
      d = order[["d"]], bd = order[["D"]], theta = NULL, log = FALSE)
    id <- paste(period, paste0("d", data$d), paste0("D", data$bd), mask, sep = "-")
    observed <- sum(!is.na(y))
    diffuse <- data$d + period * data$bd
    jd_profile <- dk_likelihood(data, TRUE)
    jd_fixed <- dk_likelihood(data, FALSE)
    assert(jd_profile[["observed"]] == observed && jd_profile[["diffuse"]] == diffuse &&
      jd_profile[["legacy"]] == 0, paste(id, "second-order DK counts and convention"))
    close(jd_fixed[names(jd_profile) != "ll"], jd_profile[names(jd_profile) != "ll"],
          paste(id, "DK fixed/profile intermediates"))
    value <- tryCatch(native(as.numeric(data$y), numeric(), numeric(), numeric(), numeric(),
      period, data$d, data$bd, mean = 0, variance = 1), error = identity)
    if (inherits(value, "error")) {
      # One observed qualification limit. Accept only this case and this explicit
      # rejection, not arbitrary failures or a silently truncated likelihood.
      assert(period == 4L && data$d == 2L && data$bd == 1L && mask == "early" &&
        grepl("^numerically ambiguous diffuse innovation;", conditionMessage(value)),
        paste(id, "unexpected second-order native rejection:", conditionMessage(value)))
      rejected <- rejected + 1L
      cat("QUALIFICATION LIMIT", id, ": explicit numerically ambiguous diffuse innovation rejection\n")
      next
    }
    assert(identical(value$likelihood_status, "available") && identical(value$profile_status, "available"),
           paste(id, "second-order native likelihood status"))
    assert(value$observed_count == observed && value$diffuse_count == diffuse &&
      value$finite_count == observed - diffuse && value$initial_diffuse_rank == diffuse &&
      value$remaining_diffuse_rank == 0, paste(id, "second-order native counts and ranks"))
    close(value$sum_squared_standardized, jd_profile[["q"]], paste(id, "second-order sum of squares"), TRUE)
    close(value$sum_log_finite_variances, jd_profile[["finite_logdet"]], paste(id, "second-order finite determinant"), TRUE)
    close(value$sum_log_diffuse_variances, jd_profile[["diffuse_logdet"]], paste(id, "second-order diffuse determinant"), TRUE)
    close(value$profiled_log_likelihood, jd_profile[["ll"]], paste(id, "second-order profiled likelihood"), TRUE)
    close(value$fixed_log_likelihood, jd_fixed[["ll"]], paste(id, "second-order fixed likelihood"), TRUE)
    second_errors <- pmax(second_errors, abs(c(
      profile = value$profiled_log_likelihood - jd_profile[["ll"]],
      fixed = value$fixed_log_likelihood - jd_fixed[["ll"]],
      finite_logdet = value$sum_log_finite_variances - jd_profile[["finite_logdet"]],
      diffuse_logdet = value$sum_log_diffuse_variances - jd_profile[["diffuse_logdet"]])))
    accepted <- accepted + 1L
    if (abs(jd_profile[["diffuse_logdet"]]) > 1e-10) nonzero_corrections <- nonzero_corrections + 1L
    cat("PASS second-order differential", id, "\n")
  }
  assert(accepted + rejected == 24L && accepted >= 23L, "second-order probe accounting")
  assert(identical(parent_before, extended_parent_hashes(base_dir)), "original corpus unchanged after second-order probes")
  cat("PASS: second-order public-DK differential checks;", accepted, "accepted;", rejected,
      "explicit qualification limits;", nonzero_corrections, "nonzero diffuse corrections;",
      checks - before_checks, "checks\n")
  print(second_errors, digits = 8)
}
