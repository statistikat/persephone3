# Rscript tools/reference/verify-diffuse.R [--native [--source] [--second-order] [--sparse]]
# Read-only probes. --native additionally requires an installed development
# persephone3 containing .native_sarima_likelihood; choose its library with R_LIBS.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][[1]])
tooling <- dirname(normalizePath(script))
options <- commandArgs(trailingOnly = TRUE)
if (any(!options %in% c("--native", "--source", "--second-order", "--sparse")) ||
    (any(c("--source", "--second-order", "--sparse") %in% options) && !"--native" %in% options))
  stop("Usage: verify-diffuse.R [--native [--source] [--second-order] [--sparse]]")
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
  coefficients <- c(-as.numeric(data$phi), data$theta, -as.numeric(data$bphi), data$btheta)
  spec <- rjd3toolkit::set_arima(spec, mean = NULL, p = length(data$phi), d = data$d,
    q = length(data$theta), bp = length(data$bphi), bd = data$bd, bq = length(data$btheta),
    coef = if (length(coefficients)) coefficients else NULL,
    coef.type = if (length(coefficients)) rep("Fixed", length(coefficients)) else NULL)
  spec <- rjd3toolkit::set_transform(spec, fun = if (data$log) "Log" else "None")
  spec <- rjd3toolkit::set_automodel(spec, enabled = FALSE)
  if (method == "tramoseats") spec <- rjd3toolkit::set_estimate(spec, exact.ml = TRUE)
  spec
}

# Only public Java entry points are invoked. javap -public was used to inspect
# signatures; no Java numerical method bodies were copied or translated.
dk_likelihood <- function(data, profile) {
  # Native/R AR coefficients enter 1-phi(B); JD+ phi/bphi are the polynomial
  # coefficients themselves. MA coefficients use the same positive sign.
  model <- rjd3toolkit::sarima_model(period = frequency(data$y),
    phi = if (length(data$phi)) -data$phi else NULL, d = data$d, bd = data$bd, theta = data$theta,
    bphi = if (length(data$bphi)) -data$bphi else NULL, btheta = data$btheta)
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
  ar_acf <- rjd3toolkit::sarima_properties(rjd3toolkit::sarima_model(period = 4L, phi = -0.3), nacf = 4L)$acf
  seasonal_acf <- rjd3toolkit::sarima_properties(rjd3toolkit::sarima_model(period = 4L, bphi = -0.25), nacf = 4L)$acf
  ma_acf <- rjd3toolkit::sarima_properties(rjd3toolkit::sarima_model(period = 4L, theta = 0.2), nacf = 4L)$acf
  close(ar_acf[2L] / ar_acf[1L], 0.3, "public JD+ regular AR sign convention")
  close(seasonal_acf[5L] / seasonal_acf[1L], 0.25, "public JD+ seasonal AR sign convention")
  close(ma_acf[2L] / ma_acf[1L], 0.2 / 1.04, "public JD+ MA sign convention")
  accepted <- excluded_by_state_bound <- nonzero_corrections <- determinant_limits <- 0L
  # Pinned public-DK intermediate accuracy limits, established against
  # independent complete-data densities and direct preprocessing likelihoods.
  # This list is explicit: no newly failing comparison is automatically exempted.
  dk_determinant_limits <- paste0("12-d2-D2-", c("white-complete", "white-edges",
    "ma-complete", "ma-internal", "ma-edges", "arma-complete", "arma-internal",
    "arma-edges", "arma-early"))
  second_errors <- c(profile = 0, fixed = 0, finite_logdet = 0, diffuse_logdet = 0)
  orders <- expand.grid(d = 0:2, D = 0:2)
  coefficients <- list(white = list(), ma = list(theta = 0.4),
    arma = list(phi = 0.3, theta = 0.2), airline = list(theta = 0.2, btheta = -0.4))
  masks <- list(complete = integer(), internal = c(20L, 21L, 44L),
    edges = c(1L, 2L, 119L, 120L), early = c(1L, 3L, 5L, 6L, 16L, 21L, 23L, 86L),
    seasonal_phase = integer(), block = integer())
  for (period in c(4L, 12L)) for (row in seq_len(nrow(orders))) for (model in names(coefficients)) {
    order <- orders[row, ]
    coef <- coefficients[[model]]
    diffuse <- order$d + period * order$D
    states <- max(1L, length(coef$phi) + period * length(coef$bphi),
                  1L + length(coef$theta) + period * length(coef$btheta)) + diffuse
    # Exclude by an independent declared dimension calculation, never by
    # catching a failed numerical evaluation and treating it as a pass.
    if (states > 32L) {
      excluded_by_state_bound <- excluded_by_state_bound + length(masks)
      next
    }
    masks$seasonal_phase <- c(1L, 1L + period, 1L + 2L * period, 2L + period, 2L + 2L * period)
    masks$block <- seq.int(2L * period + 1L, 3L * period + 3L)
    for (mask in names(masks)) {
    t <- seq_len(120L)
    # Bounded values avoid conflating structural observability with overflowing
    # high-order accumulated levels. A fixed model can evaluate any input.
    y <- sin(t * sqrt(2)) + 0.5 * cos(t * sqrt(3)) + t / 40
    y[masks[[mask]]] <- NA_real_
    data <- list(y = ts(y, start = c(2000L, 1L), frequency = period),
      d = order$d, bd = order$D, phi = coef$phi, theta = coef$theta,
      bphi = coef$bphi, btheta = coef$btheta, log = FALSE)
    id <- paste(period, paste0("d", data$d), paste0("D", data$bd), model, mask, sep = "-")
    observed <- sum(!is.na(y))
    diffuse <- data$d + period * data$bd
    jd_profile <- dk_likelihood(data, TRUE)
    jd_fixed <- dk_likelihood(data, FALSE)
    assert(jd_profile[["observed"]] == observed && jd_profile[["diffuse"]] == diffuse &&
      jd_profile[["legacy"]] == 0, paste(id, "second-order DK counts and convention"))
    close(jd_fixed[names(jd_profile) != "ll"], jd_profile[names(jd_profile) != "ll"],
          paste(id, "DK fixed/profile intermediates"))
    value <- tryCatch(native(as.numeric(data$y), as.numeric(data$phi), as.numeric(data$theta),
      as.numeric(data$bphi), as.numeric(data$btheta), period, data$d, data$bd, mean = 0, variance = 1),
      error = function(e) stop(id, ": ", conditionMessage(e), call. = FALSE))
    assert(identical(value$likelihood_status, "available") && identical(value$profile_status, "available"),
           paste(id, "second-order native likelihood status"))
    assert(value$observed_count == observed && value$diffuse_count == diffuse &&
      value$finite_count == observed - diffuse && value$initial_diffuse_rank == diffuse &&
      value$remaining_diffuse_rank == 0, paste(id, "second-order native counts and ranks"))
    close(value$sum_squared_standardized, jd_profile[["q"]], paste(id, "second-order sum of squares"), TRUE)
    if (id %in% dk_determinant_limits) {
      determinant_limits <- determinant_limits + 1L
      cat("ORACLE LIMIT", id, "public DK finite-logdet difference =",
          format(value$sum_log_finite_variances - jd_profile[["finite_logdet"]], digits = 8),
          "; alternative checks below are mandatory\n")
    } else close(value$sum_log_finite_variances, jd_profile[["finite_logdet"]], paste(id, "second-order finite determinant"), TRUE)
    close(value$sum_log_diffuse_variances, jd_profile[["diffuse_logdet"]], paste(id, "second-order diffuse determinant"), TRUE)
    close(value$profiled_log_likelihood, jd_profile[["ll"]], paste(id, "second-order profiled likelihood"), TRUE)
    close(value$fixed_log_likelihood, jd_fixed[["ll"]], paste(id, "second-order fixed likelihood"), TRUE)
    if (period == 12L && data$d == 2L && data$bd == 2L) {
      # Apply this alternate validation uniformly to all 18 models/masks in the
      # affected high-order class, not only when a DK comparison happens to fail.
      for (method in c("x13", "tramoseats")) {
        spec <- fixed_spec(method, data)
        result <- if (method == "x13") rjd3x13::regarima_fast(data$y, spec) else rjd3tramoseats::tramo_fast(data$y, spec)
        assert(length(result$description$variables) == 0L && length(result$estimation$b) == 0L &&
          length(result$estimation$parameters$val) == 0L, paste(id, method, "alternate oracle remains fully fixed"))
        for (name in c("phi", "theta", "bphi", "btheta")) {
          expected_coef <- if (name %in% c("phi", "bphi")) -as.numeric(data[[name]]) else as.numeric(data[[name]])
          actual_coef <- result$description$arima[[name]]
          assert(if (length(expected_coef)) identical(as.numeric(actual_coef["value", ]), expected_coef) &&
            all(as.character(actual_coef["type", ]) == "FIXED") else is.null(actual_coef),
            paste(id, method, name, "alternate oracle coefficient state"))
        }
        raw <- result$estimation$likelihood
        assert(raw$nobs == observed && raw$neffectiveobs == observed - diffuse && raw$nparams == 1L,
               paste(id, method, "alternate oracle counts"))
        total_logdet <- -2 * raw$ll - raw$neffectiveobs * (log(2 * pi) + 1 + log(raw$ssq / raw$neffectiveobs))
        close(value$sum_log_finite_variances + value$sum_log_diffuse_variances, total_logdet,
              paste(id, method, "alternate total determinant"), TRUE)
        close(value$sum_squared_standardized, raw$ssq, paste(id, method, "alternate sum of squares"), TRUE)
        close(value$profiled_log_likelihood, raw$ll, paste(id, method, "alternate profiled likelihood"), TRUE)
      }
      if (mask %in% c("complete", "edges")) {
        trimmed <- data$y[seq.int(min(which(!is.na(data$y))), max(which(!is.na(data$y))))]
        w <- diff(diff(trimmed, differences = 2L), lag = 12L, differences = 2L)
        phi <- if (length(data$phi)) data$phi else 0
        theta <- if (length(data$theta)) data$theta else 0
        gamma0 <- (1 + theta^2 + 2 * phi * theta) / (1 - phi^2)
        gamma1 <- phi * gamma0 + theta
        covariance <- toeplitz(c(gamma0, phi^(seq_len(length(w) - 1L) - 1L) * gamma1))
        expected_q <- drop(crossprod(w, solve(covariance, w)))
        expected_logdet <- 2 * sum(log(diag(chol(covariance))))
        close(value$sum_squared_standardized, expected_q, paste(id, "independent differenced covariance sum of squares"), TRUE)
        close(value$sum_log_finite_variances, expected_logdet, paste(id, "independent differenced covariance determinant"), TRUE)
        close(value$sum_log_diffuse_variances, 0, paste(id, "canonical complete/edge diffuse determinant"), TRUE)
      }
    }
    second_errors <- pmax(second_errors, abs(c(
      profile = value$profiled_log_likelihood - jd_profile[["ll"]],
      fixed = value$fixed_log_likelihood - jd_fixed[["ll"]],
      finite_logdet = value$sum_log_finite_variances - jd_profile[["finite_logdet"]],
      diffuse_logdet = value$sum_log_diffuse_variances - jd_profile[["diffuse_logdet"]])))
    accepted <- accepted + 1L
    if (abs(jd_profile[["diffuse_logdet"]]) > 1e-10) nonzero_corrections <- nonzero_corrections + 1L
    }
    cat("PASS second-order differential", period, paste0("d", data$d), paste0("D", data$bd),
        model, "all six missingness masks\n")
  }
  assert(accepted == 414L && excluded_by_state_bound == 18L && determinant_limits == 9L, "second-order probe accounting")
  assert(identical(parent_before, extended_parent_hashes(base_dir)), "original corpus unchanged after second-order probes")
  cat("PASS: second-order public-DK differential checks;", accepted, "accepted;", excluded_by_state_bound,
      "excluded by the declared 32-state bound;", determinant_limits, "explicit DK determinant oracle limits;",
      nonzero_corrections, "nonzero diffuse corrections;",
      checks - before_checks, "checks\n")
  print(second_errors, digits = 8)
}


if ("--sparse" %in% options) {
  # This matrix was qualified against independent Gaussian integration and both
  # fixed preprocessors before comparing native output. Its cases are declared
  # prospectively: errors are failures, not grounds for dropping a fixture.
  before_checks <- checks
  sparse_cases <- sparse_direct <- unidentified_cases <- 0L
  sparse_errors <- c(fixed = 0, profile = 0, q = 0, total_logdet = 0,
                     direct_profile = 0, dk_profile = 0)
  sparse_gaussian <- function(data) {
    y <- as.numeric(data$y)
    operator <- diag(length(y))
    operator <- diff(operator, differences = data$d)
    operator <- diff(operator, lag = frequency(data$y), differences = data$bd)
    # Exact differences have the stationary ARMA(1,1) covariance. White and
    # MA(1) are the phi=0/theta=0 special cases. This integrates missing levels
    # directly, without an initial-variance approximation or Kalman recursion.
    phi <- if (length(data$phi)) data$phi else 0
    theta <- if (length(data$theta)) data$theta else 0
    gamma0 <- (1 + theta^2 + 2 * phi * theta) / (1 - phi^2)
    gamma1 <- phi * gamma0 + theta
    count <- nrow(operator)
    covariance <- toeplitz(c(gamma0, phi^(seq_len(count - 1L) - 1L) * gamma1))
    factor <- chol(covariance)
    whitened <- forwardsolve(t(factor), operator)
    missing <- which(is.na(y))
    observed <- which(!is.na(y))
    design <- whitened[, missing, drop = FALSE]
    signal <- drop(whitened[, observed, drop = FALSE] %*% y[observed])
    singular <- svd(design, nu = 0, nv = 0)$d
    assert(length(singular) == length(missing) &&
      min(singular) > max(singular) * 1e-10,
      "sparse Gaussian missing-coordinate design is independently well conditioned")
    decomposition <- qr(design, LAPACK = TRUE)
    degrees <- nrow(operator) - length(missing)
    assert(degrees > 0, "sparse Gaussian integration has positive residual dimension")
    complement <- qr.Q(decomposition, complete = TRUE)[,
      length(missing) + seq_len(degrees), drop = FALSE]
    q <- sum(crossprod(complement, signal)^2)
    logdet <- 2 * (sum(log(diag(factor))) +
                    sum(log(abs(diag(qr.R(decomposition))))))
    likelihood(degrees, q, logdet)
  }
  for (n in c(90L, 180L, 360L)) for (period in c(4L, 12L)) for (D in 1:2) {
    diffuse <- 2L + period * D
    sparse_rows <- seq.int(1L, n, by = 5L * period)
    # Same-phase observations are polynomials of degree D+1. Their independent
    # Vandermonde rank supplies an oracle for the unresolved initial dimension.
    knots <- sparse_rows / n
    vandermonde <- outer(knots, 0:(D + 1L), `^`)
    singular <- svd(vandermonde, nu = 0, nv = 0)$d
    assert(min(singular) > max(singular) * 1e-10,
           "sparse-only Vandermonde rows/columns have a separated full rank")
    identified <- min(length(sparse_rows), D + 2L)
    unresolved <- rep(NA_real_, n)
    unresolved[sparse_rows] <- sin(sparse_rows * sqrt(2)) + cos(sparse_rows * sqrt(3)) / 2
    value <- native(unresolved, numeric(), numeric(), numeric(), numeric(),
                    period, 2L, D, mean = 0, variance = 1)
    assert(value$initial_diffuse_rank == diffuse &&
      value$remaining_diffuse_rank == diffuse - identified &&
      value$diffuse_count == identified && value$observed_count == length(sparse_rows),
      "sparse-only native rank agrees with the independent polynomial design")
    assert(identical(value$likelihood_status, "unresolved_diffuse") &&
      identical(value$profile_status, "unresolved_diffuse") &&
      is.na(value$fixed_log_likelihood) && is.na(value$profiled_log_likelihood),
      "sparse-only unidentified directions never produce an optimizer objective")
    unidentified_cases <- unidentified_cases + 1L

    tail_length <- if (period == 4L && D == 1L) 4L else diffuse
    observed_rows <- sort(unique(c(sparse_rows, seq.int(n - tail_length + 1L, n))))
    for (model in c("white", "ma", "arma")) {
      y <- rep(NA_real_, n)
      y[observed_rows] <- sin(observed_rows * sqrt(2)) + cos(observed_rows * sqrt(3)) / 2
      data <- list(y = ts(y, start = c(2000L, 1L), frequency = period), d = 2L,
        bd = D, phi = if (model == "arma") 0.3 else NULL,
        theta = if (model != "white") 0.4 else NULL, log = FALSE)
      id <- paste("sparse", n, period, paste0("d2-D", D), model, sep = "-")
      expected <- sparse_gaussian(data)
      observed <- length(observed_rows)
      jd_fixed <- dk_likelihood(data, FALSE)
      jd_profile <- dk_likelihood(data, TRUE)
      assert(jd_fixed[["observed"]] == observed && jd_fixed[["diffuse"]] == diffuse &&
        jd_fixed[["legacy"]] == 0, paste(id, "DK sample counts and normalization"))
      close(jd_fixed[["ll"]], expected[["fixed"]], paste(id, "DK fixed density"), TRUE)
      close(jd_profile[["ll"]], expected[["profile"]], paste(id, "DK profiled density"), TRUE)
      close(jd_fixed[["q"]], expected[["q"]], paste(id, "DK quadratic form"), TRUE)
      close(sum(jd_fixed[c("finite_logdet", "diffuse_logdet")]), expected[["logdet"]],
            paste(id, "DK total determinant"), TRUE)
      sparse_errors[["dk_profile"]] <- max(sparse_errors[["dk_profile"]],
        abs(jd_profile[["ll"]] - expected[["profile"]]))
      for (method in c("x13", "tramoseats")) {
        spec <- fixed_spec(method, data)
        result <- if (method == "x13") rjd3x13::regarima_fast(data$y, spec) else rjd3tramoseats::tramo_fast(data$y, spec)
        assert(length(result$description$variables) == 0L && length(result$estimation$b) == 0L &&
          length(result$estimation$parameters$val) == 0L,
          paste(id, method, "no estimated coefficients or regression effects"))
        arima <- result$description$arima
        assert(arima$d == 2L && arima$bd == D && arima$period == period,
               paste(id, method, "fixed orders"))
        for (name in c("phi", "theta", "bphi", "btheta")) {
          coef <- if (name %in% c("phi", "bphi")) -as.numeric(data[[name]]) else as.numeric(data[[name]])
          actual <- arima[[name]]
          assert(if (length(coef)) identical(as.numeric(actual["value", ]), coef) &&
            all(as.character(actual["type", ]) == "FIXED") else is.null(actual),
            paste(id, method, name, "fixed coefficient state"))
        }
        raw <- result$estimation$likelihood
        assert(raw$nobs == observed && raw$neffectiveobs == expected[["m"]] && raw$nparams == 1L,
               paste(id, method, "effective sample and variance-only parameter count"))
        close(raw$ll, expected[["profile"]], paste(id, method, "independent Gaussian profile"))
        close(raw$ssq, expected[["q"]], paste(id, method, "independent Gaussian quadratic form"))
        close(raw$adjustedll, raw$ll, paste(id, method, "identity adjustment"))
        sparse_errors[["direct_profile"]] <- max(sparse_errors[["direct_profile"]],
          abs(raw$ll - expected[["profile"]]))
        sparse_direct <- sparse_direct + 1L
      }
      value <- native(as.numeric(y), as.numeric(data$phi), as.numeric(data$theta),
                      numeric(), numeric(), period, 2L, D, mean = 0, variance = 1)
      assert(identical(value$likelihood_status, "available") &&
        identical(value$profile_status, "available"), paste(id, "native objective status"))
      assert(value$observed_count == observed && value$initial_diffuse_rank == diffuse &&
        value$remaining_diffuse_rank == 0 && value$diffuse_count == diffuse &&
        value$finite_count == expected[["m"]], paste(id, "native identified sample counts"))
      actual <- c(fixed = value$fixed_log_likelihood, profile = value$profiled_log_likelihood,
        q = value$sum_squared_standardized,
        total_logdet = value$sum_log_finite_variances + value$sum_log_diffuse_variances)
      target <- c(fixed = expected[["fixed"]], profile = expected[["profile"]],
                  q = expected[["q"]], total_logdet = expected[["logdet"]])
      close(actual, target, paste(id, "native independent Gaussian integration"), TRUE)
      close(value$profiled_variance, expected[["q"]] / expected[["m"]],
            paste(id, "native profile scale"), TRUE)
      sparse_errors[names(actual)] <- pmax(sparse_errors[names(actual)], abs(actual - target))
      sparse_cases <- sparse_cases + 1L
      cat("PASS", id, "independent Gaussian density, both fixed preprocessors and public DK\n")
    }
  }
  assert(sparse_cases == 36L && sparse_direct == 72L && unidentified_cases == 12L,
         "sparse probe accounting")
  assert(identical(parent_before, extended_parent_hashes(base_dir)),
         "original corpus unchanged after sparse probes")
  cat("PASS:", sparse_cases, "identified sparse native cases;", sparse_direct,
      "direct fixed fits;", unidentified_cases, "unidentified sparse controls;",
      checks - before_checks, "checks\n")
  print(sparse_errors, digits = 8)
}
