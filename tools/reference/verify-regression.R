# Rscript tools/reference/verify-regression.R [--native [--source]]
# Read-only fixed-ARMA regression probes; original fixtures/tolerances are immutable.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][[1L]])
tooling <- dirname(normalizePath(script))
options <- commandArgs(trailingOnly = TRUE)
if (any(!options %in% c("--native", "--source")) ||
    ("--source" %in% options && !"--native" %in% options))
  stop("Usage: verify-regression.R [--native [--source]]")
source(file.path(tooling, "common.R"))
source(file.path(tooling, "extended-fixtures.R"))
root <- reference_root()
base_dir <- reference_fixture_dir(root)
parent_before <- extended_parent_hashes(base_dir)
extended_assert_oracle(base_dir)

checks <- direct_fits <- 0L
assert <- function(value, label) {
  if (!isTRUE(value)) stop("Regression probe failed: ", label, call. = FALSE)
  checks <<- checks + 1L
}
close <- function(actual, expected, label, native_check = FALSE) {
  # Inherited bounded-fixture roundoff budgets, declared before comparison.
  # These do not qualify general production tolerances or change a stored rule.
  tolerance <- if (native_check) 2e-8 else 1e-10
  assert(length(actual) == length(expected) && all(is.finite(actual)) &&
    all(is.finite(expected)) && all(abs(actual - expected) <= tolerance * (1 + abs(expected))), label)
}

fixed_spec <- function(method, d, D, phi, theta, transformation, estimate_mean = FALSE) {
  spec <- if (method == "x13") rjd3x13::regarima_spec("rg0") else rjd3tramoseats::tramo_spec("tr0")
  # JD+ phi is the AR polynomial coefficient; the native/R AR convention negates it.
  coef <- c(-phi, theta)
  spec <- rjd3toolkit::set_arima(spec, mean = if (estimate_mean) 0 else NULL,
    mean.type = "Undefined", p = length(phi), d = d, q = length(theta),
    bp = 0L, bd = D, bq = 0L, coef = if (length(coef)) coef else NULL,
    coef.type = if (length(coef)) rep("Fixed", length(coef)) else NULL)
  spec <- rjd3toolkit::set_automodel(spec, enabled = FALSE)
  spec <- rjd3toolkit::set_transform(spec, fun = if (transformation == "log") "Log" else "None")
  if (method == "tramoseats") spec <- rjd3toolkit::set_estimate(spec, exact.ml = TRUE)
  spec
}
fit <- function(method, y, spec, columns = NULL) {
  direct_fits <<- direct_fits + 1L
  context <- if (length(columns)) rjd3toolkit::modelling_context(variables = list(r = columns)) else NULL
  if (method == "x13") rjd3x13::regarima_fast(y, spec, context = context) else
    rjd3tramoseats::tramo_fast(y, spec, context = context)
}
add_columns <- function(spec, columns, coefficients) {
  for (i in seq_along(columns)) spec <- rjd3toolkit::add_usrdefvar(spec,
    name = names(columns)[i], coef = if (is.na(coefficients[i])) NULL else coefficients[i],
    regeffect = "Trend")
  spec
}

# Independently derive the flat-history density from exact differencing and a
# dense stationary ARMA(1,1) Toeplitz covariance. Missing y coordinates are
# integrated by QR before free regression coefficients are concentrated out.
# No native model, state recursion, estimated JD+ coefficient, or large prior is used.
gaussian_regression <- function(y, x, coefficients, period, d, D, phi, theta) {
  n <- length(y)
  operator <- diag(n)
  if (d) operator <- diff(operator, differences = d)
  if (D) operator <- diff(operator, lag = period, differences = D)
  m <- nrow(operator)
  ar <- if (length(phi)) phi else 0
  ma <- if (length(theta)) theta else 0
  gamma0 <- (1 + ma^2 + 2 * ar * ma) / (1 - ar^2)
  gamma1 <- ar * gamma0 + ma
  covariance <- toeplitz(c(gamma0, ar^(seq_len(m - 1L) - 1L) * gamma1))
  factor <- chol(covariance)
  white <- forwardsolve(t(factor), operator)
  missing <- which(is.na(y)); observed <- which(!is.na(y))
  fixed <- which(!is.na(coefficients)); free <- which(is.na(coefficients))
  response <- y[observed]
  if (length(fixed)) response <- response - drop(x[observed, fixed, drop = FALSE] %*% coefficients[fixed])
  w_response <- drop(white[, observed, drop = FALSE] %*% response)
  w_design <- white[, observed, drop = FALSE] %*% x[observed, free, drop = FALSE]
  logdet <- 2 * sum(log(diag(factor)))
  if (length(missing)) {
    design <- white[, missing, drop = FALSE]
    singular <- svd(design, nu = 0, nv = 0)$d
    stopifnot(length(singular) == length(missing), min(singular) > max(singular) * 1e-10)
    qr_missing <- qr(design, LAPACK = TRUE)
    logdet <- logdet + 2 * sum(log(abs(diag(qr.R(qr_missing)))))
    complement <- qr.Q(qr_missing, complete = TRUE)[,
      length(missing) + seq_len(m - length(missing)), drop = FALSE]
    w_response <- drop(crossprod(complement, w_response))
    w_design <- crossprod(complement, w_design)
  }
  finite <- length(w_response)
  beta <- numeric()
  covariance_unscaled <- matrix(numeric(), 0, 0)
  if (length(free)) {
    singular <- svd(w_design, nu = 0, nv = 0)$d
    stopifnot(length(singular) == length(free), min(singular) > max(singular) * 1e-10)
    beta <- unname(qr.coef(qr(w_design, LAPACK = TRUE), w_response))
    covariance_unscaled <- chol2inv(chol(crossprod(w_design)))
  }
  residual <- w_response - drop(w_design %*% beta)
  squares <- sum(residual^2)
  scale <- squares / finite
  full <- coefficients
  full[free] <- beta
  list(coefficients = full, free = free, fixed = fixed, rank = length(free),
    finite = finite, residual_df = finite - length(free), squares = squares,
    scale = scale, logdet = logdet, covariance_unscaled = covariance_unscaled,
    covariance_ml = covariance_unscaled * scale,
    covariance_df = covariance_unscaled * (squares / (finite - length(free))),
    fixed_ll = -.5 * (finite * log(2 * pi) + logdet + squares),
    profile_ll = -.5 * (finite * (log(2 * pi) + 1 + log(scale)) + logdet))
}

cases <- list()
orders <- expand.grid(d = 0:2, D = 0:2)
for (period in c(4L, 12L)) for (order in seq_len(nrow(orders))) {
  d <- orders$d[order]; D <- orders$D[order]
  for (model in c("white", "arma")) for (mask in c("complete", "gaps")) {
    for (transformation in c("none", "log")) for (layout in c("mixed", "free", "fixed")) {
      time <- seq_len(120L)
      columns <- list(fixed = ts(sin(time / 7) + .1 * cos(time * sqrt(2)), start = c(2000, 1), frequency = period),
        one = ts(cos(time / 13) + sin(time * sqrt(3)), start = c(2000, 1), frequency = period),
        two = ts(sin(time * sqrt(5)) + .2 * cos(time / 17), start = c(2000, 1), frequency = period))
      signal <- 3 + .2 * columns$fixed + .3 * columns$one - .15 * columns$two +
        .04 * sin(time * sqrt(2)) + .01 * cos(time * sqrt(7))
      full <- if (transformation == "log") exp(signal) else signal
      middle <- (d + D + as.integer(model == "arma")) %% 2L == 1L
      first <- if (middle) 2L * period + 1L else 1L
      last <- if (middle) 8L * period else length(full)
      indices <- seq.int(first, last)
      local_missing <- if (mask == "gaps") c(1L, 2L, period + 3L, period + 4L, length(indices)) else integer()
      full[indices[local_missing]] <- NA_real_
      selected <- window(full, start = if (middle) c(2002, 1) else start(full),
                         end = if (middle) c(2007, period) else end(full))
      x <- do.call(cbind, lapply(columns, function(column) as.numeric(column)[indices]))
      coefficients <- switch(layout, mixed = c(.2, NA, NA), free = rep(NA_real_, 3), fixed = c(.2, .3, -.15))
      phi <- if (model == "arma") .3 else numeric()
      theta <- if (model == "arma") .2 else numeric()
      transformed <- if (transformation == "log") log(selected) else selected
      expected <- gaussian_regression(transformed, x, coefficients, period, d, D, phi, theta)
      observed <- which(!is.na(selected)); degree <- d + period * D
      jacobian_rows <- observed[observed >= min(observed) + degree]
      jacobian <- if (transformation == "log") -sum(log(selected[jacobian_rows])) else 0
      id <- paste(period, paste0("d", d), paste0("D", D), model, mask, transformation, layout, sep = "-")
      cases[[id]] <- list(id = id, input = full, selected = selected, columns = columns,
        indices = indices, coefficients = coefficients, period = period, d = d, D = D,
        phi = phi, theta = theta, transformation = transformation, expected = expected,
        estimation_start = if (middle) c(2002, 1) else NULL,
        estimation_end = if (middle) c(2007, period) else NULL, jacobian = jacobian)
    }
  }
}
assert(length(cases) == 432L, "prospective matrix size")
maximum_direct <- c(coefficients = 0, profile = 0, squares = 0, covariance_df = 0, jacobian = 0)
for (case in cases) for (method in c("x13", "tramoseats")) {
  label <- paste(method, case$id)
  spec <- fixed_spec(method, case$d, case$D, case$phi, case$theta, case$transformation)
  spec <- add_columns(spec, case$columns, case$coefficients)
  result <- fit(method, case$selected, spec, case$columns)
  assert(is.list(result) && length(result$description$variables) == 3L, paste(label, "result variables"))
  assert(length(result$estimation$parameters$val) == 0L, paste(label, "ARMA remains fixed"))
  expected <- case$expected
  actual <- vapply(result$description$variables, function(v) v$coef$value, numeric(1))
  names_actual <- vapply(result$description$variables, function(v) v$name, character(1))
  assert(identical(names_actual, paste0("r.", names(case$columns))), paste(label, "coefficient order"))
  types <- vapply(result$description$variables, function(v) v$coef$type, character(1))
  assert(identical(types, ifelse(is.na(case$coefficients), "ESTIMATED", "FIXED")), paste(label, "coefficient states"))
  close(actual, expected$coefficients, paste(label, "independent GLS coefficients"))
  close(result$estimation$b, expected$coefficients[expected$free], paste(label, "estimated-only b"))
  close(result$estimation$bvar, expected$covariance_df, paste(label, "residual-df coefficient covariance"))
  likelihood <- result$estimation$likelihood
  assert(likelihood$nobs == sum(!is.na(case$selected)) && likelihood$neffectiveobs == expected$finite &&
    likelihood$nparams == expected$rank + 1L, paste(label, "observation/parameter counts"))
  close(likelihood$ssq, expected$squares, paste(label, "independent Gaussian sum of squares"))
  close(likelihood$ll, expected$profile_ll, paste(label, "ML profile objective without regression determinant"))
  close(likelihood$adjustedll - likelihood$ll, case$jacobian, paste(label, "original-value calendar Jacobian"))
  maximum_direct <- pmax(maximum_direct, c(coefficients = max(abs(actual - expected$coefficients)),
    profile = abs(likelihood$ll - expected$profile_ll), squares = abs(likelihood$ssq - expected$squares),
    covariance_df = if (length(expected$free)) max(abs(result$estimation$bvar - expected$covariance_df)) else 0,
    jacobian = abs(likelihood$adjustedll - likelihood$ll - case$jacobian)))
}
assert(identical(parent_before, extended_parent_hashes(base_dir)), "original artifacts unchanged after regression probes")
cat("PASS:", length(cases), "regression cases;", direct_fits, "direct fits;", checks, "checks\n")
print(maximum_direct, digits = 8)

# The built-in JD+ mean is compared with a separately supplied deterministic
# column. Delta t^(d+D)/((d+D)! s^D)=1; lower-order terms are homogeneous history.
before_mean_checks <- checks; before_mean_fits <- direct_fits
maximum_mean <- c(coefficient = 0, likelihood = 0, covariance = 0)
mean_cases <- list()
for (period in c(4L, 12L)) for (d in 0:2) for (D in 0:2) {
  for (model in c("white", "arma")) for (mask in c("complete", "gaps")) for (method in c("x13", "tramoseats")) {
    time <- seq_len(120L)
    y <- ts(2 + sin(time * sqrt(2)) + .3 * cos(time * sqrt(7)), start = c(2000, 1), frequency = period)
    if (mask == "gaps") y[c(1L, 2L, 20L, 21L, 44L, 120L)] <- NA_real_
    degree <- d + D
    drift <- ts(time^degree / (factorial(degree) * period^D), start = start(y), frequency = period)
    phi <- if (model == "arma") .3 else numeric(); theta <- if (model == "arma") .2 else numeric()
    mean_spec <- fixed_spec(method, d, D, phi, theta, "none", estimate_mean = TRUE)
    column_spec <- fixed_spec(method, d, D, phi, theta, "none")
    column_spec <- add_columns(column_spec, list(drift = drift), NA_real_)
    a <- fit(method, y, mean_spec)
    b <- fit(method, y, column_spec, list(drift = drift))
    label <- paste("mean/drift", method, period, d, D, model, mask)
    assert(length(a$estimation$b) == 1L && length(b$estimation$b) == 1L, paste(label, "one estimated coefficient"))
    close(a$estimation$b, b$estimation$b, paste(label, "polynomial normalization"))
    close(a$estimation$likelihood$ll, b$estimation$likelihood$ll, paste(label, "objective"))
    close(a$estimation$bvar, b$estimation$bvar, paste(label, "covariance"))
    maximum_mean <- pmax(maximum_mean, c(coefficient = abs(a$estimation$b - b$estimation$b),
      likelihood = abs(a$estimation$likelihood$ll - b$estimation$likelihood$ll),
      covariance = abs(a$estimation$bvar - b$estimation$bvar)))
    id <- paste(period, d, D, model, mask, sep = "-")
    mean_cases[[id]] <- list(y = y, period = period, d = d, D = D, phi = phi, theta = theta,
      coefficients = a$estimation$b, covariance = a$estimation$bvar,
      likelihood = a$estimation$likelihood)
  }
}
cat("PASS:", direct_fits - before_mean_fits, "mean/drift direct fits;", checks - before_mean_checks, "checks\n")
print(maximum_mean, digits = 8)

# These are behavior observations, not a requirement to silently drop columns
# in native code. A requested free coefficient may not be uniquely identified.
before_alias <- checks
for (method in c("x13", "tramoseats")) for (kind in c("zero", "duplicate", "diffuse_constant")) {
  time <- seq_len(120L)
  one <- ts(cos(time / 13) + sin(time * sqrt(3)), start = c(2000, 1), frequency = 4)
  y <- ts(3 * one + sin(time * sqrt(2)), start = start(one), frequency = 4)
  columns <- switch(kind, zero = list(a = one * 0), duplicate = list(a = one, b = 2 * one),
                    diffuse_constant = list(a = one * 0 + 1))
  d <- as.integer(kind == "diffuse_constant")
  spec <- add_columns(fixed_spec(method, d, 0L, numeric(), numeric(), "none"), columns,
                      rep(NA_real_, length(columns)))
  result <- fit(method, y, spec, columns)
  states <- vapply(result$description$variables, function(v) v$coef$type, character(1))
  values <- vapply(result$description$variables, function(v) v$coef$value, numeric(1))
  rank <- if (kind == "duplicate") 1L else 0L
  assert(sum(states == "ESTIMATED") == rank && sum(states == "FIXED") == length(columns) - rank,
         paste(method, kind, "reference drops aliased free columns"))
  assert(all(values[states == "FIXED"] == 0), paste(method, kind, "dropped reference columns become fixed zero"))
  assert(length(result$estimation$b) == rank && result$estimation$likelihood$nparams == rank + 1L,
         paste(method, kind, "reference retained rank"))
}
cat("PASS: 6 alias behavior probes;", checks - before_alias, "checks\n")

before_context <- checks; before_context_fits <- direct_fits
future_ids <- c("4-d1-D0-white-gaps-log-mixed", "12-d1-D0-white-gaps-log-mixed",
                "4-d0-D0-arma-complete-none-mixed", "12-d0-D0-arma-complete-none-mixed")
for (id in future_ids) for (method in c("x13", "tramoseats")) {
  case <- cases[[id]]
  assert(!is.null(case$estimation_end), paste(id, "future-context fixture uses a selected window"))
  spec <- add_columns(fixed_spec(method, case$d, case$D, case$phi, case$theta, case$transformation),
                      case$columns, case$coefficients)
  shifted <- case$columns
  future <- seq_along(case$input) > max(case$indices)
  for (name in names(shifted)) shifted[[name]][future] <- shifted[[name]][future] + 1000
  before <- fit(method, case$selected, spec, case$columns)
  after <- fit(method, case$selected, spec, shifted)
  assert(identical(before$estimation, after$estimation) && identical(before$description, after$description),
         paste(method, id, "future context cannot alter the fitted-window regression"))
}
cat("PASS:", direct_fits - before_context_fits, "future-context direct fits;", checks - before_context, "checks\n")

if ("--native" %in% options) {
  if ("--source" %in% options) {
    if (!requireNamespace("pkgload", quietly = TRUE)) stop("--source requires pkgload and an already compiled development tree")
    pkgload::load_all(root, quiet = TRUE, compile = FALSE)
  }
  if (!requireNamespace("persephone3", quietly = TRUE)) stop("--native requires the development persephone3 installation")
  native <- getFromNamespace("native_regression_likelihood", "persephone3")
  before_native <- checks
  maximum_native <- c(coefficients = 0, profile = 0, fixed = 0, squares = 0,
                       covariance_unscaled = 0, covariance_residual = 0, jacobian = 0)
  for (case in cases) {
    variance <- 1.7
    value <- tryCatch(native(case$input, regressors = case$columns,
      coefficients = setNames(case$coefficients, names(case$columns)), ar = case$phi, ma = case$theta,
      d = case$d, seasonal_d = case$D, variance = variance, transformation = case$transformation,
      estimation_start = case$estimation_start, estimation_end = case$estimation_end),
      error = function(e) stop(case$id, ": ", conditionMessage(e), call. = FALSE))
    expected <- case$expected
    label <- paste("native", case$id)
    assert(inherits(value, "persephone_native_regression_likelihood") &&
      value$schema_version == 1L && value$backend == "native" &&
      value$operation == "fixed_sarima_regression_likelihood" &&
      value$initialization == "fresh_prior_at_span_start", paste(label, "result contract"))
    assert(identical(value$inputs$original, case$selected) &&
      identical(as.integer(value$span$source_rows), case$indices), paste(label, "selected dates/values/source map"))
    assert(identical(names(value$regression$coefficients), names(case$columns)) &&
      identical(unname(value$regression$coefficient_states), ifelse(is.na(case$coefficients), "estimated", "fixed")),
      paste(label, "names and coefficient states"))
    assert(value$regression$rank == expected$rank && value$regression$free_count == expected$rank &&
      value$regression$residual_df == expected$residual_df, paste(label, "rank and residual degrees"))
    assert(value$likelihood$observed_count == sum(!is.na(case$selected)) &&
      value$likelihood$finite_count == expected$finite &&
      value$likelihood$diffuse_count == case$d + case$period * case$D &&
      value$likelihood$nparams_profiled == expected$rank + 1L, paste(label, "counts"))
    assert(value$likelihood$status == "available" && value$likelihood$profile_status == "available" &&
      value$likelihood$objective == "concentrated_ml" &&
      value$likelihood$normalization == "fixed_initial_diffuse_coordinates", paste(label, "objective/status"))
    close(value$regression$coefficients, expected$coefficients, paste(label, "Gaussian coefficients"), TRUE)
    close(value$regression$unscaled_covariance, expected$covariance_unscaled, paste(label, "unscaled covariance"), TRUE)
    close(value$regression$covariance_fixed_scale, expected$covariance_unscaled * variance,
          paste(label, "fixed-scale covariance"), TRUE)
    close(value$regression$covariance_ml, expected$covariance_ml, paste(label, "ML covariance"), TRUE)
    close(value$regression$covariance_residual, expected$covariance_df, paste(label, "residual-df covariance"), TRUE)
    close(value$regression_kernel$sum_squared_standardized, expected$squares, paste(label, "Gaussian Q"), TRUE)
    close(value$likelihood$profiled_variance, expected$scale, paste(label, "ML scale denominator"), TRUE)
    expected_fixed <- -.5 * (expected$finite * log(2 * pi * variance) + expected$logdet + expected$squares / variance)
    close(value$likelihood$transformed_fixed, expected_fixed, paste(label, "fixed-variance likelihood"), TRUE)
    close(value$likelihood$transformed_profiled, expected$profile_ll, paste(label, "profile likelihood"), TRUE)
    close(value$adjustment$log_jacobian, case$jacobian, paste(label, "calendar Jacobian"), TRUE)
    close(value$likelihood$adjusted_profiled, expected$profile_ll + case$jacobian, paste(label, "adjusted profile"), TRUE)
    close(sum(value$regression_kernel$whitened_residuals^2), expected$squares, paste(label, "whitened residual sum"), TRUE)
    close(value$kernel$sum_squared_standardized, expected$squares, paste(label, "re-filtered residual sum"), TRUE)
    assert(all(vapply(value$traces, function(x) identical(tsp(x), tsp(case$selected)), logical(1))),
           paste(label, "trace dates"))
    maximum_native <- pmax(maximum_native, c(coefficients = max(abs(value$regression$coefficients - expected$coefficients)),
      profile = abs(value$likelihood$transformed_profiled - expected$profile_ll),
      fixed = abs(value$likelihood$transformed_fixed - expected_fixed),
      squares = abs(value$regression_kernel$sum_squared_standardized - expected$squares),
      covariance_unscaled = if (length(expected$free)) max(abs(value$regression$unscaled_covariance - expected$covariance_unscaled)) else 0,
      covariance_residual = if (length(expected$free)) max(abs(value$regression$covariance_residual - expected$covariance_df)) else 0,
      jacobian = abs(value$adjustment$log_jacobian - case$jacobian)))
  }
  cat("PASS:", length(cases), "native regression cases;", checks - before_native, "checks\n")
  print(maximum_native, digits = 8)

  before_native_mean <- checks
  for (id in names(mean_cases)) {
    case <- mean_cases[[id]]
    integrated <- case$d + case$D > 0L
    value <- native(case$y, ar = case$phi, ma = case$theta, d = case$d, seasonal_d = case$D,
                    include_mean = !integrated, include_drift = integrated)
    label <- paste("native generated mean/drift", id)
    close(value$regression$coefficients, case$coefficients, paste(label, "coefficient"), TRUE)
    close(value$regression$covariance_residual, case$covariance, paste(label, "residual covariance"), TRUE)
    close(value$likelihood$transformed_profiled, case$likelihood$ll, paste(label, "profile likelihood"), TRUE)
    assert(identical(value$regression$generated_columns, if (integrated) "drift" else "mean"),
           paste(label, "generated column metadata"))
  }
  cat("PASS:", length(mean_cases), "native mean/drift cases;", checks - before_native_mean, "checks\n")

  before_native_alias <- checks
  for (kind in c("zero", "duplicate", "diffuse_constant")) {
    time <- seq_len(120L)
    one <- ts(cos(time / 13) + sin(time * sqrt(3)), start = c(2000, 1), frequency = 4)
    y <- ts(3 * one + sin(time * sqrt(2)), start = start(one), frequency = 4)
    columns <- switch(kind, zero = list(a = one * 0), duplicate = list(a = one, b = 2 * one),
                      diffuse_constant = list(a = one * 0 + 1))
    value <- native(y, regressors = columns, d = as.integer(kind == "diffuse_constant"))
    assert(value$regression$status == "rank_deficient" && value$regression$rank == as.integer(kind == "duplicate"),
           paste(kind, "native nonidentifiability is explicit"))
    assert(all(is.na(value$regression$coefficients)) && all(value$regression$coefficient_states == "unidentified"),
           paste(kind, "native does not silently turn requested free coefficients into fixed zero"))
    assert(is.na(value$likelihood$transformed_profiled) && is.na(value$likelihood$nparams_profiled),
           paste(kind, "native objective/count remain unavailable"))
  }
  cat("PASS: 3 native alias cases;", checks - before_native_alias, "checks\n")

  before_native_context <- checks
  for (id in future_ids) {
    case <- cases[[id]]
    shifted <- case$columns
    future <- seq_along(case$input) > max(case$indices)
    for (name in names(shifted)) shifted[[name]][future] <- shifted[[name]][future] + 1000
    evaluate <- function(columns) native(case$input, regressors = columns,
      coefficients = setNames(case$coefficients, names(case$columns)), ar = case$phi, ma = case$theta,
      d = case$d, seasonal_d = case$D, transformation = case$transformation,
      estimation_start = case$estimation_start, estimation_end = case$estimation_end,
      forecast_horizon = case$period, backcast_horizon = case$period)
    before <- evaluate(case$columns); after <- evaluate(shifted)
    assert(identical(before$regression, after$regression) && identical(before$likelihood, after$likelihood) &&
      identical(before$traces, after$traces), paste(id, "native fitted window unchanged by future context"))
    expected_change <- numeric(length(before$effects$context))
    expected_change[tail(seq_along(expected_change), case$period)] <- 1000 * sum(before$regression$coefficients)
    close(after$effects$context - before$effects$context, expected_change,
          paste(id, "future context effect uses fixed and fitted coefficients"), TRUE)
  }
  cat("PASS: 4 native future-context cases;", checks - before_native_context, "checks\n")
}
assert(identical(parent_before, extended_parent_hashes(base_dir)), "original artifacts unchanged after all probes")
