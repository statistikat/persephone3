local({
  parameter_map <- getFromNamespace("native_arma_parameter_map", "persephone3")
  identify <- getFromNamespace("native_arma_identification", "persephone3")
  setup <- getFromNamespace("native_arma_objective_setup", "persephone3")
  optimize <- getFromNamespace("native_arma_optimize", "persephone3")
  fixture <- system.file("tinytest/fixtures/native/arma-fit-cases.R", package = "persephone3")
  source(fixture, local = TRUE)
  for (period in c(4L, 12L)) for (a in c(-.6, 0, .4)) for (difference in c(0, 1e-6, .2)) {
    parameters <- parameter_map(ar = NA_real_, ma = NA_real_, initial = list(ar = a, ma = -a + difference))
    result <- identify(parameters, period)
    # H=(1+theta B)/(1-phi B); derivative numerators are B+theta B^2
    # and B-phi B^2, in physical free coefficient coordinates.
    expected <- rbind(c(0, 0), c(1, 1), c(-a + difference, -a))
    normalized <- sweep(expected, 2, sqrt(colSums(expected^2)), "/")
    values <- svd(normalized, nu = 0, nv = 0)$d
    expect_equal(unname(result$numerator_jacobian), expected, tolerance = 1e-14)
    expect_equal(result$singular_values, values, tolerance = 1e-14)
    expect_true(abs(result$singular_ratio - min(values) / max(values)) < 1e-14)
    expect_equal(result$rank, if (difference == 0) 1L else 2L)
    expect_identical(result$status, if (difference == 0) "rank_deficient_transfer" else
      if (difference == 1e-6) "near_redundant_transfer" else "full_rank_transfer")
    expect_identical(result$statistical_identification, "not_assessed")
    expect_identical(colnames(result$numerator_jacobian), c("ar.coefficient1", "ma.coefficient1"))
    expect_identical(unserialize(serialize(result, NULL)), result)
    # Fixing one redundant coefficient removes the free cancellation direction.
    fixed <- parameter_map(ar = a, ma = NA_real_, initial = list(ma = -a + difference))
    expect_identical(identify(fixed, period)$status, "full_rank_transfer")
  }
  # Structured seasonal factors and a mixed ordinary block: compare all
  # polynomial derivatives with independent coefficient perturbations.
  for (period in c(4L, 12L)) {
    p <- parameter_map(ar = c(NA_real_, -.2), ma = NA_real_, seasonal_ma = NA_real_,
      initial = list(ar = c(.4, -.2), ma = .25, seasonal_ma = -.3))
    result <- identify(p, period)
    base <- arma_fit_polynomials(p$initial, period)
    multiply <- function(a, b) {
      out <- numeric(length(a) + length(b) - 1L)
      for (i in seq_along(a)) out[i + seq_along(b) - 1L] <- out[i + seq_along(b) - 1L] + a[i] * b
      out
    }
    expected <- matrix(0, length(base$ar) + length(base$ma) - 1L, 3L)
    for (j in seq_along(c("ar", "ma", "seasonal_ma"))) {
      key <- c("ar", "ma", "seasonal_ma")[j]
      perturbed <- p$initial; perturbed[[key]][1] <- perturbed[[key]][1] + .125
      next_poly <- arma_fit_polynomials(perturbed, period)
      expected[, j] <- (multiply(base$ar, next_poly$ma - base$ma) -
        multiply(base$ma, next_poly$ar - base$ar)) / .125
    }
    expect_equal(unname(result$numerator_jacobian), expected, tolerance = 1e-13)
    expect_equal(result$singular_values, svd(sweep(expected, 2, sqrt(colSums(expected^2)), "/"), nu = 0, nv = 0)$d,
      tolerance = 1e-13)
    expect_identical(result$status, "full_rank_transfer")
    common <- parameter_map(seasonal_ar = NA_real_, seasonal_ma = NA_real_,
      initial = list(seasonal_ar = .6, seasonal_ma = -.6))
    expect_identical(identify(common, period)$status, "rank_deficient_transfer")
  }
  # A shared root across different factor types is also visible to the expanded
  # sensitivity, when the free coefficients permit its cancellation direction.
  cross <- parameter_map(ar = rep(NA_real_, 4), seasonal_ma = NA_real_,
    initial = list(ar = c(0, 0, 0, .4), seasonal_ma = -.4))
  expect_equal(identify(cross, 4)$rank, 4L)
  near <- parameter_map(ar = NA_real_, initial = list(ar = .99999))
  expect_identical(identify(near, 12)$status, "full_rank_transfer")
  expect_equal(identify(near, 12)$minimum_reflection_slack, 1e-5, tolerance = 1e-9)
  expect_identical(identify(parameter_map(ar = .4), 4)$status, "no_free_parameters")
  expect_identical(identify(parameter_map(), 4)$status, "no_free_parameters")
  expect_true(is.na(identify(parameter_map(), 4)$minimum_reflection_slack))
  bad <- identify(near, 12, 1000)
  expect_identical(bad$status, "parameters_unavailable")
  expect_true(is.na(bad$rank) && is.na(bad$singular_ratio))
  expect_null(bad$numerator_jacobian)
  for (tolerance in list(c(0, 1e-4), c(1e-4, 1e-4), c(1e-4, 1), c(NA_real_, 1e-4))) {
    expect_error(identify(near, 4, rank_tolerance = tolerance[1], near_tolerance = tolerance[2]))
  }
  expect_error(identify(near, 5)); expect_error(identify(near, matrix(4)))
  expect_error(identify(near, 4, setNames(0, "wrong")))
  expect_error(identify(parameter_map(seasonal_ar = c(NA_real_, NA_real_)), 12))
  # A stationary point and a full-rank regressor design do not identify ARMA.
  y <- ts(rep(c(1, 0, -1, 0), 30), frequency = 4)
  fit <- optimize(setup(y, ar = NA_real_, ma = NA_real_))
  expect_true(fit$converged)
  expect_identical(fit$parameter_diagnostics$status, "rank_deficient_transfer")
  expect_identical(fit$identification, "not_assessed")
  aliased <- optimize(setup(y, regressors = list(zero = 0 * y), ar = NA_real_))
  expect_identical(aliased$final$status, "rank_deficient")
  expect_identical(aliased$parameter_diagnostics$status, "full_rank_transfer")
  expect_false(aliased$converged)
  x <- ts(sin(seq_along(y)), frequency = 4)
  weak <- optimize(setup(y, regressors = list(a = x, b = x + 1e-12 * cos(seq_along(y))), ar = NA_real_))
  expect_identical(weak$final$status, "rank_deficient")
  expect_identical(weak$parameter_diagnostics$status, "full_rank_transfer")
  expect_false(weak$converged)
})
