local({
  source(system.file("tinytest/fixtures/native/arma-fit-cases.R", package = "persephone3"), local = TRUE)
  setup <- getFromNamespace("native_arma_objective_setup", "persephone3")
  optimize <- getFromNamespace("native_arma_optimize", "persephone3")
  evaluate <- getFromNamespace("native_arma_objective", "persephone3")
  control <- list(max_iterations = 200L, max_evaluations = 4000L)
  cases <- arma_fit_cases()
  expect_equal(length(cases), 64L)
  for (case in cases) {
    context <- do.call(setup, case$arguments)
    independent <- arma_fit_minimum(case)
    original <- serialize(context, NULL)
    fits <- lapply(list(context$parameters$initial_coordinates, arma_fit_alternate(case)),
      function(start) optimize(context, start, control))
    for (fit in fits) {
      expect_true(fit$converged, info = paste(case$id, fit$status))
      expect_identical(fit$final$status, "available", info = case$id)
      expect_identical(fit$identification, "not_assessed")
      expect_identical(fit$parameter_diagnostics$statistical_identification, "not_assessed")
      expect_true(all(diff(fit$trace[, "value"]) <= 0), info = case$id)
      expect_true(all(abs(fit$gradient) + fit$gradient_error <= fit$control$gradient_tolerance), info = case$id)
      blocks <- fit$final$parameters$coefficients
      # Predeclared independent-minimum budgets; these are not production tolerances.
      expect_true(all(abs(unlist(blocks) - unlist(independent$blocks)) <= 1e-4), info = case$id)
      expect_true(abs(fit$final$value - independent$value) <= 2e-7, info = case$id)
      expected <- arma_fit_density(case, blocks)
      expect_true(abs(fit$final$value - expected$value) <= 2e-8 * (1 + abs(expected$value)), info = case$id)
      expect_equal(unname(fit$final$likelihood$regression_coefficients), expected$beta, tolerance = 2e-8, info = case$id)
      expect_equal(fit$final$likelihood$profiled_variance, expected$variance, tolerance = 2e-8, info = case$id)
      expect_identical(fit$final$value, evaluate(context, fit$coordinates)$value, info = case$id)
      fixed <- !is.na(unlist(case$specification, use.names = FALSE))
      expect_identical(unlist(blocks, use.names = FALSE)[fixed], unlist(case$specification, use.names = FALSE)[fixed])
    }
    expect_identical(serialize(context, NULL), original)
    if (case$kind == "mixed_factors" && grepl("gaps", case$id)) {
      changed <- case$arguments
      for (j in seq_along(changed$regressors)) {
        after <- seq.int(length(changed$series) + 1L, length(changed$regressors[[j]]))
        changed$regressors[[j]][after] <- changed$regressors[[j]][after] + 1000
      }
      expect_identical(optimize(do.call(setup, changed), control = control), fits[[1]])
    }
  }
  # Regression centering preserves GLS under large known translations in the
  # free design space. The high-drift airline cases above exercise convergence.
  case <- cases[["4-airline-gaps-none"]]
  context <- do.call(setup, case$arguments)
  base <- evaluate(context, c(.2, -.3))
  changed <- case$arguments
  time <- seq_along(changed$series)
  changed$series <- changed$series + 400 * time^2 / (2 * case$period)
  shifted <- evaluate(do.call(setup, changed), c(.2, -.3))
  expect_true(abs(base$value - shifted$value) < 2e-6)
  expect_true(abs(shifted$likelihood$regression_coefficients["drift"] -
    base$likelihood$regression_coefficients["drift"] - 400) < 1e-7)

  old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
  on.exit(if (is.null(old)) {
    if (exists(".Random.seed", .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
  } else assign(".Random.seed", old, .GlobalEnv), add = TRUE)
  set.seed(10101)
  noise <- rnorm(240)
  # Persistent models, including a start next to the admissibility boundary.
  # The independent AR(1) density concentrates its mean by scalar projection.
  for (period in c(4L, 12L)) for (phi in c(.97, .995)) {
    y <- ts(as.numeric(filter(noise, phi, method = "recursive")), frequency = period)
    oracle <- function(p) {
      values <- as.numeric(y)
      response <- c(sqrt(1 - p^2) * values[1], tail(values, -1) - p * head(values, -1))
      design <- c(sqrt(1 - p^2), rep(1 - p, length(y) - 1L))
      beta <- sum(response * design) / sum(design^2)
      squares <- sum((response - beta * design)^2)
      .5 * (length(y) * (log(2 * pi) + 1 + log(squares / length(y))) - log(1 - p^2))
    }
    independent <- stats::optimize(oracle, c(-.9999, .9999), tol = 1e-10)
    context <- setup(y, ar = NA_real_, include_mean = TRUE)
    for (start in c(-.8, 0, .98)) {
      fit <- optimize(context, atanh(start / (1 - context$parameters$reflection_margin)), control)
      expect_true(fit$converged)
      expect_true(abs(fit$final$parameters$coefficients$ar - independent$minimum) < 1e-6)
      expect_true(abs(fit$final$value - independent$objective) < 2e-8)
      expect_true(fit$final$parameters$coefficients$ar > .95)
      expect_identical(fit$parameter_diagnostics$status, "full_rank_transfer")
      expect_identical(fit$identification, "not_assessed")
    }
  }
})
