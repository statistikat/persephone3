# Numerical foundations, not a fitted model or native/JD3 parity claim.
# These routines use R's LAPACK, so R chol/qr provide an independent boundary
# oracle, not an independent factorization implementation. Analytic and SVD
# fixtures also test rank-deficient/underdetermined minimum-norm behavior.
chol_solve <- getFromNamespace(".native_cholesky_solve", "persephone3")
qr_solve <- getFromNamespace(".native_qr_solve", "persephone3")

a <- matrix(c(4, 1, 1, 3), 2)
b <- c(1, 2)
result <- chol_solve(a, b)
expect_equal(drop(result$solution), c(1, 7) / 11, tolerance = 1e-13)
expect_equal(result$upper, unname(chol(a)), tolerance = 1e-13)
expect_equal(result$log_determinant, log(11), tolerance = 1e-13)
expect_equal(result$condition_norm, "1")
expect_equal(result$reciprocal_condition, rcond(a, norm = "1"), tolerance = 1e-13)
expect_equal(crossprod(result$upper), a, tolerance = 1e-13)
expect_equal(dim(result$solution), c(2L, 1L))

# Multiple right-hand sides and a non-diagonal SPD fixture.
x <- matrix(c(1, 2, 4, -1, 3, 1, 2, -2, 5, 4, 3, 2), 4, 3)
spd <- crossprod(x) + diag(c(1, 2, 3))
rhs <- matrix(c(1, -2, 3, 4, 2, -1), 3, 2)
expected <- backsolve(chol(spd), forwardsolve(t(chol(spd)), rhs))
actual <- chol_solve(spd, rhs)
expect_equal(actual$solution, expected, tolerance = 1e-12)
expect_equal(actual$log_determinant, as.numeric(determinant(spd, logarithm = TRUE)$modulus), tolerance = 1e-12)
expect_equal(spd %*% actual$solution, rhs, tolerance = 1e-12)
expect_true(actual$reciprocal_condition > 0 && actual$reciprocal_condition <= 1)

# Log determinants remain finite when a raw determinant over/underflows.
expect_equal(chol_solve(diag(c(1e200, 2e200)), c(1, 1))$log_determinant,
             400 * log(10) + log(2), tolerance = 1e-12)
expect_equal(chol_solve(diag(c(1e-200, 2e-200)), c(1e-200, 1e-200))$log_determinant,
             -400 * log(10) + log(2), tolerance = 1e-12)
ill <- chol_solve(diag(c(1, 1e-12)), c(1, 1e-12))
expect_equal(drop(ill$solution), c(1, 1), tolerance = 1e-12)
expect_equal(ill$reciprocal_condition, 1e-12, tolerance = 1e-12)
expect_equal(chol_solve(matrix(1e-300, 1, 1), 1e-300)$reciprocal_condition, 1, tolerance = 1e-13)

# Both matrix triangles are checked; only documented roundoff asymmetry is
# averaged, using the same symmetric average in this independent expectation.
roundoff <- a
roundoff[1, 2] <- roundoff[1, 2] + .Machine$double.eps
expect_equal(chol_solve(roundoff, b)$solution,
             matrix(solve((roundoff + t(roundoff)) / 2, b), 2), tolerance = 1e-12)
asymmetric <- a
asymmetric[1, 2] <- 9
expect_error(chol_solve(asymmetric, b), "symmetric")
expect_error(chol_solve(matrix(c(1, 2, 2, 1), 2), b), "positive definite")
expect_error(chol_solve(matrix(1, 2, 2), b), "positive definite")
expect_error(chol_solve(diag(c(1, 0)), b), "positive definite")
expect_error(chol_solve(matrix(1, 2, 3), b), "square")
expect_error(chol_solve(a, 1:3), "dimensions")
expect_error(chol_solve(matrix(c(1.5e308, 1e308, 1e308, 1.5e308), 2), b), "1-norm")
expect_error(chol_solve(matrix(1e-300, 1, 1), 1e300), "finite")

# Deterministic overdetermined fit: LAPACK QR coefficients and residuals in the
# original observation order, with columns deliberately on different scales.
design <- cbind(1, c(-2, -1, 0, 1, 2, 3), c(3, -2, 1, 0, 2, -1) * 20)
response <- cbind(c(1, 4, 2, 6, 8, 7), c(3, 1, -2, 4, 0, 5))
reference_qr <- qr(design, LAPACK = TRUE)
fit <- qr_solve(design, response)
expect_equal(fit$coefficients, unname(qr.coef(reference_qr, response)), tolerance = 1e-12)
expect_equal(fit$residuals, unname(response - design %*% fit$coefficients), tolerance = 1e-12)
expect_equal(crossprod(design, fit$residuals), matrix(0, 3, 2), tolerance = 1e-10)
expect_equal(fit$rank, 3L)
expect_equal(fit$pivots, reference_qr$pivot)
expect_equal(sort(fit$pivots), 1:3)
expect_equal(fit$solution_kind, "minimum_norm")
expect_equal(fit$rank_tolerance, 1e-10)
expect_equal(fit$condition$available, FALSE)
expect_true(grepl("does not return", fit$condition$reason))

# Independent SVD minimum-norm oracle; keep only deliberately separated
# singular values, since DGELSY's rank threshold is not an SVD cutoff.
svd_solution <- function(a, b, tolerance = 1e-10) {
  decomp <- svd(a)
  keep <- decomp$d > tolerance * max(decomp$d)
  decomp$v[, keep, drop = FALSE] %*%
    (crossprod(decomp$u[, keep, drop = FALSE], b) / decomp$d[keep])
}

# Exact duplicate columns: x1 + 2*x2 = 3, so minimum norm is (0.6, 1.2).
rank_deficient <- cbind(1:4, 2 * (1:4))
rank_rhs <- 3 * (1:4)
deficient <- qr_solve(rank_deficient, rank_rhs)
expect_equal(deficient$rank, 1L)
expect_equal(drop(deficient$coefficients), c(0.6, 1.2), tolerance = 1e-12)
expect_equal(deficient$coefficients, svd_solution(rank_deficient, rank_rhs), tolerance = 1e-12)
expect_equal(deficient$residuals, matrix(0, 4, 1), tolerance = 1e-12)
expect_equal(sort(deficient$pivots), 1:2)

# Full-row-rank underdetermined and rank-deficient underdetermined systems.
wide <- rbind(c(1, 0, 1), c(0, 1, 1))
wide_fit <- qr_solve(wide, c(1, 1))
expect_equal(wide_fit$rank, 2L)
expect_equal(drop(wide_fit$coefficients), c(1 / 3, 1 / 3, 2 / 3), tolerance = 1e-12)
expect_equal(wide_fit$coefficients, svd_solution(wide, c(1, 1)), tolerance = 1e-12)
expect_equal(wide_fit$residuals, matrix(0, 2, 1), tolerance = 1e-12)
expect_equal(sort(wide_fit$pivots), 1:3)
wide_singular <- rbind(c(1, 2, 3), c(2, 4, 6))
wide_rhs <- cbind(c(1, 4), c(3, 1))
wide_deficient <- qr_solve(wide_singular, wide_rhs)
expect_equal(wide_deficient$rank, 1L)
expect_equal(wide_deficient$coefficients, svd_solution(wide_singular, wide_rhs), tolerance = 1e-12)
expect_equal(wide_deficient$residuals, wide_rhs - wide_singular %*% wide_deficient$coefficients, tolerance = 1e-12)
expect_equal(crossprod(wide_singular, wide_deficient$residuals), matrix(0, 3, 2), tolerance = 1e-12)

# All-zero design takes LAPACK's early-return path: expose a valid permutation
# and exact zero minimum-norm solution without treating rank zero as failure.
zero_fit <- qr_solve(matrix(0, 3, 4), 1:3)
expect_equal(zero_fit$rank, 0L)
expect_equal(zero_fit$pivots, 1:4)
expect_equal(zero_fit$coefficients, matrix(0, 4, 1))
expect_equal(zero_fit$residuals, matrix(1:3, 3, 1))

# Explicit rank policy and uniform scaling; no normal equations are formed.
separated <- diag(c(1, 1e-8))
expect_equal(qr_solve(separated, c(1, 1e-8), 1e-6)$rank, 1L)
expect_equal(qr_solve(separated, c(1, 1e-8), 1e-10)$rank, 2L)
expect_equal(qr_solve(design * 1e120, response * 1e120)$coefficients,
             fit$coefficients, tolerance = 1e-12)
expect_equal(qr_solve(design * 1e-120, response * 1e-120)$coefficients,
             fit$coefficients, tolerance = 1e-12)
expect_equal(qr_solve(matrix(2, 1, 1), 6)$coefficients, matrix(3, 1, 1), tolerance = 1e-12)
expect_error(qr_solve(matrix(1e-300, 1, 1), 1e300), "finite")

# Invalid/missing input is rejected rather than imputed or silently recycled.
for (solve in list(chol_solve, qr_solve)) {
  expect_error(solve(matrix(numeric(), 0, 0), numeric()), "positive")
  expect_error(solve(a, matrix(numeric(), 2, 0)), "positive")
  expect_error(solve(a, numeric()), "positive")
  expect_error(solve(1:4, b), "must be a matrix")
  expect_error(solve(a, array(1, c(2, 1, 1))), "two positive")
  expect_error(solve(a + 1i, b), "real numeric")
  expect_error(solve(a, c("1", "2")), "real numeric")
  for (bad in c(NA_real_, NaN, Inf, -Inf)) {
    invalid <- a
    invalid[2, 1] <- bad
    expect_error(solve(invalid, b), "finite")
    expect_error(solve(a, c(1, bad)), "finite")
  }
}
expect_error(qr_solve(design, 1:5), "dimensions")
for (bad in c(-1, 0, 1, 2, NA_real_, NaN, Inf)) {
  expect_error(qr_solve(design, response, bad), "rank tolerance")
}
for (bad in list(1e-10 + 1i, "1e-10", TRUE, factor("1e-10"),
                 c(1e-10, 1e-8), numeric(), matrix(1e-10),
                 array(1e-10, c(1, 1, 1)), list(1e-10))) {
  expect_error(qr_solve(design, response, bad), "rank tolerance.*real numeric scalar")
}
expect_equal(qr_solve(design, response, NULL), fit)

# Inputs are owned copies, including after an exception, and results are plain
# R objects suitable for serialization across later dashboard processes.
saved_design <- design
saved_response <- response
expect_error(qr_solve(design, response, -1), "rank tolerance")
expect_equal(qr_solve(design, response), fit)
expect_equal(design, saved_design)
expect_equal(response, saved_response)
expect_equal(unserialize(serialize(fit, NULL)), fit)

# Categorical storage codes are not numerical observations.
expect_error(getFromNamespace(".native_qr_solve", "persephone3")(diag(2), factor(c("a", "b"))), "real numeric")
expect_error(getFromNamespace(".native_cholesky_solve", "persephone3")(diag(2), factor(c("a", "b"))), "real numeric")
