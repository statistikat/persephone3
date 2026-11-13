# Prospective fixed-order qualification matrix. Shared by mandatory independent
# tests and the pinned live reference probe; no backend results define the data.
arma_fit_cases <- function() {
  old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
  on.exit(if (is.null(old)) {
    if (exists(".Random.seed", .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
  } else assign(".Random.seed", old, .GlobalEnv))
  set.seed(271828)
  noise <- rnorm(432)
  models <- list(
    ar = list(ar = NA_real_), ma = list(ma = NA_real_),
    arma = list(ar = NA_real_, ma = NA_real_),
    mixed_ar = list(ar = c(NA_real_, -.25)),
    seasonal_ar = list(seasonal_ar = NA_real_, seasonal_d = 1L),
    seasonal_ma = list(seasonal_ma = NA_real_, seasonal_d = 1L),
    airline = list(ma = NA_real_, seasonal_ma = NA_real_, d = 1L, seasonal_d = 1L),
    mixed_factors = list(ar = NA_real_, ma = .2, seasonal_ma = NA_real_, seasonal_d = 1L))
  cases <- list()
  for (period in c(4L, 12L)) for (kind in names(models)) {
    model <- models[[kind]]
    blocks <- lapply(c("ar", "ma", "seasonal_ar", "seasonal_ma"), function(k) {
      x <- model[[k]]
      if (is.null(x)) x <- numeric()
      x[is.na(x)] <- if (grepl("ar$", k)) .45 else .3
      x
    })
    names(blocks) <- c("ar", "ma", "seasonal_ar", "seasonal_ma")
    polys <- arma_fit_polynomials(blocks, period)
    disturbance <- as.numeric(stats::filter(noise, polys$ma, sides = 1))
    disturbance[is.na(disturbance)] <- 0
    if (length(polys$ar) > 1L) disturbance <- as.numeric(stats::filter(
      disturbance, -polys$ar[-1L], method = "recursive"))
    n <- 144L; time <- seq_len(n + 12L)
    error <- tail(disturbance, n)
    d <- if (is.null(model$d)) 0L else model$d
    D <- if (is.null(model$seasonal_d)) 0L else model$seasonal_d
    if (D) for (i in seq.int(period + 1L, n)) error[i] <- error[i] + error[i - period]
    if (d) error <- cumsum(error)
    x <- ts(sin(time / 7) + .2 * cos(time * sqrt(2)), start = c(2000, 1), frequency = period)
    fixed <- ts(cos(time / 9), start = start(x), frequency = period)
    trend <- time[seq_len(n)]^(d + D) / (factorial(d + D) * period^D)
    z <- ts(2 + .15 * trend + .3 * x[seq_len(n)] + .2 * fixed[seq_len(n)] + .08 * error,
            start = start(x), frequency = period)
    for (gaps in c(FALSE, TRUE)) for (logged in c(FALSE, TRUE)) {
      y <- if (logged) exp(z) else z
      middle <- match(kind, names(models)) %% 2L == 0L
      rows <- if (middle) seq.int(period + 1L, n - period) else seq_len(n)
      if (gaps) y[rows[c(1L, 2L, period + 5L, 2L * period + 7L, length(rows))]] <- NA_real_
      date <- function(i) c(2000L + (i - 1L) %/% period, (i - 1L) %% period + 1L)
      arguments <- c(list(series = y, regressors = list(x = x, fixed = fixed),
        coefficients = c(fixed = .2), transformation = if (logged) "log" else "none",
        estimation_start = date(min(rows)), estimation_end = date(max(rows)),
        include_mean = d + D == 0L, include_drift = d + D > 0L,
        forecast_horizon = 12L), model)
      id <- paste(period, kind, if (gaps) "gaps" else "complete", if (logged) "log" else "none", sep = "-")
      cases[[id]] <- list(id = id, period = period, kind = kind, arguments = arguments,
        rows = rows, d = d, D = D, blocks = lapply(blocks, function(b) b),
        specification = lapply(names(blocks), function(k) if (is.null(model[[k]])) numeric() else model[[k]]))
      names(cases[[id]]$specification) <- names(blocks)
    }
  }
  cases
}

arma_fit_polynomials <- function(blocks, period) {
  multiply <- function(a, b) {
    result <- numeric(length(a) + length(b) - 1L)
    for (j in seq_along(a)) result[j + seq_along(b) - 1L] <- result[j + seq_along(b) - 1L] + a[j] * b
    result
  }
  factor <- function(x, lag, sign) {
    value <- numeric(1L + lag * length(x)); value[1L] <- 1
    value[1L + lag * seq_along(x)] <- sign * x
    value
  }
  list(ar = multiply(factor(blocks$ar, 1L, -1), factor(blocks$seasonal_ar, period, -1)),
       ma = multiply(factor(blocks$ma, 1L, 1), factor(blocks$seasonal_ma, period, 1)))
}

# Independent covariance: solve the finite Yule-Walker equations, with the
# MA forcing obtained from impulse coefficients. No native state/filter is used.
arma_fit_covariance <- function(blocks, period, n) {
  p <- arma_fit_polynomials(blocks, period)
  phi <- -p$ar[-1L]; theta <- p$ma
  q <- length(theta) - 1L; order <- length(phi)
  psi <- numeric(q + 1L); psi[1L] <- 1
  if (q) for (h in seq_len(q)) {
    k <- seq_len(min(order, h))
    psi[h + 1L] <- theta[h + 1L] + sum(phi[k] * psi[h - k + 1L])
  }
  size <- max(order + 1L, q + 1L)
  system <- diag(size); rhs <- numeric(size)
  for (h in 0:(size - 1L)) {
    for (j in seq_along(phi)) system[h + 1L, abs(h - j) + 1L] <- system[h + 1L, abs(h - j) + 1L] - phi[j]
    if (h <= q) rhs[h + 1L] <- sum(theta[(h:q) + 1L] * psi[seq_len(q - h + 1L)])
  }
  gamma <- numeric(max(n, size)); gamma[seq_len(size)] <- solve(system, rhs)
  if (n > size) for (h in seq.int(size, n - 1L)) gamma[h + 1L] <- sum(phi * gamma[h - seq_along(phi) + 1L])
  toeplitz(gamma[seq_len(n)])
}

arma_fit_density <- function(case, blocks) {
  args <- case$arguments; rows <- case$rows
  y <- as.numeric(args$series)[rows]
  if (args$transformation == "log") y <- log(y)
  x <- cbind(as.numeric(args$regressors$x)[rows], rows^(case$d + case$D) /
             (factorial(case$d + case$D) * case$period^case$D))
  y <- y - .2 * as.numeric(args$regressors$fixed)[rows]
  operator <- diag(length(y))
  if (case$d) operator <- diff(operator, differences = case$d)
  if (case$D) operator <- diff(operator, lag = case$period, differences = case$D)
  root <- t(chol(arma_fit_covariance(blocks, case$period, nrow(operator))))
  white <- forwardsolve(root, operator)
  observed <- which(!is.na(y)); missing <- which(is.na(y))
  response <- drop(white[, observed, drop = FALSE] %*% y[observed])
  design <- white[, observed, drop = FALSE] %*% x[observed, , drop = FALSE]
  determinant <- 2 * sum(log(diag(root)))
  if (length(missing)) {
    qr_missing <- qr(white[, missing, drop = FALSE], LAPACK = TRUE)
    stopifnot(min(abs(diag(qr.R(qr_missing)))) > 1e-8)
    determinant <- determinant + 2 * sum(log(abs(diag(qr.R(qr_missing)))))
    complement <- qr.Q(qr_missing, complete = TRUE)[,
      length(missing) + seq_len(nrow(white) - length(missing)), drop = FALSE]
    response <- drop(crossprod(complement, response)); design <- crossprod(complement, design)
  }
  beta <- unname(qr.coef(qr(design), response))
  squares <- sum((response - drop(design %*% beta))^2); count <- length(response)
  profile <- -.5 * (count * (log(2 * pi) + 1 + log(squares / count)) + determinant)
  jacobian <- if (args$transformation == "log") -sum(log(as.numeric(args$series)[rows[
    observed[observed >= min(observed) + case$d + case$period * case$D]]])) else 0
  list(value = -profile - jacobian, profile = profile, jacobian = jacobian,
       beta = c(beta[1L], .2, beta[2L]), variance = squares / count, finite = count)
}

arma_fit_minimum <- function(case) {
  free <- is.na(unlist(case$specification, use.names = FALSE))
  vector <- unlist(case$specification, use.names = FALSE)
  blocks <- function(x) {
    value <- vector; value[free] <- x
    answer <- split(value, rep(seq_along(case$specification), lengths(case$specification)))
    result <- case$specification
    for (i in seq_along(result)) if (length(result[[i]])) result[[i]] <- unname(answer[[as.character(i)]])
    result
  }
  objective <- function(x) {
    b <- blocks(x)
    for (k in names(b)) if (length(b[[k]]) &&
        any(Mod(polyroot(c(1, b[[k]] * if (grepl("ar$", k)) -1 else 1))) <= 1.00001)) return(Inf)
    -arma_fit_density(case, b)$profile
  }
  if (sum(free) == 1L) {
    bound <- if (case$kind == "mixed_ar") 1.249 else .999
    answer <- stats::optimize(objective, c(-bound, bound), tol = 1e-8)
    return(list(parameters = answer$minimum, value = arma_fit_density(case, blocks(answer$minimum))$value,
      blocks = blocks(answer$minimum)))
  }
  initial <- unlist(case$blocks, use.names = FALSE)[free]
  answers <- lapply(list(initial, c(-.2, .5)), function(start) stats::optim(start, objective,
    method = "Nelder-Mead", control = list(reltol = 1e-13, maxit = 2000)))
  stopifnot(all(vapply(answers, function(a) a$convergence == 0L, logical(1))))
  answer <- answers[[which.min(vapply(answers, function(a) a$value, numeric(1)))]]
  list(parameters = answer$par, value = arma_fit_density(case, blocks(answer$par))$value, blocks = blocks(answer$par))
}

arma_fit_alternate <- function(case) {
  initial <- case$specification; index <- 0L
  for (k in names(initial)) for (j in seq_along(initial[[k]])) if (is.na(initial[[k]][j])) {
    index <- index + 1L
    initial[[k]][j] <- c(-.5, .55)[1L + (index - 1L) %% 2L]
  }
  parameters <- do.call(getFromNamespace("native_arma_parameter_map", "persephone3"),
    c(case$specification, list(initial = initial)))
  parameters$initial_coordinates
}
