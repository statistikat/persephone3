local({
  mapping <- getFromNamespace("native_arma_parameter_map", "persephone3")
  decode <- getFromNamespace("native_arma_decode", "persephone3")
  check <- getFromNamespace(".native_arma_admissibility", "persephone3")
  setup <- getFromNamespace("native_arma_objective_setup", "persephone3")
  evaluate <- getFromNamespace("native_arma_objective", "persephone3")
  regression <- getFromNamespace("native_regression_likelihood", "persephone3")
  raw <- getFromNamespace(".native_arma_objective", "persephone3")
  margin <- 1e-8

  expect_identical(decode(mapping())$status, "admissible")
  expect_identical(mapping()$initial_coordinates, setNames(numeric(), character()))
  expect_equal(check(c(.5, .2), FALSE)$reflections, c(.625, .2))
  expect_equal(check(c(-.5, -.2), TRUE)$reflections, c(.625, .2))
  for (ma in c(FALSE, TRUE)) {
    expect_identical(check(numeric(), ma)$status, "admissible")
    for (x in list(1, -1, 1.1, c(0, 1), c(0, -1), c(sqrt(2), -1))) {
      expect_identical(check(if (ma) -x else x, ma)$status, "inadmissible_parameters")
    }
    expect_identical(check(1 - margin / 2, ma)$status, "parameter_boundary")
    expect_identical(check(1 - 2 * margin, ma)$status, "admissible")
    for (x in c(NA_real_, NaN, Inf, -Inf)) expect_identical(check(x, ma)$status, "nonfinite_parameters")
  }

  # Independently construct real polynomials from real/conjugate root factors.
  # polyroot tests the stored coefficients, not native reflection diagnostics.
  multiply <- function(a, b) {
    result <- numeric(length(a) + length(b) - 1L)
    for (i in seq_along(a)) for (j in seq_along(b)) result[i+j-1L] <- result[i+j-1L] + a[i]*b[j]
    result
  }
  for (n in c(1:8, 12L, 16L)) for (variant in 1:4) {
    polynomial <- 1
    if (n %% 2L) polynomial <- c(1, -.5 * sin(variant))
    if (n %/% 2L) for (j in seq_len(n %/% 2L)) {
      radius <- .15 + .035 * j
      angle <- j * sqrt(2) + variant / 3
      polynomial <- multiply(polynomial, c(1, -2 * radius * cos(angle), radius^2))
    }
    expect_true(all(Mod(polyroot(polynomial)) > 1))
    for (name in c("ar", "ma", "seasonal_ar", "seasonal_ma")) {
      ma <- name %in% c("ma", "seasonal_ma")
      coefficient <- if (ma) polynomial[-1L] else -polynomial[-1L]
      spec <- setNames(list(rep(NA_real_, n)), name)
      p <- do.call(mapping, c(spec, list(initial = setNames(list(coefficient), name))))
      restored <- decode(p)
      expect_identical(restored$status, "admissible")
      expect_equal(restored$coefficients[[name]], coefficient, tolerance = 2e-12)
      expect_true(all(Mod(polyroot(c(1, if (ma) restored$coefficients[[name]] else -restored$coefficients[[name]]))) > 1))
      expect_identical(serialize(p, NULL), serialize(unserialize(serialize(p, NULL)), NULL))
      if (n > 1L) {
        spec[[name]][seq.int(2L, n, 2L)] <- coefficient[seq.int(2L, n, 2L)]
        mixed <- do.call(mapping, c(spec, list(initial = setNames(list(coefficient), name))))
        expect_identical(mixed$modes[[name]], "coefficient")
        expect_identical(decode(mixed)$coefficients[[name]], coefficient)
        proposed <- decode(mixed, unname(mixed$initial_coordinates) + .005)
        expect_identical(proposed$coefficients[[name]][mixed$fixed[[name]]], coefficient[mixed$fixed[[name]]])
        if (proposed$admissible) expect_true(all(Mod(polyroot(c(1,
          if (ma) proposed$coefficients[[name]] else -proposed$coefficients[[name]]))) > 1))
      }
    }
  }
  # Root classification for unstructured proposals, away from boundary cases.
  for (n in 1:8) for (j in 1:16) {
    coefficient <- .8 * sin(seq_len(n) * sqrt(j + 1) + j)
    roots <- Mod(polyroot(c(1, -coefficient)))
    stopifnot(min(abs(roots - 1)) > 1e-6)
    expect_identical(check(coefficient, FALSE)$status == "admissible", all(roots > 1))
    expect_identical(check(-coefficient, TRUE)$status == "admissible", all(roots > 1))
  }
  p <- mapping(ar = c(1.2, NA_real_), ma = c(NA_real_, .1), seasonal_ar = .2,
               initial = list(ar = c(1.2, -.5)))
  expect_identical(names(p$initial_coordinates), c("ar.coefficient2", "ma.coefficient1"))
  expect_identical(decode(p, c(-.6, .2))$coefficients$ar, c(1.2, -.6))
  rejected <- decode(p, c(.3, .2))
  expect_identical(rejected$status, "inadmissible_parameters")
  expect_identical(rejected$failed_block, "ar")
  expect_identical(rejected$coefficients$ar[1], 1.2)
  expect_identical(rejected$coefficients$ma[2], .1)
  expect_identical(rejected$coefficients$seasonal_ar, .2)
  expect_error(mapping(ar = c(1.2, NA_real_)), "initial.*admissible")
  expect_error(mapping(ar = .2, initial = list(ar = .3)), "fixed coefficients")
  expect_error(mapping(ma = 1.2), "admissible")
  free <- mapping(ar = NA_real_)
  expect_equal(decode(free, .3)$coefficients$ar, (1-margin) * tanh(.3))
  expect_equal(decode(mapping(ma = NA_real_), .3)$coefficients$ma, -(1-margin) * tanh(.3))
  for (z in c(-1000, 1000)) expect_identical(decode(free, z)$status, "parameter_boundary")
  for (z in c(NA_real_, NaN, Inf)) expect_identical(decode(free, z)$status, "nonfinite_parameters")
  expect_error(decode(p, rev(p$initial_coordinates)), "names and order")
  for (bad in list(1i, TRUE, matrix(.2), c(.2, .3))) expect_error(decode(free, bad))
  for (bad in list(NaN, Inf, matrix(.1), TRUE, factor("a"), rep(0,17))) expect_error(mapping(ar = bad))
  for (bad in list(0, 1, 1e-30, NA_real_, Inf, matrix(.1), TRUE)) expect_error(mapping(reflection_margin = bad))
  for (bad in list(list(x = .1), list(ar = .1, ar = .2), list(.1), list(ar = c(.1, .2)))) {
    expect_error(mapping(ar = NA_real_, initial = bad))
  }

  # Dense Gaussian GLS oracle: observation submatrices handle missing values;
  # no filter, native states, coefficients or innovations enter expectations.
  dense <- function(y, x, covariance) {
    obs <- which(!is.na(y))
    covariance <- covariance[obs, obs, drop = FALSE]
    L <- t(chol(covariance))
    yw <- forwardsolve(L, y[obs])
    xw <- forwardsolve(L, x[obs, , drop = FALSE])
    beta <- qr.coef(qr(xw), yw)
    residual <- yw - drop(xw %*% beta)
    variance <- sum(residual^2) / length(obs)
    list(value = .5 * (length(obs) * (log(2*pi) + 1 + log(variance)) +
                        2 * sum(log(diag(L)))), beta = unname(beta), variance = variance)
  }
  n <- 52L
  time <- seq_len(n)
  x <- ts(sin(time / 5), start = c(2001, 1), frequency = 4)
  fixed <- ts(cos(time / 3), start = start(x), frequency = 4)
  y <- 2 + .4*x + .35*fixed + ts(.1*sin(time*sqrt(2)), start=start(x), frequency=4)
  y[c(1, 8, 9, n)] <- NA_real_
  design <- cbind(as.numeric(x), 1)
  for (kind in c("ar", "ma")) {
    s <- do.call(setup, c(list(series = y, regressors = list(x=x, fixed=fixed),
      coefficients = c(fixed=.35), include_mean=TRUE), setNames(list(NA_real_),kind)))
    expected <- function(coef) {
      gamma <- if (kind == "ar") coef^(0:(n-1L))/(1-coef^2) else c(1+coef^2,coef,rep(0,n-2L))
      dense(as.numeric(y - .35*fixed), design, toeplitz(gamma))
    }
    objective <- function(coef) evaluate(s, atanh((if (kind=="ma") -coef else coef)/(1-margin)))
    for (coef in seq(-.85,.85,length.out=21)) {
      got <- objective(coef)
      want <- expected(coef)
      expect_identical(got$status, "available")
      expect_equal(got$value, want$value, tolerance=2e-10)
      expect_equal(unname(got$likelihood$regression_coefficients), c(want$beta[1],.35,want$beta[2]), tolerance=2e-10)
      expect_equal(got$likelihood$profiled_variance, want$variance, tolerance=2e-10)
      expect_equal(got$likelihood$nparams_profiled, 4)
    }
    # Scalar profile location and local central differences independently agree.
    minimum <- optimize(function(coef) objective(coef)$value, c(-.85,.85), tol=1e-8)
    independent <- optimize(function(coef) expected(coef)$value, c(-.85,.85), tol=1e-8)
    expect_equal(minimum$minimum, independent$minimum, tolerance=2e-6)
    expect_equal(minimum$objective, independent$objective, tolerance=2e-10)
    h <- 1e-5
    expect_equal((objective(.3+h)$value-objective(.3-h)$value)/(2*h),
                 (expected(.3+h)$value-expected(.3-h)$value)/(2*h), tolerance=2e-7)
  }

  # Preserve the delivered regression objective, log policy and source windows.
  for (period in c(4L,12L)) for (d in 0:1) for (D in 0:1) {
    data <- ts(exp(2 + .2*sin(time/6) + .1*cos(time*sqrt(3))), start=c(2000,1),frequency=period)
    reg <- list(x=ts(sin(time/9),start=start(data),frequency=period))
    data[c(2,17,n-1)] <- NA_real_
    arguments <- list(series=data,regressors=reg,ar=.2,ma=-.1,seasonal_ma=.15,d=d,
      seasonal_d=D,transformation="log",estimation_start=c(2000,3),
      include_mean=d+D==0,include_drift=d+D>0)
    context <- do.call(setup,arguments)
    candidate <- evaluate(context)
    existing <- do.call(regression,arguments)
    expect_identical(candidate$status,"available")
    expect_equal(candidate$value,-existing$likelihood$adjusted_profiled,tolerance=1e-12)
    expect_identical(candidate$adjustment,existing$adjustment)
    expect_identical(context$prepared$inputs$missing,existing$inputs$missing)
    expect_equal(candidate$likelihood$regression_coefficients,existing$regression$coefficients,tolerance=1e-12)
    expect_identical(evaluate(unserialize(serialize(context,NULL))),candidate)
  }
  short <- ts(c(2,3,2,5),frequency=4)
  unavailable <- function(context, status, coordinates=context$parameters$initial_coordinates) {
    result <- evaluate(context,coordinates)
    expect_identical(result$status,status)
    expect_false(result$available)
    expect_true(is.na(result$value))
    expect_true(is.na(result$adjusted_log_likelihood))
    result
  }
  unavailable(setup(short,list(z=0*short)),"rank_deficient")
  unavailable(setup(short,list(z=0*short+1),d=1),"rank_deficient")
  unavailable(setup(ts(c(1,rep(NA_real_,10)),frequency=4),d=1,seasonal_d=1),"unresolved_diffuse")
  unavailable(setup(0*short),"zero_scale_boundary")
  saturated <- unavailable(setup(ts(1,frequency=4),include_mean=TRUE),"zero_scale_boundary")
  expect_identical(saturated$likelihood$profiled_log_likelihood,Inf)
  unavailable(setup(short*NA_real_),"no_observations")
  unavailable(setup(short*NA_real_,include_mean=TRUE),"no_finite_innovations")
  bad <- unavailable(setup(ts(c(1e200,-1e200,1e200),frequency=4)),"numerical_failure")
  expect_true(nzchar(bad$message))
  unavailable(setup(short,ar=NA_real_),"parameter_boundary",1000)
  unavailable(setup(short,ar=NA_real_),"nonfinite_parameters",NA_real_)
  unavailable(setup(short,ar=c(NA_real_,.2)),"inadmissible_parameters",2)
  expect_identical(evaluate(setup(short))$status,"available")
  for (args in list(list(ar=rep(NA_real_,17)),list(ma=rep(NA_real_,16)),
      list(seasonal_ar=rep(NA_real_,2)),list(d=2,seasonal_d=2,seasonal_ma=.1))) {
    expect_error(do.call(setup,c(list(series=ts(1:80,frequency=12)),args)))
  }
  expect_error(setup(short,d=1,mean=1),"mean zero")
  expect_error(setup(short,rank_tolerance=0),"rank_tolerance")
  expect_error(setup(short,list(x=short),forecast_horizon=1),"cover")
  expect_error(setup(short,list(x=short),include_mean=TRUE,mean=1),"two level")
  expect_error(setup(short,mean=matrix(0)),"scalar")
  expect_error(evaluate(list()),"contract")
  expect_error(decode(list()),"map")
  s <- setup(short,ar=NA_real_)
  expect_identical(evaluate(s),evaluate(setup(short,ar=NA_real_,mean=NULL,rank_tolerance=NULL)))
  # Corrupt static inputs must still throw, even for a rejected ARMA proposal.
  for (mutation in c("data","design","rank","order","jacobian")) {
    corrupt <- s
    if(mutation=="data") corrupt$prepared$inputs$transformed[1] <- Inf
    if(mutation=="design") corrupt$prepared$design <- matrix(NA_real_,4,1)
    if(mutation=="rank") corrupt$rank_tolerance <- 0
    if(mutation=="order") corrupt$d <- 3L
    if(mutation=="jacobian") corrupt$adjustment$log_jacobian <- Inf
    expect_error(evaluate(corrupt,1000))
  }
  old <- Sys.getenv("PERSEPHONE_NUM_THREADS",unset=NA_character_)
  on.exit(if(is.na(old)) Sys.unsetenv("PERSEPHONE_NUM_THREADS") else Sys.setenv(PERSEPHONE_NUM_THREADS=old))
  Sys.setenv(PERSEPHONE_NUM_THREADS="invalid")
  expect_error(evaluate(s,1000),"PERSEPHONE_NUM_THREADS")
  Sys.setenv(PERSEPHONE_NUM_THREADS="1")
  serial <- evaluate(s)
  Sys.setenv(PERSEPHONE_NUM_THREADS="4")
  expect_identical(evaluate(s),serial)
})
