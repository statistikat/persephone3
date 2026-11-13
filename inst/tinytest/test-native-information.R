local({
  setup <- getFromNamespace("native_arma_objective_setup", "persephone3")
  optimize_native <- getFromNamespace("native_arma_optimize", "persephone3")
  information <- getFromNamespace("native_arma_information", "persephone3")
  parameter_map <- getFromNamespace("native_arma_parameter_map", "persephone3")
  source("fixtures/native/arma-fit-cases.R", local=TRUE)
  cases <- arma_fit_cases()
  # Independent dense Gaussian integration, not native innovations, supplies
  # the curvature oracle. It profiles its own GLS coefficients and scale.
  density_function <- function(case, blocks, beta=NULL) {
    force(case); force(blocks); force(beta)
    free <- is.na(unlist(case$specification)); template <- unlist(blocks)
    function(theta) {
      value <- template; value[free] <- theta; at <- 0L
      b <- lapply(lengths(blocks), function(n) { z <- value[at+seq_len(n)]; at <<- at+n; unname(z) })
      names(b) <- names(blocks)
      -arma_fit_density(case, b, beta)$profile
    }
  }
  hessian <- function(fun, x, h) {
    n <- length(x); H <- matrix(0,n,n); f <- fun(x)
    for(j in seq_len(n)) {
      a <- numeric(n); a[j] <- h
      H[j,j] <- (fun(x+a)-2*f+fun(x-a))/h^2
      for(i in seq_len(j-1L)) {
        b <- numeric(n); b[i] <- h
        H[i,j] <- H[j,i] <- (fun(x+a+b)-fun(x+a-b)-fun(x-a+b)+fun(x-a-b))/(4*h^2)
      }
    }
    H
  }
  for (case in cases) {
    context <- do.call(setup,case$arguments)
    fit <- optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L))
    before <- serialize(context,NULL)
    result <- information(context,fit$coordinates)
    expect_identical(result$status,"available",info=case$id)
    expect_true(result$available && result$stationary)
    expect_identical(result$derivative_status,"available")
    expect_identical(result$curvature_status,"positive_definite")
    expect_identical(result$reference_parity,"not_qualified")
    theta <- unlist(fit$final$parameters$coefficients)[is.na(unlist(case$specification))]
    fun <- density_function(case,fit$final$parameters$coefficients)
    expected <- (4*hessian(fun,theta,.001)-hessian(fun,theta,.002))/3
    budget <- 1e-5*(1+max(abs(expected)))
    expect_true(max(abs(result$hessian-expected))<budget,info=case$id)
    expect_equal(unname(result$covariance),solve(expected),tolerance=1e-5)
    expect_equal(unname(result$standard_errors),sqrt(diag(solve(expected))),tolerance=1e-5)
    expect_identical(result$covariance,t(result$covariance))
    expect_identical(result$hessian,t(result$hessian))
    expect_true(all(is.finite(result$hessian_error)) && all(result$hessian_error>=0))
    expect_true(all(abs(result$gradient)+result$gradient_error<=result$control$gradient_tolerance))
    expect_true(min(result$normalized_eigenvalues)>result$normalized_error_estimate)
    expect_identical(serialize(context,NULL),before)
    expect_identical(unserialize(serialize(result,NULL)),result)
    expect_identical(names(result$standard_errors),sub("[.]reflection",".coefficient",names(fit$coordinates)))
    # Repeating at a different initial stencil qualifies truncation/roundoff.
    other <- information(context,fit$coordinates,control=list(difference_step=.002))
    expect_identical(other$status,"available")
    expect_equal(other$covariance,result$covariance,tolerance=1e-5)
    conditional <- information(context,fit$coordinates,method="conditional_residual_df")
    fixed_fun <- density_function(case,fit$final$parameters$coefficients,
      unname(fit$final$likelihood$regression_coefficients[c(1L,3L)]))
    fixed_H <- (4*hessian(fixed_fun,theta,.001)-hessian(fixed_fun,theta,.002))/3
    count <- result$center$likelihood$finite_count
    multiplier <- count/(count-2-length(theta))
    expect_identical(conditional$status,"available")
    expect_identical(conditional$regression_convention,"held_at_fitted_GLS_coefficients")
    expect_identical(conditional$free_regression,2L)
    expect_equal(conditional$covariance_multiplier,multiplier,tolerance=0)
    expect_true(max(abs(conditional$hessian-fixed_H))<budget)
    expect_equal(unname(conditional$covariance),solve(fixed_H)*multiplier,tolerance=1e-5)
    expect_identical(conditional$center,result$center)
    expect_identical(conditional$evaluations,result$evaluations+1L)
    expect_identical(information(context,fit$coordinates,list(max_evaluations=1L),
      method="conditional_residual_df")$status,"evaluation_limit")
  }
  # Exact closed-form AR(1) first/second derivatives, including near-unit points.
  y <- c(.2,.6,-.3,.1,-.7,-.3,.1,.7,.6,-.2,-.5,.3)
  n <- length(y); A <- sum(y^2); B <- sum(y[-1]*y[-n]); C <- sum(y[2:(n-1)]^2)
  for (phi in c(-.5,0,.5,.97,.995)) {
    context <- setup(ts(y,frequency=4),ar=NA_real_,initial=list(ar=phi))
    result <- information(context)
    Q <- A-2*B*phi+C*phi^2; Q1 <- -2*B+2*C*phi; Q2 <- 2*C
    gradient <- .5*(n*Q1/Q+2*phi/(1-phi^2))
    curvature <- .5*(n*(Q2/Q-(Q1/Q)^2)+2*(1+phi^2)/(1-phi^2)^2)
    expect_identical(result$derivative_status,"available")
    expect_true(abs(result$gradient-gradient)<1e-6*(1+abs(gradient)))
    expect_true(abs(result$hessian-curvature)<1e-5*(1+abs(curvature)))
    expect_false(result$available)
    expect_true(is.null(result$covariance) && all(is.na(result$standard_errors)))
  }
  context <- setup(ts(y,frequency=4),ar=NA_real_)
  fit <- optimize_native(context)
  result <- information(context,fit$coordinates)
  scaled <- setup(ts(y*7,frequency=4),ar=NA_real_)
  expect_equal(information(scaled,fit$coordinates)$covariance,result$covariance,tolerance=1e-7)
  expect_identical(information(context,fit$coordinates,list(max_evaluations=2L))$status,"evaluation_limit")
  expect_identical(information(context,1000)$status,"parameters_unavailable")
  near <- setup(ts(y,frequency=4),ar=NA_real_,initial=list(ar=1-2e-8))
  expect_identical(information(near)$status,"derivatives_unresolved")
  aliased <- setup(ts(y,frequency=4),ar=NA_real_,regressors=list(
    x=ts(seq_along(y),frequency=4), duplicate=ts(seq_along(y),frequency=4)))
  unavailable <- information(aliased)
  expect_identical(unavailable$status,"center_unavailable")
  expect_identical(unavailable$center$status,"rank_deficient")
  expect_true(is.null(unavailable$covariance) && all(is.na(unavailable$standard_errors)))
  # Independent full Gaussian Hessian and its nuisance Schur complement.
  context <- setup(ts(y,frequency=4),ar=NA_real_,include_mean=TRUE)
  fit <- optimize_native(context)
  result <- information(context,fit$coordinates)
  joint <- function(x) {
    phi <- x[1]; residual <- y-x[2]; tau <- x[3]
    squares <- (1-phi^2)*residual[1]^2+sum((residual[-1]-phi*residual[-n])^2)
    .5*(n*(log(2*pi)+tau)-log(1-phi^2)+exp(-tau)*squares)
  }
  point <- c(fit$final$parameters$coefficients$ar,fit$final$likelihood$regression_coefficients,
    log(fit$final$likelihood$profiled_variance))
  full <- (4*hessian(joint,point,.0005)-hessian(joint,point,.001))/3
  schur <- full[1,1]-drop(full[1,2:3,drop=FALSE]%*%solve(full[2:3,2:3],full[2:3,1,drop=FALSE]))
  expect_equal(unname(result$hessian),matrix(schur,1L),tolerance=1e-6)
  # A fully free AR(2) has a nonlinear reflection-to-physical map. The returned
  # Hessian still uses polynomial coefficients, including cross curvature.
  case <- cases[["4-mixed_ar-complete-none"]]
  case$specification$ar <- case$arguments$ar <- c(NA_real_,NA_real_)
  context <- do.call(setup,case$arguments); fit <- optimize_native(context)
  result <- information(context,fit$coordinates)
  fun <- density_function(case,fit$final$parameters$coefficients)
  theta <- unlist(fit$final$parameters$coefficients)
  expected <- (4*hessian(fun,theta,.001)-hessian(fun,theta,.002))/3
  expect_identical(result$status,"available")
  expect_equal(unname(result$hessian),expected,tolerance=1e-5)
  expect_equal(unname(result$covariance),solve(expected),tolerance=1e-5)
  for (ma in c(-.3,-.3+1e-7)) {
    context <- setup(ts(y,frequency=4),ar=NA_real_,ma=NA_real_,initial=list(ar=.3,ma=ma))
    result <- information(context)
    expect_identical(result$status,if(ma==-.3) "structural_redundancy" else "structural_near_redundancy")
    expect_true(is.null(result$covariance) && all(is.na(result$standard_errors)))
  }
  fixed <- information(setup(ts(y,frequency=4),ar=.3))
  expect_identical(fixed$status,"no_free_parameters")
  expect_identical(dim(fixed$covariance),c(0L,0L))
  expect_true(fixed$available && length(fixed$standard_errors)==0L)
  expect_error(information(list()),"contract")
  for (method in list("expected",NA_character_,c("profile_ml","conditional_residual_df"),TRUE)) {
    expect_error(information(setup(ts(y,frequency=4),ar=NA_real_),method=method))
  }
  for (control in list(list(unknown=1),list(difference_step=0),list(weak_tolerance=1e-12),
      list(max_evaluations=0),list(difference_refinements=17),list(relative_tolerance=NA_real_),
      list(gradient_tolerance=Inf),list(absolute_tolerance=-1),list(rank_tolerance=1))) {
    expect_error(information(setup(ts(y,frequency=4),ar=NA_real_),control=control))
  }
})
