local({
  setup <- getFromNamespace("native_arma_objective_setup", "persephone3")
  optimize_native <- getFromNamespace("native_arma_optimize", "persephone3")
  evaluate <- getFromNamespace("native_arma_objective", "persephone3")
  old_seed <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
  on.exit(if (is.null(old_seed)) {
    if (exists(".Random.seed", .GlobalEnv)) rm(".Random.seed", envir=.GlobalEnv)
  } else assign(".Random.seed", old_seed, .GlobalEnv), add=TRUE)
  set.seed(17031)
  n <- 84L; t <- seq_len(n); noise <- rnorm(n)

  # Independent dense Gaussian integration, followed by GLS; no native filter,
  # state, parameter estimate, gradient or optimization result enters this oracle.
  dense <- function(y,x,covariance,d=0L,D=0L,period=4L) {
    a <- diag(length(y))
    if (d) a <- diff(a,differences=d)
    if (D) a <- diff(a,lag=period,differences=D)
    root <- t(chol(covariance))
    white <- forwardsolve(root,a)
    observed <- which(!is.na(y)); missing <- which(is.na(y))
    response <- drop(white[,observed,drop=FALSE] %*% y[observed])
    design <- white[,observed,drop=FALSE] %*% x[observed,,drop=FALSE]
    determinant <- 2*sum(log(diag(root)))
    if (length(missing)) {
      qr_missing <- qr(white[,missing,drop=FALSE],LAPACK=TRUE)
      stopifnot(min(abs(diag(qr.R(qr_missing))))>1e-8)
      determinant <- determinant+2*sum(log(abs(diag(qr.R(qr_missing)))))
      complement <- qr.Q(qr_missing,complete=TRUE)[,
        length(missing)+seq_len(nrow(white)-length(missing)),drop=FALSE]
      response <- drop(crossprod(complement,response))
      design <- crossprod(complement,design)
    }
    beta <- if(ncol(design)) unname(qr.coef(qr(design),response)) else numeric()
    q <- sum((response-drop(design %*% beta))^2)
    count <- length(response); scale <- q/count
    list(value=.5*(count*(log(2*pi)+1+log(scale))+determinant),beta=beta,scale=scale)
  }
  check_fit <- function(fit, context) {
    expect_identical(fit$status,"converged_gradient")
    expect_true(fit$converged && fit$success)
    expect_identical(fit$final$status,"available")
    expect_identical(fit$identification,"not_assessed")
    expect_true(all(abs(fit$gradient)+fit$gradient_error<=fit$control$gradient_tolerance))
    expect_true(all(diff(fit$trace[,"value"])<=0))
    expect_true(all(fit$trace[,"step_length"][-1]>0))
    expect_true(fit$evaluations<=fit$control$max_evaluations)
    expect_true(fit$iterations<=fit$control$max_iterations)
    expect_identical(tail(fit$trace[,"gradient_bound"],1),unname(max(abs(fit$gradient)+fit$gradient_error)))
    expect_identical(fit$final$value,evaluate(context,fit$coordinates)$value)
    expect_identical(fit$adjustment,context$adjustment)
    expect_identical(unserialize(serialize(fit,NULL)),fit)
  }

  for(period in c(4L,12L)) for(kind in c("ar","ma")) for(gaps in c(FALSE,TRUE)) for(logged in c(FALSE,TRUE)) {
    x <- ts(sin(t/11),start=c(2000,1),frequency=period)
    fixed <- ts(cos(t/7),start=start(x),frequency=period)
    disturbance <- if(kind=="ar") as.numeric(filter(noise,.55,method="recursive")) else
      noise+.4*c(0,head(noise,-1))
    z <- 2+.4*x+.35*fixed+.15*disturbance
    y <- if(logged) exp(z) else z
    if(gaps) y[c(9,19,20,70)] <- NA_real_
    arguments <- c(list(series=y,regressors=list(x=x,fixed=fixed),coefficients=c(fixed=.35),
      include_mean=TRUE,transformation=if(logged) "log" else "none",
      estimation_start=c(2000+(9-1)%/%period,(9-1)%%period+1),
      estimation_end=c(2000+(76-1)%/%period,(76-1)%%period+1)),setNames(list(NA_real_),kind))
    context <- do.call(setup,arguments)
    unchanged <- serialize(context,NULL)
    rows <- 9:76
    transformed <- if(logged) log(y[rows]) else y[rows]
    response <- as.numeric(transformed-.35*fixed[rows])
    design <- cbind(as.numeric(x[rows]),1)
    m <- length(rows)
    oracle <- function(coefficient) {
      gamma <- if(kind=="ar") coefficient^(0:(m-1))/(1-coefficient^2) else
        c(1+coefficient^2,coefficient,rep(0,m-2))
      value <- dense(response,design,toeplitz(gamma))
      value$value <- value$value-context$adjustment$log_jacobian
      value
    }
    independent <- stats::optimize(function(coef) oracle(coef)$value,c(-.97,.97),tol=1e-9)
    for(start in c(-.6,0,.7)) {
      coordinates <- atanh((if(kind=="ma") -start else start)/(1-context$parameters$reflection_margin))
      fit <- optimize_native(context,coordinates)
      check_fit(fit,context)
      estimate <- fit$final$parameters$coefficients[[kind]]
      expect_equal(estimate,independent$minimum,tolerance=3e-6)
      expect_equal(fit$final$value,independent$objective,tolerance=2e-8)
      expected <- oracle(estimate)
      expect_equal(unname(fit$final$likelihood$regression_coefficients),
                   c(expected$beta[1],.35,expected$beta[2]),tolerance=2e-8)
      expect_equal(fit$final$likelihood$profiled_variance,expected$scale,tolerance=2e-8)
      expect_equal(fit$final$likelihood$nparams_profiled,4)
      h <- 1e-5
      physical_gradient <- (oracle(estimate+h)$value-oracle(estimate-h)$value)/(2*h)
      working_gradient <- physical_gradient*(1-context$parameters$reflection_margin)*(if(kind=="ma") -1 else 1)
      expect_true(abs(unname(fit$gradient)-working_gradient)<=fit$gradient_error+2e-8)
    }
    expect_identical(serialize(context,NULL),unchanged)
  }

  # Mixed AR(2): phi2 remains exactly fixed. Covariance follows three independent
  # Yule-Walker equations, then its scalar AR recurrence.
  y <- ts(as.numeric(filter(noise,c(.55,-.3),method="recursive")),frequency=4)
  y[c(1,12,13)] <- NA_real_
  context <- setup(y,ar=c(NA_real_,-.3))
  oracle <- function(phi) {
    gamma <- numeric(n)
    gamma[1:3] <- solve(rbind(c(1,-phi,.3),c(-phi,1.3,0),c(.3,-phi,1)),c(1,0,0))
    for(i in 4:n) gamma[i] <- phi*gamma[i-1]-.3*gamma[i-2]
    dense(as.numeric(y),matrix(numeric(),n,0),toeplitz(gamma))$value
  }
  independent <- stats::optimize(oracle,c(-1.29,1.29),tol=1e-9)
  for(start in c(-.8,0,1.1)) {
    fit <- optimize_native(context,start)
    check_fit(fit,context)
    expect_equal(fit$final$parameters$coefficients$ar[1],independent$minimum,tolerance=3e-6)
    expect_identical(fit$final$parameters$coefficients$ar[2],-.3)
    expect_identical(names(fit$gradient),"ar.coefficient1")
    expect_equal(fit$final$value,independent$objective,tolerance=2e-8)
  }

  # Joint ARMA(1,1), checked against a separate dense covariance and a
  # derivative-free two-dimensional minimizer, with deterministic poor starts.
  disturbance <- as.numeric(filter(noise+.3*c(0,head(noise,-1)),.5,method="recursive"))
  joint_y <- ts(2+.2*sin(t/8)+.1*disturbance,frequency=4)
  joint_y[c(1,21,22)] <- NA_real_
  joint_x <- ts(sin(t/8),frequency=4)
  joint_context <- setup(joint_y,list(x=joint_x),ar=NA_real_,ma=NA_real_,include_mean=TRUE)
  joint_oracle <- function(coef) {
    if(any(abs(coef)>=.98)) return(Inf)
    phi <- coef[1]; theta <- coef[2]
    gamma0 <- (1+theta^2+2*phi*theta)/(1-phi^2)
    gamma1 <- phi*gamma0+theta
    dense(as.numeric(joint_y),cbind(as.numeric(joint_x),1),
      toeplitz(c(gamma0,phi^(0:(n-2))*gamma1)))$value
  }
  joint_reference <- stats::optim(c(.4,.2),joint_oracle,method="Nelder-Mead",
    control=list(reltol=1e-12,maxit=2000))
  stopifnot(joint_reference$convergence==0,all(abs(joint_reference$par)<.95))
  for(start in list(c(0,0),c(.6,-.4),c(-.5,.6))) {
    fit <- optimize_native(joint_context,atanh(start*c(1,-1)/(1-1e-8)))
    check_fit(fit,joint_context)
    expect_equal(c(fit$final$parameters$coefficients$ar,fit$final$parameters$coefficients$ma),
                 joint_reference$par,tolerance=2e-5)
    expect_equal(fit$final$value,joint_reference$value,tolerance=2e-8)
    expect_equal(fit$final$likelihood$nparams_profiled,5)
  }

  # Seasonal factors with seasonal integration and estimated drift. Independent
  # covariance has nonzero entries only at multiples of the seasonal lag.
  for(period in c(4L,12L)) for(kind in c("seasonal_ar","seasonal_ma")) {
    error <- noise*.1
    for(i in seq.int(period+1,n)) error[i] <- if(kind=="seasonal_ar") noise[i]*.1+.4*error[i-period] else
      noise[i]*.1+.3*noise[i-period]*.1
    series <- error
    for(i in seq.int(period+1,n)) series[i] <- series[i]+series[i-period]
    series <- ts(series+2+.2*t/period,frequency=period)
    series[c(1,32,33)] <- NA_real_
    context <- do.call(setup,c(list(series=series,seasonal_d=1L,include_drift=TRUE),setNames(list(NA_real_),kind)))
    count <- n-period
    oracle <- function(coef) {
      gamma <- numeric(count)
      if(kind=="seasonal_ar") gamma[seq.int(1,count,by=period)] <- coef^(0:((count-1)%/%period))/(1-coef^2)
      else { gamma[1] <- 1+coef^2; gamma[period+1] <- coef }
      dense(as.numeric(series),matrix(t/period,n,1),toeplitz(gamma),D=1,period=period)
    }
    independent <- stats::optimize(function(coef)oracle(coef)$value,c(-.9,.9),tol=1e-8)
    fit <- optimize_native(context)
    check_fit(fit,context)
    expect_equal(fit$final$parameters$coefficients[[kind]],independent$minimum,tolerance=1e-5)
    expect_equal(fit$final$value,independent$objective,tolerance=3e-8)
    expect_equal(unname(fit$final$likelihood$regression_coefficients),oracle(independent$minimum)$beta,tolerance=3e-7)
  }
  old_threads <- Sys.getenv("PERSEPHONE_NUM_THREADS",unset=NA_character_)
  on.exit(if(is.na(old_threads)) Sys.unsetenv("PERSEPHONE_NUM_THREADS") else
    Sys.setenv(PERSEPHONE_NUM_THREADS=old_threads),add=TRUE)
  Sys.setenv(PERSEPHONE_NUM_THREADS="1")
  serial <- optimize_native(context)
  Sys.setenv(PERSEPHONE_NUM_THREADS="4")
  expect_identical(optimize_native(context),serial)

  simple <- ts(sin(1:30),frequency=4)
  context <- setup(simple,ar=NA_real_)
  expect_identical(optimize_native(context),optimize_native(unserialize(serialize(context,NULL))))
  for(budget in c(1,2,3,5,9,12)) {
    fit <- optimize_native(context,control=list(max_evaluations=budget))
    expect_identical(fit$status,"evaluation_limit")
    expect_false(fit$success || fit$converged)
    expect_equal(fit$evaluations,budget)
    expect_identical(fit$final$value,evaluate(context,fit$coordinates)$value)
  }
  limited <- optimize_native(context,control=list(max_iterations=0))
  expect_identical(limited$status,"iteration_limit")
  expect_equal(limited$iterations,0)
  expect_false(limited$converged)
  expect_true(all(is.finite(limited$gradient)))
  unavailable <- function(context,reason,coordinates=context$parameters$initial_coordinates) {
    fit <- optimize_native(context,coordinates)
    expect_identical(fit$status,"initial_unavailable")
    expect_identical(fit$final$status,reason)
    expect_false(fit$success || fit$converged)
    expect_equal(fit$evaluations,1)
    expect_true(is.na(fit$final$value))
    fit
  }
  unavailable(context,"parameter_boundary",1000)
  unavailable(setup(simple,ar=c(NA_real_,.2)),"inadmissible_parameters",2)
  unavailable(setup(simple,list(z=0*simple),ar=NA_real_),"rank_deficient")
  unavailable(setup(0*simple,ar=NA_real_),"zero_scale_boundary")
  unavailable(setup(simple*NA_real_,ar=NA_real_),"no_observations")
  unavailable(setup(ts(c(1,NA,NA),frequency=4),seasonal_d=1,ar=NA_real_),"unresolved_diffuse")
  unavailable(setup(ts(c(1e200,-1e200,1e200),frequency=4),ar=NA_real_),"numerical_failure")
  allfixed <- optimize_native(setup(simple,ar=.2,ma=.1,include_mean=TRUE))
  expect_identical(allfixed$status,"no_free_parameters")
  expect_true(allfixed$success)
  expect_false(allfixed$converged)
  expect_equal(allfixed$evaluations,1)
  expect_equal(length(allfixed$gradient),0)
  expect_equal(allfixed$final$likelihood$nparams_profiled,2)
  # A precision failure must never be represented as convergence.
  precision <- optimize_native(context,control=list(gradient_tolerance=1e-16))
  expect_false(precision$converged)
  expect_true(precision$status %in% c("gradient_unavailable","line_search_failed","iteration_limit","evaluation_limit","stalled"))
  for(control in list(1,TRUE,list(unknown=1),list(1),list(max_iterations=1,max_iterations=2),
      list(max_iterations=NULL),list(max_iterations=-1),list(max_iterations=.5),list(max_iterations=10001),
      list(max_evaluations=0),list(max_evaluations=1000001),list(max_line_search=0),
      list(difference_refinements=21),list(gradient_tolerance=0),list(gradient_tolerance=NA_real_),
      list(armijo=.5),list(difference_step=.2),list(step_tolerance=-1),list(function_tolerance=-1),
      list(max_step=Inf),list(max_step=TRUE),list(max_step=matrix(.1)))) expect_error(optimize_native(context,control=control))
  for(start in list(NA_real_,Inf,matrix(0),TRUE,1i,numeric())) expect_error(optimize_native(context,start))
  expect_error(optimize_native(list()),"contract")
  expect_error(optimize_native(context,setNames(0,"wrong")),"names and order")
  corrupt <- context; corrupt$prepared$inputs$transformed[1] <- Inf
  expect_error(optimize_native(corrupt,1000))
  Sys.setenv(PERSEPHONE_NUM_THREADS="invalid")
  expect_error(optimize_native(context),"PERSEPHONE_NUM_THREADS")
})
