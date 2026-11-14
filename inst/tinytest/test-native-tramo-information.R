local({
  setup <- getFromNamespace("native_arma_objective_setup","persephone3")
  optimize <- getFromNamespace("native_arma_optimize","persephone3")
  information <- getFromNamespace("native_arma_information","persephone3")
  source("fixtures/native/arma-fit-cases.R",local=TRUE)
  source("fixtures/native/arma-residual-oracle.R",local=TRUE)
  cases <- arma_fit_cases()
  relative <- function(a,b) sqrt(sum((a-b)^2)/sum(b^2))
  for(ca in cases) {
    context <- do.call(setup,ca$arguments)
    fit <- optimize(context,control=list(max_iterations=200L,max_evaluations=4000L))
    before <- serialize(context,NULL)
    result <- information(context,fit$coordinates,method="tramo_residual")
    oracle <- arma_residual_information(ca,fit$final$parameters$coefficients,step=5e-5)
    expect_identical(result$status,"available",info=ca$id)
    expect_true(result$available && result$stationary)
    expect_identical(result$covariance_mode,"accurate")
    expect_false(result$comparison_only)
    expect_identical(result$information_type,"Gauss_Newton_profiled_normalized_residuals")
    expect_true(is.null(result$hessian) && is.null(result$hessian_error))
    expect_true(relative(result$covariance,oracle$covariance)<1e-6,info=ca$id)
    expect_equal(unname(result$standard_errors),sqrt(diag(oracle$covariance)),tolerance=1e-6)
    expect_equal(result$residual_center$residuals,oracle$center$residuals,tolerance=1e-9)
    expect_equal(result$residual_center$sum_squares,sum(oracle$center$residuals^2),tolerance=1e-10)
    expect_equal(result$residual_center$log_determinant,oracle$center$log_determinant,tolerance=1e-9)
    expect_equal(result$residual_center$interpolated,oracle$center$interpolated,tolerance=1e-9)
    expect_equal(unname(result$residual_center$regression_coefficients[c(1L,3L)]),oracle$center$coefficients,tolerance=1e-9)
    expect_equal(result$residual_center$effective_count,oracle$center$finite,tolerance=0)
    expect_equal(result$residual_df,oracle$center$finite-2L-ncol(oracle$jacobian),tolerance=0)
    expect_identical(result$residual_center$source_rows,tail(oracle$center$rows,length(oracle$center$residuals)))
    expected_J <- oracle$jacobian/sqrt(sum(oracle$center$normalized^2))
    expect_true(max(abs(result$residual_jacobian-expected_J))<1e-6)
    expect_equal(unname(result$normal_information),oracle$center$finite*crossprod(expected_J),tolerance=1e-6)
    expect_identical(result$covariance,t(result$covariance))
    expect_identical(serialize(context,NULL),before)
    expect_identical(unserialize(serialize(result,NULL)),result)
    other <- information(context,fit$coordinates,method="tramo_residual",control=list(difference_step=.002))
    expect_identical(other$status,"available")
    expect_true(relative(result$covariance,other$covariance)<1e-6)
    if(ca$id %in% c("4-ar-complete-none","4-arma-complete-none","4-airline-complete-none","12-airline-gaps-none")) {
      for(scale in c(7,100,.001)) {
        args <- ca$arguments; args$series <- args$series*scale; args$regressors$fixed <- args$regressors$fixed*scale
        changed <- information(do.call(setup,args),fit$coordinates,method="tramo_residual")
        expect_identical(changed$status,"available")
        expect_true(relative(result$covariance,changed$covariance)<1e-7)
      }
    }
  }
  y <- ts(c(.2,.6,-.3,.1,-.7,-.3,.1,.7,.6,-.2,-.5,.3),frequency=4)
  context <- setup(y,ar=NA_real_);fit <- optimize(context)
  accurate <- information(context,fit$coordinates,method="tramo_residual")
  b <- accurate$residual_center
  state <- list(final_coefficients=unname(accurate$coefficients),working_coefficients=unname(accurate$coefficients)+.001,
    difference_steps=1e-6,damping=.01,normalized_sum_squares=b$sum_squares*exp(b$log_determinant/b$effective_count),
    effective_count=b$effective_count)
  legacy <- information(context,fit$coordinates,method="tramo_residual",covariance="legacy",legacy_state=state)
  expect_identical(legacy$status,"available")
  expect_identical(legacy$covariance_mode,"legacy")
  expect_true(legacy$comparison_only)
  expect_identical(legacy$accurate_covariance,accurate$covariance)
  expect_true(abs(legacy$covariance-accurate$covariance)>1e-5)
  expect_equal(unname(legacy$covariance),unname(solve(legacy$legacy_information)*legacy$covariance_multiplier),tolerance=1e-12)
  # Independent closed AR(1) whitening at a deliberately different working point.
  z <- function(phi) c(sqrt(1-phi^2)*y[1],y[-1]-phi*y[-length(y)])*(1-phi^2)^(-1/(2*length(y)))
  J <- (z(state$working_coefficients+state$difference_steps)-z(state$working_coefficients))/state$difference_steps
  expected <- state$normalized_sum_squares/(length(y)-1)/(sum(J^2)+state$damping)
  expect_equal(as.numeric(legacy$covariance),expected,tolerance=1e-8)
  expect_identical(legacy$evaluations,accurate$evaluations+2L)
  for(budget in c(1L,2L,accurate$evaluations,accurate$evaluations+1L)) {
    limited <- information(context,fit$coordinates,method="tramo_residual",covariance="legacy",legacy_state=state,
      control=list(max_evaluations=budget))
    expect_false(limited$available)
    expect_true(is.null(limited$covariance) && all(is.na(limited$standard_errors)))
    expect_true(limited$evaluations<=budget)
  }
  expect_error(information(context,fit$coordinates,method="tramo_residual",covariance="legacy"))
  expect_error(information(context,fit$coordinates,covariance="legacy",legacy_state=state))
  expect_error(information(context,fit$coordinates,method="tramo_residual",legacy_state=state))
  for(field in names(state)) {
    bad <- state;bad[[field]] <- NULL
    expect_error(information(context,fit$coordinates,method="tramo_residual",covariance="legacy",legacy_state=bad))
  }
  for(field in c("damping","normalized_sum_squares","effective_count","difference_steps")) for(value in c(NA_real_,Inf)) {
    bad <- state;bad[[field]] <- value
    expect_error(information(context,fit$coordinates,method="tramo_residual",covariance="legacy",legacy_state=bad))
  }
  for(field in c("final_coefficients","normalized_sum_squares","effective_count")) {
    bad <- state;bad[[field]] <- bad[[field]]+1
    expect_error(information(context,fit$coordinates,method="tramo_residual",covariance="legacy",legacy_state=bad))
  }
  bad <- state;bad$damping <- -1
  expect_error(information(context,fit$coordinates,method="tramo_residual",covariance="legacy",legacy_state=bad))
  expect_identical(information(context,method="tramo_residual")$status,"not_stationary")
  fixed <- information(setup(y,ar=.3),method="tramo_residual")
  expect_identical(fixed$status,"no_free_parameters")
  expect_identical(dim(fixed$covariance),c(0L,0L))
  # Exact AR(1) residual derivatives, including persistent nonstationary points.
  for(phi in c(-.5,0,.5,.97,.995)) {
    context <- setup(y,ar=NA_real_,initial=list(ar=phi))
    result <- information(context,method="tramo_residual")
    n <- length(y); e <- c(sqrt(1-phi^2)*y[1],y[-1]-phi*y[-n])
    ep <- c(-phi/sqrt(1-phi^2)*y[1],-y[-n])+e*phi/(n*(1-phi^2))
    expect_identical(result$derivative_status,"available")
    expect_equal(as.numeric(result$normal_information),n*sum(ep^2)/sum(e^2),tolerance=1e-6)
    expect_equal(as.numeric(result$gradient),n*sum(ep*e)/sum(e^2),tolerance=1e-6)
    expect_false(result$available)
  }
  seed <- if(exists(".Random.seed",.GlobalEnv))get(".Random.seed",.GlobalEnv)else NULL
  set.seed(10101);noise <- rnorm(240)
  for(period in c(4L,12L))for(phi in c(.97,.995)) {
    persistent <- ts(as.numeric(filter(noise,phi,method="recursive")),frequency=period)
    context <- setup(persistent,ar=NA_real_,include_mean=TRUE)
    fit <- optimize(context,control=list(max_iterations=200L,max_evaluations=4000L))
    result <- information(context,fit$coordinates,method="tramo_residual")
    expect_identical(result$status,"available")
    expect_true(all(is.finite(result$standard_errors)))
  }
  if(is.null(seed))rm(".Random.seed",envir=.GlobalEnv)else assign(".Random.seed",seed,.GlobalEnv)
  aliased <- setup(y,ar=NA_real_,regressors=list(x=y,x2=y))
  expect_identical(information(aliased,method="tramo_residual")$status,"center_unavailable")
  common <- setup(y,ar=NA_real_,ma=NA_real_,initial=list(ar=.3,ma=-.3))
  unavailable <- information(common,method="tramo_residual")
  expect_identical(unavailable$status,"structural_redundancy")
  expect_true(is.null(unavailable$covariance))
  # Internal missing integration columns can exceed the user-regressor bound.
  many <- ts(sin(seq_len(96)*1.7)+cos(seq_len(96)*.7),frequency=4);many[seq(3,77,2)]<-NA_real_
  context <- setup(many,ar=NA_real_,include_mean=TRUE);fit <- optimize(context)
  result <- information(context,fit$coordinates,method="tramo_residual")
  expect_identical(result$status,"available")
  expect_equal(length(result$residual_center$missing_rows),38)
  expect_equal(result$residual_center$sum_squares,result$center$likelihood$profiled_variance*result$center$likelihood$finite_count,tolerance=1e-8)
  if(getFromNamespace(".native_parallel_info","persephone3")()$compiled) {
    old <- Sys.getenv("PERSEPHONE_NUM_THREADS",unset=NA_character_)
    on.exit(if(is.na(old))Sys.unsetenv("PERSEPHONE_NUM_THREADS")else Sys.setenv(PERSEPHONE_NUM_THREADS=old),add=TRUE)
    Sys.setenv(PERSEPHONE_NUM_THREADS=1)
    serial <- information(context,fit$coordinates,method="tramo_residual")
    Sys.setenv(PERSEPHONE_NUM_THREADS=2)
    expect_identical(information(context,fit$coordinates,method="tramo_residual"),serial)
  }
})
