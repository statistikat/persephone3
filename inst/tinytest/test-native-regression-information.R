local({
  setup <- getFromNamespace("native_arma_objective_setup","persephone3")
  optimize <- getFromNamespace("native_arma_optimize","persephone3")
  information <- getFromNamespace("native_arma_information","persephone3")
  regression_information <- getFromNamespace("native_regression_information","persephone3")
  source("fixtures/native/arma-fit-cases.R",local=TRUE)
  source("fixtures/native/arma-residual-oracle.R",local=TRUE)
  cases <- arma_fit_cases()
  for(ca in cases) {
    context <- do.call(setup,ca$arguments)
    fit <- optimize(context,control=list(max_iterations=200L,max_evaluations=4000L))
    original <- serialize(context,NULL)
    result <- regression_information(context,fit$coordinates)
    basis <- arma_residual_basis(ca,fit$final$parameters$coefficients)
    k <- sum(is.na(unlist(ca$specification)));df <- basis$finite-2L-k
    expected <- basis$regression_covariance_unit*sum(basis$residuals^2)/df
    expect_identical(result$status,"available",info=ca$id)
    expect_true(result$available)
    expect_identical(result$method,"conditional_residual_df")
    expect_identical(result$conditioned_on,"supplied_ARMA_coefficients")
    expect_false(result$propagates_ARMA_uncertainty)
    expect_true(is.null(result$joint_covariance) && is.null(result$cross_covariance))
    expect_equal(unname(result$coefficients),basis$coefficients,tolerance=1e-9)
    expect_equal(unname(result$covariance),expected,tolerance=1e-7,info=ca$id)
    expect_equal(unname(result$standard_errors),sqrt(diag(expected)),tolerance=1e-7)
    expect_equal(result$residual_variance,sum(basis$residuals^2)/df,tolerance=1e-10)
    expect_equal(result$residual_df,df,tolerance=0)
    expect_equal(result$effective_count,basis$finite,tolerance=0)
    expect_identical(result$free_regression,2L)
    expect_identical(result$free_ARMA,as.integer(k))
    expect_identical(result$free_columns,c(1L,3L))
    expect_identical(names(result$coefficients),context$prepared$labels[c(1L,3L)])
    expect_identical(rownames(result$covariance),names(result$standard_errors))
    expect_identical(result$covariance,t(result$covariance))
    expect_identical(serialize(context,NULL),original)
    expect_identical(unserialize(serialize(result,NULL)),result)
    if(ca$id %in% c("4-ar-complete-none","12-airline-gaps-log")) {
      for(method in c("conditional_residual_df","tramo_residual")) {
        other <- regression_information(context,fit$coordinates,arma_method=method)
        expect_identical(other$status,"available")
        expect_identical(other$covariance,result$covariance)
        expect_identical(other$coefficients,result$coefficients)
      }
      # Treating the same ARMA coefficients as fixed restores Q/(n-r), with
      # the same conditional GLS matrix; this is not joint uncertainty.
      args <- ca$arguments
      for(block in names(ca$specification))args[[block]] <- fit$final$parameters$coefficients[[block]]
      fixed <- regression_information(do.call(setup,args))
      expect_identical(fixed$status,"available")
      expect_identical(fixed$free_ARMA,0L)
      expect_equal(unname(fixed$covariance),expected*df/(basis$finite-2L),tolerance=1e-7)
    }
  }
  ca <- cases[["4-ar-complete-none"]];context <- do.call(setup,ca$arguments)
  fit <- optimize(context);base <- regression_information(context,fit$coordinates)
  for(scale in c(.001,7,100)) {
    args <- ca$arguments;args$series <- args$series*scale;args$regressors$fixed <- args$regressors$fixed*scale
    scaled <- regression_information(do.call(setup,args),fit$coordinates)
    expect_identical(scaled$status,"available")
    expect_equal(scaled$coefficients,base$coefficients*scale,tolerance=1e-8)
    expect_equal(scaled$covariance,base$covariance*scale^2,tolerance=1e-7)
  }
  args <- ca$arguments;args$regressors$x <- args$regressors$x*7
  scaled <- regression_information(do.call(setup,args),fit$coordinates)
  change <- diag(c(1/7,1))
  expect_equal(unname(scaled$covariance),change%*%base$covariance%*%change,tolerance=1e-7)
  expect_equal(unname(scaled$coefficients),drop(change%*%base$coefficients),tolerance=1e-8)
  for(control in list(list(max_evaluations=1L),list(max_evaluations=2L))) {
    unavailable <- regression_information(context,fit$coordinates,control=control)
    expect_identical(unavailable$status,"arma_information_unavailable")
    expect_false(unavailable$available)
    expect_true(is.null(unavailable$covariance) && all(is.na(unavailable$standard_errors)))
    expect_true(all(is.finite(unavailable$coefficients)))
  }
  for(point in list(context$parameters$initial_coordinates,1000)) {
    unavailable <- regression_information(context,coordinates=point)
    expect_false(unavailable$available)
    expect_true(is.null(unavailable$covariance) && all(is.na(unavailable$standard_errors)))
    expect_identical(names(unavailable$standard_errors),names(base$standard_errors))
    expect_identical(unavailable$free_ARMA,1L)
  }
  y <- ts(c(.2,.6,-.3,.1,-.7,-.3,.1,.7,.6,-.2,-.5,.3),frequency=4)
  fixed <- regression_information(setup(y,ar=.3,regressors=list(x=y*2),coefficients=c(x=.1)))
  expect_identical(fixed$status,"no_free_coefficients")
  expect_true(fixed$available)
  expect_equal(length(fixed$coefficients),0)
  expect_equal(length(fixed$standard_errors),0)
  expect_identical(dim(fixed$covariance),c(0L,0L))
  alias <- regression_information(setup(y,ar=.3,regressors=list(x=y,x2=y)))
  expect_false(alias$available)
  expect_true(is.null(alias$covariance) && all(is.na(alias$standard_errors)))
  expect_identical(names(alias$standard_errors),c("x","x2"))
  common <- regression_information(setup(y,ar=NA_real_,ma=NA_real_,initial=list(ar=.3,ma=-.3),include_mean=TRUE))
  expect_identical(common$arma_information_status,"structural_redundancy")
  expect_false(common$available)
  for(method in list(NULL,NA_character_,c("conditional_residual_df","joint"),matrix("conditional_residual_df"),"joint",1)) {
    expect_error(regression_information(context,fit$coordinates,method=method))
  }
  # Legacy numerical covariance selection does not change conditional GLS.
  record <- dget("fixtures/native/tramo-legacy-states.R")[["4-ar-complete-none"]]
  args <- ca$arguments;args$initial <- record$initial;context <- do.call(setup,args)
  accurate <- regression_information(context,arma_method="tramo_residual")
  legacy <- regression_information(context,arma_method="tramo_residual",covariance="legacy",legacy_state=record$state)
  expect_identical(legacy$covariance,accurate$covariance)
  expect_identical(legacy$coefficients,accurate$coefficients)
  expect_identical(legacy$arma_covariance_mode,"legacy")
})
