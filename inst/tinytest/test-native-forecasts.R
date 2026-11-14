local({
  setup <- getFromNamespace('native_arma_objective_setup','persephone3')
  optimize <- getFromNamespace('native_arma_optimize','persephone3')
  forecast <- getFromNamespace('native_arma_forecasts','persephone3')
  objective <- getFromNamespace('native_arma_objective','persephone3')
  interpolation <- getFromNamespace('native_interpolation_information','persephone3')
  source('fixtures/native/arma-fit-cases.R',local=TRUE)
  source('fixtures/native/interpolation-oracle.R',local=TRUE)
  source('fixtures/native/forecast-oracle.R',local=TRUE)
  points <- function(x)unname(unlist(lapply(x$points_transformed,as.numeric)))
  errors <- function(x)unname(unlist(lapply(x$standard_errors,as.numeric)))
  bounded <- function(a,b)all(is.finite(a)) && all(is.finite(b)) &&
    all(abs(a-b)<=2e-8*(1+abs(b)))
  for(ca in forecast_cases()) {
    context <- do.call(setup,ca$arguments);before <- serialize(context,NULL)
    fit <- optimize(context,control=list(max_iterations=200L,max_evaluations=4000L))
    actual <- forecast(context,fit$coordinates)
    target <- forecast_oracle(context,fit$final$parameters$coefficients,ca$period,12L)
    expect_true(actual$available && actual$point_available,info=ca$id)
    expect_identical(actual$center,fit$final)
    expect_identical(actual$rows,target$rows)
    expect_true(all(abs(points(actual)-target$points)<=2e-8*(1+abs(target$points))),info=ca$id)
    expect_equal(unname(actual$covariance),target$covariance*actual$scale_variance,tolerance=2e-8,info=ca$id)
    expect_true(bounded(unname(actual$known_regression_covariance),target$known*actual$scale_variance),info=ca$id)
    expect_true(bounded(unname(actual$regression_covariance),target$regression*actual$scale_variance),info=ca$id)
    expect_equal(actual$covariance,actual$known_regression_covariance+actual$regression_covariance,tolerance=1e-12)
    expect_equal(errors(actual),sqrt(diag(actual$covariance)),tolerance=1e-12,check.attributes=FALSE)
    expect_equal(actual$sum_squares,target$Q,tolerance=2e-8)
    expect_equal(actual$scale_variance,fit$final$likelihood$profiled_variance,tolerance=0)
    expect_identical(actual$effective_count,fit$final$likelihood$finite_count)
    expect_false(actual$propagates_ARMA_uncertainty || actual$comparison_only)
    expect_equal(actual$source_positions,c(min(ca$rows)-ca$period+seq_len(ca$period)-1,max(ca$rows)+seq_len(12L)),tolerance=0)
    expect_equal(tsp(actual$points_transformed$backcast)[2],tsp(context$prepared$inputs$original)[1]-1/ca$period,tolerance=0)
    expect_equal(tsp(actual$points_transformed$forecast)[1],tsp(context$prepared$inputs$original)[2]+1/ca$period,tolerance=0)
    expect_identical(serialize(context,NULL),before)
    expect_identical(unserialize(serialize(actual,NULL)),actual)
    expect_identical(actual$forecasts$points$y,actual$points_original$forecast)
    df <- forecast(context,fit$coordinates,scale='residual_df',forecast_error_horizon=5L,backcast_error_horizon=2L)
    expect_identical(df$points_transformed,actual$points_transformed)
    expect_equal(df$covariance,actual$covariance*actual$effective_count/actual$residual_df,tolerance=1e-12)
    expect_equal(length(df$standard_errors$forecast),5L)
    expect_equal(length(df$standard_errors$backcast),2L)
    expect_equal(tail(errors(df),5),sqrt(tail(diag(df$covariance),12))[1:5],check.attributes=FALSE,tolerance=1e-12)
    expect_equal(head(errors(df),2),sqrt(head(diag(df$covariance),ca$period))[(ca$period-1):ca$period],check.attributes=FALSE,tolerance=1e-12)
    # Requesting a smaller endpoint span cannot change its marginal prediction.
    short <- forecast(context,fit$coordinates,forecast_horizon=3L,backcast_horizon=2L)
    ix <- c((ca$period-1):ca$period,ca$period+1:3)
    expect_equal(points(short),points(actual)[ix],tolerance=2e-8)
    expect_equal(unname(short$covariance),unname(actual$covariance[ix,ix]),tolerance=2e-8)
    if(actual$transformation=='log') {
      expect_equal(unname(unlist(lapply(actual$standard_errors_original,as.numeric))),exp(points(actual)+diag(actual$covariance)/2)*sqrt(expm1(diag(actual$covariance))),check.attributes=FALSE,tolerance=2e-10)
      expect_identical(actual$original_uncertainty_status,'available')
      means <- exp(points(actual)+diag(actual$covariance)/2)
      expect_equal(unname(unlist(lapply(actual$means_original,as.numeric))),means,check.attributes=FALSE,tolerance=2e-10)
      target <- sign(actual$covariance)*exp(outer(log(means),log(means),'+')+log(abs(expm1(actual$covariance))))
      if(all(is.finite(target)) && !any(actual$covariance!=0 & target==0))
        expect_equal(actual$covariance_original,target,check.attributes=FALSE,tolerance=2e-10)
      else expect_identical(actual$original_covariance_status,'original_covariance_range')
      expect_equal(unname(unlist(lapply(actual$points_original,as.numeric))),exp(points(actual)),tolerance=1e-12)
    } else expect_identical(actual$standard_errors_original,actual$standard_errors)
    # Estimation-endpoint gaps remain unfilled by the interpolation API.
    imputed <- interpolation(context,fit$coordinates)
    expect_true(all(which(is.na(context$prepared$inputs$transformed))[c(1,length(which(is.na(context$prepared$inputs$transformed))))] %in% imputed$unfilled_rows) || !anyNA(context$prepared$inputs$transformed))
  }
  # Closed AR(1): independent past/future innovations conditional on a complete
  # observed span. The covariance is phi^|h-k| times the shorter horizon sum.
  y <- ts(c(1,3,2,4,5,2,3,5,4,2,3,6,2,4,5,3),start=2000,frequency=4)
  for(phi in c(-.9999,-.95,-.3,0,.6,.98,.9999)) {
    context <- setup(y,ar=phi,mean=2,forecast_horizon=5L,backcast_horizon=4L)
    actual <- forecast(context);hb <- 4:1;hf <- 1:5
    expect_equal(points(actual),c(2+phi^hb*(y[1]-2),2+phi^hf*(as.numeric(tail(y,1))-2)),tolerance=2e-10)
    covariance <- function(h)outer(h,h,Vectorize(function(i,j)phi^abs(i-j)*sum(phi^(2*(seq_len(min(i,j))-1)))))
    target <- matrix(0,9,9);target[1:4,1:4] <- covariance(hb);target[5:9,5:9] <- covariance(hf)
    expect_equal(unname(actual$covariance)/actual$scale_variance,target,tolerance=2e-10)
    expect_equal(unname(actual$regression_covariance),matrix(0,9,9),tolerance=0)
  }
  # Random walk with refitted drift. Backcasts subtract the forward drift.
  actual <- forecast(setup(y,d=1L,include_drift=TRUE,forecast_horizon=4L,backcast_horizon=3L))
  drift <- (as.numeric(tail(y,1))-y[1])/(length(y)-1)
  expect_equal(points(actual),c(y[1]-(3:1)*drift,as.numeric(tail(y,1))+(1:4)*drift),tolerance=1e-11)
  h <- c(-(3:1),1:4);process <- outer(abs(h),abs(h),pmin);process[outer(h,h,'*')<0] <- 0
  expect_equal(unname(actual$known_regression_covariance)/actual$scale_variance,process,tolerance=1e-10)
  expect_equal(unname(actual$regression_covariance)/actual$scale_variance,outer(h,h)/(length(y)-1),tolerance=1e-10)
  expect_true(all(actual$covariance[1:3,4:7]<0))
  # White noise with a fitted mean: common beta error couples past and future.
  actual <- forecast(setup(y,include_mean=TRUE,forecast_horizon=3L,backcast_horizon=2L))
  expect_equal(points(actual),rep(mean(y),5),tolerance=1e-12)
  expect_equal(unname(actual$known_regression_covariance)/actual$scale_variance,diag(5),tolerance=1e-12)
  expect_equal(unname(actual$regression_covariance)/actual$scale_variance,matrix(1/length(y),5,5),tolerance=1e-12)
  # A future-only regressor is unidentified if free; a supplied fixed value is
  # applied exactly. Held-out responses cannot affect a windowed prediction.
  x <- ts(c(rep(0,length(y)),1:4),start=start(y),frequency=4)
  fixed <- forecast(setup(y,regressors=list(x=x),coefficients=c(x=2),forecast_horizon=4L))
  expect_equal(points(fixed),2*(1:4),tolerance=1e-12)
  bad <- forecast(setup(y,regressors=list(x=x),forecast_horizon=4L))
  expect_false(bad$point_available || bad$available)
  expect_identical(bad$center$status,'rank_deficient')
  end <- c(2002,4);one <- setup(y,ar=.4,estimation_end=end,forecast_horizon=4L)
  yy <- y;yy[13:16] <- 1e100
  two <- setup(yy,ar=.4,estimation_end=end,forecast_horizon=4L)
  expect_identical(forecast(one),forecast(two))
  # Original point overflow retains the transformed Gaussian distribution.
  x[17:20] <- 1000
  overflow <- forecast(setup(exp(y),regressors=list(x=x),coefficients=c(x=1),transformation='log',forecast_horizon=4L))
  expect_true(overflow$available && overflow$point_available)
  expect_identical(overflow$original_point_status[['forecast']],'original_scale_range')
  expect_true(all(is.na(overflow$points_original$forecast)))
  expect_equal(as.numeric(overflow$points_transformed$forecast),rep(1000,4),tolerance=1e-12)
  # No residual df retains points. Zero horizons are a valid empty result.
  short <- forecast(setup(ts(c(1,2),frequency=4),ar=rep(NA_real_,2),forecast_horizon=2L),scale='residual_df')
  expect_true(short$point_available);expect_false(short$available)
  expect_identical(short$status,'nonpositive_residual_df')
  expect_true(all(is.na(errors(short))))
  empty <- forecast(setup(y));expect_true(empty$available && empty$point_available)
  expect_identical(empty$status,'no_requested_predictions');expect_equal(dim(empty$covariance),c(0L,0L))
  expect_true(is.null(empty$forecasts))
  for(h in list(-1,NA_real_,Inf,.5,c(1,2),'3'))expect_error(forecast(setup(y),forecast_horizon=h))
  expect_error(forecast(setup(y),forecast_horizon=1L),'prepared regressor context')
  expect_error(forecast(setup(y,forecast_horizon=2L),forecast_error_horizon=3L),'Error horizons')
  expect_error(forecast(list()))
  # Higher differencing, seasonal phase, endpoint gaps and generated drift.
  for(period in c(4L,12L))for(orders in list(c(2L,0L),c(0L,2L),c(1L,1L))) {
    t <- seq_len(120);z <- ts(10+sin(t*.3)+.1*t^2,start=c(2000,3),frequency=period)
    z[c(1,2,17,18,120)] <- NA_real_
    context <- setup(z,d=orders[1],seasonal_d=orders[2],include_drift=TRUE,
      forecast_horizon=period,backcast_horizon=period)
    actual <- forecast(context);target <- forecast_oracle(context,actual$center$parameters$coefficients,period,period)
    expect_true(actual$available)
    expect_true(bounded(points(actual),target$points))
    expect_true(bounded(unname(actual$covariance),target$covariance*actual$scale_variance))
  }
  context <- setup(y,ar=.4,include_mean=TRUE,forecast_horizon=4L,backcast_horizon=3L)
  base <- forecast(context)
  for(multiplier in c(.001,-2,1000)) {
    changed <- forecast(setup(y*multiplier,ar=.4,include_mean=TRUE,forecast_horizon=4L,backcast_horizon=3L))
    expect_equal(points(changed),points(base)*multiplier,tolerance=1e-10)
    expect_equal(changed$covariance,base$covariance*multiplier^2,tolerance=1e-10)
  }
  t <- seq.int(-2,20);x <- ts(sin(t*.7),start=c(1999,2),frequency=4)
  context <- setup(y,regressors=list(x=x),ar=.4,forecast_horizon=4L,backcast_horizon=3L)
  base <- forecast(context);x[20:23] <- x[20:23]+2
  changed <- forecast(setup(y,regressors=list(x=x),ar=.4,forecast_horizon=4L,backcast_horizon=3L))
  expect_identical(changed$center,base$center)
  expect_equal(changed$points_transformed$backcast,base$points_transformed$backcast,tolerance=1e-11)
  expect_equal(as.numeric(changed$points_transformed$forecast-base$points_transformed$forecast),
    rep(2*base$center$likelihood$regression_coefficients[['x']],4),tolerance=1e-11)
  # A nonzero variance component below binary64 range is not rounded to zero.
  x <- ts(c(0,1,0,0,0,0,1e-200),frequency=4)
  tiny <- forecast(setup(ts(c(1,0,2,3,4,5),frequency=4),regressors=list(x=x),forecast_horizon=1L))
  expect_true(tiny$point_available);expect_false(tiny$available)
  expect_true(is.null(tiny$covariance) && is.na(tiny$standard_errors$forecast[1]))
  # Actual worker path: 260 prediction coordinates and estimated-mean error.
  old <- Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  on.exit(if(is.na(old))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=old),add=TRUE)
  context <- setup(y,include_mean=TRUE,forecast_horizon=130L,backcast_horizon=130L)
  Sys.setenv(PERSEPHONE_NUM_THREADS=1);serial <- forecast(context)
  Sys.setenv(PERSEPHONE_NUM_THREADS=2);parallel <- forecast(context)
  expect_true(parallel$available);expect_identical(serial$covariance,parallel$covariance)
  expect_identical(serial$points_transformed,parallel$points_transformed)
  expect_equal(unname(serial$covariance)/serial$scale_variance,diag(260)+matrix(1/length(y),260,260),tolerance=1e-10)
})
