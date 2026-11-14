local({
  setup <- getFromNamespace("native_arma_objective_setup","persephone3")
  residuals <- getFromNamespace("native_arma_residuals","persephone3")
  optimize <- getFromNamespace("native_arma_optimize","persephone3")
  source("fixtures/native/arma-fit-cases.R",local=TRUE)
  source("fixtures/native/arma-residual-oracle.R",local=TRUE)
  source("fixtures/native/legacy-residual-cases.R",local=TRUE)
  source("fixtures/native/legacy-ar1-rounding.R",local=TRUE)
  # Pinned public-API outputs catch orientation changes at mathematically zero
  # QR pivots. These comparisons run without loading any reference package.
  controls <- legacy_residual_cases()
  for(target in legacy_ar1_rounding) {
    ca <- controls[[target$input_case]];ca$arguments$ar <- target$phi
    actual <- residuals(do.call(setup,ca$arguments),reporting="legacy")
    expect_true(actual$available)
    expect_equal(as.numeric(actual$reported$unit_residuals),target$residuals,tolerance=2e-8)
    expect_equal(start(actual$reported$unit_residuals),target$start,tolerance=0)
    expect_equal(actual$interpolation$points_original,target$points,tolerance=2e-8)
    expect_equal(actual$reported$sum_squares,actual$sum_squares,tolerance=1e-10)
  }
  for(ca in arma_fit_cases()) {
    context <- do.call(setup,ca$arguments)
    fit <- optimize(context,control=list(max_iterations=200L,max_evaluations=4000L))
    accurate <- residuals(context,fit$coordinates)
    before <- serialize(context,NULL)
    legacy <- residuals(context,fit$coordinates,reporting="legacy")
    basis <- arma_residual_basis(ca,fit$final$parameters$coefficients)
    preliminary <- legacy_preliminary_points(context$prepared$inputs$original,
      accurate$interpolation$rows,ca$arguments$transformation)
    points <- if((ca$d+ca$D)%%2)2*preliminary-basis$interpolated else basis$interpolated
    expect_true(legacy$available && legacy$reported$available,info=ca$id)
    expect_true(legacy$comparison_only && legacy$reported$comparison_only)
    expect_identical(legacy$reporting,"legacy")
    expect_identical(legacy$observed,accurate$observed)
    expect_identical(legacy$complete,accurate$complete)
    expect_identical(legacy$center,accurate$center)
    expect_identical(legacy$accurate_interpolation,accurate$interpolation)
    expect_identical(legacy$sum_squares,accurate$sum_squares)
    expect_equal(legacy$legacy$preliminary_transformed,preliminary,tolerance=1e-12)
    expect_equal(legacy$interpolation$points_transformed,points,tolerance=1e-10)
    expect_equal(legacy$interpolation$points_original,if(ca$arguments$transformation=="log")exp(points)else points,tolerance=1e-10)
    # Independently whiten the reported series at the independent GLS beta.
    y <- as.numeric(legacy$interpolation$transformed[match(basis$rows,ca$rows)])-
      .2*as.numeric(ca$arguments$regressors$fixed)[basis$rows]
    X <- cbind(as.numeric(ca$arguments$regressors$x)[basis$rows],
      basis$rows^(ca$d+ca$D)/(factorial(ca$d+ca$D)*ca$period^ca$D))
    D <- diag(length(y));if(ca$d)D<-diff(D,differences=ca$d);if(ca$D)D<-diff(D,lag=ca$period,differences=ca$D)
    expected <- drop(forwardsolve(t(chol(arma_fit_covariance(fit$final$parameters$coefficients,ca$period,nrow(D)))),
      D%*%(y-X%*%basis$coefficients)))
    expect_equal(as.numeric(legacy$reported$unit_residuals),expected,tolerance=1e-8,info=ca$id)
    expect_equal(legacy$reported$sum_squares,sum(expected^2),tolerance=1e-9)
    expect_equal(as.numeric(legacy$reported$scaled_residuals),expected/sqrt(accurate$scale_variance),tolerance=1e-8)
    expect_identical(legacy$reported$source_rows,accurate$complete$source_rows)
    expect_identical(legacy$reported$row_alignment,"complete_finite_calendar")
    expect_identical(legacy$reported$normalization_source,"accurate_likelihood")
    expect_identical(legacy$interpolation$uncertainty_status,"comparison_only")
    expect_true(all(is.na(legacy$interpolation$standard_errors)))
    present <- !context$prepared$inputs$missing
    expect_identical(legacy$interpolation$original[present],context$prepared$inputs$original[present])
    expect_identical(serialize(context,NULL),before)
    expect_identical(unserialize(serialize(legacy,NULL)),legacy)
  }
  for(ca in legacy_residual_cases()) {
    context <- do.call(setup,ca$arguments)
    accurate <- residuals(context);legacy <- residuals(context,reporting="legacy")
    expect_true(legacy$available,info=ca$id)
    expect_identical(legacy$observed,accurate$observed)
    expect_identical(legacy$complete,accurate$complete)
    preliminary <- legacy_preliminary_points(context$prepared$inputs$original,
      accurate$interpolation$rows,ca$arguments$transformation)
    expected <- if((ca$d+ca$D)%%2)2*preliminary-accurate$interpolation$points_transformed else accurate$interpolation$points_transformed
    expect_equal(legacy$interpolation$points_transformed,expected,tolerance=1e-10)
    expect_equal(legacy$legacy$preliminary_transformed,preliminary,tolerance=1e-12)
    expect_equal(tail(time(legacy$reported$unit_residuals),1),tail(time(accurate$reported$unit_residuals),1),tolerance=0)
    expect_identical(legacy$interpolation$unfilled_rows,accurate$interpolation$unfilled_rows)
    if(legacy$free_regression==0L) {
      expect_true(legacy$legacy$compressed)
      expect_equal(length(legacy$reported$unit_residuals),legacy$effective_count,tolerance=0)
      expect_equal(sum(legacy$reported$unit_residuals^2),accurate$sum_squares,tolerance=1e-9)
      expect_identical(legacy$reported$source_rows,integer())
      expect_identical(legacy$reported$row_alignment,"reported_end_aligned_not_observation_mapping")
    } else {
      expect_false(legacy$legacy$compressed)
      expect_identical(legacy$reported$calendar_rows,legacy$complete$rows)
    }
    df <- residuals(context,scale="residual_df",reporting="legacy")
    expect_identical(df$reported$unit_residuals,legacy$reported$unit_residuals)
    expect_equal(df$reported$scaled_residuals,legacy$reported$scaled_residuals*
      sqrt(legacy$residual_df/legacy$effective_count),tolerance=1e-10)
  }
  seasonal <- ts(c(7,21,3,13,9,NA,8,16,11,27,6,18,14,31,12,19),frequency=4)
  result <- residuals(setup(seasonal,seasonal_d=1L),reporting="legacy")
  expect_equal(result$interpolation$points_original,-7,tolerance=1e-12)
  expect_equal(result$accurate_interpolation$points_original,24,tolerance=1e-12)
  expect_equal(as.numeric(result$reported$unit_residuals),c(3+sqrt(2),5,3,2,3-sqrt(2),-2,2,3,4,6,1),tolerance=1e-12)
  expect_equal(result$reported$sum_squares,130,tolerance=1e-12)
  expect_equal(start(result$reported$unit_residuals),c(2,2),tolerance=0)
  expect_identical(result$reported$source_rows,integer())
  # Fixed effects and a known stationary mean do not select the free-regression
  # reporting branch; both are restored in missing-value points.
  y <- ts(c(1,NA,3,4,5,2),frequency=4);x <- ts(seq_along(y),frequency=4)
  reference <- residuals(setup(y,ar=.5),reporting="legacy")
  shifted <- residuals(setup(y+3+2*x,ar=.5,mean=3,regressors=list(x=x),coefficients=c(x=2)),reporting="legacy")
  expect_equal(shifted$reported$unit_residuals,reference$reported$unit_residuals,tolerance=1e-12)
  expect_equal(shifted$interpolation$points_original,reference$interpolation$points_original+7,tolerance=1e-12)
  for(multiplier in c(.001,-2,1000)) {
    changed <- residuals(setup(y*multiplier,ar=.5),reporting="legacy")
    expect_equal(changed$reported$unit_residuals,reference$reported$unit_residuals*multiplier,tolerance=1e-10)
    expect_equal(changed$reported$scaled_residuals,reference$reported$scaled_residuals*sign(multiplier),tolerance=1e-10)
    expect_equal(changed$interpolation$points_original,reference$interpolation$points_original*multiplier,tolerance=1e-10)
  }
  many <- ts(sin(seq_len(120)*.7)+1,frequency=4);many[seq.int(20L,98L,2L)] <- NA_real_
  many_result <- residuals(setup(many),reporting="legacy")
  expect_true(many_result$available)
  expect_equal(length(many_result$interpolation$rows),40L,tolerance=0)
  expect_equal(many_result$reported$sum_squares,sum(many^2,na.rm=TRUE),tolerance=1e-10)
  extreme <- ts(exp(c(5,20,3,13,680,NA,680,16,11,28,6,18,14,31,12,19)),frequency=4)
  range <- residuals(setup(extreme,seasonal_d=1L,transformation="log"),reporting="legacy")
  expect_false(range$available || range$interpolation$available)
  expect_true(range$accurate_available && range$accurate_interpolation$available)
  expect_identical(range$status,"legacy_original_scale_range")
  expect_equal(range$accurate_interpolation$points_transformed,24,tolerance=1e-10)
  for(value in list(ts(rep(NA_real_,12),frequency=4),ts(rep(0,12),frequency=4))) {
    unavailable <- residuals(setup(value),reporting="legacy")
    expect_false(unavailable$available || unavailable$reported$available || unavailable$interpolation$available)
    expect_identical(unavailable$interpolation$original,value)
  }
  for(reporting in list(NULL,NA_character_,c("accurate","legacy"),matrix("legacy"),"unknown",1))
    expect_error(residuals(setup(y),reporting=reporting))
  old <- Sys.getenv("PERSEPHONE_NUM_THREADS",unset=NA_character_)
  on.exit(if(is.na(old))Sys.unsetenv("PERSEPHONE_NUM_THREADS")else Sys.setenv(PERSEPHONE_NUM_THREADS=old),add=TRUE)
  y <- ts(2+sin(seq_len(70000)*1.7),frequency=12);y[100]<-NA
  context <- setup(y,include_mean=TRUE)
  Sys.setenv(PERSEPHONE_NUM_THREADS=1);serial <- residuals(context,reporting="legacy")
  Sys.setenv(PERSEPHONE_NUM_THREADS=2);threaded <- residuals(context,reporting="legacy")
  expect_true(threaded$available)
  expect_identical(serial$reported,threaded$reported)
  expect_identical(serial$interpolation,threaded$interpolation)
})
