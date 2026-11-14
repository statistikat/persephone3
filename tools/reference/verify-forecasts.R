# Independently qualified endpoint distributions and separately recorded public
# reporting comparisons. A public discrepancy never replaces an accurate point.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'arma-fit-reference.R'));source(file.path(tooling,'forecast-reference.R'))
source(file.path(tooling,'fit-timing.R'))
root <- reference_root();base_dir <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-forecasts.R [--source] [--timing] [--output=<existing empty directory>]')
output <- args[startsWith(args,'--output=')]
output <- if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
    startsWith(output,normalizePath(base_dir))))stop('Output must be empty and outside the frozen corpus')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
before <- extended_parent_hashes(base_dir);pin <- extended_assert_oracle(base_dir)
for(file in c('arma-fit-cases.R','interpolation-oracle.R','forecast-oracle.R'))
  source(file.path(root,'inst/tinytest/fixtures/native',file))
setup <- getFromNamespace('native_arma_objective_setup','persephone3')
forecast <- getFromNamespace('native_arma_forecasts','persephone3')
optimize_native <- getFromNamespace('native_arma_optimize','persephone3')
checks <- fits <- timed_calls <- 0L;rows <- timings <- list()
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
error <- function(a,b)if(length(a)!=length(b)||!all(is.finite(a))||!all(is.finite(b)))Inf else
  if(length(a))max(abs(a-b)/(1+abs(b)))else 0
close <- function(a,b,label)assert(error(a,b)<=2e-8,label)
values <- function(x)unname(unlist(lapply(x,as.numeric)))
align <- function(reference,target,period) {
  ix <- match(round(time(target)*period),round(time(reference)*period))
  assert(!anyNA(ix),'public prediction calendar covers requested native periods')
  as.numeric(reference)[ix]
}
for(ca in forecast_cases()) {
  prepared <- lapply(c('x13','tramoseats'),function(engine) {
    value <- prepare_reference(ca,engine);value$backcasts <- ca$period;value$forecasts <- 12L;value
  });names(prepared) <- c('x13','tramoseats')
  executors <- lapply(names(prepared),reference_forecast_executor);names(executors) <- names(prepared)
  for(engine in names(prepared)) {
    reference <- executors[[engine]](prepared[[engine]]);fits <- fits+1L
    description <- reference$result$description
    blocks <- lapply(c('phi','theta','bphi','btheta'),function(k){a<-description$arima[[k]];if(is.null(a))numeric()else as.numeric(a['value',])})
    names(blocks) <- c('ar','ma','seasonal_ar','seasonal_ma');blocks$ar <- -blocks$ar;blocks$seasonal_ar <- -blocks$seasonal_ar
    arguments <- ca$arguments;arguments$initial <- blocks;context <- do.call(setup,arguments)
    ml <- forecast(context);df <- forecast(context,scale='residual_df')
    independent <- forecast_oracle(context,blocks,ca$period,12L);label <- paste(ca$id,engine)
    assert(ml$available && df$available && ml$point_available,paste(label,'availability'))
    assert(ml$effective_count==reference$n && ml$residual_df==reference$df,paste(label,'original observed count/df'))
    close(ml$sum_squares,reference$Q,paste(label,'original likelihood Q'))
    close(values(ml$points_transformed),independent$points,paste(label,'independent points'))
    close(ml$covariance,independent$covariance*independent$Q/independent$n,paste(label,'independent covariance'))
    close(ml$known_regression_covariance,independent$known*independent$Q/independent$n,paste(label,'independent process component'))
    close(ml$regression_covariance,independent$regression*independent$Q/independent$n,paste(label,'independent regression component'))
    for(direction in c('backcast','forecast')) {
      points <- ml$points_transformed[[direction]];errors <- df$standard_errors[[direction]]
      reported <- reference$outputs[[direction]]
      # RawForecasts excludes fixed regression effects. Forecasts restores
      # them and applies the point inverse transform (no mean correction).
      reported_point <- if(ca$arguments$transformation=='log')log(reported$original)else reported$original
      point_error <- error(as.numeric(points),align(reported_point,points,ca$period))
      standard_error <- error(as.numeric(errors),align(reported$errors,errors,ca$period))
      rows[[length(rows)+1L]] <- data.frame(case=ca$id,engine=engine,direction=direction,
        point_public_status=if(point_error<=2e-8)'within_budget'else'public_reporting_discrepancy',
        error_public_status=if(standard_error<=2e-8)'within_budget'else'public_reporting_discrepancy',
        public_point_scaled_error=point_error,public_standard_error_scaled_error=standard_error,
        independent_point_scaled_error=error(values(ml$points_transformed),independent$points),
        independent_covariance_scaled_error=error(ml$covariance,independent$covariance*independent$Q/independent$n),
        effective_count=ml$effective_count,residual_df=ml$residual_df)
    }
  }
  if('--timing' %in% args) {
    context <- do.call(setup,ca$arguments)
    native_call <- function() {
      fit <- optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L));stopifnot(fit$converged)
      value <- forecast(context,fit$coordinates);stopifnot(value$available);value
    }
    functions <- list(native=native_call,jd_x13=function()executors$x13(prepared$x13),jd_tramo=function()executors$tramoseats(prepared$tramoseats))
    sizes <- vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
      max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
    for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
      iterations <- sizes[[engine]];invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
      while(sample$seconds<.05 && iterations<1000L) {
        iterations <- min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
        invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
      }
      assert(sample$seconds>0,paste(ca$id,'positive timing batch'))
      assert(if(engine=='native')sample$value$available else is.finite(sample$value$result$estimation$likelihood$ll),paste(ca$id,'timed fit'))
      sizes[[engine]] <- iterations
      timings[[length(timings)+1L]] <- data.frame(case=ca$id,engine=engine,round=round,iterations=iterations,
        elapsed_seconds=sample$seconds,seconds_per_fit=sample$seconds/iterations)
    }
  }
  cat(ca$id,'independent forecast distribution PASS\n')
}
# Closed reporting controls distinguish accurate backcasts and the public
# process/beta scaling without relying on any upstream numerical implementation.
controls <- list()
y <- ts(c(1,3,2,4,5,2,3,5,4,2,3,6,2,4,5,3),start=2000,frequency=4)
for(engine in c('x13','tramoseats'))for(d in 0:1)for(logged in c(FALSE,TRUE)) {
  spec <- if(engine=='x13')rjd3x13::regarima_spec('rg0')else rjd3tramoseats::tramo_spec('tr0')
  spec <- rjd3toolkit::set_arima(spec,mean=0,mean.type='Undefined',p=0,d=d,q=0,bp=0,bd=0,bq=0)
  spec <- rjd3toolkit::set_automodel(spec,enabled=FALSE)
  spec <- rjd3toolkit::set_transform(spec,fun=if(logged)'Log'else'None')
  series <- if(logged)exp(y)else y
  reference <- reference_forecast_executor(engine)(list(spec=spec,series=series,context=NULL,backcasts=3L,forecasts=3L))
  fits <- fits+1L
  actual <- forecast(setup(series,d=d,include_mean=d==0,include_drift=d==1,
    transformation=if(logged)'log'else'none',backcast_horizon=3L,forecast_horizon=3L),scale='residual_df')
  label <- paste(engine,'closed',d,logged)
  past <- if(d)1-(3:1)*2/15 else rep(mean(y),3)
  future <- if(d)3+(1:3)*2/15 else rep(mean(y),3)
  close(actual$points_transformed$backcast,past,paste(label,'closed accurate backcast'))
  close(actual$points_transformed$forecast,future,paste(label,'closed accurate forecast'))
  close(reference$outputs$forecast$points,future,paste(label,'closed public forecast'))
  public_past <- if(d)1+(3:1)*2/15 else rep(mean(y),3)
  close(reference$outputs$backcast$points,public_past,paste(label,'observed public backcast convention'))
  public_error_variance <- if(d) {
    h <- 1:3
    reference$Q/(reference$df-1)*h+reference$Q/reference$df*h^2/reference$n
  } else rep(reference$Q/(reference$df-1)+reference$Q/(reference$df*reference$n),3)
  close(as.numeric(reference$outputs$forecast$errors)^2,public_error_variance,paste(label,'closed public split-scale error'))
  controls[[length(controls)+1L]] <- data.frame(engine=engine,d=d,transformation=if(logged)'log'else'none',
    native_nearest_backcast=tail(as.numeric(actual$points_transformed$backcast),1),
    public_nearest_backcast=tail(as.numeric(reference$outputs$backcast$points),1),
    native_nearest_forecast=as.numeric(actual$points_transformed$forecast)[1],
    public_nearest_forecast=as.numeric(reference$outputs$forecast$points)[1],
    native_df_first_variance=actual$standard_errors$forecast[1]^2,
    public_first_variance=reference$outputs$forecast$errors[1]^2,
    closed_public_first_variance=public_error_variance[1],Q=reference$Q,n=reference$n,df=reference$df)
}
assert(identical(before,extended_parent_hashes(base_dir)),'frozen corpus unchanged')
report <- do.call(rbind,rows)
files <- c(file.path(tooling,c('verify-forecasts.R','forecast-reference.R','arma-fit-reference.R','fit-timing.R')),
  file.path(root,c('src/forecast_information.cpp','src/core/forecast_information.hpp','src/regression_likelihood.cpp',
    'src/core/regression_likelihood.hpp','src/rcpp_arma_objective.cpp','R/native_arma_forecasts.R',
    'inst/tinytest/fixtures/native/interpolation-oracle.R','inst/tinytest/fixtures/native/forecast-oracle.R')))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  oracle_manifest_sha256=reference_sha256(file.path(base_dir,'oracle-manifest.R')),R=R.version.string,java=pin$java,
  checks=checks,direct_reference_fits=fits,primary_cases=64L,closed_reporting_controls=8L,scaled_covariance_point_error_budget=2e-8,
  method='Accurate conditional Gaussian endpoint distribution; process and estimated-regression covariance, original-window Q/n or Q/(n-r-k)',
  public_scope='Public Forecasts restores fixed effects; log of its original point compares the transformed median. Public standard errors are compared separately; discrepancies are retained',
  timing_scope='Prepared fit plus native full joint past/future covariance versus public processing, serialization and forecast/backcast points and marginal errors',
  timing_conventions='3 warmed rounds, alternating engines, batches >=50ms when possible; setup/loading/startup and explicit pre-batch GC excluded',
  timing_calls_including_warmup_and_calibration_retries=timed_calls,native_threads=Sys.getenv('PERSEPHONE_NUM_THREADS'),
  parallel=getFromNamespace('.native_parallel_info','persephone3')())
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  write.csv(do.call(rbind,controls),file.path(output,'controls.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'independent/domain/timing checks /',fits,'public fits; public parity reported separately\n')
print(with(report,table(direction,point_public_status)));print(with(report,table(direction,error_public_status)))
print(aggregate(cbind(public_point_scaled_error,public_standard_error_scaled_error,independent_point_scaled_error,independent_covariance_scaled_error)~engine+direction,report,max),row.names=FALSE)
if(length(timings)) {
  totals <- aggregate(seconds_per_fit~engine+round,do.call(rbind,timings),sum)
  cat('Prepared fit plus endpoint distributions, median sum over 64 cases (seconds):\n')
  print(aggregate(seconds_per_fit~engine,totals,median),row.names=FALSE)
}
