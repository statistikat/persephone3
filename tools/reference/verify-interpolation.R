# Prospective conditional prediction-error qualification. Public standard
# errors are compared on the transformed scale, separately from legacy points.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'arma-fit-reference.R'));source(file.path(tooling,'residual-reference.R'))
source(file.path(tooling,'interpolation-reference.R'));source(file.path(tooling,'fit-timing.R'))
root <- reference_root();base_dir <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-interpolation.R [--source] [--timing] [--output=<existing empty directory>]')
output <- args[startsWith(args,'--output=')]
output <- if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
    startsWith(output,normalizePath(base_dir))))stop('Output must be empty and outside the frozen corpus')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
before <- extended_parent_hashes(base_dir);pin <- extended_assert_oracle(base_dir)
source(file.path(root,'inst/tinytest/fixtures/native/arma-fit-cases.R'))
source(file.path(root,'inst/tinytest/fixtures/native/legacy-residual-cases.R'))
source(file.path(root,'inst/tinytest/fixtures/native/interpolation-oracle.R'))
setup <- getFromNamespace('native_arma_objective_setup','persephone3')
information <- getFromNamespace('native_interpolation_information','persephone3')
optimize_native <- getFromNamespace('native_arma_optimize','persephone3')
checks <- fits <- timed_calls <- 0L;rows <- timings <- list()
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
close <- function(a,b,label)assert(length(a)==length(b) && all(is.finite(a)) && all(is.finite(b)) &&
  all(abs(a-b)<=2e-8*(1+abs(b))),label)
error <- function(a,b)if(length(a))max(abs(a-b)/(1+abs(b)))else 0
primary <- arma_fit_cases();focused <- legacy_residual_cases()
for(id in c('4-01-ar-no_regression-log','12-11-ar-no_regression-none'))
  for(phi in c(-.99,-.9,-.7,-.45,-.3,-.1,.1,.2,.3,.4,.45,.5,.7,.9,.99)) {
    ca <- focused[[id]];ca$arguments$ar <- phi
    ca$id <- paste('near_boundary',id,format(phi,scientific=FALSE),sep='-');focused[[ca$id]] <- ca
  }
for(period in c(4L,12L))for(kind in c('fixed_regression','fixed_mean','free_ARMA'))for(logged in c(FALSE,TRUE)) {
  t <- seq_len(72);z <- 3+sin(t*.3)+.1*cos(t*1.7)
  y <- ts(if(logged)exp(z)else z,start=c(2000,2),frequency=period);y[c(1,2,10,11,21,72)]<-NA
  a <- list(series=y,ar=if(kind=='free_ARMA')NA_real_ else .5,transformation=if(logged)'log'else'none')
  if(kind=='fixed_mean')a$mean <- 1
  if(kind=='fixed_regression') {
    a$regressors <- list(fixed=ts(cos(t),start=start(y),frequency=period));a$coefficients <- c(fixed=.2)
  }
  id <- paste(period,kind,if(logged)'log'else'none',sep='-')
  focused[[id]] <- list(id=id,period=period,d=0L,D=0L,arguments=a)
}
for(ca in c(primary,focused)) {
  is_primary <- ca$id %in% names(primary)
  prepared <- lapply(c('x13','tramoseats'),function(engine)
    if(is_primary)prepare_reference(ca,engine)else prepare_legacy_residual_reference(ca,engine))
  names(prepared) <- c('x13','tramoseats')
  executors <- lapply(names(prepared),reference_interpolation_executor);names(executors) <- names(prepared)
  for(engine in names(prepared)) {
    reference <- executors[[engine]](prepared[[engine]]);fits <- fits+1L
    description <- reference$result$description
    blocks <- lapply(c('phi','theta','bphi','btheta'),function(k){a<-description$arima[[k]];if(is.null(a))numeric()else as.numeric(a['value',])})
    names(blocks) <- c('ar','ma','seasonal_ar','seasonal_ma');blocks$ar <- -blocks$ar;blocks$seasonal_ar <- -blocks$seasonal_ar
    arguments <- ca$arguments;arguments$initial <- blocks;context <- do.call(setup,arguments)
    ml <- information(context);df <- information(context,scale='residual_df')
    independent <- interpolation_oracle(context,blocks)
    label <- paste(ca$id,engine)
    assert(ml$available && df$available && ml$point_available,paste(label,'availability'))
    assert(!ml$comparison_only && !ml$propagates_ARMA_uncertainty,paste(label,'conditional accurate contract'))
    assert(ml$effective_count==reference$n && ml$residual_df==reference$df,paste(label,'observed count/df'))
    close(ml$sum_squares,reference$Q,paste(label,'likelihood Q'))
    close(df$standard_errors,reference$standard_errors,paste(label,'public transformed errors'))
    assert(identical(ml$rows,independent$rows),paste(label,'independent missing rows'))
    first <- min(which(!context$prepared$inputs$missing))
    assert(identical(as.integer(ml$rows-first+1L),reference$rows),paste(label,'public missing rows'))
    close(ml$points_transformed,independent$points,paste(label,'accurate points'))
    close(ml$covariance,independent$covariance*independent$Q/independent$n,paste(label,'independent covariance'))
    close(ml$known_regression_covariance,independent$known*independent$Q/independent$n,paste(label,'process component'))
    close(ml$regression_covariance,independent$regression*independent$Q/independent$n,paste(label,'regression component'))
    close(df$covariance,ml$covariance*ml$effective_count/ml$residual_df,paste(label,'explicit df scaling'))
    assert(identical(df$points_transformed,ml$points_transformed),paste(label,'scale-independent point'))
    rows[[length(rows)+1L]] <- data.frame(case=ca$id,engine=engine,scope=if(is_primary)'fitted_matrix'else'focused_prediction',
      missing_count=length(ml$rows),free_regression=ml$free_regression,free_ARMA=ml$free_ARMA,
      effective_count=ml$effective_count,residual_df=ml$residual_df,
      standard_error_scaled_error=error(df$standard_errors,reference$standard_errors),
      independent_covariance_scaled_error=error(ml$covariance,independent$covariance*independent$Q/independent$n),
      independent_point_scaled_error=error(ml$points_transformed,independent$points))
  }
  if(is_primary && '--timing' %in% args) {
    context <- do.call(setup,ca$arguments)
    native_call <- function() {
      f <- optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L));stopifnot(f$converged)
      value <- information(context,f$coordinates);stopifnot(value$available);value
    }
    functions <- list(native_conditional=native_call,jd_x13=function()executors$x13(prepared$x13),
      jd_tramo=function()executors$tramoseats(prepared$tramoseats))
    sizes <- vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
      max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
    for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
      iterations <- sizes[[engine]];invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
      while(sample$seconds<.05 && iterations<1000L) {
        iterations <- min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
        invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
      }
      assert(sample$seconds>0,paste(ca$id,'positive timing batch'))
      assert(if(engine=='native_conditional')sample$value$available else is.finite(sample$value$result$estimation$likelihood$ll),paste(ca$id,'timed fit'))
      sizes[[engine]] <- iterations
      timings[[length(timings)+1L]] <- data.frame(case=ca$id,engine=engine,round=round,iterations=iterations,
        elapsed_seconds=sample$seconds,seconds_per_fit=sample$seconds/iterations)
    }
  }
  cat(ca$id,'interpolation errors PASS\n')
}
assert(identical(before,extended_parent_hashes(base_dir)),'frozen corpus unchanged')
report <- do.call(rbind,rows)
files <- c(file.path(tooling,c('verify-interpolation.R','interpolation-reference.R','residual-reference.R','arma-fit-reference.R','fit-timing.R')),
  file.path(root,c('src/interpolation_information.cpp','src/core/interpolation_information.hpp','src/arma_residuals.cpp',
    'src/core/arma_residuals.hpp','src/regression_likelihood.cpp','src/core/regression_likelihood.hpp','src/rcpp_arma_objective.cpp',
    'R/native_interpolation_information.R','inst/tinytest/fixtures/native/interpolation-oracle.R')))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  oracle_manifest_sha256=reference_sha256(file.path(base_dir,'oracle-manifest.R')),R=R.version.string,java=pin$java,
  checks=checks,direct_reference_fits=fits,scaled_covariance_point_error_budget=2e-8,
  primary_cases=length(primary),focused_cases=length(focused),
  method='Accurate Gaussian missing-coordinate covariance, known-regression and estimated-regression components; conditional on ARMA',
  public_target='MissingValueEstimation.getStandardError on transformed scale, Q/(n-r-k); independent oracle qualifies ML and full covariance',
  timing_scope='prepared native fit plus conditional interpolation covariance versus public processing, serialization and missing-value accessor',
  timing_conventions='3 warmed rounds, alternating engines, batches >=50ms when possible; setup/loading/startup and explicit pre-batch GC excluded',
  timing_calls_including_warmup_and_calibration_retries=timed_calls,native_threads=Sys.getenv('PERSEPHONE_NUM_THREADS'),
  parallel=getFromNamespace('.native_parallel_info','persephone3')())
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',fits,'public fits\n')
print(aggregate(cbind(standard_error_scaled_error,independent_covariance_scaled_error,independent_point_scaled_error)~engine,report,max),row.names=FALSE)
if(length(timings)) {
  totals <- aggregate(seconds_per_fit~engine+round,do.call(rbind,timings),sum)
  cat('Prepared fit plus conditional interpolation errors, median sum over 64 cases (seconds):\n')
  print(aggregate(seconds_per_fit~engine,totals,median),row.names=FALSE)
}
