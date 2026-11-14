# Explicit public-API comparisons. The native path receives no Java state or
# reference target vectors; all reporting calculations execute in native C++.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'arma-fit-reference.R'));source(file.path(tooling,'residual-reference.R'))
source(file.path(tooling,'fit-timing.R'))
root <- reference_root();base_dir <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--timing') & !startsWith(args,'--output=')))
 stop('Usage: verify-legacy-residuals.R [--source] [--timing] [--output=<existing empty directory>]')
output <- args[startsWith(args,'--output=')]
output <- if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
 startsWith(output,normalizePath(base_dir))))stop('Output must be empty and outside the frozen corpus')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
 if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
before <- extended_parent_hashes(base_dir);pin <- extended_assert_oracle(base_dir)
source(file.path(root,'inst/tinytest/fixtures/native/arma-fit-cases.R'))
source(file.path(root,'inst/tinytest/fixtures/native/legacy-residual-cases.R'))
setup <- getFromNamespace('native_arma_objective_setup','persephone3')
residuals <- getFromNamespace('native_arma_residuals','persephone3')
optimize_native <- getFromNamespace('native_arma_optimize','persephone3')
checks <- 0L;fits <- 0L;rows <- timings <- rounding_fixtures <- list();timed_calls <- 0L
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
close <- function(a,b,label)assert(length(a)==length(b) && all(is.finite(a)) && all(is.finite(b)) &&
  all(abs(a-b)<=2e-8*(1+abs(b))),label)
error <- function(a,b)if(length(a))max(abs(a-b)/(1+abs(b)))else 0
ordinal <- function(start,period)start[1L]*period+start[2L]-1L
primary <- arma_fit_cases();focused <- legacy_residual_cases()
for(id in c('4-01-ar-no_regression-log','12-11-ar-no_regression-none'))
 for(phi in c(-.99,-.9,-.7,-.45,-.3,-.1,.1,.2,.3,.4,.45,.5,.7,.9,.99)) {
   ca <- focused[[id]];ca$arguments$ar <- phi
   ca$id <- paste('rounding',id,format(phi,scientific=FALSE),sep='-');focused[[ca$id]] <- ca
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
# Exact dates/counts and the existing prospective 2e-8 scaled residual/point
# budget apply to every branch. Differences cannot be reclassified as passes.
for(ca in c(primary,focused)) {
 is_primary <- ca$id %in% names(primary)
 prepared <- lapply(c('x13','tramoseats'),function(engine) {
   if(is_primary)prepare_reference(ca,engine)else prepare_legacy_residual_reference(ca,engine)
 });names(prepared) <- c('x13','tramoseats')
 executors <- lapply(names(prepared),reference_residual_executor);names(executors) <- names(prepared)
 for(engine in names(prepared)) {
   reference <- executors[[engine]](prepared[[engine]]);fits <- fits+1L
   description <- reference$result$description
   blocks <- lapply(c('phi','theta','bphi','btheta'),function(k){a<-description$arima[[k]];if(is.null(a))numeric()else as.numeric(a['value',])})
   names(blocks) <- c('ar','ma','seasonal_ar','seasonal_ma');blocks$ar <- -blocks$ar;blocks$seasonal_ar <- -blocks$seasonal_ar
   arguments <- ca$arguments;arguments$initial <- blocks;context <- do.call(setup,arguments)
   accurate <- residuals(context);legacy <- residuals(context,reporting='legacy')
   df <- residuals(context,reporting='legacy',scale='residual_df')
   observed <- which(!context$prepared$inputs$missing);trim <- seq.int(min(observed),max(observed))
   label <- paste(ca$id,engine);reported <- as.numeric(legacy$reported$unit_residuals)
   assert(accurate$available && legacy$available && legacy$reported$available && df$available,paste(label,'availability'))
   assert(legacy$comparison_only && legacy$reported$comparison_only,paste(label,'explicit comparison'))
   assert(identical(legacy$observed,accurate$observed) && identical(legacy$complete,accurate$complete),paste(label,'accurate residuals retained'))
   assert(identical(legacy$center,accurate$center) && identical(legacy$sum_squares,accurate$sum_squares),paste(label,'likelihood/coefficients unchanged'))
   assert(identical(legacy$accurate_interpolation,accurate$interpolation),paste(label,'accurate points retained'))
   close(reported,reference$residuals$values,paste(label,'public residuals'))
   close(reported,reference$result$estimation$res,paste(label,'exported residuals'))
   close(legacy$interpolation$points_original,reference$missing,paste(label,'missing estimates'))
   close(as.numeric(legacy$interpolation$transformed[trim]),reference$transformed$values,paste(label,'transformed interpolation'))
   close(as.numeric(legacy$interpolation$original[trim]),reference$interpolated$values,paste(label,'original interpolation'))
   assert(ordinal(start(legacy$reported$unit_residuals),ca$period)==ordinal(reference$residuals$start,ca$period),paste(label,'reported start'))
   assert(frequency(legacy$reported$unit_residuals)==reference$residuals$frequency,paste(label,'reported frequency'))
   assert(legacy$effective_count==reference$n && legacy$residual_df==reference$dfc,paste(label,'count/df'))
   assert(legacy$legacy$compressed==(legacy$free_regression==0L),paste(label,'reporting branch'))
   assert(if(legacy$legacy$compressed)length(legacy$reported$source_rows)==0L else
     identical(legacy$reported$source_rows,legacy$complete$source_rows),paste(label,'row interpretation'))
   close(legacy$sum_squares,reference$ssq,paste(label,'accurate quadratic form'))
   close(legacy$reported$sum_squares,sum(reference$residuals$values^2),paste(label,'reported quadratic form'))
   close(as.numeric(legacy$reported$scaled_residuals),reported/sqrt(reference$ssq/reference$n),paste(label,'ML normalization'))
   close(as.numeric(df$reported$scaled_residuals),reported/sqrt(reference$ssq/reference$dfc),paste(label,'residual-df normalization'))
   if(engine=='x13' && startsWith(ca$id,'rounding-')) {
     id <- sub('^rounding-','',ca$id)
     id <- sub('-[-]?[0-9.]+$','',id)
     rounding_fixtures[[ca$id]] <- list(input_case=id,phi=ca$arguments$ar,
       residuals=reference$residuals$values,start=reference$residuals$start,
       points=reference$missing)
   }
   rows[[length(rows)+1L]] <- data.frame(case=ca$id,engine=engine,scope=if(is_primary)'fitted_matrix'else'focused_reporting',
     basis=legacy$reported$basis,correction_sign=legacy$legacy$correction_sign,free_regression=legacy$free_regression,
     free_ARMA=legacy$free_ARMA,effective_count=legacy$effective_count,reported_count=length(reported),
     likelihood_Q=legacy$sum_squares,reported_Q=legacy$reported$sum_squares,
     residual_scaled_error=error(reported,reference$residuals$values),
     missing_original_scaled_error=error(legacy$interpolation$points_original,reference$missing),
     interpolation_transformed_scaled_error=error(as.numeric(legacy$interpolation$transformed[trim]),reference$transformed$values))
 }
 if(is_primary && '--timing' %in% args) {
   context <- do.call(setup,ca$arguments)
   native_call <- function(reporting) {
     f <- optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L));stopifnot(f$converged)
     r <- residuals(context,f$coordinates,reporting=reporting);stopifnot(r$available && r$reported$available);r
   }
   functions <- list(native_accurate=function()native_call('accurate'),native_legacy=function()native_call('legacy'),
     jd_x13=function()executors$x13(prepared$x13),jd_tramo=function()executors$tramoseats(prepared$tramoseats))
   sizes <- vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
     max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
   for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
     iterations <- sizes[[engine]];invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
     while(sample$seconds<.05 && iterations<1000L) {
       iterations <- min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
       invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
     }
     assert(sample$seconds>0,paste(ca$id,'positive timing batch'))
     assert(if(startsWith(engine,'native'))sample$value$available else is.finite(sample$value$result$estimation$likelihood$ll),paste(ca$id,'timed fit'))
     sizes[[engine]] <- iterations
     timings[[length(timings)+1L]] <- data.frame(case=ca$id,engine=engine,round=round,iterations=iterations,
       elapsed_seconds=sample$seconds,seconds_per_fit=sample$seconds/iterations)
   }
 }
 cat(ca$id,'legacy reporting PASS\n')
}
assert(identical(before,extended_parent_hashes(base_dir)),'frozen corpus unchanged')
report <- do.call(rbind,rows)
files <- c(file.path(tooling,c('verify-legacy-residuals.R','residual-reference.R','arma-fit-reference.R','fit-timing.R')),
 file.path(root,c('src/arma_residuals.cpp','src/core/arma_residuals.hpp','src/regression_likelihood.cpp','src/core/regression_likelihood.hpp',
 'src/rcpp_arma_objective.cpp','R/native_arma_residuals.R','inst/tinytest/fixtures/native/legacy-residual-cases.R',
 'inst/tinytest/fixtures/native/legacy-ar1-rounding.R')))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
 parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
 oracle_manifest_sha256=reference_sha256(file.path(base_dir,'oracle-manifest.R')),R=R.version.string,java=pin$java,
 checks=checks,direct_reference_fits=fits,scaled_point_residual_budget=2e-8,
 primary_cases=length(primary),focused_cases=length(focused),
 method='Java-free preliminary fills, differencing sign, complete or chronological-missing Householder reporting; accurate outputs retained',
 inputs='native objective and reporting=legacy only; no reference target, Java state or optimizer history',
 timing_scope='prepared fits plus accurate or accurate-and-legacy outputs versus public processing, serialization and reporting accessors',
 timing_conventions='3 warmed rounds, alternating engines, batches >=50ms when possible; setup/loading/startup and explicit pre-batch GC excluded',
 timing_calls_including_warmup_and_calibration_retries=timed_calls,native_threads=Sys.getenv('PERSEPHONE_NUM_THREADS'),
 parallel=getFromNamespace('.native_parallel_info','persephone3')())
if(!is.null(output)) {
 write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
 writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
 writeLines(c('# Frozen public JDemetra 3.9.0 output; generated by verify-legacy-residuals.R.',
   '# Provenance: tools/reference/information/2026-11-14-residuals-legacy/metadata.R.',
   'legacy_ar1_rounding <-',sub('[[:blank:]]+$','',capture.output(dput(rounding_fixtures)))),
   file.path(output,'legacy-ar1-rounding.R'))
 if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',fits,'public fits\n')
print(aggregate(cbind(residual_scaled_error,missing_original_scaled_error,interpolation_transformed_scaled_error)~engine,report,max),row.names=FALSE)
if(length(timings)) {
 totals <- aggregate(seconds_per_fit~engine+round,do.call(rbind,timings),sum)
 cat('Prepared fit plus reporting, median sum over 64 cases (seconds):\n')
 print(aggregate(seconds_per_fit~engine,totals,median),row.names=FALSE)
}
