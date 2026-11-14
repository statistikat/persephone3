# Explicit pinned public-API comparison; never changes frozen reference data.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'arma-fit-reference.R'));source(file.path(tooling,'residual-reference.R'))
source(file.path(tooling,'fit-timing.R'))
root <- reference_root();base_dir <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--timing') & !startsWith(args,'--output='))) {
 stop('Usage: verify-residuals.R [--source] [--timing] [--output=<existing empty directory>]')
}
output <- args[startsWith(args,'--output=')]
output <- if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
 startsWith(output,normalizePath(base_dir))))stop('Output must be empty and outside the frozen corpus')
if('--source' %in% args) {pkgload::load_all(root,quiet=TRUE,compile=FALSE)
} else if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
before <- extended_parent_hashes(base_dir);pin <- extended_assert_oracle(base_dir)
source(file.path(root,'inst/tinytest/fixtures/native/arma-fit-cases.R'))
source(file.path(root,'inst/tinytest/fixtures/native/arma-residual-oracle.R'))
setup <- getFromNamespace('native_arma_objective_setup','persephone3')
residuals <- getFromNamespace('native_arma_residuals','persephone3')
optimize_native <- getFromNamespace('native_arma_optimize','persephone3')
checks <- 0L;rows <- timings <- list();timed_calls <- 0L
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
close <- function(a,b,tolerance,label) assert(length(a)==length(b) && all(is.finite(a)) && all(is.finite(b)) &&
  all(abs(a-b)<=tolerance*(1+abs(b))),label)
error <- function(a,b)if(length(a))max(abs(a-b)/(1+abs(b)))else 0
matches <- function(a,b)length(a)==length(b) && all(is.finite(a)) && all(is.finite(b)) &&
  all(abs(a-b)<=2e-8*(1+abs(b)))
ordinal <- function(value)value$start[1L]*value$frequency+value$start[2L]-1
# Independent reconstruction of the diagnostic residual vector for GIVEN
# imputed values and coefficients. This does not optimize missing coordinates.
reported_residuals <- function(case,blocks,basis,values) {
  rows <- basis$rows
  y <- values-.2*as.numeric(case$arguments$regressors$fixed)[rows]
  X <- cbind(as.numeric(case$arguments$regressors$x)[rows],
    rows^(case$d+case$D)/(factorial(case$d+case$D)*case$period^case$D))
  D <- diag(length(y))
  if(case$d)D <- diff(D,differences=case$d)
  if(case$D)D <- diff(D,lag=case$period,differences=case$D)
  drop(forwardsolve(t(chol(arma_fit_covariance(blocks,case$period,nrow(D)))),
    D%*%(y-X%*%basis$coefficients)))
}
# The prospective 2e-8*(1+abs(target)) budget is unchanged. Public differences
# are recorded as failed parity, separately from independent qualification and
# a bounded characterization of the reporting discrepancy. No rebaselining.
for(ca in arma_fit_cases()) {
 prepared <- list(x13=prepare_reference(ca,'x13'),tramoseats=prepare_reference(ca,'tramoseats'))
 executors <- lapply(names(prepared),reference_residual_executor);names(executors) <- names(prepared)
 for(engine in names(prepared)) {
   reference <- executors[[engine]](prepared[[engine]]);description <- reference$result$description
   blocks <- lapply(c('phi','theta','bphi','btheta'),function(k){a<-description$arima[[k]];if(is.null(a))numeric()else as.numeric(a['value',])})
   names(blocks) <- names(ca$specification);blocks$ar <- -blocks$ar;blocks$seasonal_ar <- -blocks$seasonal_ar
   arguments <- ca$arguments;arguments$initial <- blocks;context <- do.call(setup,arguments)
   native <- residuals(context);df <- residuals(context,scale='residual_df')
   basis <- arma_residual_basis(ca,blocks)
   observed <- which(!context$prepared$inputs$missing);trim <- seq.int(min(observed),max(observed))
   current <- as.numeric(native$complete$unit_residuals[native$complete$rows]);label <- paste(ca$id,engine)
   assert(native$available && native$complete$available && df$available,paste(label,'availability'))
   close(reference$residuals$values,reference$result$estimation$res,2e-8,paste(label,'public residual accessors agree'))
   close(current,basis$residuals,2e-8,paste(label,'independent complete residuals'))
   close(native$interpolation$points_transformed,basis$interpolated,2e-8,paste(label,'independent interpolation'))
   parity <- matches(current,reference$residuals$values) &&
     matches(native$interpolation$points_original,reference$missing) &&
     matches(as.numeric(native$interpolation$original[trim]),reference$interpolated$values) &&
     matches(as.numeric(native$interpolation$transformed[trim]),reference$transformed$values)
   convention <- if(parity)'matches_accurate'else'reflected_missing_points'
   expected <- as.numeric(native$interpolation$transformed[trim])
   if(!parity) {
     assert(ca$d==0L && ca$D==1L && length(basis$missing)>0L,paste(label,'bounded discrepancy scope'))
     # The public estimation response contains preliminary fills after fixed
     # effects. Restore the known effect before testing the reflection identity.
     preliminary <- as.numeric(reference$result$estimation$y)[basis$missing]+
       .2*as.numeric(ca$arguments$regressors$fixed)[basis$rows[basis$missing]]
     expected[basis$missing] <- 2*preliminary-basis$interpolated
   }
   close(expected,reference$transformed$values,2e-8,paste(label,'characterized transformed interpolation'))
   expected_original <- if(ca$arguments$transformation=='log')exp(expected)else expected
   close(expected_original,reference$interpolated$values,2e-8,paste(label,'characterized original interpolation'))
   close(expected_original[basis$missing],reference$missing,2e-8,paste(label,'characterized missing accessor'))
   reconstructed <- reported_residuals(ca,blocks,basis,reference$transformed$values)
   close(reconstructed,reference$residuals$values,2e-8,paste(label,'diagnostic residuals from public interpolation'))
   start <- context$prepared$inputs$observed_domain$start_ordinal
   assert(ordinal(reference$residuals)==start+min(native$complete$rows)-1,paste(label,'residual start'))
   assert(ordinal(reference$interpolated)==start+min(trim)-1 && ordinal(reference$transformed)==start+min(trim)-1,paste(label,'interpolation start'))
   assert(reference$residuals$frequency==ca$period && reference$interpolated$frequency==ca$period,paste(label,'frequency'))
   assert(native$effective_count==reference$n && native$residual_df==reference$dfc,paste(label,'finite count/estimated-ARMA df'))
   assert(reference$df==native$effective_count-native$free_regression,paste(label,'regression-only df'))
   close(native$sum_squares,reference$ssq,2e-8,paste(label,'quadratic form'))
   close(native$scale_variance,reference$ssq/reference$n,2e-8,paste(label,'ML scale'))
   close(df$scale_variance,reference$ssq/reference$dfc,2e-8,paste(label,'residual-df scale'))
   close(sum(native$observed$standardized_innovations^2,na.rm=TRUE),reference$n,2e-8,paste(label,'standardized finite norm'))
   assert(all(is.na(native$observed$unit_innovations[context$prepared$inputs$missing])),paste(label,'no observed innovation at missing rows'))
   rows[[length(rows)+1L]] <- data.frame(case=ca$id,engine=engine,effective_count=reference$n,
     residual_df=reference$dfc,complete_rows=length(current),imputed_rows=length(reference$missing),
     public_parity=parity,public_convention=convention,
     native_Q=native$sum_squares,public_likelihood_Q=reference$ssq,
     native_complete_Q=sum(current^2),public_complete_Q=sum(reference$residuals$values^2),
     residual_scaled_error=error(current,reference$residuals$values),
     interpolation_original_scaled_error=error(native$interpolation$points_original,reference$missing),
     interpolation_transformed_scaled_error=error(as.numeric(native$interpolation$transformed[trim]),reference$transformed$values),
     independent_residual_scaled_error=error(current,basis$residuals),
     reflected_point_scaled_error=error(expected,reference$transformed$values),
     reconstructed_public_residual_scaled_error=error(reconstructed,reference$residuals$values))
 }
 if('--timing' %in% args) {
   context <- do.call(setup,ca$arguments)
   functions <- list(native=function(){f<-optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L));
       stopifnot(f$converged);r<-residuals(context,f$coordinates);stopifnot(r$available && r$complete$available);r},
     jd_x13=function()executors$x13(prepared$x13),jd_tramo=function()executors$tramoseats(prepared$tramoseats))
   sizes <- vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
     max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
   for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
     iterations <- sizes[[engine]];invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations)
     timed_calls <- timed_calls+iterations
     while(sample$seconds<.05 && iterations<1000L) {
       iterations <- min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
       invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
     }
     assert(sample$seconds>0,paste(ca$id,'positive timing batch'))
     assert(if(engine=='native')sample$value$available && sample$value$complete$available else is.finite(sample$value$result$estimation$likelihood$ll),paste(ca$id,'timed fit'))
     sizes[[engine]] <- iterations
     timings[[length(timings)+1L]] <- data.frame(case=ca$id,engine=engine,round=round,iterations=iterations,
       elapsed_seconds=sample$seconds,seconds_per_fit=sample$seconds/iterations)
   }
 }
 cat(ca$id,'independent qualification and reference characterization PASS\n')
}
minimal <- list()
for(engine in c('x13','tramoseats')) {
 y <- ts(c(7,21,3,13,9,NA,8,16,11,27,6,18,14,31,12,19),frequency=4)
 spec <- if(engine=='x13')rjd3x13::regarima_spec('rg0')else rjd3tramoseats::tramo_spec('tr0')
 spec <- rjd3toolkit::set_arima(spec,mean=NULL,p=0L,d=0L,q=0L,bp=0L,bd=1L,bq=0L)
 spec <- rjd3toolkit::set_automodel(spec,enabled=FALSE)
 spec <- rjd3toolkit::set_transform(spec,fun='None')
 reference <- reference_residual_executor(engine)(list(series=y,spec=spec,context=NULL))
 native <- residuals(setup(y,seasonal_d=1L))
 close(native$interpolation$points_original,24,2e-8,paste(engine,'closed seasonal conditional point'))
 close(reference$missing,-7,2e-8,paste(engine,'public seasonal reporting discrepancy'))
 close(c(native$sum_squares,reference$ssq,native$complete$sum_squares,
   sum(reference$residuals$values^2)),rep(130,4),2e-8,paste(engine,'closed quadratic forms'))
 # The zero-order model reports a compressed, rotated residual basis instead
 # of either native time-aligned vector. Its norm is correct despite the wrong
 # reported point. Do not generalize a reporting-emulation rule from the matrix.
 assert(length(reference$residuals$values)==native$effective_count,
   paste(engine,'zero-order compressed residual count'))
 minimal[[engine]] <- list(series=as.numeric(y),frequency=4L,d=0L,D=1L,missing_row=6L,
   closed_point=24,preliminary_fill=8.5,native_point=native$interpolation$points_original,
   public_point=reference$missing,likelihood_Q=reference$ssq,public_residual_Q=sum(reference$residuals$values^2),
   public_residuals=reference$residuals,native_complete_residuals=as.numeric(native$complete$unit_residuals[native$complete$rows]),
   native_observed_innovations=as.numeric(native$observed$unit_innovations[native$observed$rows]))
}
assert(identical(before,extended_parent_hashes(base_dir)),'frozen corpus unchanged')
report <- do.call(rbind,rows)
assert(sum(report$public_parity)==104L,'104/128 matrix fits match public residual/interpolation outputs')
files <- c(file.path(tooling,c('verify-residuals.R','residual-reference.R','arma-fit-reference.R','fit-timing.R')),
 file.path(root,c('src/arma_residuals.cpp','src/core/arma_residuals.hpp','src/regression_likelihood.cpp','src/rcpp_arma_objective.cpp',
 'R/native_arma_residuals.R','inst/tinytest/fixtures/native/innovation-oracle.R')))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
 parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),
 source_sha256=setNames(reference_sha256(files),basename(files)),
 oracle_manifest_sha256=reference_sha256(file.path(base_dir,'oracle-manifest.R')),R=R.version.string,java=pin$java,
 checks=checks,direct_reference_fits=130L,scaled_point_residual_budget=2e-8,
 public_parity=list(passed=sum(report$public_parity),total=nrow(report),
   mismatches=unique(report$case[!report$public_parity]),status='accurate outputs qualified; separate Java-free legacy reporting requested'),
 minimal_seasonal_example=minimal,
 observed_innovations_budget=2e-7,method='finite observed innovations; separate complete-span residuals and Gaussian interpolation points',
 interpolation_scope='internal missing rows only; inverse points on log scale, no lognormal mean correction or interpolation error estimates',
 timing_scope='prepared native fit plus residual/interpolation outputs versus prepared public X13/TRAMO models, serialization and public residual/interpolation accessors',
 timing_conventions='3 warmed rounds, alternating engines, batches >=50ms when possible, GC outside timed regions; setup/loading/startup excluded; API workloads differ',
 timing_calls_including_warmup_and_calibration_retries=timed_calls,native_threads=Sys.getenv('PERSEPHONE_NUM_THREADS'),
 parallel=getFromNamespace('.native_parallel_info','persephone3')())
if(!is.null(output)) {
 write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
 writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
 if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'independent qualification / characterization / timing checks; 130 reference fits\n')
cat('PUBLIC PARITY:',sum(report$public_parity),'/',nrow(report),'matrix fits; 24 mismatches remain explicitly unqualified\n')
print(aggregate(cbind(residual_scaled_error,interpolation_original_scaled_error,interpolation_transformed_scaled_error,independent_residual_scaled_error)~engine,report,max),row.names=FALSE)
if(length(timings)) {
 totals <- aggregate(seconds_per_fit~engine+round,do.call(rbind,timings),sum)
 cat('Prepared fit plus residual/interpolation outputs, median sum over 64 cases (seconds):\n')
 print(aggregate(seconds_per_fit~engine,totals,median),row.names=FALSE)
}
