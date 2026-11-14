# Independent full Gaussian Hessian qualification, plus optional API timings.
# JDemetra's regression covariance is conditional and is not a joint target.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'arma-fit-reference.R'));source(file.path(tooling,'fit-timing.R'))
root <- reference_root();base_dir <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--timing') & !startsWith(args,'--output='))) {
 stop('Usage: verify-joint-information.R [--source] [--timing] [--output=<existing empty directory>]')
}
output <- args[startsWith(args,'--output=')]
output <- if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
 startsWith(output,normalizePath(base_dir))))stop('Output must be empty and outside the frozen corpus')
if('--source' %in% args) {pkgload::load_all(root,quiet=TRUE,compile=FALSE)
} else if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
before <- extended_parent_hashes(base_dir);pin <- extended_assert_oracle(base_dir)
for(file in c('arma-fit-cases.R','arma-residual-oracle.R','joint-information-oracle.R')) {
 source(file.path(root,'inst/tinytest/fixtures/native',file))
}
setup <- getFromNamespace('native_arma_objective_setup','persephone3')
optimize_native <- getFromNamespace('native_arma_optimize','persephone3')
information <- getFromNamespace('native_regression_information','persephone3')
checks <- 0L;rows <- timings <- list();timed_calls <- 0L
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
relative <- function(a,b)if(is.null(a)||length(a)!=length(b)||!all(is.finite(a)))Inf else sqrt(sum((a-b)^2)/sum(b^2))
# Prospective budgets match the mandatory tests: standardized joint inverse
# covariance 2e-5 relative; identity defect 2e-5; B within 2e-7*(1+abs(B)).
for(ca in arma_fit_cases()) {
 context <- do.call(setup,ca$arguments)
 fit <- optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L))
 native <- information(context,fit$coordinates,method='joint_profile_ml')
 assert(fit$converged && native$available,paste(ca$id,'joint availability'))
 basis <- arma_residual_basis(ca,fit$final$parameters$coefficients)
 base <- basis$regression_covariance_unit*sum(basis$residuals^2)/basis$finite
 oracle <- joint_information_oracle(ca,fit$final$parameters$coefficients,basis$coefficients,
   sqrt(diag(base)),sqrt(diag(native$arma_covariance)))
 scaled <- native$joint_covariance/outer(oracle$scale,oracle$scale)
 error <- relative(scaled,oracle$scaled_covariance)
 defect <- max(abs(scaled%*%oracle$scaled_hessian-diag(nrow(scaled))))
 sensitivity_error <- max(abs(native$sensitivity-oracle$sensitivity)/(1+abs(oracle$sensitivity)))
 assert(error<=2e-5,paste(ca$id,'full Gaussian inverse Hessian'))
 assert(defect<=2e-5,paste(ca$id,'inverse identity'))
 assert(sensitivity_error<=2e-7,paste(ca$id,'independent GLS sensitivity'))
 assert(relative(native$conditional_ml_covariance,base)<=1e-7,paste(ca$id,'conditional ML block'))
 assert(native$propagates_ARMA_uncertainty && native$scale_convention=='profiled_ML_Q_over_n_finite',paste(ca$id,'explicit estimator'))
 px <- prepare_reference(ca,'x13');pt <- prepare_reference(ca,'tramoseats')
 rx <- reference_executor('x13')(px);rt <- reference_executor('tramoseats')(pt)
 assert(is.finite(rx$estimation$likelihood$ll) && is.finite(rt$estimation$likelihood$ll),paste(ca$id,'prepared reference fits'))
 rows[[ca$id]] <- data.frame(case=ca$id,effective_count=native$effective_count,
   free_regression=native$free_regression,free_ARMA=native$free_ARMA,
   scaled_covariance_relative_error=error,inverse_identity_defect=defect,
   sensitivity_scaled_error=sensitivity_error,joint_eigenvalue_ratio=native$joint_eigenvalue_ratio,
   arma_evaluations=native$arma_information$evaluations,sensitivity_evaluations=native$sensitivity_evaluations)
 if('--timing' %in% args) {
   native_run <- function(method) {
     f <- optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L))
     stopifnot(f$converged);value <- information(context,f$coordinates,method=method)
     stopifnot(value$available);value
   }
   functions <- list(native_conditional=function()native_run('conditional_residual_df'),
     native_joint=function()native_run('joint_profile_ml'),jd_x13=function()reference_executor('x13')(px),
     jd_tramo=function()reference_executor('tramoseats')(pt))
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
     assert(if(startsWith(engine,'native'))isTRUE(sample$value$available)else is.finite(sample$value$estimation$likelihood$ll),paste(ca$id,'timed fit'))
     sizes[[engine]] <- iterations
     timings[[length(timings)+1L]] <- data.frame(case=ca$id,engine=engine,round=round,iterations=iterations,
       elapsed_seconds=sample$seconds,seconds_per_fit=sample$seconds/iterations)
   }
 }
 cat(ca$id,'joint PASS\n')
}
assert(identical(before,extended_parent_hashes(base_dir)),'frozen corpus unchanged')
report <- do.call(rbind,rows)
files <- c(file.path(tooling,c('verify-joint-information.R','arma-fit-reference.R','fit-timing.R')),
 file.path(root,c('src/joint_regression_information.cpp','src/vector_jacobian.cpp','src/arma_information.cpp',
 'src/core/arma_information.hpp','src/core/information.hpp','src/rcpp_arma_objective.cpp','R/native_arma_information.R',
 'inst/tinytest/fixtures/native/joint-information-oracle.R')))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
 parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),
 source_sha256=setNames(reference_sha256(files),basename(files)),
 oracle_manifest_sha256=reference_sha256(file.path(base_dir,'oracle-manifest.R')),R=R.version.string,java=pin$java,
 checks=checks,native_fits=64L,direct_reference_fits=128L,joint_covariance_budget=2e-5,
 sensitivity_scaled_budget=2e-7,method='joint profile-ML inverse Gaussian Hessian; no residual-df multiplier',
 reference_scope='JDemetra regression covariance is conditional, not a joint target',
 timing_scope='prepared fit plus regression/ARMA covariance: native conditional versus native joint versus public X13/TRAMO fits',
 timing_conventions='3 warmed rounds, alternating engines, batches >=50ms when possible, GC outside timed regions; setup/loading/startup excluded; estimators and API workloads differ',
 timing_calls_including_warmup_and_calibration_retries=timed_calls,native_threads=Sys.getenv('PERSEPHONE_NUM_THREADS'),
 parallel=getFromNamespace('.native_parallel_info','persephone3')())
if(!is.null(output)) {
 write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
 writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
 if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks / 64 independent full Gaussian Hessians / 128 reference fits\n')
cat('Maximum standardized covariance error:',max(report$scaled_covariance_relative_error),
 'identity defect:',max(report$inverse_identity_defect),'GLS sensitivity error:',max(report$sensitivity_scaled_error),'\n')
if(length(timings)) {
 totals <- aggregate(seconds_per_fit~engine+round,do.call(rbind,timings),sum)
 cat('Prepared fit plus covariance, median sum over 64 cases (seconds):\n')
 print(aggregate(seconds_per_fit~engine,totals,median),row.names=FALSE)
}
