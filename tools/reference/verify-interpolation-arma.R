# Independent delta-method prediction qualification and optional API timings.
# JDemetra's missing-value standard errors are conditional, not delta targets.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'arma-fit-reference.R'));source(file.path(tooling,'interpolation-reference.R'))
source(file.path(tooling,'fit-timing.R'))
root <- reference_root();base_dir <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-interpolation-arma.R [--source] [--timing] [--output=<existing empty directory>]')
output <- args[startsWith(args,'--output=')]
output <- if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
    startsWith(output,normalizePath(base_dir))))stop('Output must be empty and outside the frozen corpus')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
before <- extended_parent_hashes(base_dir);pin <- extended_assert_oracle(base_dir)
for(file in c('arma-fit-cases.R','arma-residual-oracle.R','interpolation-oracle.R',
    'interpolation-arma-oracle.R','joint-information-oracle.R'))
  source(file.path(root,'inst/tinytest/fixtures/native',file))
setup <- getFromNamespace('native_arma_objective_setup','persephone3')
optimize_native <- getFromNamespace('native_arma_optimize','persephone3')
information <- getFromNamespace('native_interpolation_information','persephone3')
checks <- timed_calls <- qualified <- 0L;rows <- timings <- list()
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
relative <- function(a,b)if(length(a)!=length(b)||!all(is.finite(a)))Inf else sqrt(sum((a-b)^2)/sum(b^2))
# Prospective budgets: normalized total covariance 2e-5 relative; point
# sensitivity within 2e-7*(1+abs(G)). Empty predictions are separate controls.
for(ca in arma_fit_cases()) {
  context <- do.call(setup,ca$arguments)
  fit <- optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L))
  native <- information(context,fit$coordinates,method='delta_profile_ml')
  conditional <- information(context,fit$coordinates)
  assert(fit$converged && native$available,paste(ca$id,'propagation availability'))
  assert(identical(native$conditional_covariance,conditional$covariance),paste(ca$id,'retained conditional covariance'))
  assert(identical(native$points_transformed,conditional$points_transformed),paste(ca$id,'unchanged accurate points'))
  assert(native$method=='delta_profile_ml' && native$propagates_ARMA_uncertainty && !native$comparison_only,paste(ca$id,'explicit estimator'))
  covariance_error <- sensitivity_error <- 0
  if(length(native$rows)) {
    qualified <- qualified+1L
    assert(native$arma_information_status=='available' && native$arma_information$method=='profile_ml' &&
      native$arma_information$covariance_mode=='accurate',paste(ca$id,'qualified accurate profile information'))
    basis <- arma_residual_basis(ca,fit$final$parameters$coefficients)
    base <- basis$regression_covariance_unit*sum(basis$residuals^2)/basis$finite
    oracle <- joint_information_oracle(ca,fit$final$parameters$coefficients,basis$coefficients,
      sqrt(diag(base)),sqrt(diag(native$arma_parameter_covariance)))
    ix <- 2L+seq_len(native$free_ARMA)
    Vtheta <- oracle$scaled_covariance[ix,ix,drop=FALSE]*outer(oracle$scale[ix],oracle$scale[ix])
    points <- interpolation_oracle(context,fit$final$parameters$coefficients)
    G <- interpolation_arma_sensitivity(context,fit$final$parameters$coefficients)$sensitivity
    target <- points$covariance*points$Q/points$n+G%*%Vtheta%*%t(G)
    units <- outer(conditional$standard_errors,conditional$standard_errors)
    covariance_error <- relative(native$covariance/units,target/units)
    sensitivity_error <- max(abs(native$sensitivity-G)/(1+abs(G)))
    assert(covariance_error<=2e-5,paste(ca$id,'independent Gaussian-Hessian delta covariance'))
    assert(sensitivity_error<=2e-7,paste(ca$id,'independent point sensitivity'))
    assert(native$evaluations==native$arma_information$evaluations+native$sensitivity_evaluations &&
      native$evaluations<=native$evaluation_budget,paste(ca$id,'shared evaluation budget'))
    assert(all(native$standard_errors>=native$conditional_standard_errors),paste(ca$id,'nonnegative increment'))
  } else assert(native$status=='no_internal_missing' && native$evaluations==0L,paste(ca$id,'empty prediction control'))
  px <- prepare_reference(ca,'x13');pt <- prepare_reference(ca,'tramoseats')
  xcall <- reference_interpolation_executor('x13');tcall <- reference_interpolation_executor('tramoseats')
  rx <- xcall(px);rt <- tcall(pt)
  assert(is.finite(rx$result$estimation$likelihood$ll) && is.finite(rt$result$estimation$likelihood$ll),paste(ca$id,'prepared reference fits'))
  rows[[ca$id]] <- data.frame(case=ca$id,scope=if(length(native$rows))'delta_covariance'else'no_internal_missing',
    missing_count=length(native$rows),free_regression=native$free_regression,free_ARMA=native$free_ARMA,
    covariance_scaled_relative_error=covariance_error,sensitivity_scaled_error=sensitivity_error,
    evaluations=native$evaluations,sensitivity_evaluations=native$sensitivity_evaluations,
    mean_conditional_variance=if(length(native$rows))mean(diag(native$conditional_covariance))else NA_real_,
    mean_ARMA_increment=if(length(native$rows))mean(diag(native$arma_increment))else NA_real_)
  if('--timing' %in% args) {
    native_run <- function(method) {
      f <- optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L));stopifnot(f$converged)
      value <- information(context,f$coordinates,method=method);stopifnot(value$available);value
    }
    functions <- list(native_conditional=function()native_run('conditional'),native_delta=function()native_run('delta_profile_ml'),
      jd_x13=function()xcall(px),jd_tramo=function()tcall(pt))
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
  cat(ca$id,'delta prediction PASS\n')
}
assert(identical(before,extended_parent_hashes(base_dir)),'frozen corpus unchanged')
report <- do.call(rbind,rows)
files <- c(file.path(tooling,c('verify-interpolation-arma.R','interpolation-reference.R','arma-fit-reference.R','fit-timing.R')),
  file.path(root,c('src/interpolation_arma_information.cpp','src/interpolation_information.cpp','src/core/interpolation_information.hpp',
    'src/arma_information.cpp','src/vector_jacobian.cpp','src/core/information.hpp','src/rcpp_arma_objective.cpp',
    'R/native_interpolation_information.R','inst/tinytest/fixtures/native/interpolation-arma-oracle.R',
    'inst/tinytest/fixtures/native/interpolation-oracle.R','inst/tinytest/fixtures/native/joint-information-oracle.R')))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  oracle_manifest_sha256=reference_sha256(file.path(base_dir,'oracle-manifest.R')),R=R.version.string,java=pin$java,
  checks=checks,native_fits=64L,independent_delta_covariances=qualified,empty_prediction_controls=64L-qualified,direct_reference_fits=128L,
  covariance_scaled_relative_budget=2e-5,sensitivity_scaled_budget=2e-7,
  method='First-order delta propagation of accurate observed profile-ML ARMA covariance through refitted conditional Gaussian points',
  reference_scope='Public missing-value standard errors are conditional; independent Gaussian calculations qualify the delta result',
  timing_scope='prepared native fits plus conditional/delta interpolation covariance versus public processing, serialization and missing-value accessor; scopes differ',
  timing_conventions='3 warmed rounds, alternating engines, batches >=50ms when possible; setup/loading/startup and explicit pre-batch GC excluded',
  timing_calls_including_warmup_and_calibration_retries=timed_calls,native_threads=Sys.getenv('PERSEPHONE_NUM_THREADS'),
  parallel=getFromNamespace('.native_parallel_info','persephone3')())
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',qualified,'independent delta covariances /',64L-qualified,'empty controls / 128 public fits\n')
cat('Maximum normalized covariance relative error:',max(report$covariance_scaled_relative_error),
  'point-sensitivity scaled error:',max(report$sensitivity_scaled_error),'\n')
if(length(timings)) {
  totals <- aggregate(seconds_per_fit~engine+round,do.call(rbind,timings),sum)
  cat('Prepared fit plus interpolation uncertainty, median sum over 64 cases (seconds):\n')
  print(aggregate(seconds_per_fit~engine,totals,median),row.names=FALSE)
}
