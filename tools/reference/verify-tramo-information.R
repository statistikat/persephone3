# Prospective public-API covariance qualification; never modifies frozen data.
args <- commandArgs(trailingOnly=FALSE)
script <- sub("^--file=","",args[startsWith(args,"--file=")][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling,"common.R"));source(file.path(tooling,"extended-fixtures.R"))
source(file.path(tooling,"arma-fit-reference.R"));source(file.path(tooling,"fit-timing.R"))
root <- reference_root();base_dir <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub("=.*$","",args)) || any(!args %in% c("--source","--timing") & !startsWith(args,"--output="))) {
  stop("Usage: verify-tramo-information.R [--source] [--timing] [--output=<existing empty directory>]")
}
output <- args[startsWith(args,"--output=")]
output <- if(length(output))normalizePath(sub("^--output=","",output),mustWork=TRUE)else NULL
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
    startsWith(output,normalizePath(base_dir))))stop("Output must be empty and outside the frozen corpus")
if("--source" %in% args) { pkgload::load_all(root,quiet=TRUE,compile=FALSE)
} else if(!requireNamespace("persephone3",quietly=TRUE))stop("Install persephone3 first")
before <- extended_parent_hashes(base_dir); pin <- extended_assert_oracle(base_dir)
source(file.path(root,"inst/tinytest/fixtures/native/arma-fit-cases.R"))
source(file.path(root,"inst/tinytest/fixtures/native/arma-residual-oracle.R"))
setup <- getFromNamespace("native_arma_objective_setup","persephone3")
information <- getFromNamespace("native_arma_information","persephone3")
optimize_native <- getFromNamespace("native_arma_optimize","persephone3")
# Compile only this independently authored observer against the pinned public API.
# The class is tooling, never a native runtime dependency or a copied optimizer.
probe <- tempfile("persephone-legacy-probe-");dir.create(probe)
jars <- list.files(system.file("java",package="rjd3jars"),pattern="[.]jar$",full.names=TRUE)
classpath <- paste(jars,collapse=.Platform$path.sep)
javac <- Sys.which("javac");if(!nzchar(javac))stop("The optional public-API probe requires javac on PATH")
status <- system2(javac,c("-d",shQuote(probe),"-cp",shQuote(classpath),shQuote(file.path(tooling,"LegacyCovarianceProbe.java"))))
if(status!=0L)stop("Public-API observer compilation failed")
rJava::.jaddClassPath(probe)
checks <- 0L; raw <- list(); rows <- list(); timings <- list(); timed_calls <- 0L
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop("Qualification failed: ",label)}
relative <- function(a,b) {
  if(is.null(a) || length(a)!=length(b) || !all(is.finite(a)))return(Inf)
  sqrt(sum((a-b)^2)/sum(b^2))
}
for(ca in arma_fit_cases()) {
 p <- prepare_reference(ca,"tramoseats")
 model <- rJava::.jcall("jdplus/tramoseats/base/r/Tramo","Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel;","process",
   rjd3toolkit::.r2jd_tsdata(p$series),getFromNamespace(".r2jd_spec_tramo","rjd3tramoseats")(p$spec),rjd3toolkit::.r2jd_modellingcontext(p$context))
 original <- getFromNamespace(".regarima_rslts","rjd3tramoseats")(model)
 reg <- rJava::.jcall(model,"Ljdplus/toolkit/base/core/regarima/RegArimaModel;","regarima")
 mapping <- rJava::.jcall(model,"Ljdplus/toolkit/base/core/arima/estimation/IArimaMapping;","mapping")
 y <- as.numeric(ca$arguments$series)[ca$rows];observed <- which(!is.na(y))
 missing <- which(is.na(y[seq.int(min(observed),max(observed))]))-1L
 # The public exported regarima model contains interpolated values: explicitly
 # restore missing-coordinate integration when asking the processor to refit.
 captured <- rJava::.jcall("LegacyCovarianceProbe","[D","capture",reg,mapping,as.integer(missing))
 k <- as.integer(captured[1]);at <- 4L
 get <- function(n){v<-captured[at+seq_len(n)];at<<-at+n;v}
 final <- get(k);working <- get(k);steps <- get(k);covariance <- matrix(get(k*k),k,k)
 jdorder <- c("ar","seasonal_ar","ma","seasonal_ma");nativeorder <- names(ca$specification)
 labels <- function(order)unlist(lapply(order,function(b){j<-which(is.na(ca$specification[[b]]));if(length(j))paste0(b,j)else character()}),use.names=FALSE)
 permutation <- match(labels(nativeorder),labels(jdorder))
 sign <- rep(c(-1,1,-1,1),lengths(ca$specification))[is.na(unlist(ca$specification))]
 physical <- final[permutation]*sign;initial <- ca$specification;all <- unlist(initial);all[is.na(all)] <- physical;at <- 0L
 initial <- lapply(lengths(initial),function(n){x<-all[at+seq_len(n)];at<<-at+n;unname(x)});names(initial) <- nativeorder
 arguments <- ca$arguments;arguments$initial <- initial;context <- do.call(setup,arguments)
 accurate <- information(context,method="tramo_residual")
 state <- list(final_coefficients=physical,working_coefficients=working[permutation]*sign,difference_steps=steps[permutation]*sign,
  damping=captured[3],normalized_sum_squares=captured[2],effective_count=accurate$center$likelihood$finite_count)
 replay <- information(context,method="tramo_residual",covariance="legacy",legacy_state=state)
 target <- covariance[permutation,permutation,drop=FALSE]*outer(sign,sign)
 original_cov <- original$estimation$parameters$cov[permutation,permutation,drop=FALSE]*outer(sign,sign)
 oracle <- arma_residual_information(ca,initial,step=5e-5)
 assert(accurate$status=="available",paste(ca$id,"accurate availability"))
 assert(replay$status=="available",paste(ca$id,"legacy availability"))
 assert(relative(accurate$covariance,oracle$covariance)<1e-6,paste(ca$id,"independent final-fit covariance"))
 assert(relative(replay$covariance,target)<=2e-5,paste(ca$id,"retained-matrix covariance"))
 assert(identical(accurate$covariance,replay$accurate_covariance),paste(ca$id,"explicit default preserved"))
 assert(captured[4]<=2e-5,paste(ca$id,"public retained-matrix reconstruction defect"))
 assert(state$damping>=0 && is.finite(state$damping),paste(ca$id,"nonnegative retained damping"))
 raw[[ca$id]] <- list(state=state,initial=initial,reference_covariance=target,isotropy=captured[4])
 rows[[ca$id]] <- data.frame(case=ca$id,n=state$effective_count,damping=state$damping,
   reconstruction_defect=captured[4],accurate_dense_relative_error=relative(accurate$covariance,oracle$covariance),
   legacy_replay_relative_error=relative(replay$covariance,target),
   refit_vs_original_legacy_relative_difference=relative(target,original_cov))
 if("--timing" %in% args) {
   native_context <- do.call(setup,ca$arguments)
   functions <- list(native=function(){f<-optimize_native(native_context,control=list(max_iterations=200L,max_evaluations=4000L));
     stopifnot(f$converged);information(native_context,f$coordinates,method="tramo_residual")},
     jd_tramo=function()reference_executor("tramoseats")(p))
   sizes <- vapply(functions,function(f){x<-fit_timing_once(f);max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
   for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
     iterations <- sizes[[engine]];invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations)
     timed_calls <- timed_calls+iterations
     while(sample$seconds<.05 && iterations<1000L) {
       iterations <- min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
       invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
     }
     assert(sample$seconds>0,paste(ca$id,"positive timing batch"))
     assert(if(engine=="native")isTRUE(sample$value$available)else is.finite(sample$value$estimation$likelihood$ll),paste(ca$id,"timed covariance fit"))
     sizes[[engine]] <- iterations
     timings[[length(timings)+1L]] <- data.frame(case=ca$id,engine=engine,round=round,iterations=iterations,
       elapsed_seconds=sample$seconds,seconds_per_fit=sample$seconds/iterations)
   }
 }
 cat(ca$id,"accurate/legacy PASS\n")
}
assert(identical(before,extended_parent_hashes(base_dir)),"frozen corpus unchanged")
report <- do.call(rbind,rows)
files <- c(file.path(tooling,c("verify-tramo-information.R","LegacyCovarianceProbe.java","arma-fit-reference.R")),
 file.path(root,"inst/tinytest/fixtures/native/arma-residual-oracle.R"))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz="UTC",usetz=TRUE),
 parent_source_commit=system2("git",c("-C",shQuote(root),"rev-parse","HEAD"),stdout=TRUE),
 source_sha256=setNames(reference_sha256(files),basename(files)),
 states_sha256=reference_sha256(file.path(root,"inst/tinytest/fixtures/native/tramo-legacy-states.R")),
 oracle_manifest_sha256=reference_sha256(file.path(base_dir,"oracle-manifest.R")),R=R.version.string,java=pin$java,
 checks=checks,public_preprocessor_calls=64L,observed_processor_refits=64L,
 covariance_budget=2e-5,legacy_scope="replay of recorded public optimizer history; no inference of history from final coefficients",
 reconstruction="positive isotropic part of retained curvature/2 minus recorded Jacobian Gram; residual defect reported",
 timing_scope="prepared native fit plus accurate TRAMO ARMA covariance versus prepared JDemetra TRAMO fit with legacy covariance",
 timing_conventions="3 warmed rounds, alternating engines, batches >=50ms when possible, GC outside timed regions; setup/JVM/startup/observer excluded; API workloads differ",
 timing_calls_including_calibration_retries=timed_calls,native_threads=Sys.getenv("PERSEPHONE_NUM_THREADS"),
 parallel=getFromNamespace(".native_parallel_info","persephone3")())
if(!is.null(output)) {
 save_plain <- function(x,path)writeLines(sub("[[:blank:]]+$","",capture.output(dput(x))),path)
 write.csv(report,file.path(output,"matrix.csv"),row.names=FALSE)
 save_plain(raw,file.path(output,"states.R"));save_plain(metadata,file.path(output,"metadata.R"))
 if(length(timings))write.csv(do.call(rbind,timings),file.path(output,"timings.csv"),row.names=FALSE)
}
unlink(probe,recursive=TRUE)
cat("PASS:",checks,"checks; maximum legacy replay relative error",max(report$legacy_replay_relative_error),"\n")
if(length(timings)) {
 totals <- aggregate(seconds_per_fit~engine+round,do.call(rbind,timings),sum)
 cat("Prepared fit plus covariance, median sum over 64 cases (seconds):\n")
 print(aggregate(seconds_per_fit~engine,totals,median),row.names=FALSE)
}
