# Estimated-ARMA conditional GLS covariance; no frozen oracle is modified.
args <- commandArgs(trailingOnly=FALSE)
script <- sub("^--file=","",args[startsWith(args,"--file=")][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling,"common.R"));source(file.path(tooling,"extended-fixtures.R"))
source(file.path(tooling,"arma-fit-reference.R"))
root <- reference_root();base_dir <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub("=.*$","",args)) || any(!args %in% "--source" & !startsWith(args,"--output="))) {
 stop("Usage: verify-regression-information.R [--source] [--output=<existing empty directory>]")
}
output <- args[startsWith(args,"--output=")]
output <- if(length(output))normalizePath(sub("^--output=","",output),mustWork=TRUE)else NULL
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
 startsWith(output,normalizePath(base_dir))))stop("Output must be empty and outside the frozen corpus")
if("--source" %in% args) {pkgload::load_all(root,quiet=TRUE,compile=FALSE)
} else if(!requireNamespace("persephone3",quietly=TRUE))stop("Install persephone3 first")
before <- extended_parent_hashes(base_dir);pin <- extended_assert_oracle(base_dir)
source(file.path(root,"inst/tinytest/fixtures/native/arma-fit-cases.R"))
source(file.path(root,"inst/tinytest/fixtures/native/arma-residual-oracle.R"))
setup <- getFromNamespace("native_arma_objective_setup","persephone3")
information <- getFromNamespace("native_regression_information","persephone3")
checks <- 0L;rows <- list()
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop("Qualification failed: ",label)}
relative <- function(a,b)if(is.null(a)||length(a)!=length(b)||!all(is.finite(a)))Inf else sqrt(sum((a-b)^2)/sum(b^2))
# Prospective budgets: covariance relative Frobenius <=2e-5 against public APIs,
# <=1e-7 against independently whitened Gaussian GLS; beta <=1e-8*(1+abs(beta)).
for(ca in arma_fit_cases())for(engine in c("x13","tramoseats")) {
 reference <- reference_executor(engine)(prepare_reference(ca,engine))
 blocks <- lapply(c("phi","theta","bphi","btheta"),function(k){
   a <- reference$description$arima[[k]];if(is.null(a))numeric()else as.numeric(a["value",])})
 names(blocks) <- names(ca$specification);blocks$ar <- -blocks$ar;blocks$seasonal_ar <- -blocks$seasonal_ar
 arguments <- ca$arguments;arguments$initial <- blocks
 context <- do.call(setup,arguments);native <- information(context)
 oracle <- arma_residual_basis(ca,blocks);k <- sum(is.na(unlist(ca$specification)))
 df <- oracle$finite-2L-k
 expected <- oracle$regression_covariance_unit*sum(oracle$residuals^2)/df
 target <- reference$estimation$bvar[c(2L,1L),c(2L,1L),drop=FALSE]
 beta <- reference$estimation$b[c(2L,1L)]
 label <- paste(ca$id,engine)
 assert(native$status=="available",paste(label,"available"))
 assert(relative(native$covariance,target)<=2e-5,paste(label,"reference covariance"))
 assert(relative(native$covariance,expected)<=1e-7,paste(label,"independent covariance"))
 assert(all(abs(native$coefficients-beta)<=1e-8*(1+abs(beta))),paste(label,"reference coefficients"))
 assert(all(abs(native$coefficients-oracle$coefficients)<=1e-8*(1+abs(oracle$coefficients))),paste(label,"independent coefficients"))
 assert(native$residual_df==df,paste(label,"estimated-ARMA residual df"))
 assert(native$effective_count==reference$estimation$likelihood$neffectiveobs,paste(label,"effective count"))
 assert(reference$estimation$likelihood$nparams==k+3L,paste(label,"scale in parameter count only"))
 assert(native$free_ARMA==k && native$free_regression==2L,paste(label,"free counts"))
 assert(identical(dim(native$covariance),c(2L,2L)),paste(label,"fixed coefficient omitted"))
 assert(!native$propagates_ARMA_uncertainty && is.null(native$cross_covariance),paste(label,"conditional label"))
 rows[[length(rows)+1L]] <- data.frame(case=ca$id,engine=engine,n=native$effective_count,
   free_regression=native$free_regression,free_ARMA=k,residual_df=df,
   reference_covariance_relative_error=relative(native$covariance,target),
   independent_covariance_relative_error=relative(native$covariance,expected),
   coefficient_max_error=max(abs(native$coefficients-beta)))
}
assert(identical(before,extended_parent_hashes(base_dir)),"frozen corpus unchanged")
report <- do.call(rbind,rows)
files <- c(file.path(tooling,c("verify-regression-information.R","arma-fit-reference.R")),
 file.path(root,c("src/arma_information.cpp","src/core/arma_information.hpp","src/rcpp_arma_objective.cpp",
 "R/native_arma_information.R","inst/tinytest/fixtures/native/arma-residual-oracle.R")))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz="UTC",usetz=TRUE),
 parent_source_commit=system2("git",c("-C",shQuote(root),"rev-parse","HEAD"),stdout=TRUE),
 source_sha256=setNames(reference_sha256(files),basename(files)),
 oracle_manifest_sha256=reference_sha256(file.path(base_dir,"oracle-manifest.R")),R=R.version.string,java=pin$java,
 checks=checks,direct_fits=128L,covariance_budget=2e-5,independent_covariance_budget=1e-7,
 method="conditional_GLS_Q_over_n_minus_r_minus_k; no ARMA uncertainty propagation")
if(!is.null(output)) {
 write.csv(report,file.path(output,"matrix.csv"),row.names=FALSE)
 writeLines(sub("[[:blank:]]+$","",capture.output(dput(metadata))),file.path(output,"metadata.R"))
}
cat("PASS:",checks,"checks / 128 direct fits\n")
print(aggregate(cbind(reference_covariance_relative_error,independent_covariance_relative_error,coefficient_max_error)~engine,report,max),row.names=FALSE)
