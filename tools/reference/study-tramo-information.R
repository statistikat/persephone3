# Read-only statistical-convention study; this is not a parity acceptance tool.
args <- commandArgs(trailingOnly=FALSE)
script <- sub("^--file=","",args[startsWith(args,"--file=")][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling,"common.R"));source(file.path(tooling,"extended-fixtures.R"))
source(file.path(tooling,"arma-fit-reference.R"));source(file.path(tooling,"arma-residual-oracle.R"))
root <- reference_root();base_dir <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)>1L || (length(args) && !startsWith(args,"--output="))) stop("Only --output=<existing empty directory> is supported")
output <- if(length(args)) normalizePath(sub("^--output=","",args),mustWork=TRUE) else NULL
if(!is.null(output)) {
  frozen <- normalizePath(base_dir,mustWork=TRUE)
  if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
      output==frozen || startsWith(output,paste0(frozen,"/"))) stop("Output must be an empty directory outside the frozen corpus")
}
before <- extended_parent_hashes(base_dir);pin <- extended_assert_oracle(base_dir)
source(file.path(root,"inst/tinytest/fixtures/native/arma-fit-cases.R"))
checks <- 0L;direct_fits <- 0L;rows <- list();raw <- list();scale_rows <- list()
assert <- function(x,label) {checks <<- checks+1L;if(!isTRUE(x))stop("Independent study check failed: ",label)}
close <- function(a,b,tol,label) assert(length(a)==length(b) && all(is.finite(a)) && all(is.finite(b)) &&
  all(abs(a-b)<=tol*(1+abs(b))),label)
relative <- function(a,b) sqrt(sum((a-b)^2)/sum(b^2))
analyze <- function(case) {
  prepared <- prepare_reference(case,"tramoseats")
  fit <- reference_executor("tramoseats")(prepared);direct_fits <<- direct_fits+1L
  blocks <- lapply(c("phi","theta","bphi","btheta"),function(k) {
    a <- fit$description$arima[[k]];if(is.null(a))numeric() else as.numeric(a["value",])
  })
  names(blocks) <- names(case$specification);blocks$ar <- -blocks$ar;blocks$seasonal_ar <- -blocks$seasonal_ar
  oracle <- arma_residual_information(case,blocks)
  fine <- arma_residual_information(case,blocks,step=5e-5)
  # Richardson check against another independently evaluated dense stencil.
  J <- (4*fine$jacobian-oracle$jacobian)/3
  n <- oracle$center$finite;k <- ncol(J);Q <- sum(oracle$center$normalized^2)
  covariance <- Q/(n-2L-k)*solve(crossprod(J))
  close(sum(oracle$center$residuals^2),fit$estimation$likelihood$ssq,2e-8,paste(case$id,"likelihood residual quadratic form"))
  close(oracle$center$profile,fit$estimation$likelihood$ll,2e-8,paste(case$id,"density"))
  close(oracle$center$finite,fit$estimation$likelihood$neffectiveobs,0,paste(case$id,"effective count"))
  close(oracle$center$coefficients,fit$estimation$b[c(2L,1L)],1e-6,paste(case$id,"GLS"))
  assert(relative(covariance,fine$covariance)<2e-5,paste(case$id,"stable independent residual derivative"))
  # The public residual field has an additional missing-row contract and cannot
  # be used unmasked as the optimization residual vector. For complete AR(1),
  # qualify it directly against analytic and dense residual derivatives.
  coordinates <- arma_residual_coordinates(case,blocks)
  public_residual <- function(theta) {
    b <- coordinates$split(theta);p <- prepared
    order <- c("ar","seasonal_ar","ma","seasonal_ma")
    sign <- rep(c(-1,-1,1,1),lengths(b[order]))
    p$spec <- rjd3toolkit::set_arima(p$spec,mean=0,mean.type="Undefined",
      p=length(b$ar),d=case$d,q=length(b$ma),bp=length(b$seasonal_ar),bd=case$D,bq=length(b$seasonal_ma),
      coef=unlist(b[order],use.names=FALSE)*sign,coef.type=rep("Fixed",sum(lengths(b))))
    value <- reference_executor("tramoseats")(p);direct_fits <<- direct_fits+1L
    l <- value$estimation$likelihood
    determinant <- -2*l$ll-l$neffectiveobs*(log(2*pi)+1+log(l$ssq/l$neffectiveobs))
    value$estimation$res*exp(determinant/(2*l$neffectiveobs))
  }
  public_covariance <- NULL;public_error <- NA_real_
  if(case$kind=="ar" && !anyNA(case$arguments$series[case$rows])) {
    public_J <- vapply(seq_len(k),function(j) {
      h <- numeric(k);h[j] <- 1e-5
      (public_residual(coordinates$value+h)-public_residual(coordinates$value-h))/(2e-5)
    },numeric(length(fit$estimation$res)))
    public_covariance <- Q/(n-2L-k)*solve(crossprod(public_J))
    public_error <- relative(covariance,public_covariance)
    assert(public_error<2e-5,paste(case$id,"independent versus public residual information"))
  }
  native_order <- c("ar","ma","seasonal_ar","seasonal_ma")
  jd_order <- c("ar","seasonal_ar","ma","seasonal_ma")
  labels <- function(order) unlist(lapply(order,function(b) {
    j <- which(is.na(case$specification[[b]]));if(length(j))paste0(b,j) else character()
  }),use.names=FALSE)
  permutation <- match(labels(native_order),labels(jd_order))
  sign <- rep(c(-1,1,-1,1),lengths(blocks))[is.na(unlist(case$specification))]
  jd_covariance <- fit$estimation$parameters$cov[permutation,permutation,drop=FALSE]*outer(sign,sign)
  score <- fit$estimation$parameters$score[permutation]*sign
  gradient <- n/Q*drop(crossprod(J,oracle$center$normalized))
  if(case$kind=="ar" && !anyNA(case$arguments$series[case$rows])) {
    analytic <- arma_ar1_residual_information(case,blocks$ar)
    close(covariance,matrix(analytic,1L),1e-8,paste(case$id,"analytic AR(1) residual information"))
  }
  list(coefficients=unlist(blocks,use.names=FALSE),likelihood=fit$estimation$likelihood$ll,
    covariance=covariance,jd_covariance=jd_covariance,public_residual_covariance=public_covariance,
    score=score,gradient=gradient,n=n,
    covariance_relative_error=relative(covariance,jd_covariance),
    public_residual_information_error=public_error,
    residual_error=max(abs(oracle$center$residuals-fit$estimation$res)),
    stencil_change=relative(covariance,fine$covariance))
}
cases <- arma_fit_cases()
for(case in cases) {
  result <- analyze(case);raw[[case$id]] <- result
  rows[[length(rows)+1L]] <- data.frame(case=case$id,period=case$period,model=case$kind,
    covariance_relative_error=result$covariance_relative_error,
    public_residual_information_error=result$public_residual_information_error,
    score_max_error=max(abs(result$score+result$gradient)),residual_max_error=result$residual_error,
    stencil_relative_change=result$stencil_change,
    within_prospective_budget=result$covariance_relative_error<=2e-5)
}
# Changing response units and the fixed-regressor units leaves the ARMA model
# and its covariance invariant. Fitted regression coefficients scale instead.
for(id in c("4-ar-complete-none","4-arma-complete-none","4-airline-complete-none","12-airline-gaps-none")) {
  original <- cases[[id]];base <- raw[[id]]
  for(scale in c(1,7,100,.001)) {
    case <- original;case$arguments$series <- case$arguments$series*scale
    case$arguments$regressors$fixed <- case$arguments$regressors$fixed*scale
    result <- if(scale==1)base else analyze(case)
    close(result$coefficients,base$coefficients,1e-5,paste(id,"unit-scale coefficient invariance",scale))
    close(result$likelihood+result$n*log(scale),base$likelihood,2e-8,paste(id,"unit-scale density identity",scale))
    assert(relative(result$covariance,base$covariance)<2e-5,paste(id,"independent covariance unit invariance",scale))
    scale_rows[[length(scale_rows)+1L]] <- data.frame(case=id,response_scale=scale,
      coefficient_max_change=max(abs(result$coefficients-base$coefficients)),
      independent_covariance_relative_change=relative(result$covariance,base$covariance),
      jd_covariance_relative_change=relative(result$jd_covariance,base$jd_covariance),
      independent_vs_jd_relative_error=result$covariance_relative_error,
      independent_first_variance=result$covariance[1,1],jd_first_variance=result$jd_covariance[1,1])
  }
}
assert(identical(before,extended_parent_hashes(base_dir)),"frozen corpus unchanged")
matrix_report <- do.call(rbind,rows);scale_report <- do.call(rbind,scale_rows)
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz="UTC",usetz=TRUE),
  parent_source_commit=system2("git",c("-C",shQuote(root),"rev-parse","HEAD"),stdout=TRUE),
  source_sha256=reference_sha256(file.path(tooling,c("study-tramo-information.R","arma-residual-oracle.R","arma-fit-reference.R"))),
  oracle_manifest_sha256=reference_sha256(file.path(base_dir,"oracle-manifest.R")),
  R=R.version.string,java=pin$java,checks=checks,direct_fits=direct_fits,
  covariance_budget=2e-5,compatibility_status="not_qualified_requires_policy_decision")
if(!is.null(output)) {
  write.csv(matrix_report,file.path(output,"matrix.csv"),row.names=FALSE)
  write.csv(scale_report,file.path(output,"scale.csv"),row.names=FALSE)
  save_plain <- function(x,path) writeLines(sub("[[:blank:]]+$","",capture.output(dput(x))),path)
  save_plain(raw,file.path(output,"raw.R"));save_plain(metadata,file.path(output,"metadata.R"))
}
cat("Independent checks",checks,"direct fits",metadata$direct_fits,"\n")
cat("Within prospective covariance budget",sum(matrix_report$within_prospective_budget),"of",nrow(matrix_report),"\n")
print(scale_report,digits=9,row.names=FALSE)
cat("PASS: independent residual identities. TRAMO covariance parity is NOT established; policy decision remains open.\n")
