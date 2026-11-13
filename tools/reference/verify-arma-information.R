# Prospective covariance/score qualification. Never modifies frozen fixtures.
args <- commandArgs(trailingOnly=FALSE)
script <- sub("^--file=", "", args[startsWith(args,"--file=")][[1L]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling,"common.R")); source(file.path(tooling,"extended-fixtures.R"))
source(file.path(tooling,"arma-fit-reference.R"))
root <- reference_root(); base_dir <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if (any(!args %in% "--source") || anyDuplicated(args)) stop("Only --source is supported")
if ("--source" %in% args) {
  pkgload::load_all(root,quiet=TRUE,compile=FALSE)
} else if (!requireNamespace("persephone3",quietly=TRUE)) stop("Install persephone3 first")
before <- extended_parent_hashes(base_dir); extended_assert_oracle(base_dir)
source(file.path(root,"inst/tinytest/fixtures/native/arma-fit-cases.R"))
setup <- getFromNamespace("native_arma_objective_setup","persephone3")
optimize_native <- getFromNamespace("native_arma_optimize","persephone3")
information <- getFromNamespace("native_arma_information","persephone3")
checks <- 0L; failures <- character(); rows <- list()
assert <- function(x,label) { checks <<- checks+1L; if(!isTRUE(x)) failures <<- c(failures,label) }
close <- function(a,b,tol,label) assert(length(a)==length(b) && all(is.finite(a)) && all(is.finite(b)) &&
  all(abs(a-b)<=tol*(1+abs(b))),label)
# Declared before comparison: covariance relative Frobenius norm <= 2e-5;
# log-likelihood score difference in physical coordinates <= 1e-4 absolute.
# These are prospective tooling checks, not changes to the frozen tolerances.
compare <- function(id,context,reference,method) {
  coefficient_labels <- function(order) unlist(lapply(order,function(b) {
    free <- which(is.na(context$parameters$specification[[b]]))
    if(length(free)) paste0(b,".coefficient",free) else character()
  }),use.names=FALSE)
  labels <- coefficient_labels(names(context$parameters$specification))
  b <- lapply(c("phi","theta","bphi","btheta"),function(k) {
    x <- reference$description$arima[[k]]; if(is.null(x)) numeric() else as.numeric(x["value",])
  })
  names(b) <- c("ar","ma","seasonal_ar","seasonal_ma")
  b$ar <- -b$ar; b$seasonal_ar <- -b$seasonal_ar
  map <- do.call(getFromNamespace("native_arma_parameter_map","persephone3"),
    c(context$parameters$specification,list(initial=b,reflection_margin=context$parameters$reflection_margin)))
  point <- map$initial_coordinates
  profile <- information(context,point)
  conditional <- information(context,point,method="conditional_residual_df")
  label <- paste(id,method)
  for(result in list(conditional,profile)) {
    assert(result$status %in% c("available","not_stationary"),paste(label,"information",result$status))
    assert(result$derivative_status=="available" && result$curvature_status=="positive_definite",
      paste(label,"qualified curvature"))
    if(result$status=="not_stationary") assert(!result$available && is.null(result$covariance) &&
      all(is.na(result$standard_errors)),paste(label,"reference point fails stationarity; inference withheld"))
  }
  if(is.null(conditional$hessian) || is.null(profile$hessian)) return(invisible(NULL))
  # At reference points that fail the native stationarity gate, compare the
  # curvature algebra only. The API still withholds inferential covariance.
  diagnostic_covariance <- solve(conditional$hessian)*conditional$covariance_multiplier
  profile_covariance <- solve(profile$hessian)
  jd_order <- c("ar","seasonal_ar","ma","seasonal_ma")
  jd_labels <- coefficient_labels(jd_order)
  permutation <- match(labels,jd_labels)
  sign <- ifelse(grepl("^(ar|seasonal_ar)[.]",labels),-1,1)
  parameters <- reference$estimation$parameters
  assert(identical(parameters$description,"sarima (true signs)"),paste(label,"reference coordinate description"))
  close(parameters$val[permutation]*sign,conditional$coefficients,1e-12,paste(label,"coordinate signs/order"))
  covariance <- parameters$cov[permutation,permutation,drop=FALSE]*outer(sign,sign)
  score <- parameters$score[permutation]*sign
  assert(identical(dim(covariance),dim(diagnostic_covariance)),paste(label,"free-only covariance dimensions"))
  assert(all(eigen(covariance,symmetric=TRUE,only.values=TRUE)$values>0),paste(label,"positive reference covariance"))
  error <- sqrt(sum((covariance-diagnostic_covariance)^2)/sum(covariance^2))
  # X-13 and TRAMO report different covariance estimators. Conditional observed
  # information is the prospective X-13 contract; TRAMO differences are measured
  # without asserting that its residual-Jacobian convention is the same method.
  if(method=="x13") {
    assert(error<=2e-5,paste(label,"conditional covariance relative norm",format(error)))
    close(-conditional$gradient,score,1e-4,paste(label,"physical log-likelihood score"))
  }
  n <- conditional$center$likelihood$finite_count
  k <- length(conditional$coefficients); r <- conditional$free_regression
  close(conditional$residual_df,n-r-k,0,paste(label,"residual degrees of freedom"))
  close(conditional$covariance_multiplier,n/(n-r-k),0,paste(label,"residual scale multiplier"))
  close(reference$estimation$likelihood$nparams,k+r+1L,0,paste(label,"estimated parameter count includes scale"))
  close(profile$center$value,conditional$center$value,0,paste(label,"same fitted likelihood"))
  close(profile$center$likelihood$regression_coefficients,conditional$center$likelihood$regression_coefficients,
    0,paste(label,"same fitted regression"))
  other <- information(context,point,control=list(difference_step=.002),method="conditional_residual_df")
  assert(other$derivative_status=="available",paste(label,"alternate stencil available"))
  close(solve(other$hessian)*other$covariance_multiplier,diagnostic_covariance,1e-6,
    paste(label,"alternate stencil covariance"))
  rows[[length(rows)+1L]] <<- data.frame(case=id,engine=method,n=n,regression=r,arma=k,
    covariance_relative_error=error,score_max_error=max(abs(score+conditional$gradient)),
    reference_point_status=conditional$status,
    profile_covariance_relative_difference=sqrt(sum((profile_covariance-covariance)^2)/sum(covariance^2)))
}
for(case in arma_fit_cases()) {
  context <- do.call(setup,case$arguments)
  native <- optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L))
  assert(native$converged,paste(case$id,"native converged"))
  assert(information(context,native$coordinates,method="conditional_residual_df")$available,
    paste(case$id,"information at native optimum"))
  for(method in c("x13","tramoseats")) compare(case$id,context,
    reference_executor(method)(prepare_reference(case,method)),method)
}
set.seed(10101); noise <- rnorm(240)
for(period in c(4L,12L)) for(phi in c(.97,.995)) {
  y <- ts(as.numeric(filter(noise,phi,method="recursive")),frequency=period)
  context <- setup(y,ar=NA_real_,include_mean=TRUE)
  native <- optimize_native(context,control=list(max_iterations=200L,max_evaluations=4000L))
  assert(information(context,native$coordinates,method="conditional_residual_df")$available,"persistent native information")
  for(method in c("x13","tramoseats")) {
    spec <- if(method=="x13") rjd3x13::regarima_spec("rg0") else rjd3tramoseats::tramo_spec("tr0")
    spec <- rjd3toolkit::set_arima(spec,mean=0,mean.type="Undefined",p=1L,d=0L,q=0L,bp=0L,bd=0L,bq=0L,
      coef=0,coef.type="Initial")
    spec <- rjd3toolkit::set_automodel(spec,enabled=FALSE)
    spec <- rjd3toolkit::set_transform(spec,fun="None")
    spec <- rjd3toolkit::set_estimate(spec,tol=1e-12,exact.ml=TRUE)
    fit <- if(method=="x13") rjd3x13::regarima_fast(y,spec) else rjd3tramoseats::tramo_fast(y,spec)
    compare(paste(period,"persistent",phi,sep="-"),context,fit,method)
  }
}
assert(identical(before,extended_parent_hashes(base_dir)),"frozen artifacts unchanged")
results <- do.call(rbind,rows)
print(aggregate(cbind(covariance_relative_error,score_max_error,profile_covariance_relative_difference)~engine,
  results,max),digits=9,row.names=FALSE)
cat("Direct fits",nrow(results),"checks",checks,"failures",length(failures),"\n")
if(length(failures)) { print(failures); stop("Information qualification failed") }
cat("PASS: pinned X-13 conditional information; TRAMO covariance differences remain a separate qualification gate\n")
