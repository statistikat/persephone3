# The fitted-density fixtures index regressors by source response row. Endpoint
# cases prepend past context, so align their estimation inputs by calendar first.
forecast_density_case <- function(case) {
  case$arguments$regressors <- lapply(case$arguments$regressors,function(x)
    window(x,start=start(case$arguments$series)))
  case
}

# Dense covariance whitening followed by an SVD least-squares projection. This
# avoids the cancellation in a differenced precision Schur complement when
# differentiating endpoints of twice-integrated series. No native filtering,
# gains, regression QR, parameter information or numerical differences are used.
forecast_svd_points <- function(context,blocks,backcasts,forecasts) {
  data <- context$prepared;n <- length(data$inputs$transformed)
  rows <- data$inputs$backcast_horizon-backcasts+seq_len(backcasts+n+forecasts)
  X <- data$inputs$regressors[rows,,drop=FALSE];fixed <- !is.na(data$fixed)
  y <- c(rep(NA_real_,backcasts),as.numeric(data$inputs$transformed),rep(NA_real_,forecasts))
  if(any(fixed))y <- y-drop(X[,fixed,drop=FALSE]%*%data$fixed[fixed])
  y <- y-context$mean;X <- X[,!fixed,drop=FALSE]
  missing <- which(is.na(y));y[missing] <- 0
  D <- diag(length(y))
  if(context$d)D <- diff(D,differences=context$d)
  if(context$seasonal_d)D <- diff(D,lag=context$period,differences=context$seasonal_d)
  W <- forwardsolve(t(chol(arma_fit_covariance(blocks,context$period,nrow(D)))),D)
  A <- cbind(W%*%X,-W[,missing,drop=FALSE]);decomposition <- svd(A)
  stopifnot(min(decomposition$d)>1e-12*max(decomposition$d))
  coefficients <- drop(decomposition$v%*%(crossprod(decomposition$u,W%*%y)/decomposition$d))
  point <- coefficients[ncol(X)+seq_along(missing)]+context$mean
  if(any(fixed))point <- point+drop(data$inputs$regressors[rows[missing],fixed,drop=FALSE]%*%data$fixed[fixed])
  selected <- match(c(seq_len(backcasts),backcasts+n+seq_len(forecasts)),missing)
  point[selected]
}

# OLS centering removes theta-independent regression and homogeneous integrated
# directions without using the native fitted coefficients. The null space comes
# from the explicit differencing matrix, rather than production state histories.
forecast_sensitivity_center <- function(context) {
  data <- context$prepared;y <- as.numeric(data$inputs$transformed)
  X <- data$design;fixed <- !is.na(data$fixed)
  if(any(fixed))y <- y-drop(X[,fixed,drop=FALSE]%*%data$fixed[fixed])
  y <- y-context$mean;X <- X[,!fixed,drop=FALSE]
  D <- diag(length(y))
  if(context$d)D <- diff(D,differences=context$d)
  if(context$seasonal_d)D <- diff(D,lag=context$period,differences=context$seasonal_d)
  if(nrow(D)<length(y))X <- cbind(X,qr.Q(qr(t(D)),complete=TRUE)[,seq.int(nrow(D)+1L,length(y)),drop=FALSE])
  observed <- which(!is.na(y))
  if(ncol(X))y[observed] <- y[observed]-qr.fitted(qr(X[observed,,drop=FALSE]),y[observed])
  context$prepared$inputs$transformed <- y
  context$prepared$fixed[fixed] <- 0;context$mean <- 0
  context
}

# Independent fixed h/h2 central stencils with Richardson extrapolation; the
# production adaptive point derivatives and profile-information assembly are unused.
forecast_arma_sensitivity <- function(context,blocks,backcasts,forecasts,step=1e-4) {
  context <- forecast_sensitivity_center(context)
  values <- unlist(blocks,use.names=FALSE)
  free <- which(is.na(unlist(context$parameters$specification,use.names=FALSE)))
  split <- function(value) {
    at <- 0L
    answer <- lapply(lengths(blocks),function(n) {x<-value[at+seq_len(n)];at<<-at+n;x})
    names(answer) <- names(blocks);answer
  }
  derivative <- function(h) {
    answer <- matrix(0,backcasts+forecasts,length(free))
    for(j in seq_along(free)) {
      delta <- numeric(length(values));delta[free[j]] <- h
      answer[,j] <- (forecast_svd_points(context,split(values+delta),backcasts,forecasts)-
        forecast_svd_points(context,split(values-delta),backcasts,forecasts))/(2*h)
    }
    answer
  }
  coarse <- derivative(step);fine <- derivative(step/2)
  list(sensitivity=(4*fine-coarse)/3,error=abs(fine-coarse))
}

# Closed AR(1) derivatives including the effect of refitting a mean. Each
# observed transition is independent given its predecessor, even across gaps.
ar1_forecast_sensitivity <- function(y,phi,backcasts,forecasts,estimate_mean=FALSE,mean=0) {
  observed <- which(!is.na(y));z <- as.numeric(y[observed])-mean
  beta <- derivative <- 0
  if(estimate_mean) {
    numerator <- (1-phi^2)*z[1L];denominator <- 1-phi^2
    np <- -2*phi*z[1L];dp <- -2*phi
    for(i in 2:length(observed)) {
      gap <- observed[i]-observed[i-1L];power <- phi^gap;pp <- gap*phi^(gap-1L)
      powers <- seq_len(gap)-1L;F <- sum(phi^(2*powers))
      positive <- powers[powers>0L];Fp <- sum(2*positive*phi^(2*positive-1L))
      a <- 1-power;ap <- -pp;e <- z[i]-power*z[i-1L];ep <- -pp*z[i-1L]
      numerator <- numerator+a*e/F;denominator <- denominator+a^2/F
      np <- np+(ap*e+a*ep)/F-a*e*Fp/F^2
      dp <- dp+2*a*ap/F-a^2*Fp/F^2
    }
    beta <- numerator/denominator;derivative <- (np-beta*dp)/denominator
  }
  hb <- rev(seq_len(backcasts))+min(observed)-1L
  hf <- seq_len(forecasts)+length(y)-max(observed)
  h <- c(hb,hf);endpoints <- c(rep(z[1L],backcasts),rep(tail(z,1L),forecasts))
  h*phi^(h-1L)*(endpoints-beta)+(1-phi^h)*derivative
}
