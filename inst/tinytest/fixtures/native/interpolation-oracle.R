# Dense intrinsic-Gaussian precision and Schur-complement oracle. It does not
# use production filtering, gains, missing dummy regression or QR covariance.
# arma_fit_covariance() independently solves the stationary covariance equations.
interpolation_oracle <- function(context,blocks,include_endpoints=FALSE) {
  data <- context$prepared;y <- as.numeric(data$inputs$transformed)
  rows <- if(include_endpoints)seq_along(y)else seq.int(min(which(!is.na(y))),max(which(!is.na(y))))
  y <- y[rows];X <- data$design[rows,,drop=FALSE];fixed <- !is.na(data$fixed)
  if(any(fixed))y <- y-drop(X[,fixed,drop=FALSE]%*%data$fixed[fixed])
  y <- y-context$mean;X <- X[,!fixed,drop=FALSE]
  missing <- which(is.na(y));observed <- which(!is.na(y));n <- length(y)
  D <- diag(n)
  if(context$d)D <- diff(D,differences=context$d)
  if(context$seasonal_d)D <- diff(D,lag=context$period,differences=context$seasonal_d)
  root <- t(chol(arma_fit_covariance(blocks,context$period,nrow(D))))
  W <- forwardsolve(root,D);P <- crossprod(W)
  U <- if(length(missing))solve(P[missing,missing,drop=FALSE])else matrix(numeric(),0,0)
  L <- if(length(missing)) -U%*%P[missing,observed,drop=FALSE]else matrix(numeric(),0,length(observed))
  marginal <- P[observed,observed,drop=FALSE]-P[observed,missing,drop=FALSE]%*%U%*%P[missing,observed,drop=FALSE]
  Xo <- X[observed,,drop=FALSE]
  beta_covariance <- if(ncol(X))solve(crossprod(Xo,marginal%*%Xo))else matrix(numeric(),0,0)
  beta <- if(ncol(X))drop(beta_covariance%*%crossprod(Xo,marginal%*%y[observed]))else numeric()
  point <- drop(X[missing,,drop=FALSE]%*%beta+L%*%(y[observed]-drop(Xo%*%beta)))
  sensitivity <- X[missing,,drop=FALSE]-L%*%Xo
  increment <- sensitivity%*%beta_covariance%*%t(sensitivity)
  complete <- y;complete[missing] <- point
  Q <- sum((W%*%(complete-drop(X%*%beta)))^2)
  if(any(fixed))point <- point+drop(data$design[rows[missing],fixed,drop=FALSE]%*%data$fixed[fixed])
  point <- point+context$mean
  list(rows=rows[missing],points=point,known=unname(U),regression=unname(increment),
    covariance=unname(U+increment),Q=Q,n=length(observed)-(n-nrow(D)))
}
