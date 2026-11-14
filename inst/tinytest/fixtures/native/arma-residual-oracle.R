# Independent dense Gaussian oracle for the existing 64 synthetic fit cases.
# This deliberately slow reference calculation is not a production backend.
# It profiles missing coordinates as dummy columns before differentiating the
# complete residual vector. A parameter-dependent orthogonal rotation would
# preserve likelihood but generally change the Gauss-Newton approximation.
arma_residual_basis <- function(case, blocks) {
  rows <- case$rows; args <- case$arguments
  y <- as.numeric(args$series)[rows]
  if(args$transformation=="log") y <- log(y)
  first <- min(which(!is.na(y))); last <- max(which(!is.na(y)))
  rows <- rows[seq.int(first,last)]
  y <- y[seq.int(first,last)]
  missing <- which(is.na(y)); y[missing] <- 0
  y <- y-.2*as.numeric(args$regressors$fixed)[rows]
  x <- cbind(as.numeric(args$regressors$x)[rows],rows^(case$d+case$D)/
    (factorial(case$d+case$D)*case$period^case$D))
  design <- cbind(x,diag(length(y))[,missing,drop=FALSE])
  operator <- diag(length(y))
  if(case$d) operator <- diff(operator,differences=case$d)
  if(case$D) operator <- diff(operator,lag=case$period,differences=case$D)
  root <- t(chol(arma_fit_covariance(blocks,case$period,nrow(operator))))
  white <- forwardsolve(root,operator)
  response <- drop(white%*%y); X <- white%*%design
  beta <- qr.coef(qr(X),response)
  residuals <- response-drop(X%*%beta)
  determinant <- 2*sum(log(diag(root)))
  if(length(missing)) {
    volume <- qr.R(qr(X[,2L+seq_along(missing),drop=FALSE],LAPACK=TRUE))
    stopifnot(min(abs(diag(volume)))>1e-10)
    determinant <- determinant+2*sum(log(abs(diag(volume))))
  }
  effective <- length(residuals)-length(missing)
  squares <- sum(residuals^2)
  list(residuals=residuals,normalized=residuals*exp(determinant/(2*effective)),
    coefficients=unname(beta[1:2]),interpolated=-tail(beta,length(missing)),
    finite=effective,missing=missing,rows=rows,log_determinant=determinant,
    profile=-.5*(effective*(log(2*pi)+1+log(squares/effective))+determinant))
}

arma_residual_coordinates <- function(case, blocks) {
  template <- unlist(blocks,use.names=FALSE); free <- is.na(unlist(case$specification,use.names=FALSE))
  split <- function(theta) {
    value <- template; value[free] <- theta; at <- 0L
    answer <- lapply(lengths(blocks),function(n) { z <- value[at+seq_len(n)]; at <<- at+n; z })
    names(answer) <- names(blocks); answer
  }
  list(value=template[free],split=split)
}

arma_residual_information <- function(case, blocks, step=1e-4) {
  coordinates <- arma_residual_coordinates(case,blocks); theta <- coordinates$value
  center <- arma_residual_basis(case,blocks)
  J <- vapply(seq_along(theta),function(j) {
    h <- numeric(length(theta)); h[j] <- step
    (arma_residual_basis(case,coordinates$split(theta+h))$normalized-
       arma_residual_basis(case,coordinates$split(theta-h))$normalized)/(2*step)
  },numeric(length(center$normalized)))
  Q <- sum(center$normalized^2); n <- center$finite; df <- n-2L-length(theta)
  stopifnot(df>0L,qr(J)$rank==length(theta))
  list(covariance=Q/df*solve(crossprod(J)),gradient=n/Q*drop(crossprod(J,center$normalized)),
    jacobian=J,center=center)
}

# Exact AR(1) GLS/residual derivatives: independent of numerical differences,
# native filtering and the dense Yule-Walker/Cholesky construction above.
arma_ar1_residual_information <- function(case, phi) {
  stopifnot(case$kind=="ar",case$d==0L,case$D==0L,
    all(!is.na(case$arguments$series[case$rows])))
  args <- case$arguments; rows <- case$rows; n <- length(rows)
  y <- as.numeric(args$series)[rows]
  if(args$transformation=="log") y <- log(y)
  y <- y-.2*as.numeric(args$regressors$fixed)[rows]
  X <- cbind(as.numeric(args$regressors$x)[rows],1)
  whiten <- function(x) rbind(sqrt(1-phi^2)*x[1,,drop=FALSE],
    x[-1,,drop=FALSE]-phi*x[-n,,drop=FALSE])
  derivative <- function(x) rbind(-phi/sqrt(1-phi^2)*x[1,,drop=FALSE],-x[-n,,drop=FALSE])
  y <- matrix(y,n,1); yw <- whiten(y); Xw <- whiten(X)
  yp <- derivative(y); Xp <- derivative(X)
  beta <- solve(crossprod(Xw),crossprod(Xw,yw)); e <- yw-Xw%*%beta
  bp <- solve(crossprod(Xw),crossprod(Xp,e)+crossprod(Xw,yp-Xp%*%beta))
  ep <- yp-Xp%*%beta-Xw%*%bp
  normalized_derivative <- ep+e*phi/(n*(1-phi^2))
  sum(e^2)/(n-3L)/sum(normalized_derivative^2)
}
