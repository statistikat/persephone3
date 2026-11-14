# Dense intrinsic Gaussian conditioning, independent of the production state
# recursion and whitening. D*y has stationary ARMA covariance; an orthonormal
# basis H of null(D) spans the unknown diffuse initial coordinates.
innovation_oracle <- function(case, blocks, beta) {
  rows <- case$rows;args <- case$arguments;y <- as.numeric(args$series)[rows]
  if(args$transformation=="log")y <- log(y)
  x <- cbind(as.numeric(args$regressors$x)[rows],as.numeric(args$regressors$fixed)[rows],
    rows^(case$d+case$D)/(factorial(case$d+case$D)*case$period^case$D))
  response <- y-drop(x%*%beta);n <- length(y)
  D <- diag(n)
  if(case$d)D <- diff(D,differences=case$d)
  if(case$D)D <- diff(D,lag=case$period,differences=case$D)
  m <- nrow(D);q <- n-m;covariance <- arma_fit_covariance(blocks,case$period,m)
  H <- if(q)qr.Q(qr(t(D)),complete=TRUE)[,m+seq_len(q),drop=FALSE]else matrix(0,n,0L)
  B <- if(q)solve(rbind(t(H),D))[,q+seq_len(m),drop=FALSE]else diag(n)
  V <- B%*%covariance%*%t(B)
  observed <- integer();unit <- variance <- rep(NA_real_,n)
  kind <- rep("missing",n)
  for(t in which(!is.na(y))) {
    p <- length(observed);rank <- 0L;weight <- numeric(p)
    U <- diag(p)
    identified <- !q
    if(q && p) {
      sv <- svd(H[observed,,drop=FALSE],nu=p,nv=q)
      rank <- sum(sv$d>1e-10*max(sv$d))
      directions <- sv$v[,seq_len(rank),drop=FALSE]
      projected <- drop(crossprod(directions,H[t,]))
      identified <- max(abs(H[t,]-drop(directions%*%projected)))<1e-9
      if(identified)weight <- drop(sv$u[,seq_len(rank),drop=FALSE]%*%(projected/sv$d[seq_len(rank)]))
      U <- sv$u
    }
    if(identified) {
      # Best linear predictor subject to reproducing every identified diffuse
      # direction. Solve only in null(H_past') to avoid an indefinite saddle
      # system. Evaluate variance in stationary coordinates to avoid subtracting
      # large integrated variances with almost complete cancellation.
      if(p>rank) {
        Z <- U[,rank+seq_len(p-rank),drop=FALSE]
        A <- V[observed,observed,drop=FALSE]
        adjustment <- solve(crossprod(Z,A%*%Z),crossprod(Z,V[observed,t]-drop(A%*%weight)))
        weight <- weight+drop(Z%*%adjustment)
      }
      contrast <- B[t,]-if(p)drop(crossprod(weight,B[observed,,drop=FALSE]))else 0
      variance[t] <- drop(crossprod(contrast,covariance%*%contrast))
      unit[t] <- (response[t]-sum(weight*response[observed]))/sqrt(variance[t])
      kind[t] <- "finite"
    } else kind[t] <- "diffuse"
    observed <- c(observed,t)
  }
  list(unit=unit,variance=variance,kind=kind,rows=which(kind=="finite"))
}
