# Independently whitened full Gaussian likelihood; no production derivatives or
# Schur-complement assembly are used for the joint covariance target.
joint_information_oracle <- function(case, blocks, beta, beta_sd, theta_sd, step=.004) {
  free <- is.na(unlist(case$specification));theta <- unlist(blocks,use.names=FALSE)[free]
  block_values <- function(value) {
    vector <- unlist(blocks,use.names=FALSE);vector[free] <- value
    answer <- blocks;at <- 0L
    for(i in seq_along(answer)) if(length(answer[[i]])) {
      ix <- at+seq_along(answer[[i]]);answer[[i]] <- unname(vector[ix]);at <- max(ix)
    }
    answer
  }
  # Independent GLS sensitivity, needed to diagnose cross-block errors.
  derivatives <- function(h) vapply(seq_along(theta),function(j) {
    plus <- minus <- theta;plus[j] <- plus[j]+h;minus[j] <- minus[j]-h
    (arma_fit_density(case,block_values(plus))$beta[c(1L,3L)]-
      arma_fit_density(case,block_values(minus))$beta[c(1L,3L)])/(2*h)
  },numeric(2))
  B <- (4*derivatives(5e-5)-derivatives(1e-4))/3
  scale <- c(beta_sd,theta_sd);p <- length(scale)
  objective <- function(point) {
    adjusted <- c(beta,theta)+scale*point
    -arma_fit_density(case,block_values(tail(adjusted,length(theta))),head(adjusted,2L))$profile
  }
  hessian <- function(h) {
    H <- matrix(0,p,p);zero <- numeric(p);f <- objective(zero)
    for(j in seq_len(p)) {
      plus <- minus <- zero;plus[j] <- h;minus[j] <- -h
      H[j,j] <- ((objective(plus)-f)+(objective(minus)-f))/h^2
      for(i in seq_len(j-1L)) {
        pp <- pm <- plus;mp <- mm <- minus;pp[i] <- mp[i] <- h;pm[i] <- mm[i] <- -h
        H[i,j] <- H[j,i] <- ((objective(pp)-objective(pm))-(objective(mp)-objective(mm)))/(4*h*h)
      }
    }
    H
  }
  H <- (4*hessian(step/2)-hessian(step))/3
  list(sensitivity=B,scaled_hessian=H,scaled_covariance=solve(H),scale=scale)
}
