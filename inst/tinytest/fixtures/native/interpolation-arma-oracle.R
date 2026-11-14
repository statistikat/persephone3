# Independent dense-precision point sensitivity, using smaller fixed central
# stencils than the production qualified adaptive differences.
interpolation_arma_sensitivity <- function(context,blocks,step=1e-4) {
  values <- unlist(blocks,use.names=FALSE)
  free <- which(is.na(unlist(context$parameters$specification,use.names=FALSE)))
  m <- length(interpolation_oracle(context,blocks)$points)
  split <- function(value) {
    at <- 0L
    answer <- lapply(lengths(blocks),function(n) {x<-value[at+seq_len(n)];at<<-at+n;x})
    names(answer) <- names(blocks);answer
  }
  derivative <- function(h) {
    answer <- matrix(0,m,length(free))
    for(j in seq_along(free)) {
      delta <- numeric(length(values));delta[free[j]] <- h
      answer[,j] <- (interpolation_oracle(context,split(values+delta))$points-
        interpolation_oracle(context,split(values-delta))$points)/(2*h)
    }
    answer
  }
  coarse <- derivative(step);fine <- derivative(step/2)
  list(sensitivity=(4*fine-coarse)/3,error=abs(fine-coarse))
}

# Exact AR(1) missing-point derivative, including a re-estimated intercept.
# Transition intervals between observed rows may be longer than one period.
ar1_interpolation_sensitivity <- function(y,phi,estimate_mean=FALSE,mean=0) {
  missing <- which(is.na(y));stopifnot(length(missing)==1L,missing>1L,missing<length(y))
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
  neighbours <- sum(as.numeric(y[missing+c(-1L,1L)]))-2*(mean+beta)
  (1-phi^2)/(1+phi^2)^2*neighbours+(1-2*phi/(1+phi^2))*derivative
}
