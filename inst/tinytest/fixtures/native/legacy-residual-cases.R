# Reporting controls independent of the fitted 64-case matrix. The same inputs
# drive mandatory Java-free invariants and explicit public-API qualification.
legacy_residual_cases <- function() {
  answer <- list()
  for(period in c(4L,12L))for(diff in list(c(0L,0L),c(1L,0L),c(0L,1L),c(1L,1L),c(2L,0L),c(0L,2L))) {
    for(kind in c("white","ar","ma","arma"))for(regression in c(FALSE,TRUE)) {
      n <- if(period==4L)72L else 96L;t <- seq_len(n)
      z <- 3+.02*t+.6*sin(t*.63)+.25*cos(t*.17)+.11*sin(t*1.97)
      logged <- (match(kind,c("white","ar","ma","arma"))+as.integer(regression)+(period==12L))%%2L==0L
      y <- ts(if(logged)exp(z)else z,start=c(2000,2),frequency=period)
      y[c(1L,2L,period+2L,period+3L,2L*period+5L,n)] <- NA_real_
      args <- list(series=y,d=diff[1L],seasonal_d=diff[2L],transformation=if(logged)"log"else"none")
      if(kind %in% c("ar","arma"))args$ar <- .45
      if(kind %in% c("ma","arma"))args$ma <- .3
      if(regression) {
        args$include_mean <- sum(diff)==0L;args$include_drift <- sum(diff)>0L
        args$regressors <- list(fixed=ts(cos(t*.23),start=start(y),frequency=period))
        args$coefficients <- c(fixed=.2)
      }
      id <- paste(period,paste(diff,collapse=""),kind,if(regression)"regression"else"no_regression",if(logged)"log"else"none",sep="-")
      answer[[id]] <- list(id=id,period=period,d=diff[1L],D=diff[2L],arguments=args)
    }
  }
  answer
}

# Original-scale adjacent endpoints fill a whole internal gap with one value;
# transformation follows filling. This is an explicit reporting convention,
# not the conditional Gaussian point calculation.
legacy_preliminary_points <- function(series,rows,transformation) {
  values <- as.numeric(series);points <- numeric(length(rows))
  for(j in seq_along(rows)) {
    left <- max(which(!is.na(values) & seq_along(values)<rows[j]))
    right <- min(which(!is.na(values) & seq_along(values)>rows[j]))
    points[j] <- (values[left]+values[right])/2
  }
  if(transformation=="log")log(points)else points
}
