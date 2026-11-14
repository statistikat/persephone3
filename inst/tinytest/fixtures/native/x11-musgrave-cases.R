x11_musgrave_cases <- function() {
  cases <- list()
  add <- function(frequency,length,available,ratio) {
    id <- paste(frequency,length,available,format(ratio,digits=15,trim=TRUE),sep='-')
    cases[[id]] <<- list(id=id,frequency=frequency,length=length,available=available,ic_ratio=ratio)
  }
  common <- c(5L,7L,9L,13L,23L)
  for(frequency in c(4L,12L)) {
    for(length in common)for(ratio in c(.001,.99,1,3.5,4.5,7,100))for(available in seq_len(length%/%2L)-1L)
      add(frequency,length,available,ratio)
    for(length in setdiff(seq.int(5L,101L,2L),common)) {
      h <- length%/%2L
      for(available in unique(c(0L,h%/%2L,h-1L)))add(frequency,length,available,exp(1))
    }
  }
  cases
}
# Independent constrained minimum-revision solve. The free coordinates are an
# orthonormal basis orthogonal to constants; a rectangular least-squares system
# represents irregular variance and the squared linear-trend revision. It does
# not evaluate the closed coefficient formula used by the C++ implementation.
x11_musgrave_minimum <- function(weights,available,ratio) {
  h <- length(weights)%/%2L;m <- h+available+1L;x <- seq.int(-h,available)
  target <- sum(weights*seq.int(-h,h));base <- rep(1/m,m)
  if(ratio==0) {
    C <- rbind(1,x);w <- weights[seq_len(m)]
    return(as.numeric(w+t(C)%*%solve(tcrossprod(C),c(1,target)-C%*%w)))
  }
  Z <- qr.Q(qr(matrix(1,m,1)),complete=TRUE)[,-1,drop=FALSE]
  noise <- if(ratio<=1)sqrt(pi)*ratio/2 else 1
  slope <- if(ratio<=1)1 else (2/sqrt(pi))/ratio
  A <- rbind(noise*Z,slope*as.numeric(x%*%Z))
  b <- c(noise*(weights[seq_len(m)]-base),slope*(target-sum(x*base)))
  as.numeric(base+Z%*%qr.solve(A,b,tol=1e-14))
}
x11_musgrave_direct <- function(values,symmetric,right) {
  n <- length(values);h <- length(symmetric)%/%2L
  output <- rep(NA_real_,n)
  for(i in seq_len(n)) {
    if(i<=h) {w<-rev(right[[i]]);at<-seq_len(h+i)}
    else if(i>n-h) {q<-n-i;w<-right[[q+1L]];at<-seq.int(i-h,n)}
    else {w<-symmetric;at<-seq.int(i-h,i+h)}
    active <- w!=0
    if(!anyNA(values[at[active]]))output[i] <- sum(w[active]*values[at[active]])
  }
  output
}
