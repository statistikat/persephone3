x11_seasonal15_cases <- function() {
  cases <- list()
  for(frequency in c(4L,12L)) {
    for(n in c(20L*frequency+c(0L,1L,frequency-1L,frequency),37L*frequency+3L))for(period in c(1L,3L,frequency)) {
      id <- paste(frequency,n,period,sep='-')
      cases[[id]] <- list(id=id,frequency=frequency,type='s3x15',N=15L,n=n,start=c(-2L,period))
    }
  }
  cases
}
x11_seasonal15_input <- function(case) {
  stats::ts(as.numeric(x11_symmetric_input(case$frequency,case$n)),start=case$start,frequency=case$frequency)
}
# Independent dense composition: eight constant virtual observations per side,
# based on the nearest five annual observations; ordinary 3x15 convolution.
# Only boundary rows are quantized to 1e-5 and closed at the farthest observed
# tap. This finite-decimal convention was characterized with public API calls.
x11_seasonal15_operator <- function(n,quantized=TRUE) {
  stopifnot(n>=17L)
  extension <- matrix(0,n+16L,n);extension[8L+seq_len(n),] <- diag(n)
  extension[1:8,1:5] <- 1/5;extension[n+8L+1:8,n-4:0] <- 1/5
  kernel <- as.numeric(stats::convolve(rep(1/3,3),rep(1/15,15),type='open'))
  A <- t(vapply(seq_len(n),function(i)as.numeric(kernel%*%extension[i+(0:16),]),numeric(n)))
  if(quantized)for(i in c(1:8,n-7:0)) {
    columns <- which(A[i,]!=0);A[i,columns] <- round(A[i,columns],5L)
    close <- if(i<=8L)max(columns)else min(columns)
    A[i,close] <- A[i,close]+1-sum(A[i,])
  }
  A
}
x11_seasonal15_phases <- function(values,frequency,quantized=TRUE) {
  result <- rep(NA_real_,length(values))
  for(phase in seq_len(frequency)) {
    rows <- seq.int(phase,length(values),frequency)
    A <- x11_seasonal15_operator(length(rows),quantized)
    for(i in seq_along(rows)) {
      support <- which(A[i,]!=0)
      result[rows[i]] <- sum(A[i,support]*values[rows[support]])
    }
  }
  result
}
