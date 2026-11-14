x11_mean_cases <- function() {
  cases <- list()
  for(frequency in c(4L,12L))for(n in unique(c(seq.int(frequency%/%2L,frequency+3L),17L,43L,257L))) {
    id <- paste(frequency,n,sep='-')
    cases[[id]] <- list(id=id,frequency=frequency,n=n)
  }
  cases
}
# Independent dense linear operator. Map virtual boundary observations to means
# of the original edge windows, then compose a centered moving-average matrix.
# This differs from the production padding, masking and finite-filter loops.
x11_mean_operator <- function(frequency,n) {
  h <- frequency%/%2L
  stopifnot(n>=h)
  extension <- matrix(0,n+2L*h,n)
  extension[h+seq_len(n),] <- diag(n)
  extension[seq_len(h),seq_len(h)] <- 1/h
  extension[h+n+seq_len(h),seq.int(n-h+1L,n)] <- 1/h
  average <- matrix(0,n,n+2L*h)
  w <- c(.5,rep(1,frequency-1L),.5)/frequency
  for(row in seq_len(n))average[row,row+seq_along(w)-1L] <- w
  average%*%extension
}
x11_mean_independent <- function(values,frequency) {
  A <- x11_mean_operator(frequency,length(values))
  missing <- as.numeric((A!=0)%*%is.na(values))>0
  values[is.na(values)] <- 0
  result <- as.numeric(A%*%values);result[missing] <- NA_real_;result
}
