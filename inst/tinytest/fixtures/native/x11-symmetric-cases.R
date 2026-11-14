# Prospective finite-input filter matrix. No native/reference result defines
# these inputs, options, sample dates or independently testable identities.
x11_symmetric_cases <- function() {
  result <- list()
  for(frequency in c(4L,12L)) {
    for(type in c('trend','s3x1','s3x3','s3x5','s3x9','s3x15')) {
      id <- paste(frequency,type,sep='-')
      result[[id]] <- list(id=id,type=type,frequency=frequency,length=NULL)
    }
    for(length in seq.int(1L,101L,2L)) {
      id <- paste(frequency,'henderson',length,sep='-')
      result[[id]] <- list(id=id,type='henderson',frequency=frequency,length=length)
    }
  }
  result
}
x11_symmetric_input <- function(frequency,n=257L) {
  t <- seq_len(n)
  ts(100+t/17+sin(t*.31)+.2*cos(t*sqrt(2))+sin(2*pi*t/frequency),start=c(2000,3),frequency=frequency)
}
# Independent minimum squared third differences, with zero continuation of the
# coefficient sequence outside its finite support and polynomial constraints.
henderson_minimum_roughness <- function(length) {
  if(length<=3L)return(1)
  half <- length%/%2L;j <- (-half):half
  D <- diff(diag(length+6L),differences=3L)[,3L+seq_len(length),drop=FALSE]
  C <- rbind(rep(1,length),(j/half)^2)
  inverse_constraints <- solve(crossprod(D),t(C))
  drop(inverse_constraints%*%solve(C%*%inverse_constraints,c(1,0)))
}
# Separately evaluate taps by original indices. No production finite filter is
# used; missing zero-weight taps have no effect on the mathematical sum.
x11_filter_sum <- function(y,weights,offsets) {
  vapply(seq_along(y),function(i) {
    active <- weights!=0;rows <- i+offsets[active]
    if(any(rows<1L | rows>length(y)) || anyNA(y[rows]))NA_real_ else sum(y[rows]*weights[active])
  },numeric(1))
}
