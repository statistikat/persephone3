x11_seasonal_cases <- function() {
  cases <- list()
  for(frequency in c(4L,12L))for(N in c(1L,3L,5L,9L)) {
    minimum <- frequency*max(5L,N+2L)
    for(n in unique(c(minimum+c(0L,1L,frequency-1L,frequency),23L*frequency+3L)))for(period in c(1L,3L,frequency)) {
      type <- paste0('s3x',N);id <- paste(frequency,type,n,period,sep='-')
      cases[[id]] <- list(id=id,frequency=frequency,type=type,N=N,n=n,start=c(-2L,period))
    }
  }
  cases
}
x11_seasonal_input <- function(case) {
  stats::ts(as.numeric(x11_symmetric_input(case$frequency,case$n)),start=case$start,frequency=case$frequency)
}
# Independent construction from the published virtual-observation rules, using
# actual extended annual values and R convolution rather than coefficient tables.
x11_seasonal_virtual <- function(values,N) {
  stopifnot(N %in% c(3L,5L));h <- (N+1L)%/%2L;n <- length(values)
  if(N==3L) {
    left <- c(values[2L],mean(head(values,3L)))
    right <- c(mean(tail(values,3L)),values[n-1L])
  } else {
    left <- c(values[3L],rep(mean(head(values,4L)),2L))
    right <- c(rep(mean(tail(values,4L)),2L),values[n-2L])
  }
  w <- as.numeric(stats::convolve(rep(1/3,3L),rep(1/N,N),type='open'))
  as.numeric(stats::filter(c(left,values,right),w,sides=2L))[h+seq_len(n)]
}
x11_seasonal_virtual_phases <- function(values,frequency,N) {
  result <- rep(NA_real_,length(values))
  for(phase in seq_len(frequency)) {
    rows <- seq.int(phase,length(values),frequency)
    result[rows] <- x11_seasonal_virtual(values[rows],N)
  }
  result
}
