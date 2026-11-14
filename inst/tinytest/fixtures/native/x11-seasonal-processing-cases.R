x11_processing_cases <- function() {
  cases <- list();types<-c('s3x1','s3x3','s3x5','s3x9','s3x15','stable')
  for(f in c(4L,12L)) {
    lengths <- unique(c(f,2L*f,4L*f,5L*f-1L,5L*f,5L*f+1L,6L*f-1L,6L*f,7L*f,10L*f,11L*f,
      19L*f,19L*f+1L,20L*f-1L,20L*f,20L*f+1L,23L*f+3L))
    for(type in types)for(n in lengths)for(period in c(1L,3L,f)) {
      id<-paste(f,type,n,period,sep='-')
      cases[[id]]<-list(id=id,frequency=f,n=n,start=c(-2L,period),filters=rep(type,f),input_group='uniform')
    }
    for(shift in 0:2)for(n in c(f,5L*f-1L,5L*f,5L*f+1L,10L*f,19L*f,19L*f+1L,20L*f+1L))for(period in c(1L,3L,f)) {
      id<-paste(f,paste0('mixed',shift),n,period,sep='-')
      cases[[id]]<-list(id=id,frequency=f,n=n,start=c(-2L,period),
        filters=types[(seq_len(f)-1L+shift)%%length(types)+1L],input_group='mixed')
    }
  }
  cases
}
x11_processing_input <- function(case) {
  stats::ts(as.numeric(x11_symmetric_input(case$frequency,case$n)),start=case$start,frequency=case$frequency)
}
# Independent annual operators. Endpoint definitions reuse published fixed
# tables or the separately qualified R virtual-value constructions, never native
# coefficients. Cache only these small deterministic mathematical definitions.
x11_processing_left_weights <- local({
  cache<-list()
  function(N) {
    key<-as.character(N);if(!is.null(cache[[key]]))return(cache[[key]])
    if(N==1L)weights<-list(c(61,39)/100)else if(N==9L)weights<-lapply(list(
      c(246,221,197,173,112,51),c(208,192,176,160,144,92,28),
      c(173,163,154,143,133,123,79,32),c(141,137,132,128,123,117,113,75,34),
      c(84,120,118,117,116,114,113,111,73,34)),function(x)x/1000)else {
      A<-if(N==15L)x11_seasonal15_operator(25L)else
        vapply(seq_len(25L),function(j)x11_seasonal_virtual(as.numeric(seq_len(25L)==j),N),numeric(25L))
      weights<-lapply(seq_len((N+1L)%/%2L),function(i)A[i,A[i,]!=0])
    }
    cache[[key]]<<-weights;weights
  }
})
x11_processing_annual_operator <- function(n,N) {
  if(N==0L)return(matrix(1/n,n,n))
  A<-matrix(0,n,n);h<-(N+1L)%/%2L
  kernel<-as.numeric(stats::convolve(rep(1/3,3),rep(1/N,N),type='open'))
  left<-x11_processing_left_weights(N)
  for(i in seq_len(n)) {
    if(i>h && i<=n-h)A[i,(i-h):(i+h)]<-kernel
    else if(i<=h && n>=h+i)A[i,seq_len(h+i)]<-left[[i]]
    else if(i>n-h && n>=h+n-i+1L)A[i,(i-h):n]<-rev(left[[n-i+1L]])
    else A[i,]<-1/n
  }
  A
}
x11_processing_oracle <- function(values,frequency,filters,start_period) {
  n<-length(values);periods<-(seq_len(n)-1L+start_period-1L)%%frequency+1L
  result<-rep(NA_real_,n);operators<-list()
  for(period in seq_len(frequency)) {
    rows<-which(periods==period);type<-filters[period]
    N<-if(type=='stable')0L else as.integer(sub('s3x','',type,fixed=TRUE))
    if(n%/%frequency<5L || (N==15L && ceiling(n/frequency)<20L))N<-0L
    A<-x11_processing_annual_operator(length(rows),N);operators[[period]]<-A
    for(i in seq_along(rows)) {
      support<-which(A[i,]!=0);result[rows[i]]<-sum(A[i,support]*values[rows[support]])
    }
  }
  list(values=result,operators=operators)
}
