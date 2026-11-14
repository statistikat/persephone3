x11_normalization_cases <- function() {
  out<-list()
  for(f in c(4L,12L)) {
    choices<-list(s3x1='s3x1',s3x3='s3x3',s3x5='s3x5',s3x9='s3x9',s3x15='s3x15',stable='stable',
      mixed_stable=c('stable',rep('s3x3',f-1L)),mixed15=c('s3x15',rep('s3x9',f-1L)))
    for(n in c(2L*f,4L*f,5L*f-1L,5L*f,5L*f+1L,19L*f,19L*f+1L,20L*f+1L))
      for(name in names(choices))for(first in if(startsWith(name,'mixed'))c(1L,2L,f)else c(1L,f))for(extend in c(0L,f%/%2L)) {
        id<-paste(f,n,name,first,extend,sep='-')
        out[[id]]<-list(id=id,frequency=f,n=n,filters=choices[[name]],choice=name,start=c(2000L,first),extend=extend,shape='smooth')
      }
    for(name in c('s3x3','stable','mixed_stable','s3x15'))for(shape in c('constant','linear','annual','impulse','gap','all_missing')) {
      id<-paste('control',f,name,shape,sep='-')
      out[[id]]<-list(id=id,frequency=f,n=10L*f+3L,filters=choices[[name]],choice=name,start=c(2000L,2L),extend=f+3L,shape=shape)
    }
  }
  out
}
x11_normalization_input <- function(ca) {
  t<-seq_len(ca$n);y<-2+.1*sin(t*.73)+.2*cos(t*.11)
  if(ca$shape=='constant')y[]<-2
  if(ca$shape=='linear')y<-as.numeric(t)
  if(ca$shape=='annual')y<-((t-1L)%%ca$frequency)-1
  if(ca$shape=='impulse')y<-as.numeric(t==ca$frequency+2L)
  if(ca$shape=='gap')y[c(1L,ca$frequency+1L,ca$n)]<-NA_real_
  if(ca$shape=='all_missing')y[]<-NA_real_
  ts(y,start=ca$start,frequency=ca$frequency)
}
# Independent dense centered annual convolution, followed by an explicit
# endpoint-row selection operator and periodic restoration of normalized rows.
x11_normalization_operator <- function(y,filters,extend=0L) {
  f<-as.integer(frequency(y));n<-length(y);h<-f%/%2L
  filters<-rep(filters,length.out=f)
  reason<-ifelse(filters=='stable','requested_stable',if(n<5*f)'short_span'else
    ifelse(filters=='s3x15' & ceiling(n/f)<20,'short_3x15_span','nearest_central_mean'))
  kernel<-c(.5,rep(1,f-1L),.5)/f;center<-matrix(0,n,n)
  for(row in seq.int(h+1L,n-h))center[row,seq.int(row-h,row+h)]<-kernel
  source<-seq_len(n)
  for(row in c(seq_len(h),seq.int(n-h+1L,n))) {
    periodic<-reason[cycle(y)[row]]!='nearest_central_mean'
    source[row]<-if(periodic)row+if(row<=h)f else -f else if(row<=h)h+1L else n-h
  }
  normalizer<-center[source,,drop=FALSE]
  dates<-seq.int(1L-extend,n+extend);restored<-dates
  for(row in which(dates<1L))restored[row]<-which(cycle(y)==((dates[row]+cycle(y)[1L]-2L)%%f+1L))[1L]
  for(row in which(dates>n))restored[row]<-tail(which(cycle(y)==((dates[row]+cycle(y)[1L]-2L)%%f+1L)),1L)
  list(normalization=normalizer,values=(diag(n)-normalizer)[restored,,drop=FALSE],normalization_source_rows=source,
    source_rows=restored,reasons=reason)
}
x11_normalization_oracle <- function(y,filters,extend=0L) {
  operators<-x11_normalization_operator(y,filters,extend)
  apply<-function(a) {
    # Resolve missing taps before arithmetic, preserving structural zeros.
    value<-numeric(nrow(a))
    for(j in seq_len(nrow(a))) {at<-which(a[j,]!=0);value[j]<-if(anyNA(y[at]))NA_real_ else sum(a[j,at]*y[at])}
    value
  }
  c(list(values=apply(operators$values),normalization=apply(operators$normalization)),operators[c('normalization_source_rows','source_rows','reasons')])
}
