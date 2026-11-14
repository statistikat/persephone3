x11_initial_cases <- function() {
  out<-list()
  for(f in c(4L,12L)) {
    choices<-list(s3x1='s3x1',s3x3='s3x3',s3x5='s3x5',s3x9='s3x9',s3x15='s3x15',stable='stable',
      mixed_stable=c('stable',rep('s3x3',f-1L)),mixed15=c('s3x15',rep('s3x9',f-1L)))
    for(n in c(3L*f,5L*f,6L*f-1L,6L*f,6L*f+1L,20L*f,20L*f+1L,21L*f+1L))
      for(name in names(choices))for(first in if(startsWith(name,'mixed'))c(1L,2L,f)else c(1L,f)) {
        id<-paste(f,n,name,first,sep='-')
        out[[id]]<-list(id=id,frequency=f,n=n,filters=choices[[name]],choice=name,start=c(2000L,first),shape='smooth')
      }
    for(name in c('s3x3','stable','mixed_stable','s3x15'))for(shape in c('constant','linear','annual','impulse','gap','all_missing')) {
      id<-paste('control',f,name,shape,sep='-')
      out[[id]]<-list(id=id,frequency=f,n=10L*f+3L,filters=choices[[name]],choice=name,start=c(2000L,2L),shape=shape)
    }
  }
  out
}
x11_initial_input <- function(ca) {
  t<-seq_len(ca$n);y<-100+t/17+sin(t*.31)+.2*cos(t*sqrt(2))+sin(2*pi*t/ca$frequency)
  if(ca$shape=='constant')y[]<-2
  if(ca$shape=='linear')y<-as.numeric(t)
  if(ca$shape=='annual')y<-((t-1L)%%ca$frequency)-1
  if(ca$shape=='impulse')y<-as.numeric(t==ca$frequency+2L)
  if(ca$shape=='gap')y[c(1L,ca$frequency+1L,ca$n)]<-NA_real_
  if(ca$shape=='all_missing')y[]<-NA_real_
  stats::ts(y,start=ca$start,frequency=ca$frequency)
}
# Independent composition of the published annual convolution and independently
# specified seasonal/normalization operators. Prerequisites are the seasonal,
# seasonal15, seasonal-processing and normalization case helpers.
x11_initial_oracle <- function(y,filters) {
  f<-as.integer(stats::frequency(y));h<-f%/%2L;rows<-seq.int(h+1L,length(y)-h)
  b2<-as.numeric(stats::filter(y,c(.5,rep(1,f-1L),.5)/f,sides=2))[rows]
  b3<-stats::ts(as.numeric(y)[rows]-b2,start=as.numeric(stats::time(y)[rows[1L]]),frequency=f)
  b4a<-stats::ts(x11_processing_oracle(as.numeric(b3),f,rep(filters,length.out=f),as.integer(stats::cycle(b3)[1L]))$values,
    start=stats::start(b3),frequency=f)
  normalized<-x11_normalization_oracle(b4a,filters)$values
  list(B1=as.numeric(y),B2=b2,B3=as.numeric(b3),B4a=as.numeric(b4a),B4anorm=normalized,B4d=as.numeric(b3)-normalized)
}
# Dense operators for finite impulses, and a separate structural support chain
# for missing values. Algebraic cancellation must not erase a missing operand
# from an earlier stage of the algorithm.
x11_initial_operator <- function(y,filters) {
  f<-as.integer(stats::frequency(y));n<-length(y);h<-f%/%2L;rows<-h+seq_len(n-f)
  crop<-stats::ts(rep(0,n-f),start=as.numeric(stats::time(y)[rows[1L]]),frequency=f)
  trend<-matrix(0,n-f,n)
  for(j in seq_len(n-f))trend[j,seq.int(j,j+f)]<-c(.5,rep(1,f-1L),.5)/f
  selector<-diag(n)[rows,,drop=FALSE];si<-selector-trend
  s<-x11_processing_oracle(as.numeric(crop),f,rep(filters,length.out=f),as.integer(stats::cycle(crop)[1L]))
  seasonal<-matrix(0,n-f,n-f)
  for(p in seq_len(f)){at<-which(stats::cycle(crop)==p);seasonal[at,at]<-s$operators[[p]]}
  normal<-x11_normalization_operator(crop,filters)$values
  tables<-list(B1=diag(n),B2=trend,B3=si,B4a=seasonal%*%si,B4anorm=normal%*%seasonal%*%si,
    B4d=si-normal%*%seasonal%*%si)
  support<-list(B1=diag(n)!=0,B2=trend!=0,B3=(selector!=0)|(trend!=0))
  support$B4a<-(seasonal!=0)%*%support$B3>0
  support$B4anorm<-(normal!=0)%*%support$B4a>0
  support$B4d<-support$B3|support$B4anorm
  list(tables=tables,support=support)
}
