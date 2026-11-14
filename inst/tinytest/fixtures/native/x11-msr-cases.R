# Prospective component tables, independent of native calculations/targets.
x11_msr_cases <- function() {
  out<-list();modes<-c('additive','multiplicative','log_additive','pseudo_additive')
  for(f in c(4L,12L)) {
    lengths<-c(2L*f-1L,2L*f,3L*f,4L*f-1L,4L*f,5L*f,6L*f,7L*f,7L*f+1L,8L*f-1L,20L*f+1L,101L*f-1L)
    for(n in lengths)for(start in c(1L,2L,f))for(mode in modes)for(shape in c('smooth','rough')) {
      id<-paste(f,n,start,mode,shape,sep='-')
      out[[id]]<-list(id=id,frequency=f,n=n,start=c(2000L,start),mode=mode,shape=shape)
    }
    for(mode in modes)for(shape in c('constant','seasonal_constant','irregular_constant','one_constant_phase','seasonal_gap','irregular_gap','zero','negative')) {
      id<-paste(f,mode,shape,sep='-')
      out[[id]]<-list(id=id,frequency=f,n=10L*f+1L,start=c(2000L,f),mode=mode,shape=shape)
    }
  }
  out
}
x11_msr_input <- function(ca) {
  t<-seq_len(ca$n);f<-ca$frequency
  s<-100+t*.13+sin(t*.31);i<-10+sin(t*.7)+cos(t*1.13)*.2
  if(ca$shape=='rough') {s<-2+sin(t*1.61)*.5;i<-1+cos(t*.36)*.35+sin(t*.95)*.2}
  if(ca$shape %in% c('constant','seasonal_constant'))s[]<-10
  if(ca$shape %in% c('constant','irregular_constant'))i[]<-10
  if(ca$shape=='one_constant_phase') {at<-seq.int(1L,ca$n,f);s[at]<-i[at]<-10}
  if(ca$shape=='seasonal_gap')s[f+2L]<-NA_real_
  if(ca$shape=='irregular_gap')i[f+2L]<-NA_real_
  if(ca$shape=='zero')s[f+2L]<-0
  if(ca$shape=='negative')i[f+2L]<--2
  list(seasonal=ts(s,start=ca$start,frequency=f),irregular=ts(i,start=ca$start,frequency=f))
}
x11_msr_mode_cases <- function() {
  out<-list()
  for(f in c(4L,12L))for(first in c(1L,f))for(mode in c('additive','multiplicative','log_additive','pseudo_additive')) {
    id<-paste(f,first,mode,sep='-');t<-seq_len(10L*f)
    out[[id]]<-list(id=id,mode=mode,y=ts(2+sin(t*.11)*.1+cos(t*.49)*.1,start=c(2000L,first),frequency=f))
  }
  out
}
# Independent seven-term mean-extension operator, used only to qualify mode
# semantics against the public selector's returned component table. The public
# iteration count identifies the target span; this does not test native choice.
x11_msr_mode_components <- function(ca,iterations) {
  y<-ca$y;f<-frequency(y);n<-length(y)-tail(as.integer(cycle(y)),1L)%%f-(iterations-1L)*f
  y<-ts(as.numeric(y)[seq_len(n)],start=start(y),frequency=f);s<-y
  for(p in seq_len(f)) {
    at<-which(cycle(y)==p);z<-as.numeric(y)[at];m<-length(z)
    # Dense extension/averaging multiplication independent of core arithmetic.
    extension<-rbind(matrix(rep(c(rep(1/3,3),rep(0,m-3)),each=3),3),diag(m),
      matrix(rep(c(rep(0,m-3),rep(1/3,3)),each=3),3))
    smoothing<-matrix(0,m,m+6)
    for(j in seq_len(m))smoothing[j,j:(j+6L)]<-1/7
    s[at]<-drop(smoothing%*%extension%*%z)
  }
  list(seasonal=s,irregular=if(ca$mode=='multiplicative')y/s else y-s)
}
# Direct formula oracle: no native calls, ordinary per-phase differences and
# weighted.mean. Round the published radicals rather than fitting constants.
x11_msr_oracle <- function(y,mode) {
  f<-frequency(y$seasonal);period<-as.integer(cycle(y$seasonal))
  relative<-mode=='multiplicative'
  factors<-function(n) {
    if(n<4)return(c(1,1))
    if(n==4)return(c(3,round(90/(2*sqrt(842)+21*sqrt(2)),5)))
    if(n==5)return(round(c(3*sqrt(2)/(1+sqrt(3)),60/(sqrt(894)+2*sqrt(211))),5))
    if(n==6)return(round(c(5*sqrt(6)/(8+sqrt(2)),25*sqrt(3)/(2*sqrt(298)+sqrt(67))),5))
    # Algebraically divide numerator/denominator by the leading coefficient.
    a<-round(c(sqrt(3),5*sqrt(6)),6);b<-round(c(6*sqrt(2),6*sqrt(149)),6)
    (n-1)/(n-7+b/a)
  }
  rows<-lapply(seq_len(f),function(p) {
    at<-which(period==p);n<-length(at);correction<-factors(n)
    raw<-vapply(y,function(component) {
      z<-as.numeric(component)[at]
      if(n<2 || anyNA(z) || (relative && any(z<=0)))return(NA_real_)
      mean(abs(diff(z))/if(relative)head(z,-1)else 1)
    },numeric(1))
    c(observations=n,changes=max(0,n-1),seasonal_raw=unname(raw[1]),irregular_raw=unname(raw[2]),
      seasonal_correction=correction[1],irregular_correction=correction[2],seasonal=unname(raw[1])*correction[1],irregular=unname(raw[2])*correction[2])
  })
  phases<-as.data.frame(do.call(rbind,rows));phases$msr<-phases$irregular/phases$seasonal
  # Require every calendar phase, including zero-weight phases with no change.
  # weighted.mean otherwise silently drops their missing values.
  complete_mean<-function(x)if(anyNA(x))NA_real_ else weighted.mean(x,phases$changes)
  means<-c(seasonal=complete_mean(phases$seasonal),irregular=complete_mean(phases$irregular))
  list(phases=phases,means=means,global=unname(means[2]/means[1]))
}
