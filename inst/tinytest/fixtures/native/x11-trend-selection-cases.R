# Prospective inputs and a separately composed minimum-roughness pilot oracle.
# No native weights, selection results or public targets define these inputs.
x11_trend_oracle <- local({
  cache<-list()
  function(y,frequency,relative=FALSE,backcasts=0L,forecasts=0L) {
    key<-as.character(frequency)
    if(is.null(cache[[key]]))cache[[key]]<<-henderson_minimum_roughness(frequency+1L)
    y<-as.numeric(y)[seq.int(backcasts+1L,length(y)-forecasts)]
    h<-frequency%/%2L;trend<-as.numeric(stats::filter(y,cache[[key]],sides=2))
    rows<-seq.int(h+1L,length(y)-h);trend<-trend[rows];input<-y[rows]
    irregular<-if(relative)input/trend else input-trend
    change<-function(x)mean(if(relative)abs(diff(x))/head(x,-1L)else abs(diff(x)))
    cm<-change(trend);im<-change(irregular);raw<-im/cm
    list(trend=trend,irregular=irregular,trend_mean_change=cm,irregular_mean_change=im,
      raw_ic_ratio=raw,comparison_ic_ratio=raw*(if(frequency==4L)3 else 1))
  }
})
x11_trend_cases <- function() {
  result<-list();modes<-c('additive','multiplicative','log_additive','pseudo_additive')
  for(f in c(4L,12L))for(n in c(f+2L,7L*f,17L*f+3L))for(type in c('smooth','rough','curved'))
    for(mode in modes)for(back in c(0L,2L))for(fore in c(0L,3L)) {
      id<-paste(f,n,type,mode,back,fore,sep='-')
      result[[id]]<-list(id=id,frequency=f,n=n+back+fore,type=type,mode=mode,
        backcasts=back,forecasts=fore,start=c(-10L,3L),group='regular')
    }
  for(f in c(4L,12L))for(mode in modes)for(target in c(.9999,1.0001,3.4999,3.5001)) {
    relative<-mode %in% c('multiplicative','pseudo_additive');t<-seq_len(121L)
    amplitude<-uniroot(function(a)x11_trend_oracle(100+.1*t+a*(-1)^t,f,relative)$comparison_ic_ratio-target,
      c(0,1),tol=1e-12)$root
    id<-paste(f,'threshold',mode,target,sep='-')
    result[[id]]<-list(id=id,frequency=f,n=121L,type='threshold',mode=mode,
      amplitude=amplitude,target=target,backcasts=0L,forecasts=0L,start=c(-10L,3L),group='threshold')
  }
  for(f in c(4L,12L))for(mode in modes)for(type in c('constant','zero','missing','short','negative','outlier','excluded_missing')) {
    id<-paste(f,'control',type,mode,sep='-');relative<-mode %in% c('multiplicative','pseudo_additive')
    expected<-switch(type,constant='undefined_ic_ratio',zero=if(relative)'nonpositive_relative_input'else'undefined_ic_ratio',
      missing='missing_observations',short='insufficient_observations',negative=if(relative)'nonpositive_relative_input'else'available',
      outlier=if(relative)'nonpositive_relative_trend'else'available',excluded_missing='available')
    result[[id]]<-list(id=id,frequency=f,n=if(type=='short')f+1L else 121L,type=type,mode=mode,
      backcasts=as.integer(type=='excluded_missing'),forecasts=as.integer(type=='excluded_missing'),
      start=c(-10L,3L),group='control',expected_status=expected)
  }
  result
}
x11_trend_input <- function(ca) {
  t<-seq_len(ca$n)
  y<-switch(ca$type,smooth=100+.7*t+.3*sin(t*.31),rough=100+.7*t+3*sin(t*sqrt(3)),
    curved=exp(.001*t)*(100+.7*t+2*sin(t*sqrt(3))),threshold=100+.1*t+ca$amplitude*(-1)^t,
    constant=rep(3,ca$n),zero=rep(0,ca$n),negative=-100-t/17+sin(t*.31),
    outlier=replace(rep(1,ca$n),61L,100),100+t/17+sin(t*.31))
  if(ca$type=='missing')y[61L]<-NA_real_
  if(ca$type=='excluded_missing')y[c(1L,ca$n)]<-NA_real_
  ts(y,start=ca$start,frequency=ca$frequency)
}
x11_trend_policy_cases <- function() {
  ratios<-c(zero=0,tiny=1e-100,below_one=1-.Machine$double.eps/2,one=1,
    above_one=1+.Machine$double.eps,below_upper=3.5-2*.Machine$double.eps,upper=3.5,
    above_upper=3.5+2*.Machine$double.eps,large=1e100,infinite=Inf)
  do.call(rbind,lapply(c(4L,12L),function(f)data.frame(id=paste(f,names(ratios),sep='-'),frequency=f,ratio=unname(ratios))))
}
