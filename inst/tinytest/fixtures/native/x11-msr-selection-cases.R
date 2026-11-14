# Requires x11-msr-cases.R for the independently expressed correction tables.
x11_msr_pilot_operator <- function(n) {
  stopifnot(n>=3L)
  e<-rbind(matrix(rep(c(rep(1/3,3),rep(0,n-3)),each=3),3),diag(n),
    matrix(rep(c(rep(0,n-3),rep(1/3,3)),each=3),3))
  a<-matrix(0,n,n+6L)
  for(j in seq_len(n))a[j,j:(j+6L)]<-1/7
  a%*%e
}
x11_msr_pilot_oracle <- function(y,mode,dense=TRUE) {
  s<-y
  for(p in seq_len(frequency(y))) {
    at<-which(cycle(y)==p);z<-as.numeric(y)[at];m<-length(z)
    if(all(z==z[1]))s[at]<-z[1] else if(dense)s[at]<-drop(x11_msr_pilot_operator(m)%*%z) else {
      padded<-c(rep(mean(head(z,3)),3),z,rep(mean(tail(z,3)),3))
      s[at]<-as.numeric(stats::filter(padded,rep(1/7,7),sides=2))[seq_len(m)+3L]
    }
  }
  list(seasonal=s,irregular=if(mode=='multiplicative')y/s else y-s)
}
x11_msr_selection_amplitude <- function(target,shape='alternating') {
  value<-function(amplitude) {
    year<-seq_len(20);z<-year+amplitude*if(shape=='alternating')(-1)^year else sin(year*1.37)
    y<-ts(rep(z,each=4),frequency=4)
    x11_msr_oracle(x11_msr_pilot_oracle(y,'additive'),'additive')$global
  }
  uniroot(function(a)value(a)-target,c(0,30),tol=1e-13)$root
}
x11_msr_selection_cases <- function() {
  out<-list();modes<-c('additive','multiplicative','log_additive','pseudo_additive')
  add<-function(id,f,n,first,mode,shape,back=0L,fore=0L,exclude=TRUE,amplitude=NA_real_) {
    out[[id]]<<-list(id=id,frequency=f,n=n,start=c(2000L,first),mode=mode,shape=shape,
      backcasts=back,forecasts=fore,exclude_forecast=exclude,amplitude=amplitude)
  }
  for(f in c(4L,12L)) {
    for(n in c(4L*f,5L*f-1L,5L*f,5L*f+1L,6L*f,10L*f+1L,20L*f))for(first in c(1L,2L,f))for(mode in modes)for(shape in c('smooth','rough'))
      add(paste('regular',f,n,first,mode,shape,sep='-'),f,n,first,mode,shape)
    for(mode in modes)for(back in c(0L,3L))for(fore in c(0L,5L))for(exclude in c(TRUE,FALSE))
      add(paste('extensions',f,mode,back,fore,exclude,sep='-'),f,10L*f+3L,f-1L,mode,'smooth',back,fore,exclude)
    for(mode in modes)for(shape in c('constant','annual_constant','gap','gap_back','gap_fore','gap_calendar',
      'zero','negative','zero_back','negative_fore','negative_calendar','all_missing'))
      add(paste('control',f,mode,shape,sep='-'),f,10L*f+3L,2L,mode,shape,2L,3L)
  }
  targets<-c(2.4,2.6,3.4,3.6,5.4,5.6,6.4,6.6,as.vector(outer(c(-1,1)*1e-5,c(2.5,3.5,5.5,6.5),'+')))
  for(target in targets) {
    amplitude<-x11_msr_selection_amplitude(target)
    for(f in c(4L,12L))add(paste('ratio',f,sprintf('%.6f',target),sep='-'),f,20L*f,1L,'additive','alternating',amplitude=amplitude)
  }
  amplitude<-x11_msr_selection_amplitude(2.6,'sine')
  for(f in c(4L,12L))add(paste0('policy-difference-',f),f,20L*f,1L,'additive','sine',amplitude=amplitude)
  out
}
x11_msr_selection_input <- function(ca) {
  t<-seq_len(ca$n);f<-ca$frequency;y<-2+.1*sin(t*.11)+.1*cos(t*.49)
  if(ca$shape=='rough')y<-10+.5*sin(t*.73)+.3*cos(t*1.37)
  if(ca$shape %in% c('alternating','sine')) {
    year<-(t-1L)%/%f+1L;y<-year+ca$amplitude*if(ca$shape=='alternating')(-1)^year else sin(year*1.37)
  }
  if(ca$shape=='constant')y[]<-2
  if(ca$shape=='annual_constant')y<-2+.1*((t-1L+ca$start[2L]-1L)%%f)
  if(ca$shape=='gap')y[ca$backcasts+f+2L]<-NA_real_
  if(ca$shape=='gap_back')y[1]<-NA_real_
  if(ca$shape=='gap_fore')y[ca$n]<-NA_real_
  if(ca$shape=='gap_calendar')y[ca$n-ca$forecasts]<-NA_real_
  if(ca$shape=='zero')y[ca$backcasts+f+2L]<-0
  if(ca$shape=='negative')y[ca$backcasts+f+2L]<--1
  if(ca$shape=='zero_back')y[1]<-0
  if(ca$shape=='negative_fore')y[ca$n]<--1
  if(ca$shape=='negative_calendar')y[ca$n-ca$forecasts]<--1
  if(ca$shape=='all_missing')y[]<-NA_real_
  ts(y,start=ca$start,frequency=f)
}
x11_msr_selection_rows <- function(y,back,fore) {
  n<-length(y);rows<-seq_len(n)
  retained<-rows[rows>back & rows<=n-fore]
  if(!length(retained))return(list(estimation=retained,calendar=retained))
  periods<-as.integer(cycle(y));years<-floor(as.numeric(time(y))+1e-8)
  last<-tail(retained,1);last_year<-years[last]-as.integer(periods[last]<frequency(y))
  list(estimation=retained,calendar=retained[years[retained]<=last_year])
}
x11_msr_selection_oracle <- function(y,mode='additive',iteration_policy='jdplus',backcasts=0L,forecasts=0L,max_iterations=1000L) {
  f<-as.integer(frequency(y));spans<-x11_msr_selection_rows(y,backcasts,forecasts);rows<-spans$calendar
  result<-list(status='available',reason='insufficient_initial_history',selected_filter='s3x5',using_default=TRUE,
    estimation_rows=spans$estimation,calendar_rows=rows,selection_rows=rows,attempts=list(),pilot=NULL,table=NULL)
  if(length(rows)<5L*f)return(result)
  result$selected_filter<-NA_character_;result$using_default<-FALSE;result$reason<-'unavailable'
  for(iteration in seq_len(max_iterations)) {
    current<-ts(as.numeric(y)[rows],start=as.numeric(time(y)[rows[1L]]),frequency=f)
    result$selection_rows<-rows;result$pilot<-result$table<-NULL
    status<-if(anyNA(current))'missing_observations'else if(mode=='multiplicative' && any(current<=0))'nonpositive_relative_input'else'available'
    ratio<-NA_real_;band<-'unavailable'
    if(status=='available') {
      result$pilot<-x11_msr_pilot_oracle(current,mode)
      result$table<-x11_msr_oracle(result$pilot,mode);ratio<-result$table$global
      if(is.na(ratio))status<-'undefined_msr'else band<-if(ratio<2.5)'s3x3'else if(ratio<3.5)'lower_gap'else
        if(ratio<5.5)'s3x5'else if(ratio<6.5)'upper_gap'else's3x9'
    }
    result$attempts[[iteration]]<-list(count=length(rows),global_msr=ratio,band=band,status=status)
    if(status!='available') {result$status<-status;return(result)}
    if(band %in% c('s3x3','s3x5','s3x9')) {result$selected_filter<-band;result$reason<-'decisive_ratio';return(result)}
    if(iteration_policy=='x11_five_year_cap' && iteration==6L) {result$reason<-'five_year_cap';break}
    if(length(rows)-f<5L*f) {result$reason<-'insufficient_remaining_history';break}
    if(iteration==max_iterations) {result$status<-'budget_exhausted';result$reason<-'iteration_budget';return(result)}
    rows<-head(rows,-f)
  }
  result$selected_filter<-'s3x5';result$using_default<-TRUE;result
}
