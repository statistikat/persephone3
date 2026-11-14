# Prospective requests, spans, calendars and working-mode labels.
x11_seasonal_plan_cases <- function() {
  result<-list();types<-c('stable','s3x1','s3x3','s3x5','s3x9','s3x15','x11default','msr')
  modes<-c('additive','multiplicative','log_additive','pseudo_additive')
  for(f in c(4L,12L)) {
    requests<-setNames(as.list(types),paste0('scalar-',types))
    for(type in types)requests[[paste0('vector-',type)]]<-rep(type,f)
    requests[['mixed-msr-first']]<-c('s3x5',rep('msr',f-1L))
    requests[['mixed-msr-last']]<-c(rep('msr',f-1L),'s3x5')
    for(rotation in 0:2)requests[[paste0('mixed-',rotation)]]<-rep(types[((seq_along(types)-1L+rotation)%%length(types))+1L],length.out=f)
    for(name in names(requests))for(n in c(4L*f-1L,5L*f,19L*f+1L,20L*f+1L))for(start in c(1L,f)) {
      id<-paste(f,name,n,start,sep='-')
      result[[id]]<-list(id=id,frequency=f,request=name,filters=requests[[name]],n=n,start=c(2000L,start),
        mode=modes[(length(result)%%length(modes))+1L])
    }
  }
  result
}
x11_seasonal_plan_input <- function(ca)ts(100+seq_len(ca$n)/17,start=ca$start,frequency=ca$frequency)
x11_seasonal_stages <- function()c('b_initial','b_final','c_initial','c_final','d_initial','d_final')
