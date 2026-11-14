# Public signatures and execution only, no implementation inspection.
reference_x11_trend_ratio <- function(values,frequency,mode,backcasts=0L,forecasts=0L,
    trend_length=0L,exclude_forecasts=FALSE) {
  builder_class<-'jdplus/x13/base/core/x11/X11Context$Builder';context_class<-'jdplus/x13/base/core/x11/X11Context'
  b<-rJava::.jcall(context_class,paste0('L',builder_class,';'),'builder')
  option<-switch(mode,additive='Additive',multiplicative='Multiplicative',log_additive='LogAdditive',pseudo_additive='PseudoAdditive')
  enum<-rJava::.jfield('jdplus/sa/base/api/DecompositionMode','Ljdplus/sa/base/api/DecompositionMode;',option)
  b<-rJava::.jcall(b,paste0('L',builder_class,';'),'mode',enum)
  for(key in c('period','trendFilterLength','forecastHorizon','backcastHorizon'))
    b<-rJava::.jcall(b,paste0('L',builder_class,';'),key,as.integer(switch(key,period=frequency,
      trendFilterLength=trend_length,forecastHorizon=forecasts,backcastHorizon=backcasts)))
  b<-rJava::.jcall(b,paste0('L',builder_class,';'),'excludefcast',exclude_forecasts)
  context<-rJava::.jcall(b,paste0('L',context_class,';'),'build')
  input<-rJava::.jcall('jdplus/toolkit/base/api/data/DoubleSeq','Ljdplus/toolkit/base/api/data/DoubleSeq;',
    'of',rJava::.jarray(as.numeric(values)))
  rJava::.jcall('jdplus/x13/base/core/x11/filter/AutomaticHenderson','D','calcICR',context,input)
}
reference_x11_trend_length <- function(ratio,frequency)rJava::.jcall(
  'jdplus/x13/base/core/x11/filter/AutomaticHenderson','I','selectFilter',as.double(ratio),as.integer(frequency))
reference_x11_trend_default <- function(length,frequency)rJava::.jcall(
  'jdplus/x13/base/core/x11/filter/MusgraveFilterFactory','D','findR',as.integer(length),as.integer(frequency))
reference_x11_trend_bank <- function(length,frequency)rev(rJava::.jcall(
  'jdplus/x13/base/core/x11/filter/MusgraveFilterFactory','[Ljdplus/toolkit/base/core/math/linearfilters/IFiniteFilter;',
  'makeFiltersForHenderson',as.integer(length),as.integer(frequency)))
