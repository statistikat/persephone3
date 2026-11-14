# Public context/selector/getter APIs and public seven-term filter application.
# No upstream numerical implementation, bytecode or decompilation is read.
reference_x11_msr_select <- function(y,mode,backcasts=0L,forecasts=0L,exclude_forecast=TRUE) {
  builder<-'jdplus/x13/base/api/x11/X11Spec$Builder';option<-'jdplus/x13/base/api/x11/SeasonalFilterOption'
  b<-rJava::.jcall('jdplus/x13/base/api/x11/X11Spec',paste0('L',builder,';'),'builder')
  b<-rJava::.jcall(b,paste0('L',builder,';'),'filters',rJava::.jarray(list(rJava::.jfield(option,paste0('L',option,';'),'Msr')),contents.class=option))
  b<-rJava::.jcall(b,paste0('L',builder,';'),'backcastHorizon',as.integer(backcasts))
  b<-rJava::.jcall(b,paste0('L',builder,';'),'forecastHorizon',as.integer(forecasts))
  b<-rJava::.jcall(b,paste0('L',builder,';'),'excludeForecast',exclude_forecast)
  name<-switch(mode,additive='Additive',multiplicative='Multiplicative',log_additive='LogAdditive',pseudo_additive='PseudoAdditive')
  b<-rJava::.jcall(b,paste0('L',builder,';'),'mode',rJava::.jfield('jdplus/sa/base/api/DecompositionMode','Ljdplus/sa/base/api/DecompositionMode;',name))
  spec<-rJava::.jcall(b,'Ljdplus/x13/base/api/x11/X11Spec;','buildWithoutValidation')
  spec<-rJava::.jcall(spec,'Ljdplus/x13/base/api/x11/X11Spec;','validate')
  spec<-rJava::.jcall(spec,'Ljdplus/x13/base/api/x11/X11Spec;','checkAnnualFrequency',as.integer(stats::frequency(y)))
  context<-rJava::.jcall('jdplus/x13/base/core/x11/X11Context','Ljdplus/x13/base/core/x11/X11Context;','of',spec,rjd3toolkit::.r2jd_tsdata(y))
  cls<-if(mode=='pseudo_additive')'jdplus/x13/base/core/x11/pseudoadd/MsrFilterSelectionPseudoAdd'else'jdplus/x13/base/core/x11/filter/MsrFilterSelection'
  object<-rJava::.jnew(cls)
  seq_java<-rJava::.jcall('jdplus/toolkit/base/api/data/DoubleSeq','Ljdplus/toolkit/base/api/data/DoubleSeq;','of',rJava::.jarray(as.numeric(y)))
  choice<-rJava::.jcall(object,paste0('L',option,';'),'doMSR',seq_java,context)
  value<-rJava::.jcall(object,'Ljdplus/x13/base/api/x11/MsrTable;','getMsrTable')
  list(iterations=rJava::.jcall(object,'I','getIterCount'),choice=rJava::.jcall(choice,'S','name'),
    using_default=rJava::.jcall(object,'Z','isUsingDefault'),global=rJava::.jcall(object,'D','getGlobalMsr'),
    table=if(rJava::is.jnull(value))NULL else list(seasonal=rJava::.jcall(value,'[D','getMeanSeasonalEvolutions'),
      irregular=rJava::.jcall(value,'[D','getMeanIrregularEvolutions'),global=rJava::.jcall(value,'D','getGlobalMsr')))
}
reference_x11_msr_pilot <- function(y,mode) {
  s<-y;f<-as.integer(stats::frequency(y))
  for(p in seq_len(f)) {
    at<-which(stats::cycle(y)==p)
    # The public odd-length trend factory supplies the seven-term uniform
    # kernel; 7 here is a kernel length, not a supported annual frequency.
    s[at]<-reference_x11_mean(7L,as.numeric(y)[at])$values
  }
  list(seasonal=s,irregular=if(mode=='multiplicative')y/s else y-s)
}
