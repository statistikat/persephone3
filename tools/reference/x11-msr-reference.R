# Execute public MsrTable API only. No upstream numerical implementation read.
reference_x11_msr_table <- function(seasonal,irregular,mode) {
  seq_java<-function(y)rJava::.jcall('jdplus/toolkit/base/api/data/DoubleSeq','Ljdplus/toolkit/base/api/data/DoubleSeq;',
    'of',rJava::.jarray(as.numeric(y)))
  f<-as.integer(stats::frequency(seasonal));first<-as.integer(stats::cycle(seasonal)[1L]-1L)
  value<-rJava::.jcall('jdplus/x13/base/api/x11/MsrTable','Ljdplus/x13/base/api/x11/MsrTable;','of',
    seq_java(seasonal),seq_java(irregular),f,first,mode=='multiplicative')
  list(seasonal=rJava::.jcall(value,'[D','getMeanSeasonalEvolutions'),irregular=rJava::.jcall(value,'[D','getMeanIrregularEvolutions'),
    msr=vapply(0:(f-1L),function(p)rJava::.jcall(value,'D','getMsr',p),numeric(1)),global=rJava::.jcall(value,'D','getGlobalMsr'))
}

# A separate public-selector audit fixes mode semantics; no implementation
# source is used. Its returned iteration count is evidence, not native policy.
reference_x11_msr_mode <- function(y,mode) {
  builder<-'jdplus/x13/base/api/x11/X11Spec$Builder';option<-'jdplus/x13/base/api/x11/SeasonalFilterOption'
  b<-rJava::.jcall('jdplus/x13/base/api/x11/X11Spec',paste0('L',builder,';'),'builder')
  b<-rJava::.jcall(b,paste0('L',builder,';'),'filters',rJava::.jarray(list(rJava::.jfield(option,paste0('L',option,';'),'Msr')),contents.class=option))
  for(field in c('backcastHorizon','forecastHorizon'))b<-rJava::.jcall(b,paste0('L',builder,';'),field,0L)
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
  if(rJava::is.jnull(value))stop('Mode audit requires a returned public MSR table')
  list(iterations=rJava::.jcall(object,'I','getIterCount'),choice=rJava::.jcall(choice,'S','name'),using_default=rJava::.jcall(object,'Z','isUsingDefault'),
    seasonal=rJava::.jcall(value,'[D','getMeanSeasonalEvolutions'),irregular=rJava::.jcall(value,'[D','getMeanIrregularEvolutions'),
    global=rJava::.jcall(value,'D','getGlobalMsr'))
}
