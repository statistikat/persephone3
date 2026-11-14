# Public validated spec/context and normalizer APIs only. The four-argument
# overload cancels the restoration offset because this context already starts
# at the supplied normalization input, rather than at the restored domain.
reference_x11_normalization <- function(y,filters,extend=0L) {
  builder<-'jdplus/x13/base/api/x11/X11Spec$Builder';option<-'jdplus/x13/base/api/x11/SeasonalFilterOption'
  b<-rJava::.jcall('jdplus/x13/base/api/x11/X11Spec',paste0('L',builder,';'),'builder')
  choices<-lapply(filters,function(x)rJava::.jfield(option,paste0('L',option,';'),if(x=='stable')'Stable'else toupper(x)))
  b<-rJava::.jcall(b,paste0('L',builder,';'),'filters',rJava::.jarray(choices,contents.class=option))
  for(field in c('backcastHorizon','forecastHorizon'))b<-rJava::.jcall(b,paste0('L',builder,';'),field,0L)
  b<-rJava::.jcall(b,paste0('L',builder,';'),'mode',rJava::.jfield('jdplus/sa/base/api/DecompositionMode','Ljdplus/sa/base/api/DecompositionMode;','Additive'))
  spec<-rJava::.jcall(b,'Ljdplus/x13/base/api/x11/X11Spec;','buildWithoutValidation')
  spec<-rJava::.jcall(spec,'Ljdplus/x13/base/api/x11/X11Spec;','validate')
  spec<-rJava::.jcall(spec,'Ljdplus/x13/base/api/x11/X11Spec;','checkAnnualFrequency',as.integer(stats::frequency(y)))
  context<-rJava::.jcall('jdplus/x13/base/core/x11/X11Context','Ljdplus/x13/base/core/x11/X11Context;','of',spec,rjd3toolkit::.r2jd_tsdata(y))
  input<-rJava::.jcall('jdplus/toolkit/base/api/data/DoubleSeq','Ljdplus/toolkit/base/api/data/DoubleSeq;','of',rJava::.jarray(as.numeric(y)))
  result<-rJava::.jcall('jdplus/x13/base/core/x11/filter/DefaultSeasonalNormalizer','Ljdplus/toolkit/base/api/data/DoubleSeq;',
    'normalize',input,as.integer(extend),context,as.integer(extend))
  rJava::.jcall(result,'[D','toArray')
}
