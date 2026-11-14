# Public X11Spec/X11Context construction and getters; no numerical source.
reference_x11_seasonal_plan <- function(values,filters,mode) {
  builder<-'jdplus/x13/base/api/x11/X11Spec$Builder';option<-'jdplus/x13/base/api/x11/SeasonalFilterOption'
  b<-rJava::.jcall('jdplus/x13/base/api/x11/X11Spec',paste0('L',builder,';'),'builder')
  choices<-lapply(filters,function(type)rJava::.jfield(option,paste0('L',option,';'),
    switch(type,stable='Stable',x11default='X11Default',msr='Msr',toupper(type))))
  b<-rJava::.jcall(b,paste0('L',builder,';'),'filters',rJava::.jarray(choices,contents.class=option))
  for(field in c('backcastHorizon','forecastHorizon'))b<-rJava::.jcall(b,paste0('L',builder,';'),field,0L)
  mode<-switch(mode,additive='Additive',multiplicative='Multiplicative',log_additive='LogAdditive',pseudo_additive='PseudoAdditive')
  b<-rJava::.jcall(b,paste0('L',builder,';'),'mode',rJava::.jfield('jdplus/sa/base/api/DecompositionMode','Ljdplus/sa/base/api/DecompositionMode;',mode))
  spec<-rJava::.jcall(b,'Ljdplus/x13/base/api/x11/X11Spec;','buildWithoutValidation')
  spec<-rJava::.jcall(spec,'Ljdplus/x13/base/api/x11/X11Spec;','validate')
  spec<-rJava::.jcall(spec,'Ljdplus/x13/base/api/x11/X11Spec;','checkAnnualFrequency',as.integer(stats::frequency(values)))
  context<-rJava::.jcall('jdplus/x13/base/core/x11/X11Context','Ljdplus/x13/base/core/x11/X11Context;',
    'of',spec,rjd3toolkit::.r2jd_tsdata(values))
  names<-function(field)vapply(rJava::.jcall(context,paste0('[L',option,';'),field),
    function(x)tolower(rJava::.jcall(x,'S','name')),character(1))
  list(initial_filters=names('getInitialSeasonalFilter'),final_filters=names('getFinalSeasonalFilter'),
    global_msr=rJava::.jcall(context,'Z','isMSR'))
}
