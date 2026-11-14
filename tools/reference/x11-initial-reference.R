# Public validated contexts, B-step getters and matching public primitive chain.
# No upstream numerical implementation is inspected or translated.
reference_x11_initial_context <- function(y,mode='additive',filters='s3x5') {
 builder<-'jdplus/x13/base/api/x11/X11Spec$Builder';b<-rJava::.jcall('jdplus/x13/base/api/x11/X11Spec',paste0('L',builder,';'),'builder')
 for(field in c('backcastHorizon','forecastHorizon'))b<-rJava::.jcall(b,paste0('L',builder,';'),field,0L)
 option<-'jdplus/x13/base/api/x11/SeasonalFilterOption';choices<-lapply(filters,function(x)rJava::.jfield(option,paste0('L',option,';'),if(x=='stable')'Stable'else toupper(x)))
 b<-rJava::.jcall(b,paste0('L',builder,';'),'filters',rJava::.jarray(choices,contents.class=option))
 name<-switch(mode,additive='Additive',multiplicative='Multiplicative',log_additive='LogAdditive',pseudo_additive='PseudoAdditive')
 b<-rJava::.jcall(b,paste0('L',builder,';'),'mode',rJava::.jfield('jdplus/sa/base/api/DecompositionMode','Ljdplus/sa/base/api/DecompositionMode;',name))
 spec<-rJava::.jcall(b,'Ljdplus/x13/base/api/x11/X11Spec;','buildWithoutValidation');spec<-rJava::.jcall(spec,'Ljdplus/x13/base/api/x11/X11Spec;','validate')
 spec<-rJava::.jcall(spec,'Ljdplus/x13/base/api/x11/X11Spec;','checkAnnualFrequency',as.integer(frequency(y)))
 rJava::.jcall('jdplus/x13/base/core/x11/X11Context','Ljdplus/x13/base/core/x11/X11Context;','of',spec,rjd3toolkit::.r2jd_tsdata(y))
}
reference_x11_initial_bstep <- function(y,filters) {
 ctx<-reference_x11_initial_context(y,'additive',filters)
 x<-rJava::.jcall('jdplus/toolkit/base/api/data/DoubleSeq','Ljdplus/toolkit/base/api/data/DoubleSeq;','of',rJava::.jarray(as.numeric(y)))
 step<-rJava::.jnew('jdplus/x13/base/core/x11/X11BStep');rJava::.jcall(step,'V','process',x,ctx)
 out<-lapply(c('B1','B2','B3','B4a','B4anorm','B4d'),function(name) {
  obj<-rJava::.jcall(step,'Ljdplus/toolkit/base/api/data/DoubleSeq;',paste0('get',name));rJava::.jcall(obj,'[D','toArray')
 });names(out)<-c('B1','B2','B3','B4a','B4anorm','B4d');out$drop<-rJava::.jcall(step,'I','getB2drop');out
}
reference_x11_initial_primitives <- function(y,filters) {
 f<-as.integer(frequency(y));h<-f%/%2L;n<-length(y)-f
 crop<-ts(as.numeric(y)[h+seq_len(n)],start=as.numeric(time(y)[h+1L]),frequency=f)
 ctx<-reference_x11_initial_context(crop,'additive',filters)
 seq_java<-function(z)rJava::.jcall('jdplus/toolkit/base/api/data/DoubleSeq','Ljdplus/toolkit/base/api/data/DoubleSeq;','of',rJava::.jarray(as.numeric(z)))
 block<-'jdplus/toolkit/base/core/data/DataBlock';input<-rJava::.jcall(block,paste0('L',block,';'),'of',as.numeric(y))
 b2<-rJava::.jcall(block,paste0('L',block,';'),'make',n)
 kernel<-rJava::.jcall('jdplus/x13/base/core/x11/filter/X11TrendCycleFilterFactory','Ljdplus/toolkit/base/core/math/linearfilters/SymmetricFilter;','makeTrendFilter',f)
 rJava::.jcall(kernel,'V','apply',input,b2)
 b3<-rJava::.jcall(ctx,'Ljdplus/toolkit/base/api/data/DoubleSeq;','remove',seq_java(crop),rJava::.jcast(b2,'jdplus/toolkit/base/api/data/DoubleSeq'))
 opts<-rJava::.jcall(ctx,'[Ljdplus/x13/base/api/x11/SeasonalFilterOption;','getInitialSeasonalFilter')
 proc<-rJava::.jcall('jdplus/x13/base/core/x11/filter/X11SeasonalFiltersFactory','Ljdplus/x13/base/core/x11/filter/X11SeasonalFilterProcessor;',
   'filter',f,rJava::.jarray(opts,contents.class='jdplus/x13/base/api/x11/SeasonalFilterOption'))
 b4a<-rJava::.jcall(proc,'Ljdplus/toolkit/base/api/data/DoubleSeq;','process',b3,as.integer(cycle(crop)[1]-1L))
 norm<-rJava::.jcall('jdplus/x13/base/core/x11/filter/DefaultSeasonalNormalizer','Ljdplus/toolkit/base/api/data/DoubleSeq;','normalize',b4a,0L,ctx)
 rem<-rJava::.jcall(ctx,'Ljdplus/toolkit/base/api/data/DoubleSeq;','remove',b3,norm)
 out<-lapply(list(B2=b2,B3=b3,B4a=b4a,B4anorm=norm,B4d=rem),function(z)rJava::.jcall(z,'[D','toArray'))
 c(list(B1=as.numeric(y)),out,list(drop=h))
}
