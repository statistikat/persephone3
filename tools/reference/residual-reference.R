# Public-model accessors only; no Java numerical implementation is inspected or
# reproduced here. Used solely by explicit differential/benchmark tooling.
reference_ts <- function(value) {
 start <- rJava::.jcall(value,'Ljdplus/toolkit/base/api/timeseries/TsPeriod;','getStart')
 data <- rJava::.jcall(value,'Ljdplus/toolkit/base/api/data/DoubleSeq;','getValues')
 list(values=rJava::.jcall(data,'[D','toArray'),
   start=c(rJava::.jcall(start,'I','year'),rJava::.jcall(start,'I','annualPosition')+1L),
   frequency=rJava::.jcall(start,'I','annualFrequency'))
}
reference_residual_executor <- function(engine) {
 package <- if(engine=='x13')'rjd3x13'else'rjd3tramoseats'
 class <- if(engine=='x13')'jdplus/x13/base/r/RegArima'else'jdplus/tramoseats/base/r/Tramo'
 convert <- getFromNamespace(if(engine=='x13')'.r2jd_spec_regarima'else'.r2jd_spec_tramo',package)
 result <- getFromNamespace('.regarima_rslts',package)
 force(engine);force(class);force(convert);force(result)
 function(prepared) {
   model <- rJava::.jcall(class,'Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel;','process',
     rjd3toolkit::.r2jd_tsdata(prepared$series),convert(prepared$spec),rjd3toolkit::.r2jd_modellingcontext(prepared$context))
   residuals <- rJava::.jcall(model,'Ljdplus/toolkit/base/api/timeseries/TsResiduals;','getResiduals')
   list(result=result(model),
     residuals=reference_ts(rJava::.jcall(model,'Ljdplus/toolkit/base/api/timeseries/TsData;','fullResiduals')),
     interpolated=reference_ts(rJava::.jcall(model,'Ljdplus/toolkit/base/api/timeseries/TsData;','interpolatedSeries',FALSE)),
     transformed=reference_ts(rJava::.jcall(model,'Ljdplus/toolkit/base/api/timeseries/TsData;','interpolatedSeries',TRUE)),
     missing=as.numeric(rJava::.jcall(model,'[D','missingEstimates')),
     n=rJava::.jcall(residuals,'I','getN'),df=rJava::.jcall(residuals,'I','getDf'),
     dfc=rJava::.jcall(residuals,'I','getDfc'),ssq=rJava::.jcall(residuals,'D','getSsq'))
 }
}
