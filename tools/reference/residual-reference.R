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

# Focused reporting controls use explicitly supplied fixed/free parameters.
prepare_legacy_residual_reference <- function(case,engine) {
 a <- case$arguments
 spec <- if(engine=='x13')rjd3x13::regarima_spec('rg0')else rjd3tramoseats::tramo_spec('tr0')
 blocks <- lapply(c('ar','seasonal_ar','ma','seasonal_ma'),function(k)if(is.null(a[[k]]))numeric()else a[[k]])
 coefficient <- unlist(blocks,use.names=FALSE);free <- is.na(coefficient)
 coefficient[free] <- .35
 signs <- rep(c(-1,-1,1,1),lengths(blocks))
 estimate_mean <- isTRUE(a$include_mean) || isTRUE(a$include_drift)
 known_mean <- if(is.null(a$mean))0 else a$mean
 spec <- rjd3toolkit::set_arima(spec,mean=if(estimate_mean)0 else if(known_mean!=0)known_mean else NULL,
   mean.type=if(estimate_mean)'Undefined'else'Fixed',
   p=length(blocks[[1L]]),d=case$d,q=length(blocks[[3L]]),bp=length(blocks[[2L]]),bd=case$D,bq=length(blocks[[4L]]),
   coef=if(length(coefficient))coefficient*signs else NULL,coef.type=if(length(coefficient))ifelse(free,'Initial','Fixed')else'Undefined')
 spec <- rjd3toolkit::set_automodel(spec,enabled=FALSE)
 spec <- rjd3toolkit::set_transform(spec,fun=if(a$transformation=='log')'Log'else'None')
 spec <- rjd3toolkit::set_estimate(spec,tol=1e-12,exact.ml=TRUE)
 context <- NULL
 if(length(a$regressors)) {
   for(name in names(a$regressors)) spec <- rjd3toolkit::add_usrdefvar(spec,name=name,regeffect='Trend',
     coef=if(name %in% names(a$coefficients))unname(a$coefficients[name])else NULL)
   context <- rjd3toolkit::modelling_context(variables=list(r=a$regressors))
 }
 list(series=a$series,spec=spec,context=context)
}
