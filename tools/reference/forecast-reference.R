# Public signatures/accessors only. The native prediction runtime receives no
# Java objects, reference forecasts or public covariance targets.
reference_forecast_executor <- function(engine) {
  package <- if(engine=='x13')'rjd3x13'else'rjd3tramoseats'
  class <- if(engine=='x13')'jdplus/x13/base/r/RegArima'else'jdplus/tramoseats/base/r/Tramo'
  convert <- getFromNamespace(if(engine=='x13')'.r2jd_spec_regarima'else'.r2jd_spec_tramo',package)
  result <- getFromNamespace('.regarima_rslts',package)
  force(class);force(convert);force(result)
  function(prepared) {
    model <- rJava::.jcall(class,'Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel;','process',
      rjd3toolkit::.r2jd_tsdata(prepared$series),convert(prepared$spec),rjd3toolkit::.r2jd_modellingcontext(prepared$context))
    observed <- which(!is.na(prepared$series))
    lengths <- c(backcasts=prepared$backcasts+min(observed)-1L,
      forecasts=prepared$forecasts+length(prepared$series)-max(observed))
    outputs <- lapply(names(lengths),function(direction) {
      value <- rJava::.jcall(model,'Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel$Forecasts;',direction,as.integer(lengths[[direction]]))
      fields <- c(points='getRawForecasts',errors='getRawForecastsStdev',original='getForecasts',original_errors='getForecastsStdev')
      lapply(fields,function(field)rjd3toolkit::.jd2r_tsdata(rJava::.jcall(value,'Ljdplus/toolkit/base/api/timeseries/TsData;',field)))
    });names(outputs) <- c('backcast','forecast')
    residuals <- rJava::.jcall(model,'Ljdplus/toolkit/base/api/timeseries/TsResiduals;','getResiduals')
    list(result=result(model),outputs=outputs,n=rJava::.jcall(residuals,'I','getN'),
      df=rJava::.jcall(residuals,'I','getDfc'),Q=rJava::.jcall(residuals,'D','getSsq'))
  }
}
