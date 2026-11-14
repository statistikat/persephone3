# Only public model/estimation APIs. The native uncertainty calculation never
# receives these Java objects or target estimates/errors.
reference_interpolation_executor <- function(engine) {
  package <- if(engine=="x13")"rjd3x13"else"rjd3tramoseats"
  class <- if(engine=="x13")"jdplus/x13/base/r/RegArima"else"jdplus/tramoseats/base/r/Tramo"
  convert <- getFromNamespace(if(engine=="x13")".r2jd_spec_regarima"else".r2jd_spec_tramo",package)
  result <- getFromNamespace(".regarima_rslts",package)
  force(class);force(convert);force(result)
  function(prepared) {
    model <- rJava::.jcall(class,"Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel;","process",
      rjd3toolkit::.r2jd_tsdata(prepared$series),convert(prepared$spec),rjd3toolkit::.r2jd_modellingcontext(prepared$context))
    estimation <- rJava::.jcall(model,"Ljdplus/toolkit/base/core/modelling/GeneralLinearModel$Estimation;","getEstimation")
    missing <- rJava::.jcall(estimation,"[Ljdplus/toolkit/base/api/timeseries/regression/MissingValueEstimation;","getMissing")
    residuals <- rJava::.jcall(model,"Ljdplus/toolkit/base/api/timeseries/TsResiduals;","getResiduals")
    list(result=result(model),rows=vapply(missing,function(m)rJava::.jcall(m,"I","getPosition")+1L,integer(1)),
      values=vapply(missing,function(m)rJava::.jcall(m,"D","getValue"),numeric(1)),
      standard_errors=vapply(missing,function(m)rJava::.jcall(m,"D","getStandardError"),numeric(1)),
      n=rJava::.jcall(residuals,"I","getN"),df=rJava::.jcall(residuals,"I","getDfc"),Q=rJava::.jcall(residuals,"D","getSsq"))
  }
}
