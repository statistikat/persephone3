# Public bulk mean endpoint processor plus symmetric DataBlock application.
reference_x11_mean <- function(frequency,values) {
  full <- reference_x11_filter(list(type='trend',frequency=frequency,length=NULL))
  data <- 'jdplus/toolkit/base/core/data/DataBlock'
  input <- rJava::.jcall(data,paste0('L',data,';'),'of',rJava::.jarray(as.numeric(values)))
  output <- rJava::.jcall(data,paste0('L',data,';'),'make',as.integer(length(values)))
  h <- rJava::.jcall(full,'I','getUpperBound')
  if(length(values)>2L*h) {
    central <- rJava::.jcall(output,paste0('L',data,';'),'range',h,as.integer(length(values))-h)
    rJava::.jcall(full,'V','apply',input,central)
  }
  processor <- rJava::.jnew('jdplus/x13/base/core/x11/filter/endpoints/FilteredMeanEndPoints',full)
  rJava::.jcall(processor,'V','process',rJava::.jcast(input,'jdplus/toolkit/base/api/data/DoubleSeq'),output)
  list(values=rJava::.jcall(output,'[D','toArray'),weights=rJava::.jcall(full,'[D','weightsToArray'))
}
