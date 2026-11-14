# Public bulk application for complete-series comparisons and timings. Separate
# from the frozen per-endpoint capture; no upstream implementation was read.
reference_musgrave_process <- function(frequency,length,ratio,values) {
  full <- reference_x11_filter(list(type='henderson',frequency=frequency,length=length))
  filters <- rJava::.jcall('jdplus/x13/base/core/x11/filter/MusgraveFilterFactory',
    '[Ljdplus/toolkit/base/core/math/linearfilters/FiniteFilter;','makeFilters',
    rJava::.jcast(full,'jdplus/toolkit/base/core/math/linearfilters/IFiniteFilter'),as.double(ratio),evalArray=FALSE)
  processor <- rJava::.jnew('jdplus/x13/base/core/x11/filter/endpoints/AsymmetricEndPoints',
    rJava::.jcast(filters,'[Ljdplus/toolkit/base/core/math/linearfilters/IFiniteFilter;'),0L)
  data <- 'jdplus/toolkit/base/core/data/DataBlock'
  input <- rJava::.jcall(data,paste0('L',data,';'),'of',as.numeric(values))
  output <- rJava::.jcall(data,paste0('L',data,';'),'make',as.integer(length(values)))
  h <- rJava::.jcall(full,'I','getUpperBound')
  central <- rJava::.jcall(output,paste0('L',data,';'),'range',h,as.integer(length(values))-h)
  rJava::.jcall(full,'V','apply',input,central)
  rJava::.jcall(processor,'V','process',rJava::.jcast(input,'jdplus/toolkit/base/api/data/DoubleSeq'),output)
  list(values=rJava::.jcall(output,'[D','toArray'),weights=rJava::.jcall(full,'[D','weightsToArray'))
}
