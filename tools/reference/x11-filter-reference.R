# Public factory/field/application APIs only; no implementation or bytecode.
reference_x11_filter <- function(case) {
  interface <- 'Ljdplus/toolkit/base/core/math/linearfilters/SymmetricFilter;'
  factory <- 'jdplus/x13/base/core/x11/filter/X11TrendCycleFilterFactory'
  if(case$type=='trend')return(rJava::.jcall(factory,interface,'makeTrendFilter',case$frequency))
  if(case$type=='henderson')return(rJava::.jcall(factory,interface,'makeHendersonFilter',case$length))
  rJava::.jfield('jdplus/x13/base/core/x11/filter/X11SeasonalFiltersFactory',interface,toupper(case$type))
}
reference_x11_apply <- function(filter,values,spacing) {
  lower <- rJava::.jcall(filter,'I','getLowerBound');upper <- rJava::.jcall(filter,'I','getUpperBound')
  output <- rep(NA_real_,length(values));data <- 'jdplus/toolkit/base/core/data/DataBlock'
  for(phase in seq_len(min(spacing,length(values)))) {
    rows <- seq.int(phase,length(values),spacing);n <- length(rows)-upper+lower
    if(n<=0L)next
    input <- rJava::.jcall(data,paste0('L',data,';'),'of',as.numeric(values[rows]))
    target <- rJava::.jcall(data,paste0('L',data,';'),'make',as.integer(n))
    rJava::.jcall(filter,'V','apply',input,target)
    output[rows[seq.int(1L-lower,length(rows)-upper)]] <- rJava::.jcall(target,'[D','toArray')
  }
  output
}
