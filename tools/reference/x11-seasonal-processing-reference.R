# Public per-calendar-period enum/factory/processor calls only.
reference_x11_seasonal_processing <- function(frequency,filters,values,start_period) {
  options <- lapply(filters,function(type)rJava::.jfield('jdplus/x13/base/api/x11/SeasonalFilterOption',
    'Ljdplus/x13/base/api/x11/SeasonalFilterOption;',if(type=='stable')'Stable'else toupper(type)))
  processor <- rJava::.jcall('jdplus/x13/base/core/x11/filter/X11SeasonalFiltersFactory',
    'Ljdplus/x13/base/core/x11/filter/X11SeasonalFilterProcessor;','filter',as.integer(frequency),
    rJava::.jarray(options,contents.class='jdplus/x13/base/api/x11/SeasonalFilterOption'))
  input <- rJava::.jcall('jdplus/toolkit/base/api/data/DoubleSeq',
    'Ljdplus/toolkit/base/api/data/DoubleSeq;','of',rJava::.jarray(as.numeric(values)))
  result <- rJava::.jcall(processor,'Ljdplus/toolkit/base/api/data/DoubleSeq;','process',input,as.integer(start_period-1L))
  list(values=rJava::.jcall(result,'[D','toArray'))
}
