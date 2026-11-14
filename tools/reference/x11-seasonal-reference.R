# Only public enum/factory/processor calls; no numerical implementation read.
reference_x11_seasonal <- function(frequency,type,values,start_period) {
  option <- rJava::.jfield('jdplus/x13/base/api/x11/SeasonalFilterOption',
    'Ljdplus/x13/base/api/x11/SeasonalFilterOption;',toupper(type))
  options <- rJava::.jarray(rep(list(option),frequency),contents.class='jdplus/x13/base/api/x11/SeasonalFilterOption')
  processor <- rJava::.jcall('jdplus/x13/base/core/x11/filter/X11SeasonalFiltersFactory',
    'Ljdplus/x13/base/core/x11/filter/X11SeasonalFilterProcessor;','filter',as.integer(frequency),options)
  input <- rJava::.jcall('jdplus/toolkit/base/api/data/DoubleSeq',
    'Ljdplus/toolkit/base/api/data/DoubleSeq;','of',rJava::.jarray(as.numeric(values)))
  result <- rJava::.jcall(processor,'Ljdplus/toolkit/base/api/data/DoubleSeq;','process',input,as.integer(start_period-1L))
  list(values=rJava::.jcall(result,'[D','toArray'))
}
