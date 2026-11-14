# Fixed per-period choices and the explicit X-11 short-series policy. This
# processor does not compute data-driven MSR selection or decomposition passes.
native_x11_seasonal_process <- function(series,filters,short_series="x11") {
  domain<-native_regular_domain(series);f<-as.integer(domain$frequency)
  periods<-as.character(seq_len(f))
  if(!is.null(names(filters))) {
    if(length(filters)!=f || anyNA(names(filters)) || anyDuplicated(names(filters)) ||
        !setequal(names(filters),periods))stop("Named filters must identify every calendar period exactly once.",call.=FALSE)
    filters<-filters[match(periods,names(filters))]
  }
  result<-.native_x11_seasonal_process(as.numeric(series),f,domain$start[2L],filters,short_series)
  native_x11_seasonal_result(result,series,domain,short_series)
}
native_x11_seasonal_result <- function(result,series,domain,short_series) {
  f<-as.integer(domain$frequency);periods<-as.character(seq_len(f))
  result$values<-stats::ts(result$values,start=domain$start,frequency=f)
  for(field in c("requested_filters","selected_filters","phase_lengths","selection_reasons","coefficient_conventions"))
    names(result[[field]])<-periods
  result$row_methods<-data.frame(row=seq_along(series),period=as.integer(stats::cycle(series)),
    method=result$row_methods,available=result$available)
  result$available<-NULL
  result$available_rows<-which(!result$missing)
  result$status<-if(length(result$available_rows))"available"else"no_available_outputs"
  result$schema_version<-1L;result$backend<-"native";result$scope<-"seasonal_processing_only"
  result$short_series<-short_series;result$input_domain<-domain
  structure(result,class="persephone_native_x11_seasonal")
}
