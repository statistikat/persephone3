# Additive seasonal normalization and explicit periodic domain restoration.
# Supply fixed requested filters, before the seasonal processor's length fallback.
native_x11_seasonal_normalize <- function(series,filters,extend=0L,mode="additive") {
  if(!identical(mode,"additive"))stop("Seasonal normalization currently supports additive mode only.",call.=FALSE)
  domain<-native_regular_domain(series);f<-domain$frequency;periods<-as.character(seq_len(f))
  if(!is.null(names(filters))) {
    if(length(filters)!=f || anyNA(names(filters)) || anyDuplicated(names(filters)) || !setequal(names(filters),periods))
      stop("Named filters must identify every calendar period exactly once.",call.=FALSE)
    filters<-filters[match(periods,names(filters))]
  }
  result<-.native_x11_seasonal_normalize(as.numeric(series),f,domain$start[2L],filters,extend)
  ordinal<-domain$start_ordinal-result$extend
  result$output_domain<-.native_time_domain(floor(ordinal/f),ordinal%%f+1,f,length(result$values))
  result$output_domain$start_ordinal<-ordinal
  result$normalization<-stats::ts(result$normalization,start=domain$start,frequency=f)
  result$values<-stats::ts(result$values,start=result$output_domain$start,frequency=f)
  for(field in c("requested_filters","endpoint_policies","endpoint_reasons"))names(result[[field]])<-periods
  result$original_rows<-result$extend+seq_along(series)
  result$missing<-is.na(result$values);result$available_rows<-which(!result$missing)
  result$status<-if(length(result$available_rows))"available"else"no_available_outputs"
  result$schema_version<-1L;result$backend<-"native";result$scope<-"additive_seasonal_normalization_only"
  result$input_domain<-domain;result$mode<-mode;result$extension_policy<-"nearest_annual_cycle"
  structure(result,class="persephone_native_x11_normalization")
}
