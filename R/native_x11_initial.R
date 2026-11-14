# The first fixed additive B tables, before extreme-value correction.
native_x11_initial_additive <- function(series,filters,mode="additive") {
  if(!identical(mode,"additive"))stop("Initial X-11 tables currently support additive mode only.",call.=FALSE)
  domain<-native_regular_domain(series);f<-domain$frequency;periods<-as.character(seq_len(f))
  if(!is.null(names(filters))) {
    if(length(filters)!=f || anyNA(names(filters)) || anyDuplicated(names(filters)) || !setequal(names(filters),periods))
      stop("Named filters must identify every calendar period exactly once.",call.=FALSE)
    filters<-filters[match(periods,names(filters))]
  }
  result<-.native_x11_initial_additive(as.numeric(series),f,domain$start[2L],filters)
  ordinal<-domain$start_ordinal+result$drop
  crop<-.native_time_domain(floor(ordinal/f),ordinal%%f+1,f,length(series)-2L*result$drop)
  crop$start_ordinal<-ordinal
  result$tables$B1<-stats::ts(result$tables$B1,start=domain$start,frequency=f)
  for(name in c("B2","B3","B4d"))result$tables[[name]]<-stats::ts(result$tables[[name]],start=crop$start,frequency=f)
  result$seasonal<-native_x11_seasonal_result(result$seasonal,result$tables$B3,crop,"x11")
  result$normalization<-native_x11_normalization_result(result$normalization,result$seasonal$values,crop,mode)
  result$tables$B4a<-result$seasonal$values;result$tables$B4anorm<-result$normalization$values
  result$tables<-result$tables[c("B1","B2","B3","B4a","B4anorm","B4d")]
  cropped_rows<-result$drop+seq_len(crop$length)
  result$row_basis<-c(list(B1=seq_along(series)),setNames(rep(list(cropped_rows),5L),names(result$tables)[-1L]))
  result$available_rows<-lapply(names(result$tables),function(name)result$row_basis[[name]][!is.na(result$tables[[name]])])
  names(result$available_rows)<-names(result$tables)
  result$complete<-all(vapply(result$tables,function(x)!anyNA(x),logical(1)))
  result$status<-if(length(result$available_rows$B4d))"available"else"no_available_outputs"
  result$input_domain<-domain;result$cropped_domain<-crop;result$mode<-mode
  result$schema_version<-1L;result$backend<-"native";result$scope<-"additive_initial_tables_only"
  result$complete_b_pass<-FALSE
  structure(result,class="persephone_native_x11_initial")
}
