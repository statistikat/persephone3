# Resolve stage requests before data/length-dependent seasonal processing.
native_x11_seasonal_plan <- function(frequency,filters,stage) {
  # Validate frequency before using it to order names. Scalar requests have
  # no named-period interpretation.
  if(!is.numeric(frequency) || length(frequency)!=1L || is.na(frequency) ||
      !is.null(dim(frequency)) || !frequency %in% c(4,12))stop("frequency must be 4 or 12.",call.=FALSE)
  periods<-as.character(seq_len(frequency))
  if(!is.null(names(filters))) {
    if(length(filters)!=frequency || anyNA(names(filters)) || anyDuplicated(names(filters)) ||
        !setequal(names(filters),periods))stop("Named filters must identify every calendar period exactly once.",call.=FALSE)
    filters<-filters[match(periods,names(filters))]
  }
  result<-.native_x11_seasonal_plan(frequency,filters,stage)
  for(field in c("requested_filters","base_filters","resolved_filters","resolution_reasons"))names(result[[field]])<-periods
  result$stage<-stage;result$schema_version<-1L;result$backend<-"native";result$scope<-"seasonal_stage_policy_only"
  structure(result,class="persephone_native_x11_seasonal_plan")
}
