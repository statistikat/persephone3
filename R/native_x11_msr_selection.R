# Ratios and series are already on the requested MSR working scale.
native_x11_msr_policy <- function(ratio) {
  result<-.native_x11_msr_policy(ratio)
  result$schema_version<-1L;result$backend<-"native";result$scope<-"msr_threshold_policy_only"
  structure(result,class="persephone_native_x11_msr_policy")
}
native_x11_msr_pilot_result <- function(result,domain,mode) {
  if(result$status=="available")for(field in c("seasonal","irregular"))
    result[[field]]<-stats::ts(result[[field]],start=domain$start,frequency=domain$frequency)
  result$input_domain<-domain;result$mode<-mode
  result$schema_version<-1L;result$backend<-"native";result$scope<-"msr_pilot_only"
  structure(result,class="persephone_native_x11_msr_pilot")
}
native_x11_msr_pilot <- function(series,mode="additive") {
  domain<-native_regular_domain(series)
  native_x11_msr_pilot_result(.native_x11_msr_pilot(as.numeric(series),domain$frequency,mode),domain,mode)
}
native_x11_msr_select <- function(series,mode="additive",iteration_policy="jdplus",
                                  backcasts=0L,forecasts=0L,max_iterations=1000L) {
  domain<-native_regular_domain(series);f<-domain$frequency
  result<-.native_x11_msr_select(as.numeric(series),f,domain$start[2L],mode,iteration_policy,backcasts,forecasts,max_iterations)
  ordinal<-domain$start_ordinal+result$backcasts
  selection_domain<-.native_time_domain(floor(ordinal/f),ordinal%%f+1,f,result$final_count)
  selection_domain$start_ordinal<-ordinal
  bounds<-function(n)if(n>0)c(first=result$backcasts+1L,last=result$backcasts+n)else integer()
  result$estimation_rows<-bounds(result$estimation_count);result$calendar_rows<-bounds(result$calendar_count)
  result$selection_rows<-bounds(result$final_count)
  result$input_domain<-domain;result$selection_domain<-selection_domain;result$mode<-mode
  if(!is.null(result$pilot))result$pilot<-native_x11_msr_pilot_result(result$pilot,selection_domain,mode)
  if(!is.null(result$table))result$table<-native_x11_msr_table_result(result$table,selection_domain,mode)
  result$global_msr<-if(is.null(result$table))NA_real_ else result$table$global_msr
  result$schema_version<-1L;result$backend<-"native";result$scope<-"msr_selection_only"
  structure(result,class="persephone_native_x11_msr_selection")
}
# Resolve only a canonical pending D-final plan. Failure/budget results retain
# missing resolved filters and an explicit pending status; processing them fails.
native_x11_msr_resolve <- function(plan,selection) {
  if(!inherits(plan,"persephone_native_x11_seasonal_plan") || !isTRUE(plan$requires_msr) ||
      !identical(plan,native_x11_seasonal_plan(plan$frequency,plan$requested_filters,plan$stage)))
    stop("Provide a canonical pending D-final MSR plan.",call.=FALSE)
  valid<-inherits(selection,"persephone_native_x11_msr_selection") && identical(selection$schema_version,1L) &&
    identical(selection$backend,"native") && identical(selection$scope,"msr_selection_only") &&
    identical(selection$frequency,plan$frequency) && length(selection$status)==1L &&
    isTRUE(selection$status %in% c("available","missing_observations","nonpositive_relative_input",
      "undefined_msr","numerical_range","incomplete_table","budget_exhausted")) &&
    length(selection$iteration_policy)==1L && isTRUE(selection$iteration_policy %in% c("jdplus","x11_five_year_cap")) &&
    is.logical(selection$using_default) && length(selection$using_default)==1L && !is.na(selection$using_default) &&
    is.character(selection$selected_filter) && length(selection$selected_filter)==1L
  if(!valid || (selection$status=="available" && !selection$selected_filter %in% c("s3x3","s3x5","s3x9")) ||
      (selection$status!="available" && !is.na(selection$selected_filter)) ||
      (selection$using_default && (selection$status!="available" || selection$selected_filter!="s3x5")))
    stop("Provide a valid native MSR selection with matching frequency.",call.=FALSE)
  result<-plan;result$msr_selection<-selection;result$msr_status<-selection$status
  if(selection$status=="available") {
    result$resolved_filters[]<-selection$selected_filter
    result$resolution_reasons[]<-if(selection$using_default)"msr_default"else"msr_selected"
    result$requires_msr<-FALSE;result$status<-"available"
  }
  result$scope<-"seasonal_stage_policy_with_msr_selection"
  result
}
