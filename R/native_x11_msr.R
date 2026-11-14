# Components must already be on the requested working scale. No transformation,
# trimming, pilot estimation, filter selection or B/C/D pass occurs here.
native_x11_msr_table <- function(seasonal,irregular,mode="additive") {
  domain<-native_regular_domain(seasonal)
  if(!identical(domain,native_regular_domain(irregular)))stop("MSR components must have identical time domains.",call.=FALSE)
  result<-.native_x11_msr_table(as.numeric(seasonal),as.numeric(irregular),domain$frequency,domain$start[2L],mode)
  result$input_domain<-domain;result$mode<-mode;result$schema_version<-1L;result$backend<-"native"
  result$scope<-"msr_change_table_only"
  result$correction_convention<-"x11_decimal_change_count"
  result$global_weight<-"adjacent_annual_changes"
  structure(result,class="persephone_native_x11_msr_table")
}
