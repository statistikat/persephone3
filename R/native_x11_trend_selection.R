# Scalar threshold/default conventions, distinct from pilot-ratio estimation.
# ic_ratio uses the comparison scale (three times the raw quarterly I/C).
native_x11_trend_policy <- function(frequency,ic_ratio=NULL,length=NULL) {
  result<-.native_x11_trend_policy(frequency,ic_ratio,length)
  result$schema_version<-1L;result$backend<-"native";result$scope<-"henderson_policy_only"
  structure(result,class="persephone_native_x11_trend_policy")
}

# Input is a seasonally adjusted working series. log_additive expects its
# already-logged values; this function performs no transformation or X-11 pass.
native_x11_trend_select <- function(series,mode="additive",backcasts=0L,forecasts=0L) {
  domain<-native_regular_domain(series)
  result<-.native_x11_trend_select(as.numeric(series),domain$frequency,mode,backcasts,forecasts)
  n<-as.integer(domain$length);h<-result$pilot_length%/%2L
  bounds<-function(first,last)if(first<=last)c(first=as.integer(first),last=as.integer(last))else integer()
  result$estimation_rows<-bounds(as.double(result$backcasts)+1,n-result$forecasts)
  result$pilot_rows<-bounds(as.double(result$backcasts)+h+1,n-result$forecasts-h)
  result$change_rows<-bounds(as.double(result$backcasts)+h+2,n-result$forecasts-h)
  result$input_domain<-domain;result$mode<-mode
  result$schema_version<-1L;result$backend<-"native";result$scope<-"henderson_selection_only"
  structure(result,class="persephone_native_x11_trend_selection")
}
