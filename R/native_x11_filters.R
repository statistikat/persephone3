# Explicit filter primitives, not an X-11 decomposition or selector.
native_x11_filter_spec <- function(type,frequency,length=NULL) {
  result <- .native_x11_filter_spec(type,frequency,length)
  names(result$weights) <- as.character(result$offsets)
  structure(result,class="persephone_native_x11_filter_spec")
}

native_x11_musgrave_spec <- function(frequency,length,available,ic_ratio,side="right") {
  result <- .native_x11_musgrave_spec(frequency,length,available,ic_ratio,side)
  names(result$weights) <- as.character(result$offsets)
  structure(result,class="persephone_native_x11_filter_spec")
}

native_x11_filter <- function(series,type,length=NULL,endpoints="none",ic_ratio=NULL) {
  if(!is.character(endpoints) || length(endpoints)!=1L || is.na(endpoints) ||
      !is.null(dim(endpoints)) || !endpoints %in% c("none","musgrave"))
    stop("endpoints must be 'none' or 'musgrave'.",call.=FALSE)
  if(endpoints=="none" && !is.null(ic_ratio))
    stop("ic_ratio requires Musgrave endpoints.",call.=FALSE)
  domain <- native_regular_domain(series)
  if(endpoints=="musgrave") {
    result <- .native_x11_filter_spec(type,domain$frequency,length)
    if(result$type!="henderson")stop("Musgrave endpoints currently require Henderson filters.",call.=FALSE)
    applied <- .native_x11_musgrave_filter(as.numeric(series),domain$frequency,length,ic_ratio)
    result$values <- applied$values;result$missing <- applied$missing
    result$endpoint_policy <- "musgrave";result$ic_ratio <- as.numeric(ic_ratio)
    result$scope <- "henderson_filter_with_endpoints"
    r <- result$radius;n <- domain$length
    result$endpoint_rows <- data.frame(row=c(seq_len(r),if(r)seq.int(n-r+1L,n)else integer()),
      side=rep(c("left","right"),each=r),available=c(seq_len(r)-1L,rev(seq_len(r)-1L)))
  } else result <- .native_x11_filter(as.numeric(series),type,domain$frequency,length)
  names(result$weights) <- as.character(result$offsets)
  result$values <- stats::ts(result$values,start=domain$start,frequency=domain$frequency)
  result$input_domain <- domain
  n <- domain$length;r <- result$radius
  result$central_rows <- if(n>2L*r)seq.int(r+1L,n-r)else integer()
  result$boundary_rows <- setdiff(seq_len(n),result$central_rows)
  result$available_rows <- which(!result$missing)
  result$central_domain <- if(length(result$central_rows))backend_domain(stats::ts(
    result$values[result$central_rows],start=stats::time(result$values)[r+1L],frequency=domain$frequency))else NULL
  result$status <- if(length(result$available_rows))"available"else"no_available_outputs"
  structure(result,class="persephone_native_x11_filtered")
}
