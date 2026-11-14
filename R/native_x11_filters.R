# Explicit symmetric filter primitives, not an X-11 decomposition or selector.
native_x11_filter_spec <- function(type,frequency,length=NULL) {
  result <- .native_x11_filter_spec(type,frequency,length)
  names(result$weights) <- as.character(result$offsets)
  structure(result,class="persephone_native_x11_filter_spec")
}

native_x11_filter <- function(series,type,length=NULL) {
  domain <- native_regular_domain(series)
  result <- .native_x11_filter(as.numeric(series),type,domain$frequency,length)
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
