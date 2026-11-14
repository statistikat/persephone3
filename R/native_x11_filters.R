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

native_x11_seasonal_spec <- function(type,frequency,available,side="right") {
  result <- .native_x11_seasonal_spec(type,frequency,available,side)
  names(result$weights) <- as.character(result$offsets)
  structure(result,class="persephone_native_x11_filter_spec")
}

native_x11_filter <- function(series,type,length=NULL,endpoints="none",ic_ratio=NULL) {
  if(!is.character(endpoints) || length(endpoints)!=1L || is.na(endpoints) ||
      !is.null(dim(endpoints)) || !endpoints %in% c("none","musgrave","mean","seasonal"))
    stop("endpoints must be 'none', 'musgrave', 'mean' or 'seasonal'.",call.=FALSE)
  if(endpoints!="musgrave" && !is.null(ic_ratio))
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
  } else if(endpoints=="mean") {
    result <- .native_x11_filter_spec(type,domain$frequency,length)
    if(result$type!="trend")stop("Mean endpoints currently require the centered trend filter.",call.=FALSE)
    applied <- .native_x11_mean_filter(as.numeric(series),domain$frequency)
    result$values <- applied$values;result$missing <- applied$missing
    result$endpoint_policy <- "mean";result$padding_means <- applied$padding_means
    result$scope <- "centered_trend_with_endpoints"
    r <- result$radius;n <- as.integer(domain$length)
    rows <- seq_len(n);left <- pmax(0L,r+1L-rows);right <- pmax(0L,rows+r-n)
    boundary <- left>0L | right>0L
    result$endpoint_rows <- data.frame(row=rows[boundary],
      left_padding=left[boundary],right_padding=right[boundary])
  } else if(endpoints=="seasonal") {
    result <- .native_x11_filter_spec(type,domain$frequency,length)
    applied <- .native_x11_seasonal_filter(as.numeric(series),type,domain$frequency)
    result$values <- applied$values;result$missing <- applied$missing
    result$endpoint_policy <- "seasonal_x11";result$scope <- "seasonal_filter_with_endpoints"
    result$minimum_phase_length <- max(5L,result$terms)
    r <- result$radius;n <- as.integer(domain$length);f <- as.integer(domain$frequency)
    rows <- c(seq_len(r),seq.int(n-r+1L,n))
    periods <- as.integer(stats::cycle(series))
    result$phase_lengths <- stats::setNames(tabulate(periods,nbins=f),as.character(seq_len(f)))
    result$endpoint_rows <- data.frame(row=rows,period=periods[rows],
      side=rep(c("left","right"),each=r),
      available=c((seq_len(r)-1L)%/%f,rev((seq_len(r)-1L)%/%f)))
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
