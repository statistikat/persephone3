#' Update rjd3-spec object for seasonal adjustment
#'
#' Internal specification dispatching for rjd3x13/rjd3tramoseats.
#'
#' @keywords internal
#' @param spec   an rjd3-specification object.
#' @param method the seasonal adjustment method to be used, i.e. one of the characters "x13" oder "tramoseats".
#' @param freq   the frequency of the time series, i.e. one of the integers 12 (months) or 4 (quarters).
#' Used for parsing outlier dates.
#' @param backend Effective backend name.
#' @param spec_list  a list containing the additional arguments for customizing the model specifications.
#' Passed to the rjd3 specification functions internally. See [`perX13`] or [`perTramo`] for more details.
#' @return  An updated specification object (JD3_*_SPEC).
update_spec <- function(spec = NULL, method = c("x13","tramoseats"), freq, spec_list, backend = NULL) {
  # browser()  # Debugger startet hier
  method <- match.arg(method)
  backend <- resolve_backend(backend)
  if (is.null(spec_list)) spec_list <- list()
  if (!is.list(spec_list) || (length(spec_list) &&
      (is.null(names(spec_list)) || anyNA(names(spec_list)) ||
       any(!nzchar(names(spec_list))) || anyDuplicated(names(spec_list))))) {
    stop("Specification changes must be a uniquely named list.", call. = FALSE)
  }
  if (!length(spec_list)) return(spec)
  # spec_list <- list(...)
  spec_new <- spec
  # exists("specfun_list", envir = asNamespace("persephone3"))
  # change persephone specific parameter names to rjd3 names
  # persephone specific parameters aber evt irgendwo aufheben?
  if("outliers" %in% names(spec_list)) {
    if(!is.null(spec_list[["outliers"]])) {
      rjd3otl.cp <- rjd3otl <- spec_list[["outliers"]]
      spec_list[["outliers"]] <- NULL
      type <- substr(rjd3otl, 1,2)
      stopifnot(all(type %in% c("AO", "LS", "TC")))
      date <- gsub("AO|LS|TC", "", rjd3otl)
      if(freq == 12) {
        date <- sub("\\.", "-", date)
        date <- paste0(date, "-01")
        date <- as.Date(date, format = "%Y-%m-%d")
      } else if (freq == 4) {
        date <- strsplit(date, "\\.")
        year <- sapply(date, `[`, 1)
        quarter <- as.integer(sapply(date, `[`, 2))
        month <- (quarter - 1) * 3 + 1
        date <- as.Date(paste(year, sprintf("%02d", month), "01", sep = "-"))
      }
      spec_list[["addout.type"]] <- type
      spec_list[["addout.date"]] <- date
    }
  }

  # Match a complete block prefix; e.g. arima.p must not select unrelated keys.
  blocks <- sub("\\..*$", "", names(spec_list))
  for (block in unique(blocks)) {
    setter <- backend_spec_setter(block, method, backend)
    selected <- which(blocks == block)
    spec_sublist <- spec_list[selected]
    prefix <- paste0(block, ".")
    if (any(!startsWith(names(spec_sublist), prefix))) {
      stop("Specification parameters must use 'block.parameter' names: ", block,
           call. = FALSE)
    }
    names(spec_sublist) <- substring(names(spec_sublist), nchar(prefix) + 1L)
    if (any(!nzchar(names(spec_sublist)))) {
      stop("Missing parameter name in specification block: ", block, call. = FALSE)
    }
    spec_new <- do.call(setter, c(list(x = spec_new), spec_sublist))
  }
  spec_new
}
