#' Update rjd3-spec object for seasonal adjustment
#'
#' Internal specification dispatching for rjd3x13/rjd3tramoseats.
#'
#' @keywords internal
#' @param spec   an rjd3-specification object.
#' @param method the seasonal adjustment method to be used, i.e. one of the characters "x13" oder "tramoseats".
#' @param freq   the frequency of the time series, i.e. one of the integers 12 (months) or 4 (quarters).
#' Used for parsing outlier dates.
#' @param spec_list  a list containing the additional arguments for customizing the model specifications.
#' Passed to the rjd3 specification functions internally. See [`perX13`] or [`perTramo`] for more details.
#' @return  An updated specification object (JD3_*_SPEC).
update_spec <- function(spec = NULL, method = c("x13","tramoseats"), freq, spec_list) {
  # browser()  # Debugger startet hier
  method <- match.arg(method)
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

  # # spec-blocks and their corresponding functions
  # specfun_list <- list(arima = rjd3toolkit::set_arima,
  #                      automdl = rjd3toolkit::set_automodel,
  #                      basic = rjd3toolkit::set_basic,
  #                      easter = rjd3toolkit::set_easter,
  #                      estimate = rjd3toolkit::set_estimate,
  #                      setout = rjd3toolkit::set_outlier,
  #                      td = rjd3toolkit::set_tradingdays,
  #                      transform = rjd3toolkit::set_transform,
  #                      addout = rjd3toolkit::add_outlier,
  #                      remout = rjd3toolkit::remove_outlier,
  #                      addramp = rjd3toolkit::add_ramp,
  #                      remramp = rjd3toolkit::remove_ramp,
  #                      usrdefvar = rjd3toolkit::add_usrdefvar,
  #                      x11 = rjd3x13::set_x11, # only for x13
  #                      benchmarking = rjd3toolkit::set_benchmarking)
  # # moved to helpers.R

  spec_blocks <- unique(sapply(names(spec_list), function(s) unlist(strsplit(s, ".", fixed = TRUE))[1] ))

  # i <- 1
  for (i in seq_along(spec_blocks)) {
    # paste0("^",spec_blocks[i]))# prüfen ob Zeichenkette mit spec_blocks[i] beginnt:
    spec_sublist <-  spec_list[grepl(paste0("^",spec_blocks[i]), names(spec_list))]
    names(spec_sublist) <- gsub(paste0(spec_blocks[i],"."), "", names(spec_sublist), fixed = TRUE)

    spec_new <- do.call(specfun_list[[spec_blocks[i]]], c(list(x = spec_new), spec_sublist))
  }
  #}
  return(spec_new)
}

# Weitere Wünsche zur Parametersetzung:
#
# 1) setout.type -> outliers.type = c("AO", "LS", "TC")
# das will ich nicht jedes Mal alles hinschreiben müssen,
# da wollen wir wieder sowas wie outliers.enabled = TRUE/FALSE
#
# 2)Arima Model als Vektor c(0,1,1,0,1,1) oder so.
#
# Anm.: wobei die rjd3toolkit Parameter selbst schon auch alle funktionieren sollen,
# da diese dokumentiert sind -> unsere nur ergänzend
