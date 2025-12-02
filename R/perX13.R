#' x13 class
#'
#' An R6 class that represents a x13 model.
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' obj$output
#' @export
x13Single <- R6::R6Class(
  "x13Single",
  inherit = persephoneSingle,
  # ---- Private Methods ----
  private = list(
    # updateFun = function(...) {
    #   rjd3x13::x13_spec(...)
    # },
    updateFun = function(name = NULL, freq, init_spec = NULL, ...) {
      # browser()  # Debugger startet hier
      # initialize basic spec
      # soll init_spec jedes Mal neu ausgeführt werden? Oder wollen wir alte spec-Stände modifizieren können?
      # Wenn init_spec NULL ist, nimm das gespeicherte Spec
      if (is.null(init_spec)) {
        init_spec <- private$params_internal
      }
      # Falls das auch NULL ist (z. B. beim ersten Mal), erzeuge neues Spec
      if (is.null(init_spec)) {
        init_spec <- rjd3x13::x13_spec(name = name)
      }
      spec_new <- init_spec

      # extract additional parameters from ...
      spec_list <- list(...) # extra arguments
      if(length(spec_list) > 0) {
        # change persephone specific parameter names to rjd3 names
        # persephone specific parameters aber evt irgendwo aufheben?
        if("outliers" %in% names(spec_list)) {
          if(!is.null(outliers)) {
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
          # spec-blocks and their corresponding functions
          specfun_list <- list(arima = rjd3toolkit::set_arima, automdl = rjd3toolkit::set_automodel,
                               basic = rjd3toolkit::set_basic, easter = rjd3toolkit::set_easter,
                               estimate = rjd3toolkit::set_estimate, setout = rjd3toolkit::set_outlier,
                               td = rjd3toolkit::set_tradingdays, transform = rjd3toolkit::set_transform,
                               addout = rjd3toolkit::add_outlier, remout = rjd3toolkit::remove_outlier,
                               addramp = rjd3toolkit::add_ramp, remramp = rjd3toolkit::remove_ramp,
                               usrdefvar = rjd3toolkit::add_usrdefvar,
                               x11 = rjd3x13::set_x11, benchmarking = rjd3toolkit::set_benchmarking)

          spec_blocks <- unique(sapply(names(spec_list), function(s) unlist(strsplit(s, ".", fixed = TRUE))[1] ))

          # i <- 1
          for (i in seq_along(spec_blocks)) {
            # paste0("^",spec_blocks[i]))# prüfen ob Zeichenkette mit spec_blocks[i] beginnt:
            spec_sublist <-  spec_list[grepl(paste0("^",spec_blocks[i]), names(spec_list))]
            names(spec_sublist) <- gsub(paste0(spec_blocks[i],"."), "", names(spec_sublist), fixed = TRUE)

            spec_new <- do.call(specfun_list[[spec_blocks[i]]], c(list(x = spec_new), spec_sublist))

          }
        } else {
          # noch implementieren! Wird wahrsch. mit private params und remove outliers funktionieren müssen.
        }
        # setout.type -> outliers.type = c("AO", "LS", "TC")
        # das will ich nicht jedes Mal alles hinschreiben müssen,
        # da wollen wir wieder sowas wie outliers.enabled = TRUE/FALSE
        # wobei die rjd3toolkit Parameter auch funktionieren sollen, da
        # dokumentiert, unsere nur ergänzend
      }
      # neue specs speichern
      private$params_internal <- spec_new
      return(spec_new)
    },
    runFun = function(...) {
      rjd3x13::x13_fast(...)
    }
  )
)
#' x13 class
#'
#' An R6 class that represents a x13 model.
#'
#' @section Inherits:
#' The returned object is an instance of the [`x13Single`] class, which inherits from
#' [`persephoneSingle`] and ultimately from [`persephone`].
#' @param ts a time series.
#' @param template the name of the predefined specification, passed as the `name` argument to [x13_spec()].
#' Must be one of:
#' `"rsa0"`, `"rsa1"`, `"rsa2c"`, `"rsa3"`, `"rsa4"`, `"rsa5c"`.
#' Defaults to `"rsa3"`.
#' @param context a list of external regressors (calendar or other) to be used for estimation,
#' passed as the `context` argument of [x13_fast()].
#' @param userdefined a character vector of user-defined variables to include in the output,
#' passed as the `userdefined` argument to [x13_fast()].
#' @param ... additional arguments for customizing the model specifications,
#' passed to the rjd3 specification functions internally.
#'
#' These arguments allow you to set or update the specifications of the
#' seasonal adjustment method by modifying one or more parameters.
#' Parameters are organized into **spec-blocks**:
#' - The first part of the parameter name (before the dot) indicates the
#'   block.
#' - The second part (after the dot) specifies the parameter within that
#'   block.
#'
#' Supported spec-blocks and their corresponding functions:
#' - `arima`        → [`rjd3toolkit::set_arima`]
#' - `automdl`      → [`rjd3toolkit::set_automodel`]
#' - `basic`        → [`rjd3toolkit::set_basic`]
#' - `easter`       → [`rjd3toolkit::set_easter`]
#' - `estimate`     → [`rjd3toolkit::set_estimate`]
#' - `setout`       → [`rjd3toolkit::set_outlier`]
#' - `td`           → [`rjd3toolkit::set_tradingdays`]
#' - `transform`    → [`rjd3toolkit::set_transform`]
#' - `addout`       → [`rjd3toolkit::add_outlier`]
#' - `remout`       → [`rjd3toolkit::remove_outlier`]
#' - `addramp`      → [`rjd3toolkit::add_ramp`]
#' - `remramp`      → [`rjd3toolkit::remove_ramp`]
#' - `usrdefvar`    → [`rjd3toolkit::add_usrdefvar`]
#' - `x11`          → [`rjd3x13::set_x11`]
#' - `benchmarking` → [`rjd3toolkit::set_benchmarking`]
#'
#' @details
#' The function initializes an x13 model with the given time series and
#' template. Internally, it creates an [`x13Single`] object, stores the
#' specification, and prepares the model for execution. To perform the
#' seasonal adjustment, call the `$run()` method on the returned object.
#'
#' @return An object of class [`x13Single`] representing an x13 seasonal
#' adjustment model.
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' # create an x13Single object with the specifications settings of the rjd3 "rsa3" template
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#'
#' # access the ts object stored in the x13Single object
#' obj$ts
#'
#' # inspect the rjd3 output
#' obj$output
#'
#' # set some additional parameters when initializing the object
#' obj <- perX13(AirPassengers, "rsa3", transform.fun = "Log", transform.adjust = NA,
#'               easter.enabled = TRUE, outliers = c("AO1953.7", "AO1954.6"))
#' obj$run()
#'
#' @seealso [rjd3x13::x13_spec()], [rjd3x13::x13_fast()]
#' @references
#' For more details on x13 specifications and the underlying implementation,
#' see the rjd3x13 documentation:
#' \url{https://rjdverse.github.io/rjd3x13/}
#' @export
perX13 <- function(ts, template = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"),
                   context = NULL, userdefined = NULL, ...) {
  x13Single$new(ts, match.arg(template), context, userdefined, ...)
}
