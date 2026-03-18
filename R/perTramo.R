#' tramoseats class
#'
#' An R6 class that represents a tramoseats model.
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' obj <- perTramo(AirPassengers, "rsa3")
#' obj$run()
#' obj$output
#' @export
tramoseatsSingle <- R6::R6Class(
  "tramoseatsSingle",
  inherit = persephoneSingle,
  ## read-only access to params, ts, and output
  active = list(
    #' @field method SA method used
    method = function() {
      private$.method
    }),
  # ---- Private Methods ----
  private = list(
    # updateFun = function(...) {
    #   rjd3tramoseats::tramoseats_spec(...)
    # },
    .method = "tramoseats",
    updateFun = function(name = NULL, freq, init_spec = NULL, method = NULL, spec_list = NULL, ...) {
      # browser()  # Debugger startet hier
      # initialize basic spec
      # soll init_spec jedes Mal neu ausgeführt werden? Oder wollen wir alte spec-Stände modifizieren können?
      # Wenn init_spec NULL ist, nimm das gespeicherte Spec
      if (is.null(init_spec)) {
        init_spec <- private$params_internal
      }
      # Falls das auch NULL ist (z. B. beim ersten Mal), erzeuge neues Spec
      if (is.null(init_spec)) {
        init_spec <- rjd3tramoseats::tramoseats_spec(name = name)
      }
      if (is.null(method)) {
        method <- "tramoseats"
      }
      if(is.null(spec_list)) {
        # extract additional parameters from ...
        spec_list <- list(...) # extra arguments
      }
      if(length(spec_list) > 0) {
        spec_new <- update_spec(spec = init_spec, method = method, freq = freq, spec_list = spec_list)
      } else {
        spec_new <- init_spec
      }
      # neue specs speichern
      private$params_internal <- spec_new
      return(spec_new)
    },
    runFun = function(...) {
      rjd3tramoseats::tramoseats_fast(...)
    }
  )
)
#' tramoseats class
#'
#' An R6 class that represents a tramoseats model.
#'
#' @section Inherits:
#' The returned object is an instance of the [`tramoseatsSingle`] class, which inherits from
#' [`persephoneSingle`] and ultimately from [`persephone`].
#' @param ts a time series.
#' @param template the name of the predefined specification, passed as the `name` argument to [rjd3tramoseats::tramoseats_spec()].
#' Must be one of:
#' `"rsafull"`, `"rsa0"`, `"rsa1"`, `"rsa2"`, `"rsa3"`, `"rsa4"`, `"rsa5"`.
#' Defaults to `"rsa3"`.
#' @param context a list of external regressors (calendar or other) to be used for estimation,
#' passed as the `context` argument of [rjd3tramoseats::tramoseats_fast()].
#' @param userdefined a character vector of user-defined variables to be included in the output,
#' passed as the `userdefined` argument to [rjd3tramoseats::tramoseats_fast()].
#' @param speclist a list of additional arguments for customizing the model specifications,
#' passed to the rjd3 specification functions internally. As alternative input to `...`.
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
#' - `x11`          → [`rjd3x13::set_x11`] (x13 only)
#' - `benchmarking` → [`rjd3toolkit::set_benchmarking`]
#'
#' The package offers convenient options for setting certain parameters to improve usability.
#' Documentation pending.
#'
#' @details
#' The function initializes an tramoseats model with the given time series and
#' template. Internally, it creates an [`tramoseatsSingle`] object, stores the
#' specification, and prepares the model for execution. To perform the
#' seasonal adjustment, call the `$run()` method on the returned object.
#'
#' @return An object of class [`tramoseatsSingle`] representing an tramoseats seasonal
#' adjustment model.
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' # create an tramoseatsSingle object with the specifications settings of the rjd3 "rsa3" template
#' obj <- perTramo(AirPassengers, "rsa3")
#' obj$run()
#'
#' # access the ts object stored in the tramoseatsSingle object
#' obj$ts
#'
#' # inspect the rjd3 output
#' obj$output
#'
#' # set some additional parameters when initializing the object
#' # by using the persephone3 parameters for setting outliers
#' obj <- perTramo(AirPassengers, "rsa3",
#'               transform.fun = "Log", transform.adjust = NA,
#'               easter.enabled = TRUE,
#'               outliers = c("AO1953.7", "LS1954.6"),
#'               automdl.enabled = FALSE,
#'               arima.p = 1, arima.d = 2, arima.q = 0,
#'               arima.bp = 1, arima.bd = 1, arima.bq = 0)
#' obj$run()
#' obj$output
#'
#' # set some additional parameters when initializing the object
#' # by using the rjd3 parameters for setting outliers
#' obj2 <- perTramo(AirPassengers, "rsa3",
#'                transform.fun = "Log", transform.adjust = NA,
#'                easter.enabled = TRUE,
#'                addout.type = c("AO","LS"),
#'                addout.date = c("1953-07-01","1954-06-01"),
#'                automdl.enabled = FALSE,
#'                arima.p = 1, arima.d = 2, arima.q = 0,
#'                arima.bp = 1, arima.bd = 1, arima.bq = 0)
#' obj2$run()
#' obj2$output
#'
#' @seealso [rjd3tramoseats::tramoseats_spec()], [rjd3tramoseats::tramoseats_fast()]
#' @references
#' For more details on tramoseats specifications and the underlying implementation,
#' see the rjd3tramoseats documentation:
#' \url{https://rjdverse.github.io/rjd3tramoseats/}
#' @export
perTramo <- function(ts,
                   template = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"),
                   context = NULL,
                   userdefined = NULL,
                   speclist = NULL,
                   ...) {
  tramoseatsSingle$new(ts, match.arg(template), context, userdefined, speclist, ...)
}
