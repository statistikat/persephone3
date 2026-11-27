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
  ## read-only access to params, ts, and output
  active = list(
    #' @field method SA method used
    method = function() {
      private$.method
    }),
  # ---- Private Methods ----
  private = list(
    # updateFun = function(...) {
    #   rjd3x13::x13_spec(...)
    # },
    .method = "x13",
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
        init_spec <- backend_default_spec("x13", name, freq, self$backend)
      }
      if (is.null(method)) {
        method <- "x13"
      }
      init_spec <- backend_spec_parameters(init_spec, method, self$backend, freq)
      if(is.null(spec_list)) {
      # extract additional parameters from ...
      spec_list <- list(...) # extra arguments
      }
      if(length(spec_list) > 0) {
        spec_new <- backend_update_spec(spec = init_spec, method = method, freq = freq, spec_list = spec_list, backend = self$backend)
      } else {
        spec_new <- init_spec
      }
      # neue specs speichern
      if (self$backend == "native")
        backend_validate_native_spec(spec_new, method, private$ts_internal, allow_unset_filters = TRUE)
      private$params_internal <- spec_new
      return(spec_new)
    },
    runFun = function(...) {
      backend_fit(..., method = "x13", backend = self$backend)
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
#' @param template the name of the predefined specification, passed as the `name` argument to [rjd3x13::x13_spec()].
#' Must be one of:
#' `"rsa0"`, `"rsa1"`, `"rsa2c"`, `"rsa3"`, `"rsa4"`, `"rsa5c"`.
#' Defaults to `"rsa3"`.
#' @param context a list of external regressors (calendar or other) to be used for estimation,
#' passed as the `context` argument of [rjd3x13::x13_fast()].
#' @param userdefined a character vector of user-defined variables to be included in the output,
#' passed as the `userdefined` argument to [rjd3x13::x13_fast()].
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
#' # by using the persephone3 parameters for setting outliers
#' obj <- perX13(AirPassengers, "rsa3", backend = "rjd3",
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
#' obj2 <- perX13(AirPassengers, "rsa3", backend = "rjd3",
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
#' @seealso [rjd3x13::x13_spec()], [rjd3x13::x13_fast()]
#' @references
#' For more details on x13 specifications and the underlying implementation,
#' see the rjd3x13 documentation:
#' \url{https://rjdverse.github.io/rjd3x13/}
#' @param backend Backend name, or NULL to use option persephone.backend
#'   (defaulting to native when unset; rjd3 remains fully supported through
#'   explicit selection, Java optional).
#'   The native backend qualifies the restricted fixed-coefficient X-13 fit
#'   through `template = "restricted"` (rkalos gating contract
#'   `capabilities()$native_x13_fit_restricted`) and the automatic X-13 fit
#'   through `template = "rsa3"`, whose module state is a pinned decode table
#'   of the JDemetra+ rsa3 specification
#'   (`capabilities()$native_x13_fit_automatic`) -- never
#'   `capabilities()$fit`. For the native rsa3 route the horizons default to the
#'   JDemetra+ template's own: forecast one year (`-1`, twelve months) and
#'   backcast zero. Supply whole numbers through speclist or updateParams. Positive
#'   counts specify months and negative counts specify years. The forecasts field
#'   returns dated original, trend, adjusted, seasonal and calendar points.
#'   Monthly stock RSA0, RSA1 and RSA2c estimate the airline coefficients and select the X-11 filters, with twelve forecasts. Other native templates fail explicitly with
#'   the violated scope field(s) named. See the examples in the README for a
#'   restricted run.
#' @export
perX13 <- function(ts,
                   template = c( "rsa3", "rsa0", "rsa1", "rsa2c", "rsa4","rsa5c"),
                   context = NULL,
                   userdefined = NULL,
                   speclist = NULL,
                   ..., backend = NULL) {
  backend <- resolve_backend(backend)
  if (identical(backend, "native")) {
    # The native template contract owns the templates: 'restricted' and the
    # pinned automatic 'rsa3' qualify; everything else is refused naming the
    # violated scope field(s).
    if (missing(template)) template <- "rsa3"
    backend_native_check_template(template)
  } else {
    template <- match.arg(template)
  }
  x13Single$new(ts, template, context, userdefined, speclist, ..., backend = backend)
}
