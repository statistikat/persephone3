#' Class for single time series
#'
#' A common class for the wrapper classes [x13Single] and [tramoseatsSingle]
#'
#' @export
persephoneSingle <- R6::R6Class(
  "persephoneSingle",
  inherit = persephone,
  # ---- Public Methods ----
  public = list(
    #' @description create a new single object
    #' @param ts a time series object
    #' @param template passed as the `spec` argument of [x13_fast()] or
    #'   [tramoseats_fast()]
    #' @param context passed as the `context` argument of [x13_fast()] or
    #'   [tramoseats_fast()], a list of external regressors (calendar or other) to be used for estimation
    #' @param userdefined passed as the `userdefined` argument of [x13_fast()] or
    #'   [tramoseats_fast()]
    #' @param speclist a list of additional arguments for customizing the model specifications,
    #' passed to the rjd3 specification functions internally. As alternative input to `...`.
    #' @param ... passed to [x13_spec()] or [tramoseats_spec()] if no speclist parameter is set.
    initialize = function(ts, template = "rsa3", context = NULL, userdefined = NULL, speclist = NULL, ...) {
      private$ts_internal <- ts
      private$tsp_internal <- tsp(ts)
      private$context_internal <- context
      private$userdefined <- union(userdefined, userdefined_default)
      # wollen wir speclist falls vorhanden doch in slot abspeichern?
      private$params_internal <- private$updateFun(name = template,
                                                   freq = frequency(private$ts_internal),
                                                   init_spec = private$params_internal,
                                                   spec_list = speclist,
                                                   ...)
    },
    #' @description run the model
    #' @param verbose wether to display outputs after the run
    run = function(verbose = FALSE) {
      output <- private$runFun(ts = private$ts_internal,
                               spec = private$params_internal,
                               context = private$context_internal,
                               userdefined = private$userdefined)
      private$output_internal <- output
      if (!verbose)
        invisible(output)
      else
        output
    }
  )
)
