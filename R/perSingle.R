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
    #' @param td trading day regressors for the regression model. Either a
    #'   national calendar of class `JD3_CALENDAR`
    #'   ([rjd3toolkit::national_calendar()]), from which the regressors are
    #'   computed with [rjd3toolkit::calendar_td()], a precomputed `mts` object
    #'   (e.g. `genTd()[[3]][, 1:6]`), or a list as returned by
    #'   [td_context()]. The regressors are added to `context` and the trading
    #'   days spec block is set to `"UserDefined"` (see [perTd()]). `td.*`
    #'   arguments in `...` or `speclist` override the generated defaults.
    #' @param userdefined passed as the `userdefined` argument of [x13_fast()] or
    #'   [tramoseats_fast()]
    #' @param speclist a list of additional arguments for customizing the model specifications,
    #' passed to the rjd3 specification functions internally. As alternative input to `...`.
    #' @param ... passed to [x13_spec()] or [tramoseats_spec()] if no speclist parameter is set.
    initialize = function(ts, template = "rsa3", context = NULL, userdefined = NULL, speclist = NULL, td = NULL, ...) {
      private$ts_internal <- ts
      private$tsp_internal <- tsp(ts)
      dots <- list(...)
      if (!is.null(td)) {
        td_obj <- resolve_td_input(ts, td, private$.method)
        # merge the trading day regressors into an existing context, if any
        if (!is.null(context)) {
          if (!is.list(context) || !all(c("calendars", "variables") %in% names(context))) {
            stop(
              "'context' must be a modelling context created with ",
              "rjd3toolkit::modelling_context()",
              call. = FALSE
            )
          }
          td_obj$context <- modifyList(
            context,
            list(variables = td_obj$context$variables)
          )
        }
        context <- td_obj$context
        # user td.* arguments (speclist and dots) override the generated ones;
        # since updateFun ignores the dots once a speclist is given, all
        # arguments are merged into one list (speclist entries winning)
        if (is.null(speclist)) {
          speclist <- modifyList(td_obj$speclist, dots)
          dots <- list()
        } else {
          speclist <- modifyList(modifyList(td_obj$speclist, dots), speclist)
          dots <- list()
        }
      }
      private$context_internal <- context
      private$userdefined <- union(userdefined, userdefined_default)
      # wollen wir speclist falls vorhanden doch in slot abspeichern?
      update_args <- list(
        name = template,
        freq = frequency(private$ts_internal),
        init_spec = private$params_internal,
        spec_list = speclist
      )
      private$params_internal <- do.call(
        private$updateFun,
        c(update_args, dots)
      )
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
