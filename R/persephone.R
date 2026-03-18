#' persephone class
#'
#' @description Common interface for x13 and tramoseats
#'
#' @section Constructor:
#'
#' - `new()`: Initializes an object. See the documentation for the derived
#'   classes for details.
#'
#' @section Methods:
#'
#' - `run(verbose = FALSE)`: Runs the model
#' - `updateParams(...)`: Updates the current parameters
#' - `plot(...)`: Interactive time series plot for a persephone object, see
#'   [plot.persephoneSingle] or [plot.hierarchicalTimeSeries]
#' - `plotSeasIrrCal(...)`: Interactive plot of the seasonal component,
#'   irregular component and calendar
#' effects for a persephone object, see [plotSeasIrrCal]
#' - `plotResiduals(...)`: Several interactive plots in connection with
#'   residuals for a persephone
#' object, see [plotResiduals]
#'
#' @section Read-Only fields:
#'
#' - `params`: A parameters object of class `SA_spec`. See [x13_spec] and
#'   [tramoseats_spec].
#' - `ts`: The time series.
#' - `output`: The return value from the underlying functions `x13_fast` or
#'   `tramoseats_fast`. This slot will
#'   be empty (`NULL`) before `run()` is called for the first time.
#'
#' @section Parameters:
#' - `verbose`: should the output from the underlying rjd3x13/rjd3tramoseats
#'   function be displayed after the
#'   run? If `FALSE` (the default), the output will be returned invisibly.
#' @section derived classes:
#' [perX13()], [perTramo()], [perHts()]
#' @import rjd3x13
#' @import rjd3tramoseats
#' @export
persephone <- R6::R6Class(
  "persephone",
  # ---- Public Methods ----
  public = list(
    #' @description create a new persephone object
    #' @details this function should not be used directly because this class is
    #'   abstract. Only initialize derived classes.
    initialize = function() {
      stop("implement this function")
    },
    #' @description perform a run of the model
    #' @param verbose whether to show outputs
    run = function(verbose = FALSE) {
      stop("implement this function")
    },
    #' @description update parameters for the adjustment
    #' @param template optional template name used as the starting specification
    #' @param speclist optional named list of specification updates
    #' @param spec optional explicit specification object to update
    #' @param context optional external regressor context used at run time
    #' @param userdefined optional user-defined outputs to request
    #' @param iterate logical flag used by subclasses that support recursive updates
    #' @param component optional component identifier used by subclasses
    #' @param ... passed to `x13_spec()` or `tramoseats_spec()`
    updateParams = function(template = NULL,
                            speclist = NULL,
                            spec = NULL,
                            context = NULL,
                            userdefined = NULL,
                            iterate = FALSE,
                            component = "",
                            ...) {
#browser()
      if (!missing(context)) {
        private$context_internal <- context
      }
      if (!missing(userdefined)) {
        private$userdefined <- union(userdefined, userdefined_default)
      }
      init_spec <- if (!missing(spec) && !is.null(spec)) spec else private$params_internal

      private$params_internal <- private$updateFun(name = template,
                                                   freq = frequency(private$ts_internal),
                                                   init_spec = init_spec,
                                                   spec_list = speclist,
                                                   ...)
    },
    #' @description visualize the results of an adjustment
    #' @param ...  passed to [plot()]
    #'   see [plot.persephoneSingle()] or [plot.hierarchicalTimeSeries()]
    plot = function(...) {
      plot(self, ...)
    },
    #' @description visualize the results of an adjustment
    #' @param ... passed to [highcharter::hchart()]
    hchart = function(...) {
      hchart(self, ...)
    },
    #' @description visualize residuals
    #' @param ... passed to [plotResiduals()]
    plotResiduals = function(...) {
      plotResiduals(self, ...)
    },
    #' @description  visualize the seasonal component, irregular
    #'   component and calendar effects for a persephone object
    #' @param ... passed to [plotSeasIrrCal()]
    plotSeasIrrCal = function(...) {
      plotSeasIrrCal(self, ...)
    },
    #' @description  visualize SI-ratios (and, in case of x11, of seasonal factors) by
    #' period
    #' @param ... passed to [plotSiRatios()]
    plotSiRatios = function(...) {
      plotSiRatios(self, ...)
    },
    #' @description visualize the periodogram or the autogregressive spectrum.
    #' @param ... passed to [plotSpectrum()]
    plotSpectrum = function(...) {
      plotSpectrum(self, ...)
    },
    #' @description display a persephone object in the console
    print = function() {
      message("A persephone object")
      if (!is.null(self$output)) {
        message("Output:")
        tbl <- private$printTable("")
       # tbl <- tbl[, -(1:3)]
        print(tbl, right = FALSE, row.names = FALSE)
      } else {
        message("Not yet run.")
      }
    },
    #' @description see [perHts]
    #' @param fun see [perHts]
    #' @param asTable see [perHts]
    #' @param unnest see [perHts]
    iterate = function(fun, asTable = FALSE, unnest = FALSE) {
      res <- list(value = fun(self))
      private$convert_list(res, asTable, unnest)
    },# TO DO: iterate anderen Namen geben, verwechselt man sonst mit Parameter iterate
    #' @description create a table for the eurostat quality report
    generateQrTable = function() {
      self$iterate(generate_Qr_List, asTable = TRUE)
    },
    #' @description fix the arima model
    #' @param ... passed to the internal `fixModel()` helper
    fixModel = function(...) {
      fixModel(self, ...)
    },
    #' @description create a new single object
    #' @param ... passed to the internal `fixOutlier()` helper
    fixOutlier = function(...) {
      fixOutlier(self, ...)
    }
  ),
  # ---- Active Bindings ----
  ## read-only access to params, ts, and output
  active = list(
    #' @field params A parameters object of class `SA_spec`. See [x13_spec()]
    #'   and [tramoseats_spec()].
    params = function() {
      private$params_internal
    },
    #' @field method SA method used
    method = function() {
      private$method
    },
    #' @field ts the (unajusted) time series
    ts = function() {
      private$ts_internal
    },
    #' @field tsp the [tsp()] of the underlying time series
    tsp = function() {
      private$tsp_internal
    },
    #' @field output The return value from the underlying functions x13_fast() or
    #'   tramoseats_fast(). This slot will be empty (NULL) before run() is called for
    #'   the first time.
    output = function() {
      private$output_internal
    },
    #' @field context passed to [x13_fast()] and [tramoseats_fast()] when the
    #'   `$run()` method is invoked
    context = function() {
      private$context_internal
    },
    #' @field adjusted get the adjusted series
    adjusted = function() {
      self$adjustedDirect
    },
    #' @field adjustedDirect see [perHts]
    adjustedDirect = function() {
      self$output$user_defined$sa
    },
    #' @field forecasts get forecasts from the model
    forecasts = function(){
      self$output$final$forecasts
    },
    #' @field forecastsDirect get direct forecasts from the model
    forecastsDirect = function(){
      self$output$final$forecasts
    }
  ),
  # ---- Private Methods ----
  private = list(
    convert_list = function(res, asTable = FALSE, unnest = FALSE) {
      if (asTable)
        return(asTable_nested_list(res))
      else if (unnest)
        return(unnest_nested_list(res))
      else
        return(res)
    },
    ts_internal = NULL,
    tsp_internal = NULL,
    params_internal = NULL,
    output_internal = NULL,
    context_internal = NULL,
    userdefined = NULL,
    # spec_internal = NULL,
    printTable = function(prefix) {
      # cbind(
      #   data.frame(
      #     component = sub("/", "", prefix),
      #     class = class(self)[1],
      #     run = !is.null(self$output)
      #   ),
        printDiagnostics(self)
      # )
    },
    updateFun = function(params, ...) {
      # placeholder
      #print(class(params))
      #print(str(params))
      stop("implement this method")
    }
  )
)
# In der Basisklasse persephone (oder persephoneSingle) ist updateFun
# als abstrakte Methode vorgesehen.
# -> Each derived class must override this method
# to create the specification for its model.

# userdefined_default <- c(
#   "y", "t", "sa", "s", "i", "cal", "y_f", "t_f", "sa_f", "s_f", "i_f",
#   "cal_f", "preprocessing.model.y_f", "preprocessing.model.y_ef",
#   "decomposition.d6", "decomposition.d7", "decomposition.d9",
#   "mode"
# )
#userdefined_default_neu nur helper beim Umschreiben.
userdefined_default <- c(
  "y", "t", "sa", "s", "i", "cal", "y_f", "t_f", "sa_f", "s_f","i_f",
  "cal_f", "y_ef",
  "decomposition.d6", "decomposition.d7", "decomposition.d9","decomposition.d13",
  "decomposition.s_cmp","decomposition.i_cmp","mode",
  "decomposition.mode", "arima.p","arima.d", "arima.q" ,"arima.bp" , "arima.bd","arima.bq",
  "regression.easter","regression.lp","regression.td(*)","regression.leaster","regression.ntd",
  "regression.outlier(*)","regression.nout",
  "diagnostics.seas-si-combined","diagnostics.seas-si-combined3",
  "residuals.tsres","residuals.ser"
)
