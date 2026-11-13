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
#'   object, see [plotResiduals]
#' - `getOutliers()`: Extract outlier information from the model run, see
#'   [getOutliers()]
#' - `getRegressors()`: Extract the coefficients of all regression variables
#'   of the model run, see [getRegressors()]
#'
#' @section Read-Only fields:
#'
#' - `params`: A parameters object of class `SA_spec`. See [rjd3x13::x13_spec] and
#'   [rjd3tramoseats::tramoseats_spec].
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
#'
#' @section Forecasts:
#' After a successful fit, `$forecasts$points$y` contains original-series
#' forecasts; other named components are `t`, `sa`, `s`, `i` and `cal`.
#' The direct model's same contract is available through `$forecastsDirect`.
#' Each field retains its own time domain: a backend may supply fewer calendar
#' forecasts or standard errors than original-series forecasts. The contract's
#' `availability` table records these partial horizons and unavailable fields;
#' standard errors are never recycled to fill a longer horizon.
#'
#' Hierarchies aggregate level forecasts using their usual sum or weighted mean.
#' Selected indirect forecasts honor nested children's direct/indirect choices,
#' whereas `$forecastsIndirect` uses fully indirect child results. For multiple
#' components, multiplicative seasonal, irregular and calendar factors cannot
#' be summed; these aggregate
#' fields remain unavailable unless all decompositions are additive. Uncertainty
#' for multiple components requires cross-component covariance and is therefore
#' unavailable. Before fitting or with no available horizon, the result is NULL.
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
    #' @details A non-NULL template replaces the specification. A complete spec
    #'   also replaces it; the two cannot be supplied together. Named changes
    #'   are incremental. Previous output is retained and is_stale records changed
    #'   settings until run() succeeds. Invalid updates leave state unchanged.
    #' @param template template name passed to `x13_spec()` or `tramoseats_spec()`
    #' @param speclist a list of additional arguments for customizing the model specifications
    #' @param spec a specification object to use as starting point
    #' @param context a list of external regressors (calendar or other) to be used for estimation
    #' @param userdefined character vector of user-defined output variables
    #' @param iterate logical, whether to iterate (for hierarchical objects)
    #' @param component character, component id for hierarchical objects
    #' @param ... additional arguments passed to `x13_spec()` or `tramoseats_spec()`
    updateParams = function(
      template = NULL,
      speclist = NULL,
      spec = NULL,
      context = NULL,
      userdefined = NULL,
      iterate = FALSE,
      component = "",
      ...
    ) {
      changes <- specification_changes(speclist, list(...))
      validate_update_context(context, userdefined)
      proposed <- prepare_specification_update(
        private$params_internal, self$method, frequency(private$ts_internal),
        self$backend, template, spec, changes)
      # Validation/setters above must succeed before any state is changed.
      private$params_internal <- proposed
      if (!is.null(template)) private$template_internal <- template
      if (!is.null(spec)) private$template_internal <- NULL
      if (!missing(context)) private$context_internal <- context
      if (!missing(userdefined)) private$userdefined <- union(userdefined, userdefined_default)
      invisible(proposed)
    },
    #' @description visualize the results of an adjustment
    #' @param ...  passed to [plot()]
    #'   see plot method for persephoneSingle or hierarchicalTimeSeries objects
    plot = function(...) {
      plot(self, ...)
    },
    #' @description visualize the results of an adjustment
    #' @param ...  passed to [hchart()]
    #'   see the hchart method for persephoneSingle or hierarchicalTimeSeries objects
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
    #' @description display a persephone object in the console
    #' @description create a table for the eurostat quality report
    diagnosticsTable = function() {
      self$iterate(generate_diagnostics_table, asTable = TRUE)
    },
    #' @description see [perHts]
    #' @param fun see [perHts]
    #' @param asTable see [perHts]
    #' @param unnest see [perHts]
    iterate = function(fun, asTable = FALSE, unnest = FALSE) {
      res <- list(value = fun(self))
      private$convert_list(res, asTable, unnest)
    }, # TO DO: iterate anderen Namen geben, verwechselt man sonst mit Parameter iterate
    #' @description create a table for the eurostat quality report
    generateQrTable = function() {
      self$iterate(generate_Qr_List, asTable = TRUE)
    },
    #' @description fix the arima model
    #' @param ... additional arguments passed to \code{\link{fixModel}}
    fixModel = function(...) {
      fixModel(self, ...)
    },
    #' @description create a new single object
    #' @param timespan number of months from the end of the time series
    #' where outliers are not fixed
    #' @param ... additional arguments passed to \code{\link{fixOutlier}}
    fixOutlier = function(...) {
      fixOutlier(self, ...)
    },
    #' @description extract outlier information from the model run,
    #'   see [getOutliers()]
    #' @return A typed data frame as described in [getOutliers()], with zero rows
    #'   for a fitted model without outliers. An unrun model cannot be extracted.
    getOutliers = function() {
      getOutliers(self)
    },
    #' @description extract the coefficients of all regression variables of
    #'   the model run, see [getRegressors()]
    #' @return A data frame as described in [getRegressors()], `NULL` if the
    #'   model contains no regression variables
    getRegressors = function() {
      getRegressors(self)
    }
  ),
  # ---- Active Bindings ----
  ## read-only access to params, ts, and output
  active = list(
    #' @field backend Effective computation backend, fixed at construction.
    backend = function() private$backend_internal %||% "rjd3",
    #' @field specification Versioned specification envelope; raw params remain available.
    specification = function() {
      new_backend_spec(self$method, private$template_internal,
                       frequency(private$ts_internal), private$params_internal,
                       self$backend)
    },
    #' @field result Versioned result from the last successful run, or NULL.
    result = function() private$result_internal,
    #' @field is_stale Whether the settings have changed since the last successful fit.
    is_stale = function() {
      !is.null(private$output_internal) &&
        !identical(private$fit_signature, private$current_signature())
    },
    #' @field params A parameters object of class `SA_spec`. See [rjd3x13::x13_spec()]
    #'   and [rjd3tramoseats::tramoseats_spec()].
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
    #' @field context passed to [rjd3x13::x13_fast()] and [rjd3tramoseats::tramoseats_fast()] when the
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
    # # ' @field spec specifications passed to [rjd3x13::x13_fast()] and [rjd3tramoseats::tramoseats_fast()] when the
    # # '   `$run()` method is invoked
    # spec = function() {
    #   private$params_internal
    # },
    #' @field forecasts A serializable forecast contract, or NULL before fitting
    #'   or when no forecast horizon is available. `points` contains named ts
    #'   series y, t, sa, s, i and cal. `domains` and `standard_error_domains`
    #'   retain each field's own horizon; standard errors may cover only a
    #'   prefix. `scales` and `availability` describe units and missing outputs.
    forecasts = function() {
      self$forecastsDirect
    },
    #' @field forecastsDirect The direct model's forecast contract. Original-series
    #'   standard errors are retained when supplied by the computation backend.
    forecastsDirect = function() {
      forecasts_from_result(self$result, self$output, self$ts)
    }
  ),
  # ---- Private Methods ----
  private = list(
    convert_list = function(res, asTable = FALSE, unnest = FALSE) {
      if (asTable) {
        return(asTable_nested_list(res))
      } else if (unnest) {
        return(unnest_nested_list(res))
      } else {
        return(res)
      }
    },
    backend_internal = NULL,
    template_internal = NULL,
    result_internal = NULL,
    fit_signature = NULL,
    current_signature = function() {
      list(ts = private$ts_internal, params = private$params_internal,
           context = self$context, userdefined = private$userdefined)
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
# -> Jede abgeleitete Klasse muss diese Methode überschreiben,
# um die Spezifikation des jeweiligen Modells zu erzeugen.

# userdefined_default <- c(
#   "y", "t", "sa", "s", "i", "cal", "y_f", "t_f", "sa_f", "s_f", "i_f",
#   "cal_f", "preprocessing.model.y_f", "preprocessing.model.y_ef",
#   "decomposition.d6", "decomposition.d7", "decomposition.d9",
#   "mode"
# )
#userdefined_default_neu nur helper beim Umschreiben.
userdefined_default <- c(
  "y",
  "t",
  "sa",
  "s",
  "i",
  "cal",
  "y_f",
  "t_f",
  "sa_f",
  "s_f",
  "i_f",
  "cal_f",
  "y_ef",
  "decomposition.d6",
  "decomposition.d7",
  "decomposition.d9",
  "decomposition.d13",
  "decomposition.s_cmp",
  "decomposition.i_cmp",
  "mode",
  "decomposition.mode",
  "arima.p",
  "arima.d",
  "arima.q",
  "arima.bp",
  "arima.bd",
  "arima.bq",
  "regression.easter",
  "regression.lp",
  "regression.td(*)",
  "regression.leaster",
  "regression.ntd",
  "regression.outlier(*)",
  "regression.nout",
  "diagnostics.seas-si-combined",
  "diagnostics.seas-si-combined3",
  "residuals.tsres",
  "residuals.ser", # ab hier zusätzliche Ausgabe
  "regression.td-ftest",
  "residuals.doornikhansen",
  "residuals.lb",
  "regression.td-derived",
  "diagnostics.seas-sa-f",
  "diagnostics.seas-sa-qs",
  "diagnostics.td-sa-all",
  "diagnostics.td-sa-last",
  "diagnostics.td-i-all",
  "diagnostics.td-i-last",
  "diagnostics.seas-res-periodogram",
  "diagnostics.seas-sa-periodogram",
  "diagnostics.seas-i-periodogram",
  "diagnostics.seas-res-spectralpeaks",
  "diagnostics.seas-sa-spectralpeaks",
  "diagnostics.seas-i-spectralpeaks",
  "m-statistics.q",
  "m-statistics.q-m2",
  "quality.summary"
)
