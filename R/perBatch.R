#' R6 Class for a batch of time series
#'
#' @description Combine multiple objects of persephone objects into a new
#'   persephone object. On the resulting objects seasonal adjustment can
#'   be performed to all elements at once
#' @param ... one or more objects which are either of class persephone or
#'  can be coerced to persephone objects with asPersephone. If more than
#'  one element is supplied, the underlying time series must have the same
#'  time instances. All elements supplied in ... must be named.
#'
#'  In addition to the ts/mts/persephone objects,
#'  `...` accepts arguments for customizing the model specifications as alternative input to speclist.
#'  These are passed to the rjd3 specification functions internally.
#' @param list a list of persephone objects as alternative input to `...`. This
#'   argument can also handle mts objects.
#' @param method specifies the method to be used for the direct adjustment of
#'   the aggregate series, i.e. "tramoseats" or "x13".
#' @param template the name of the predefined specification template, passed
#' as the `name` argument to [rjd3x13::x13_spec()] or [rjd3tramoseats::tramoseats_spec()].
#' Must be one of:
#' `"rsa0"`, `"rsa1"`, `"rsa2c"`, `"rsa3"`, `"rsa4"`, `"rsa5c"` for x13
#' `"rsafull"`, `"rsa0"`, `"rsa1"`, `"rsa2"`, `"rsa3"`, `"rsa4"`, `"rsa5"` for tramoseats.
#' Defaults to `"rsa3"`.
#' @param context a list of external regressors (calendar or other) to be used for estimation,
#' passed as the `context` argument of [rjd3x13::x13_fast()] or [rjd3tramoseats::tramoseats_fast()]..
#' @param userdefined a character vector of user-defined variables to be included in the output,
#' passed as the `userdefined` argument to [rjd3x13::x13_fast()] or
#' [rjd3tramoseats::tramoseats_fast()].
#' @param speclist a list of additional arguments for customizing the model specifications,
#' passed to the rjd3 specification functions internally. As alternative input to `...`.
#' @param spec a model specification object of class JD3_X13_SPEC or JD3_TRAMOSEATS_SPEC
#' @examples
#' \dontrun{
#' objX13 <- perX13(AirPassengers, "rsa3")
#'
#' bt <- perBatch(a = objX13, b = objX13)
#' bt$run()
#'
#' # access the list of unadjusted series
#' bt$ts
#' # access the list of adjusted series
#' bt$adjusted
#'
#' bt$updateParams(easter.enabled = FALSE)
#'
#' bt$updateParams(component = "a", automdl.enabled = FALSE,
#'                 arima.p = 1, arima.d = 2, arima.q = 0,
#'                 arima.bp = 1, arima.bd = 1, arima.bq = 0)
#' bt$run()
#' bt
#' }
#'
#' @export
multipleTimeSeries <- R6::R6Class(
  "multipleTimeSeries",
  inherit = persephone,
  # ---- Public Methods ----
  public = list(
    #' @description create a new multiple time series object
    #' @param list a list of persephone objects as alternative input to `...`.
    #'   This argument can also handle mts objects
    #' @param method specifies the method to be used for the direct adjustment of
    #'   the aggregate series, i.e. "tramoseats" or "x13"
    #' @param template the name of the predefined specification template, passed
    #'   as the `name` argument to [rjd3x13::x13_spec()] or [rjd3tramoseats::tramoseats_spec()]
    #' @param context a list of external regressors (calendar or other) to be used for estimation,
    #'   passed as the `context` argument of [rjd3x13::x13_fast()] or [rjd3tramoseats::tramoseats_fast()]
    #' @param userdefined a character vector of user-defined variables to be included in the output,
    #'   passed as the `userdefined` argument to [rjd3x13::x13_fast()] or [rjd3tramoseats::tramoseats_fast()]
    #' @param speclist a list of additional arguments for customizing the model specifications,
    #'   passed to the rjd3 specification functions internally. As alternative input to `...`
    #' @param spec a model specification object of class JD3_X13_SPEC or JD3_TRAMOSEATS_SPEC
    #' @param iterate logical, whether to iterate over components (currently not used)
    #' @param ... one or more objects which are either of class persephone or
    #'   can be coerced to persephone objects with asPersephone. If more than
    #'   one element is supplied, the underlying time series must have the same
    #'   time instances. All elements supplied in ... must be named.
    #'   In addition to the ts/mts/persephone objects,
    #'   `...` accepts arguments for customizing the model specifications as alternative input to speclist
    initialize = function(list = NULL,
                          method = c("tramoseats", "x13"),
                          template = NULL,
                          context = NULL,
                          userdefined = NULL,
                          speclist = NULL,
                          spec = NULL,
                          iterate = FALSE,
                          ...) {
      #browser()
      private$method <- match.arg(method)
      # template default and validation depending on method
      if (missing(template) || is.null(template)) {
        template <- "rsa3"
      }
      # allowed template sets per method
      x13_templates <- c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c")
      tramoseats_templates <- c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")

      if (private$method == "x13") {
        if (!template %in% x13_templates) {
          stop(sprintf("Invalid template '%s' for method 'x13'. Allowed values: %s",
                       template, paste(x13_templates, collapse = ", ")))
        }
      } else { # tramoseats
        if (!template %in% tramoseats_templates) {
          stop(sprintf("Invalid template '%s' for method 'tramoseats'. Allowed values: %s",
                       template, paste(tramoseats_templates, collapse = ", ")))
        }
      }
      private$template <- template
      # keep context and userdefined in private state for runDirect
      private$context <- context
      # userdefined_default expected to be defined elsewhere in the package
      private$userdefined <- union(userdefined, userdefined_default)
      private$params_internal <- spec

      if (!is.null(list)) {
        components <- list
        if (is.null(names(components))) {
          names(components) <- paste0("ts", seq_along(components))
        }
        if(is.null(speclist)) {
          spec_list <- list(...)
        } else {
          spec_list <- speclist
        }
      } else {
        components <- list(...)
        if (is.null(names(components)) || "" %in% names(components)) {
          stop("all arguments must be named")
        }
        if (!is.null(speclist)) {
          spec_list <- speclist
        } else {
          # detect and separate potential spec arguments passed in dots via naming convention
          sel_spec <- integer(0)
          if (exists("specfun_list", envir = asNamespace("persephone3"), inherits = FALSE)) {
            for (cp in paste0(names(specfun_list), ".")) {
              sel_spec <- c(sel_spec, which(startsWith(names(components), cp)))
            }
            for (cp in p3_spec_names) { #p3_spec_names werden auch in helpers.R definiert
              sel_spec <- c(sel_spec, which(startsWith(names(components), cp)))
            }
          }
          # TO DO: Hier abfangen wenn falsche spec-Block-Namen!!!
          # Checken, ob dann eh alle components ts/mts/persephone Objekte
          sel_spec <- unique(sel_spec)
          spec_list <- list()
          if (length(sel_spec) > 0) {
            spec_list <- components[sel_spec]
            components <- components[-sel_spec]
          }
        }
      }

      components <- lapply(components, as.persephone)
      private$check_classes(components)
      names(components) <- private$coerce_component_names(components)
      private$tsp_internal <- private$check_time_instances(components)
      self$components <- components

      # apply the same spec updates to all components
      if (exists("spec_list") && length(spec_list) > 0) {
        lapply(self$components, function(node) {
          #args_for_node <- c(list(iterate = TRUE), spec_list)
          # use do.call so we don't rely on ... inside this anonymous function
          # do.call(node$updateParams, args_for_node)
          do.call(node$updateParams, spec_list)
          invisible(NULL)
        })
      }
      invisible(NULL)

    },
    #' @description run the model
    #' @param verbose if `FALSE` (the default), the results of the run will
    #'   be returned invisibly
    #' @examples perX13(AirPassengers)$run()
    run = function(verbose = FALSE) {
      invisible(lapply(self$components, function(component) {
        component$run(verbose = verbose)
      }))
    },
    #' @field components the series of the multiple time series
    components = NULL,
    #' @description print mutliple timeseries to screen
    print = function() {
      tbl <- private$printTable()
      if (all(!tbl$run))
        tbl <- tbl[, 1:3]
      print(tbl, right = FALSE, row.names = FALSE)
    },
    #' @description iterate over all components
    #' @details this function is similar to `lapply()` in the sense that it
    #'   can be used to apply a function to several persephone objects
    #'   simultaniusely
    #' @param fun a function that takes a persephone object as a parameter
    #' @param asTable if true, the return value of this method will be coerced
    #'   to a data.frame
    #' @param component the id of the component
    #' @param unnest if `asTable = FALSE`, converts the return value from a
    #'   nested list into a flat list
    iterate = function(fun, asTable = FALSE, component = "", unnest = FALSE) {
      if (component != "") {
        root <- self$getComponent(component)
        return(root$iterate(fun, asTable))
      }
      comp <- lapply(
        self$components,
        function(component) {
          component$iterate(fun)
        }
      )

      private$convert_list(comp, asTable, unnest)
    },
    #' @description extract a component series
    #' @param componentId the id of a component
    getComponent = function(componentId) {
      if(!componentId%in%names(self$components)){
        stop(componentId," not found.")
      }
      self$components[[componentId]]
    },
    #' @description change all or some parameters of components
    #' @details this functions provides the possibility to update
    #' parameters of one or more persephone single objects
    #'
    #' @param component character vector with names of the components
    #' for which the parameters should be changed. If NULL (default) the
    #' parameters of all components will be changed
    #' @param ... named arguments to be changed
    updateParams = function(component = NULL, ...) {
      #browser()
      if(is.null(component)){
        invisible(lapply(self$components, function(x) x$updateParams(...)))
      }else{
        invisible(lapply(self$components[component], function(x) x$updateParams(...)))
      }
    },
    #' @description Generate a table for the eurostat quality report
    #' @param component (optional) a sub-component to create the report for
    #' @param ... additional arguments for the generate qr table function
    generateQrTable = function(component = "", ...) {
      if(component!=""){
        self$getComponent(component)$generateQrTable(...)
      }else{
        self$iterate(generate_Qr_List, asTable = TRUE)
      }
    }
  ),
  # ---- Active Bindings ----
  active = list(
    #' @field ts the time series (inherited from persephone)
    ts = function() {
      lapply(self$components,function(x)x$ts)
    },
    #' @field adjusted results from the seasonal adjustment
    adjusted = function() {
      lapply(self$components,function(x)x$adjusted)
    },
    #' @field params of all components
    params = function() {
      lapply(self$components,function(x)x$params)
    },
    #' @field forecasts get forecasts
    forecasts = function() {
      lapply(self$components,function(x)x$forecasts)
    }
  ),
  # ---- Private Methods ----
  private = list(
    template = NULL, # one of c("rsa3", "rsa0", "rsa1", "rsa2c", "rsa4", "rsa5c")
    method = NULL,   # one of c("tramoseats","x13")
    spec_list = NULL,
    spec = NULL,     # rjd3 spec object (if used directly)
    context = NULL,
    userdefined = NULL,

    check_classes = function(components) {
      lapply(components, function(component) {
        stopifnot(is.persephone(component))
      })
    },
    printTable = function() {
      self$iterate(printDiagnostics, asTable = TRUE)
    },
    check_time_instances = function(components) {
      tsps <- lapply(components, function(component) {
        component$tsp
      })
      if (length(unique(tsps)) != 1)
        stop("All components need to have the same time instances")
      tsps[[1]]
    },
    coerce_component_names = function(components) {
      lapply(seq_along(components), function(i) {
        parname <- names(components)[i]
        if (!is.null(parname) && parname != "")
          return(parname)
        else
          stop("all components in 'multipleTimeSeries' must be named")
      })
    }#,
  )

)

#' Define a multiple time series
#'
#' Combine mutliple objects of persephone objects into a new persephone batch object.
#' On the resulting objects, you can perform seasonal adjustment.
#'
#' @param ... one or more objects which are either of class persephone or
#'  can be coerced to persephone objects with asPersephone. If more than
#'  one element is supplied, the underlying time series must have the same
#'  time instances. All elements supplied in ... must be named.
#'
#'  In addition to the ts/mts/persephone objects,
#'  `...` accepts arguments for customizing the model specifications as alternative input to speclist.
#'  These are passed to the rjd3 specification functions internally.
#' @param list a list of persephone objects as alternative input to `...`. This
#'   argument can also handle mts objects.
#' @param method specifies the method to be used for the direct adjustment of
#'   the aggregate series, i.e. "tramoseats" or "x13".
#' @param template the name of the predefined specification template, passed
#' as the `name` argument to [rjd3x13::x13_spec()] or [rjd3tramoseats::tramoseats_spec()].
#' Must be one of:
#' `"rsa0"`, `"rsa1"`, `"rsa2c"`, `"rsa3"`, `"rsa4"`, `"rsa5c"` for x13
#' `"rsafull"`, `"rsa0"`, `"rsa1"`, `"rsa2"`, `"rsa3"`, `"rsa4"`, `"rsa5"` for tramoseats.
#' Defaults to `"rsa3"`.
#' @param context a list of external regressors (calendar or other) to be used for estimation,
#' passed as the `context` argument of [rjd3x13::x13_fast()] or [rjd3tramoseats::tramoseats_fast()]..
#' @param userdefined a character vector of user-defined variables to be included in the output,
#' passed as the `userdefined` argument to [rjd3x13::x13_fast()] or
#' [rjd3tramoseats::tramoseats_fast()].
#' @param speclist a list of additional arguments for customizing the model specifications,
#' passed to the rjd3 specification functions internally. As alternative input to `...`.
#' @param spec a model specification object of class JD3_X13_SPEC or JD3_TRAMOSEATS_SPEC
#' @export
perBatch <- function(..., list = NULL,
                     method = c("tramoseats", "x13"),
                     template = "rsa3",
                     context = NULL, userdefined = NULL, speclist = NULL, spec = NULL) {
  multipleTimeSeries$new(..., list = list, method = method, template = template,
                         context = context, userdefined = userdefined,
                         speclist = speclist, spec = spec)
}
