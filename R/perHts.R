#' R6 Class for hierarchical time series
#'
#' @description Combine mutliple objects of persephone objects into a new
#'   hierachical persephone object. On the resulting time series the user can perform direct and
#'   indirect seasonal adjustments.
#' @examples
#' \dontrun{
#' objX13 <- perX13(AirPassengers, "rsa3")
#'
#' # change parameters for all series (iterate = TRUE)
#' ht <- perHts(a = objX13, b = objX13, method = "x13", template = "rsa0",
#'             automdl.enabled = FALSE,
#'             arima.p = 1, arima.d = 2, arima.q = 0,
#'             arima.bp = 1, arima.bd = 1, arima.bq = 0,
#'             iterate = TRUE)
#'
#' ht$run()
#' ht
#'
#' # change parameters only for aggregate series (iterate = FALSE)
#' ht2 <- perHts(a = ht, b = objX13, method = "x13", template = "rsa0",
#'              transform.fun = "Log", transform.adjust = NA,
#'              easter.enabled = TRUE,
#'              outliers = c("AO1953.7", "LS1954.6"),
#'              iterate = FALSE)
#' ht2$run()
#' ht2
#' }
#' @export
hierarchicalTimeSeries <- R6::R6Class(
  "hierarchicalTimeSeries",
  inherit = multipleTimeSeries,
  # ---- Public Methods ----
  public = list(
    #' @description create a new hierarchical time series
    #' @param ... one or more objects which are either of class persephone or
    #'  can be coerced to persephone objects with asPersephone. If more than
    #'  one element is supplied, the underlying time series must have the same
    #'  time instances. All elements supplied in ... must be named.
    #'
    #'  In addition to the ts/mts/persephone objects,
    #'  `...` accepts arguments for customizing the model specifications as alternative input to speclist.
    #'  These are passed to the rjd3 specification functions internally.
    #' @param list a list of persephone objects as alternative input to `...`.
    #'   This argument can also handle mts objects
    #' @param method specifies the method to be used for the direct adjustment
    #'   of the aggregate series. tramoseats or x13
    #' @param template the name of the predefined specification template, passed
    #' as the `name` argument to [rjd3x13::x13_spec()] or [rjd3tramoseats::tramoseats_spec()].
    #' Must be one of:
    #' `"rsa0"`, `"rsa1"`, `"rsa2c"`, `"rsa3"`, `"rsa4"`, `"rsa5c"` for x13
    #' `"rsafull"`, `"rsa0"`, `"rsa1"`, `"rsa2"`, `"rsa3"`, `"rsa4"`, `"rsa5"` for tramoseats.
    #' Defaults to `"rsa3"`.
    #' @param context passed as the `context` argument of [rjd3x13::x13_fast()] or
    #'   [rjd3tramoseats::tramoseats_fast()], a list of external regressors (calendar or other) to be used for estimation.
    #' @param userdefined passed as the userdefined argument to x13_fast() or
    #'   tramoseats_fast()
    #' @param speclist a list of additional arguments for customizing the model specifications,
    #' passed to the rjd3 specification functions internally. As alternative input to `...`.
    #' @param spec a model specification object of class JD3_X13_SPEC or JD3_TRAMOSEATS_SPEC
    #'   returned by [rjd3x13::x13_spec()] or [rjd3tramoseats::tramoseats_spec()].
    #' @param backend Backend for the aggregate and new components; existing objects retain theirs.
    #' @param weights either a vector if the same weight is used for all time
    #'   points or a list of ts objects or a mts object if the weight varies
    #'   for different time points. They must have the same length as the
    #'   number of components. Without weights, components are summed. With
    #'   weights, aggregation is a weighted mean. Named inputs are aligned by
    #'   component name. A short weight series must cover the first observation;
    #'   its final value is carried forward only after its last supplied period.
    #'   Parent weights can be inherited as sums of weighted child groups.
    #' @param iterate logical, if TRUE apply the given ... spec/params also to all components.
    #'   For plain ts/mts components, method and template are inherited only if iterate is TRUE.
    initialize = function(
      ...,
      list = NULL,
      method = c("tramoseats", "x13"),
      template = NULL,
      context = NULL,
      userdefined = NULL,
      spec = NULL,
      weights = NULL,
      iterate = TRUE,
      speclist = NULL,
      backend = NULL
    ) {
      #browser()
      private$method <- match.arg(method)
      private$backend_internal <- resolve_backend(backend)
      # template default and validation depending on method
      if (missing(template) || is.null(template)) {
        template <- "rsa3"
      }
      check_template(template, private$method)
      private$template <- template
      # keep context and userdefined in private state for runDirect
      private$context_internal <- context
      # userdefined_default expected to be defined elsewhere in the package
      private$userdefined <- union(userdefined, userdefined_default)
      private$params_internal <- spec

      if (!is.null(list)) {
        components <- list
        if (is.null(names(components))) {
          names(components) <- paste0("ts", seq_along(components))
        }
        if (is.null(speclist)) {
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
          if (
            exists(
              "specfun_list",
              envir = asNamespace("persephone3"),
              inherits = FALSE
            )
          ) {
            for (cp in paste0(names(specfun_list), ".")) {
              sel_spec <- c(sel_spec, which(startsWith(names(components), cp)))
            }
            for (cp in p3_spec_names) {
              #p3_spec_names werden auch in helpers.R definiert
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

      # Nur wenn iterate = TRUE gilt die Konfiguration der Aggregatsreihe auch
      # fuer die Subreihen; sonst erhalten rohe ts-Objekte die Paket-Defaults.
      if (isTRUE(iterate)) {
        components <- lapply(
          components,
          as.persephone,
          method = private$method,
          template = private$template,
          backend = self$backend
        )
        check_component_method(components, private$method, iterate)
      } else {
        components <- lapply(components, as.persephone, backend = self$backend)
      }
      private$check_classes(components)
      names(components) <- private$coerce_component_names(components)
      private$tsp_internal <- private$check_time_instances(components)
      self$components <- components
      self$weights <- hts_prepare_weights(components, weights)
      private$ts_internal <- private$aggregate(components, self$weights)
      # Public fields remain readable for compatibility. Reconstruct a hierarchy
      # to change aggregation inputs; otherwise a cached direct fit can refer to
      # different data from the displayed indirect aggregate.
      private$aggregation_signature <- list(
        names = names(components), weights = self$weights,
        series = lapply(components, function(x) x$ts))

      # create/initialise spec for aggregate series using updateFun
      private$params_internal <- private$updateFun(
        name = private$template,
        freq = frequency(private$ts_internal),
        init_spec = private$params_internal,
        spec_list = if (exists("spec_list")) spec_list else NULL,
        ...
      )

      # If requested, also apply the same ... updates to all components
      # if (isTRUE(iterate)) {
      #   lapply(self$components, function(node) {
      #     # call each component's updateParams with the same named args from ...
      #     # components are persephone objects and expected to accept updateParams(...)
      #     node$updateParams(...)
      #     invisible(NULL)
      #   })
      # }
      # If requested, also apply the same spec updates to all components
      if (isTRUE(iterate) && exists("spec_list") && length(spec_list) > 0) {
        lapply(self$components, function(node) {
          args_for_node <- c(list(iterate = TRUE), spec_list)
          # use do.call so we don't rely on ... inside this anonymous function
          do.call(node$updateParams, args_for_node)
          invisible(NULL)
        })
      }
      invisible(NULL)
    },

    #' @description run the model
    #' @param verbose if `FALSE` (the default), the results of the run will
    #'   be returned invisibly
    run = function(verbose = FALSE) {
      private$check_aggregation_inputs()
      ## indirect: run child components first (they fill their own outputs)
      lapply(self$components, function(component) {
        component$run(verbose = verbose)
      })
      ## direct: run aggregate using private$ts_internal (aggregated series)
      ts_to_run <- if (!is.null(self$ts)) self$ts else private$ts_internal
      private$runDirect(ts_to_run)
      invisible(NULL)
    },

    #' @field components The subseries. Reconstruct the hierarchy to replace
    #'   components; changing model settings or direct/indirect choices is allowed.
    components = NULL,
    #' @field weights Aligned weights for weighted means (NULL for sums).
    #'   Reconstruct the hierarchy to change weights after construction.
    weights = NULL,
    #' @field indirect whether to use direct or indirect adjustment
    indirect = NA,

    #' @description print a hierarchical timeseries to screen
    print = function() {
      tbl <- private$print_table()
      if (all(!tbl$run)) {
        tbl <- tbl[, 1:4]
      } # hier 1:3 wenn class statt method + type
      print(tbl, right = FALSE, row.names = FALSE)
    },

    #' @description iterate over all components
    #' @details this function is similar to `lapply()` in the sense that it
    #'   can be used to apply a function to several persephone objects
    #'   simultaneously
    #' @param fun a function that takes a persephone object as a parameter
    #' @param asTable if true, the return value of this method will be coerced
    #'   to a data.frame
    #' @param component the id of the component
    #' @param unnest if `asTable = FALSE`, converts the return value from a
    #'   nested list into a flat list
    iterate = function(fun, asTable = FALSE, component = "", unnest = FALSE) {
      # browser()
      if (component != "") {
        root <- self$getComponent(component)
        return(root$iterate(fun, asTable))
      }
      # Ergebnisse der Komponenten (jede Komponente liefert ihre eigene iterate-Ausgabe)
      comp <- lapply(
        self$components,
        function(component) {
          component$iterate(fun)
        }
      )

      # Ergebnis für die Aggregatsreihe (fun applied to this HTS object)
      agg <- list(value = fun(self))

      # kombiniere Aggregate + Komponenten (keine doppelte Verwendung von super$iterate)
      res <- c(agg, comp)

      private$convert_list(res, asTable, unnest)
    },
    #' @description extract a component series
    #' @param componentId the id of a component
    getComponent = function(componentId) {
      if (length(componentId) == 0 || componentId == "") {
        return(self)
      }
      component_path <- strsplit(componentId, "/")[[1]]
      direct_child <- component_path[1]
      if (length(component_path) == 1) {
        return(self$components[[direct_child]])
      }
      rest <- paste(component_path[-1], collapse = "/")
      self$components[[direct_child]]$getComponent(rest)
    },

    #' @description change parameters of components or aggregate
    #' @param component character scalar name of the component to change; if empty, operate on aggregate
    #' @param iterate logical: if TRUE, apply parameter changes to all components recursively
    #' @param template Replacement template, or NULL for incremental changes.
    #' @param spec Complete replacement specification; mutually exclusive with template.
    #' @param context Replacement regressor/calendar context when supplied.
    #' @param userdefined Replacement additional output requests when supplied.
    #' @param speclist list with named arguments to be changed
    #' @param ... named arguments to be changed (spec options forwarded to updateFun)
    updateParams = function(
      component = "", iterate = TRUE, speclist = NULL, ...,
      template = NULL, spec = NULL, context = NULL, userdefined = NULL
    ) {
      if (!is.character(component) || length(component) != 1L || is.na(component)) {
        stop("'component' must be a single path.", call. = FALSE)
      }
      if (!is.logical(iterate) || length(iterate) != 1L || is.na(iterate)) {
        stop("'iterate' must be TRUE or FALSE.", call. = FALSE)
      }
      changes <- specification_changes(speclist, list(...))
      validate_update_context(context, userdefined)
      args <- list(template = template, spec = spec, speclist = changes, iterate = iterate)
      if (!missing(context)) args$context <- context
      if (!missing(userdefined)) args$userdefined <- userdefined
      # Keep explicit NULL values so contexts and optional outputs can be cleared.
      if (!missing(context) && is.null(context)) args["context"] <- list(NULL)
      if (!missing(userdefined) && is.null(userdefined)) args["userdefined"] <- list(NULL)
      if (nzchar(component)) {
        root <- self$getComponent(component)
        if (is.null(root)) stop("Unknown component: ", component, call. = FALSE)
        do.call(root$updateParams, args)
        return(invisible(NULL))
      }
      proposed <- prepare_specification_update(
        private$params_internal, self$method, frequency(private$ts_internal),
        self$backend, template, spec, changes)
      # Preflight clones to avoid partially changing a hierarchy when a later
      # node rejects an option. Real component identities are preserved.
      if (iterate) {
        for (node in self$components) do.call(node$clone(deep = TRUE)$updateParams, args)
      }
      private$params_internal <- proposed
      if (!is.null(template)) private$template <- template
      if (!is.null(spec)) private$template <- NULL
      if (!missing(context)) private$context_internal <- context
      if (!missing(userdefined)) private$userdefined <- union(userdefined, userdefined_default)
      if (iterate) for (node in self$components) do.call(node$updateParams, args)
      invisible(NULL)
    },

    #' @description Generate a table for the eurostat quality report
    #' @param component (optional) a sub-component to create the report for
    generateQrTable = function(component = "") {
      self$iterate(generate_Qr_List, asTable = TRUE, component = component)
    }
  ),

  # ---- Active Bindings ----
  active = list(
    #' @field specification Canonical specification for the direct aggregate.
    specification = function() {
      new_backend_spec(self$method, private$template, frequency(private$ts_internal),
                       private$params_internal, self$backend)
    },
    #' @field result Canonical result of the direct aggregate fit.
    result = function() private$result_internal,
    #' @field is_stale Whether the direct aggregate or a component fit has stale settings.
    is_stale = function() {
      private$check_aggregation_inputs()
      (!is.null(private$output_internal) &&
         !identical(private$fit_signature, private$current_signature())) || super$is_stale
    },
    #' @field ts the aggregated time series
    ts = function() {
      private$check_aggregation_inputs()
      private$ts_internal
    },
    #' @field params of all components and the aggregated series
    params = function() {
      c(list(aggregate = private$params_internal), super$params)
    },
    # # ' @field methodDirect SA method used for the aggregated series
    # methodDirect = function() {
    #   private$method
    # },
    #' @field paramsDirect params of the aggregated series
    paramsDirect = function() {
      private$params_internal
    },

    #' @field adjustedIndirect results from the indirect adjustment where
    #'   all components are adjusted and then aggregated
    adjustedIndirect = function() {
      private$check_aggregation_inputs()
      if (is.null(private$output_internal)) {
        return(NULL)
      }
      private$aggregate(
        self$components,
        self$weights,
        which = "adjustedIndirect"
      )
    },

    #' @field adjusted results from the seasonal adjustment
    adjusted = function() {
      private$check_aggregation_inputs()
      if (is.na(self$indirect)) {
        warning(
          "The decision between direct and indirect adjustment was not recorded yet. \nDirect adjustment is returned."
        )
      } else if (self$indirect) {
        return(private$adjusted_indirect_one_step())
      }
      return(self$adjustedDirect)
    },

    #' @field forecasts get forecasts
    forecasts = function() {
      if (is.na(self$indirect)) {
        warning(
          "The decision between direct and indirect adjustment was not recorded yet. \nDirect forecasts are returned."
        )
      } else if (self$indirect) {
        return(private$forecasts_indirect_one_step())
      }
      return(self$forecastsDirect)
    },

    #' @field forecastsIndirect get forecasts according to indirect adjustments
    forecastsIndirect = function() {
      if (is.null(private$output_internal)) {
        return(NULL)
      }
      private$aggregate(
        self$components,
        self$weights,
        which = "forecastsIndirect"
      )
    }
  ),

  # ---- Private Methods ----
  private = list(
    template = NULL, # one of c("rsa3", "rsa0", "rsa1", "rsa2c", "rsa4", "rsa5c") for x13, others for tramoseats
    method = NULL, # one of c("tramoseats","x13")
    spec_list = NULL, # specification parameters
    spec = NULL, # rjd3 spec object (if used directly)
    context_internal = NULL, # list of external regressors (calendar or other) for
    userdefined = NULL, # additional output values from rjd3
    ts_internal = NULL, # aggregate series (sum or weighted mean)
    aggregation_signature = NULL,
    check_aggregation_inputs = function() {
      if (is.null(private$aggregation_signature)) return(invisible(NULL))
      snapshot <- private$aggregation_signature
      if (!identical(names(self$components), snapshot$names) ||
          !identical(self$weights, snapshot$weights) ||
          !all(vapply(self$components, is.persephone, logical(1))) ||
          !identical(lapply(self$components, function(x) x$ts), snapshot$series)) {
        stop("Aggregation inputs changed: reconstruct the hierarchy to change weights or component series.",
             call. = FALSE)
      }
      invisible(NULL)
    },
    output_internal = NULL, # output for direct aggregate
    params_internal = NULL, # spec for aggregate series

    forecasts_indirect_one_step = function() {
      if (is.null(private$output_internal)) {
        return(NULL)
      }
      private$aggregate(self$components, self$weights, which = "forecasts")
    },

    adjusted_indirect_one_step = function() {
      if (is.null(private$output_internal)) {
        return(NULL)
      }
      private$aggregate(self$components, self$weights, which = "adjusted")
    },

    check_classes = function(components) {
      lapply(components, function(component) {
        stopifnot(is.persephone(component))
      })
    },

    print_table = function() {
      self$iterate(printDiagnostics, asTable = TRUE)
    },

    check_time_instances = function(components) {
      tsps <- lapply(components, function(component) {
        component$tsp
      })
      if (length(unique(tsps)) != 1) {
        stop("All components need to have the same time instances")
      }
      tsps[[1]]
    },

    aggregate = function(components, weights, which = "ts") {
      # Each hierarchical child owns its aggregation rule. In particular,
      # 'adjusted' respects its selected direct/indirect fit; 'adjustedIndirect'
      # recursively asks for the fully indirect child result.
      tss <- lapply(components, function(component) {
        if (inherits(component, "persephoneSingle")) {
          if (which == "adjustedIndirect") return(component$adjusted)
          if (which == "forecastsIndirect") return(component$forecasts)
        }
        component[[which]]
      })
      if (all(vapply(tss, is.null, logical(1)))) return(NULL)
      hts_aggregate_series(tss, weights)
    },

    coerce_component_names = function(components) {
      component_names <- names(components)
      if (!length(components) || is.null(component_names) || anyNA(component_names) ||
          any(!nzchar(component_names)) || anyDuplicated(component_names)) {
        stop("all components in 'hierarchicalTimeSeries' must have distinct nonempty names",
             call. = FALSE)
      }
      component_names
    },

    # update specification helper: centralizes spec creation / update logic
    updateFun = function(
      name = NULL,
      freq,
      init_spec = NULL,
      spec_list = NULL,
      ...
    ) {
      # if init_spec is NULL, fall back to saved params
      if (is.null(init_spec)) {
        init_spec <- private$params_internal
      }
      # if still NULL -> create a new spec using template
      if (is.null(init_spec)) {
        tmpl <- if (!is.null(name)) name else private$template
        init_spec <- backend_default_spec(private$method, tmpl, freq, self$backend)
      }
      init_spec <- backend_spec_parameters(init_spec, private$method, self$backend)
      # if spec_list not provided, construct from ... to allow passing named spec args
      if (is.null(spec_list)) {
        spec_list <- list(...)
      }
      if (length(spec_list) > 0) {
        spec_new <- backend_update_spec(
          spec = init_spec,
          method = if (private$method == "x13") "x13" else "tramoseats",
          freq = freq,
          spec_list = spec_list,
          backend = self$backend
        )
      } else {
        spec_new <- init_spec
      }
      private$params_internal <- spec_new
      return(spec_new)
    },
    # TO DO: Funktion irgendwo verwenden?
    updateParamsDirect = function(...) {
      # kept for compatibility; forwards to updateFun
      freq <- frequency(private$ts_internal)
      private$updateFun(
        name = private$template,
        freq = freq,
        init_spec = private$params_internal,
        spec_list = if (exists("spec_list")) spec_list else NULL,
        ...
      )
      invisible(NULL)
    },

    runDirect = function(ts) {
      if (is.null(private$params_internal)) {
        private$params_internal <- backend_default_spec(
          private$method, private$template, frequency(private$ts_internal), self$backend)
      }
      output <- backend_fit(
        ts = private$ts_internal, spec = private$params_internal,
        userdefined = private$userdefined, context = private$context_internal,
        method = private$method, backend = self$backend)
      normalized <- normalize_backend_result(output, private$method, self$backend,
                                              self$specification, private$ts_internal)
      private$output_internal <- output
      private$result_internal <- normalized
      private$fit_signature <- private$current_signature()
    }
  )
)

#' Define a hierarchical time series
#'
#' Combine mutliple objects of persephone objects into a new persephone object.
#' The resulting time series can perform direct and indirect adjustments.
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
#'   returned by [rjd3x13::x13_spec()] or [rjd3tramoseats::tramoseats_spec()].
#' @param backend Backend for the aggregate and newly created components. Existing
#'   component objects retain their recorded backend, independently of iterate.
#' @param weights  either a vector if the same weight is used for all time
#'   points or a list of ts objects or a mts object if the weight varies for
#'   different time points. They must have the same length as the number of
#'   components. Unweighted aggregation sums components; weighted aggregation
#'   computes a weighted mean. Named weights are matched to component names.
#'   Weight histories must cover the first observation. Values after the last
#'   supplied weight use its final value, preserving earlier variation.
#'   For nested weighted groups, omitted group weights are their child-weight
#'   sums. Explicit weights for all immediate components override those group
#'   totals; weights for only the non-hierarchical children retain inherited
#'   group totals. Mixed weighted/unweighted groups need explicit weights for
#'   all immediate components. Reconstruct the object to change weights or
#'   component series after construction.
#' @param iterate logical, if TRUE apply the given spec/params also to all components recursively
#'   (only takes effect when a spec/parameter is supplied). Plain ts/mts components inherit
#'   method and template only if iterate is TRUE; otherwise they are created with the
#'   method-specific defaults of [perTramo()] / [perX13()].
#' @examples
#' \dontrun{
#' objX13 <- perX13(AirPassengers, "rsa3")
#'
#' # change parameters for all series (iterate = TRUE)
#' ht <- perHts(a = objX13, b = objX13, method = "x13", template = "rsa0",
#'             automdl.enabled = FALSE,
#'             arima.p = 1, arima.d = 2, arima.q = 0,
#'             arima.bp = 1, arima.bd = 1, arima.bq = 0,
#'             iterate = TRUE)
#'
#' ht$run()
#' ht
#'
#' # change parameters only for aggregate series (iterate = FALSE)
#' ht2 <- perHts(a = ht, b = objX13, method = "x13", template = "rsa0",
#'              transform.fun = "Log", transform.adjust = NA,
#'              easter.enabled = TRUE,
#'              outliers = c("AO1953.7", "LS1954.6"),
#'              iterate = FALSE)
#' ht2$run()
#' ht2
#' ht2$adjustedDirect
#' ht2$adjustedIndirect
#' }
#' @export
perHts <- function(
  ...,
  list = NULL,
  method = c("tramoseats", "x13"),
  template = "rsa3",
  context = NULL,
  userdefined = NULL,
  speclist = NULL,
  spec = NULL,
  weights = NULL,
  iterate = TRUE,
  backend = NULL
) {
  hierarchicalTimeSeries$new(
    ...,
    method = method,
    template = template,
    context = context,
    userdefined = userdefined,
    speclist = speclist,
    spec = spec,
    list = list,
    weights = weights,
    iterate = iterate,
    backend = backend
  )
}

# TO DO: für Quartalsdaten erweitern - mit freq parameter statt 12 hardcoded
# startEndAsDecimal <- function(x){
#   x[1] + (x[2] - 1) / 12
# }
# generalisierte start/end -> Dezimal-Position (funktioniert für beliebige Frequenz)
startEndAsDecimal <- function(x, freq = 12) {
  # x: numeric vector like end(ts) or start(ts) -> c(year, period)
  if (is.null(x) || length(x) < 2) {
    stop(
      "startEndAsDecimal: 'x' must be a numeric vector of length >= 2 (as returned by start()/end())."
    )
  }
  if (!is.numeric(freq) || length(freq) != 1 || is.na(freq) || freq <= 0) {
    stop(
      "startEndAsDecimal: 'freq' must be a single positive number (e.g. 12 for monthly, 4 for quarterly)."
    )
  }
  year <- as.numeric(x[1])
  period <- as.numeric(x[2])
  year + (period - 1) / freq
}

# Internal aggregation primitives. Keeping these independent of a computation
# engine lets calendar alignment and weighted values be checked without Java.
hts_series_domain <- function(series) {
  if (!length(series) || any(vapply(series, function(x) {
    !is.ts(x) || !is.numeric(x) || !NROW(x)
  }, logical(1)))) {
    stop("Every component must provide a nonempty numeric time series.", call. = FALSE)
  }
  domains <- lapply(series, tsp)
  if (!all(vapply(domains, function(x) {
    isTRUE(all.equal(x, domains[[1]], tolerance = 1e-9))
  }, logical(1)))) {
    stop("All components need the same time instances and frequency.", call. = FALSE)
  }
  dimensions <- vapply(series, NCOL, integer(1))
  if (any(dimensions != dimensions[1])) {
    stop("All component series must have the same number of columns.", call. = FALSE)
  }
  if (dimensions[1] > 1 && !all(vapply(series, function(x) {
    identical(colnames(x), colnames(series[[1]]))
  }, logical(1)))) {
    stop("Multivariate component column names must match.", call. = FALSE)
  }
  domains[[1]]
}

hts_order_named <- function(values, expected, supplied_names = names(values)) {
  if (is.null(supplied_names)) return(values)
  if (anyNA(supplied_names) || any(!nzchar(supplied_names)) ||
      anyDuplicated(supplied_names) || !setequal(supplied_names, expected)) {
    stop("Weight names must match the intended component names exactly.", call. = FALSE)
  }
  values[match(expected, supplied_names)]
}

hts_weight_list <- function(weights) {
  if (is.ts(weights) && is.matrix(weights)) {
    result <- lapply(seq_len(ncol(weights)), function(i) weights[, i])
    names(result) <- colnames(weights)
    return(result)
  }
  if (is.ts(weights)) return(list(weights))
  if (is.list(weights)) return(weights)
  if (is.numeric(weights) && is.null(dim(weights))) return(as.list(weights))
  stop("Weights must be a numeric vector, a list of ts weights, or an mts object.",
       call. = FALSE)
}

hts_weight_values <- function(weight, target, component) {
  if (!is.numeric(weight) || !length(weight) || any(!is.finite(weight))) {
    stop("Weights for '", component, "' must be nonempty, finite and nonmissing.",
         call. = FALSE)
  }
  if (!is.ts(weight)) {
    if (length(weight) != 1L || !is.null(dim(weight))) {
      stop("Varying weights for '", component, "' must be a univariate ts.", call. = FALSE)
    }
    return(rep(as.numeric(weight), NROW(target)))
  }
  if (NCOL(weight) != 1L || frequency(weight) != frequency(target)) {
    stop("Weight frequency and dimensions must match component '", component, "'.",
         call. = FALSE)
  }
  shift <- (tsp(target)[1] - tsp(weight)[1]) * frequency(target)
  if (abs(shift - round(shift)) > 1e-6) {
    stop("Weight time indices must align with component '", component, "'.", call. = FALSE)
  }
  if (shift < -1e-6) {
    stop("Weights for '", component, "' are missing the beginning of the component history.",
         call. = FALSE)
  }
  positions <- seq_len(NROW(target)) + round(shift)
  # Preserve every supplied historic value and extend only beyond its tail.
  as.numeric(weight)[pmin(positions, length(weight))]
}

hts_align_weights <- function(weights, target, component_names) {
  if (is.null(weights)) return(NULL)
  weights <- hts_weight_list(weights)
  if (length(weights) != length(component_names)) {
    stop("Weight count must equal the number of immediate components.", call. = FALSE)
  }
  weights <- hts_order_named(weights, component_names)
  values <- lapply(seq_along(weights), function(i) {
    hts_weight_values(weights[[i]], target, component_names[i])
  })
  values <- do.call(cbind, values)
  totals <- rowSums(values)
  if (any(!is.finite(totals)) || any(totals == 0)) {
    stop("Total component weight must be finite and nonzero in every period.", call. = FALSE)
  }
  result <- ts(values, start = stats::start(target), frequency = frequency(target))
  colnames(result) <- component_names
  result
}

hts_prepare_weights <- function(components, weights) {
  series <- lapply(components, function(x) x$ts)
  hts_series_domain(series)
  component_names <- names(components)
  nested <- vapply(components, inherits, logical(1), "hierarchicalTimeSeries")
  group_weights <- lapply(components[nested], function(x) x$weights)
  weighted_groups <- !vapply(group_weights, is.null, logical(1))
  if (is.null(weights) && !any(weighted_groups)) return(NULL)

  supplied <- if (is.null(weights)) list() else hts_weight_list(weights)
  if (!is.null(weights) && length(supplied) == length(components)) {
    # Explicit weights refer to immediate nodes, even if a node is a group.
    supplied <- hts_order_named(supplied, component_names)
  } else {
    if (length(supplied) != sum(!nested)) {
      stop("Weight count must match all immediate components or just the non-hierarchical children.",
           call. = FALSE)
    }
    if (any(!weighted_groups)) {
      stop(paste0("Mixed weighted/unweighted components require explicit weights for all ",
                  "immediate components; inherited group weights require every group to be weighted."),
           call. = FALSE)
    }
    leaf_weights <- hts_order_named(supplied, component_names[!nested])
    supplied <- vector("list", length(components))
    supplied[!nested] <- leaf_weights
    for (i in which(nested)) {
      child <- components[[i]]$weights
      supplied[[i]] <- ts(rowSums(as.matrix(child)), start = stats::start(child),
                          frequency = frequency(child))
    }
  }
  names(supplied) <- component_names

  # Retain explicitly supplied future weights for later forecast aggregation.
  # Shorter series are aligned/extended independently, without cbind's NA fill.
  reference <- series[[1]]
  for (i in seq_along(supplied)) {
    hts_weight_values(supplied[[i]], reference, component_names[i])
  }
  last <- max(c(tsp(reference)[2], vapply(supplied, function(x) {
    if (is.ts(x)) tsp(x)[2] else tsp(reference)[2]
  }, numeric(1))))
  count <- round((last - tsp(reference)[1]) * frequency(reference)) + 1
  target <- ts(rep(0, count), start = stats::start(reference), frequency = frequency(reference))
  hts_align_weights(supplied, target, component_names)
}

hts_aggregate_series <- function(series, weights = NULL) {
  domain <- hts_series_domain(series)
  component_names <- names(series)
  if (is.null(component_names)) component_names <- paste0("component", seq_along(series))
  aligned <- hts_align_weights(weights, series[[1]], component_names)
  columns <- NCOL(series[[1]])
  result <- lapply(seq_len(columns), function(column) {
    values <- do.call(cbind, lapply(series, function(x) {
      if (is.null(dim(x))) as.numeric(x) else x[, column]
    }))
    if (is.null(aligned)) return(rowSums(values))
    weights_matrix <- as.matrix(aligned)
    terms <- values * weights_matrix
    # Match weighted.mean: a zero-weight missing component contributes nothing.
    terms[weights_matrix == 0] <- 0
    rowSums(terms) / rowSums(weights_matrix)
  })
  if (columns == 1L) {
    return(ts(result[[1]], start = domain[1], frequency = domain[3]))
  }
  result <- ts(do.call(cbind, result), start = domain[1], frequency = domain[3])
  colnames(result) <- colnames(series[[1]])
  result
}
