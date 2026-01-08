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
    #' @param weights either a vector if the same weight is used for all time
    #'   points or a list of ts objects or a mts object if the weight varies
    #'   for different time points. They must have the same length as the
    #'   number of components.
    #' @param recursive logical, if TRUE, apply this setting to all subseries as well. If FALSE,
    #'   only apply to aggregate
    #' @param iterate logical, if TRUE apply the given ... spec/params also to all components
    initialize = function(...,
                          list = NULL,
                          method = c("tramoseats", "x13"),
                          template = NULL,
                          context = NULL,
                          userdefined = NULL,
                          spec = NULL,
                          weights = NULL,
                          recursive = TRUE,
                          iterate = TRUE,
                          speclist = NULL) {
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
      componentsHts <- sapply(
        components,
        function(x) "hierarchicalTimeSeries" %in% class(x))

      if (!is.null(weights)) {
        if (ifelse(is.list(weights) || is.vector(weights),
                   length(weights), ncol(weights)) != sum(!componentsHts)) {
          stop("If the weights argument is provided, its length must be equal to the number of components.")
        }
      }
      weights_ts <- list()
      if (any(componentsHts)) {
        weightsNull <- sapply(components[componentsHts],
                              function(x) is.null(x$weights))
        if (any(!weightsNull)) {
          if (any(weightsNull)) {
            stop("At the moment it is only supported to use either weights for all components or none.")
          }
          for (i in which(componentsHts)) {
            weights_ts[[i]] <- ts(
              rowSums(components[[i]]$weights),
              start = start(components[[i]]$weights[, 1]),
              end = end(components[[i]]$weights[, 1]),
              frequency = frequency(components[[i]]$weights[, 1]))
          }
        }
      }

      if (!is.list(weights) & !is.null(weights)) {
        which_components_not_Hts <- which(!componentsHts)
        for (i in seq_along(which_components_not_Hts)) {
          j <- which_components_not_Hts[i]
          weights_ts[[j]] <- ts(weights[i], start = start(components[[j]]$ts),
                                end = end(components[[j]]$ts),
                                frequency = frequency(components[[j]]$ts))
        }
      }

      private$check_classes(components)
      names(components) <- private$coerce_component_names(components)
      private$tsp_internal <- private$check_time_instances(components)
      self$components <- components
# # Ensure each component has an initial spec (so paramsDirect is present).
# # Only initialize if missing to avoid overwriting user-provided specs.
# lapply(self$components, function(node) {
#   ok <- FALSE
#   # if public paramsDirect exists and is non-null, skip
#   try({
#     if (!is.null(node$paramsDirect)) ok <- TRUE
#   }, silent = TRUE)
#   if (!ok) {
#     tryCatch({
#       node$updateParams()
#     }, error = function(e) {
#       # fallback: try updateParamsDirect if available
#       if (!is.null(node$updateParamsDirect) && is.function(node$updateParamsDirect)) {
#         tryCatch(node$updateParamsDirect(), error = function(e2) NULL)
#       }
#     })
#   }
#   invisible(NULL)
# })
      if ("mts" %in% class(weights)) {
        weights_ts[!componentsHts] <- as.list(weights)
      }
      if (length(weights_ts) == 0 && !is.list(weights)) {
        weights_ts <- NULL
      } else if (length(weights_ts) == 0 && is.list(weights)) {
        weights_ts <- do.call("cbind", weights)
        colnames(weights_ts) <- names(components)
      } else if (length(weights_ts) > 0 && is.list(weights)) {
        weights_ts[!componentsHts] <- weights
        weights_ts <- do.call("cbind", weights_ts)
        colnames(weights_ts) <- names(components)
      } else {
        weights_ts <- do.call("cbind", weights_ts)
        colnames(weights_ts) <- names(components)
      }

      self$weights <- weights_ts
      private$ts_internal <- private$aggregate(components, self$weights)

      # create/initialise spec for aggregate series using updateFun
      private$params_internal <- private$updateFun(
        name      = private$template,
        freq      = frequency(private$ts_internal),
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
      ## indirect: run child components first (they fill their own outputs)
      lapply(self$components, function(component) {
        component$run(verbose = verbose)
      })
      ## direct: run aggregate using private$ts_internal (aggregated series)
      ts_to_run <- if (!is.null(self$ts)) self$ts else private$ts_internal
      private$runDirect(ts_to_run)
      invisible(NULL)
    },

    #' @field components the sub series of the hierarchical time series
    components = NULL,
    #' @field weights the weights used for aggregating components
    weights = NULL,
    #' @field indirect whether to use direct or indirect adjustment
    indirect = NA,

    #' @description print a hierarchical timeseries to screen
    print = function() {
      tbl <- private$print_table()
      if (all(!tbl$run))
        tbl <- tbl[, 1:4] # hier 1:3 wenn class statt method + type
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
      if (length(componentId) == 0 || componentId == "")
        return(self)
      component_path <- strsplit(componentId, "/")[[1]]
      direct_child <- component_path[1]
      if (length(component_path) == 1)
        return(self$components[[direct_child]])
      rest <- paste(component_path[-1], collapse = "/")
      self$components[[direct_child]]$getComponent(rest)
    },

    #' @description change parameters of components or aggregate
    #' @param component character scalar name of the component to change; if empty, operate on aggregate
    #' @param iterate logical: if TRUE, apply parameter changes to all components recursively
    #' @param speclist list with named arguments to be changed
    #' @param ... named arguments to be changed (spec options forwarded to updateFun)
    updateParams = function(component = "", iterate = FALSE, speclist = NULL, ...) {
      # If a specific subcomponent is requested, forward the call to that subcomponent
      # nzchar is a fast way to find out if elements of a character vector are non-empty strings.

      if (nzchar(component)) {
        root <- self$getComponent(component)
        root$updateParams(iterate = iterate, speclist = speclist, ...)   # subtree handles recursion itself
        return(invisible(NULL))
      }

      # Update aggregate spec using updateFun (applies ... to aggregate spec)
      private$updateFun(
        name      = private$template,
        freq      = frequency(private$ts_internal),
        init_spec = private$params_internal,
        spec_list = speclist,
        ...
      )

      # If requested, apply the same changes to all components recursively
      if (isTRUE(iterate)) {
        lapply(self$components, function(node) {
          node$updateParams(iterate = TRUE, speclist = speclist, ...)
          invisible(NULL)
        })
      }
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
    #' @field ts the aggregated time series
    ts = function() {
      private$ts_internal
    },
    #' @field params of all components and the aggregated series
    params = function() {
      c(list(aggregate = private$params_internal), super$params)
    },
    #' #' @field methodDirect SA method used for the aggregated series
    #' methodDirect = function() {
    #'   private$method
    #' },
    #' @field paramsDirect params of the aggregated series
    paramsDirect = function() {
      private$params_internal
    },

    #' @field adjustedIndirect results from the indirect adjustment where
    #'   all components are adjusted and then aggregated
    adjustedIndirect = function() {
      if (is.null(private$output_internal))
        return(NULL)
      private$aggregate(self$components, self$weights,
                        which = "adjustedIndirect")
    },

    #' @field adjusted results from the seasonal adjustment
    adjusted = function() {
      if (is.na(self$indirect)) {
        warning("The decision between direct and indirect adjustment was not recorded yet. \nDirect adjustment is returned.")
      } else if (self$indirect) {
        return(private$adjusted_indirect_one_step())
      }
      return(self$adjustedDirect)
    },

    #' @field forecasts get forecasts
    forecasts = function() {
      if (is.na(self$indirect)) {
        warning("The decision between direct and indirect adjustment was not recorded yet. \nDirect forecasts are returned.")
      } else if (self$indirect) {
        return(private$forecasts_indirect_one_step())
      }
      return(self$forecastsDirect)
    },

    #' @field forecastsIndirect get forecasts according to indirect adjustments
    forecastsIndirect = function() {
      if (is.null(private$output_internal))
        return(NULL)
      private$aggregate(self$components, self$weights,
                        which = "forecastsIndirect")
    }
  ),

  # ---- Private Methods ----
  private = list(
    template = NULL, # one of c("rsa3", "rsa0", "rsa1", "rsa2c", "rsa4", "rsa5c") for x13, others for tramoseats
    method = NULL,   # one of c("tramoseats","x13")
    spec_list = NULL, # specification parameters
    spec = NULL,     # rjd3 spec object (if used directly)
    context = NULL,           # list of external regressors (calendar or other) for
    userdefined = NULL,       # additional output values from rjd3
    ts_internal = NULL,       # aggregate series (sum or weighted sum)
    output_internal = NULL,   # output for direct aggregate
    params_internal = NULL,   # spec for aggregate series

    forecasts_indirect_one_step = function() {
      if (is.null(private$output_internal))
        return(NULL)
      private$aggregate(self$components, self$weights, which = "forecasts")
    },

    adjusted_indirect_one_step = function() {
      if (is.null(private$output_internal))
        return(NULL)
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
      if (length(unique(tsps)) != 1)
        stop("All components need to have the same time instances")
      tsps[[1]]
    },

    # aggregate = function(components, weights, which = "ts") {
    #   tss <- lapply(components, function(component) {
    #     if (which == "adjustedIndirect" &
    #         "persephoneSingle" %in% class(component)) {
    #       return(component[["adjusted"]])
    #     }
    #     if (which == "forecastsIndirect" &
    #         "persephoneSingle" %in% class(component)) {
    #       return(component[["forecasts"]])
    #     }
    #     component[[which]]
    #   })
    #   weights_ts <- NULL
    #   if (!is.null(weights)) {
    #     weights_ts <- lapply(tss, function(x) {
    #       x <- x * 0 + 1
    #       x[is.na(x)] <- 1
    #     })
    #     for (i in seq_along(tss)) {
    #       if (startEndAsDecimal(end(weights[, i])) <
    #           startEndAsDecimal(end(tss[[i]]))) {
    #         weights_ts[[i]] <- ts(tail(weights[, i], 1),
    #                               start = start(tss[[i]]),
    #                               end = end(tss[[i]]), frequency = frequency(tss[[i]]))
    #       } else {
    #         weights_ts[[i]] <- window(weights[, i], start = start(tss[[i]]),
    #                                   end = end(tss[[i]]))
    #       }
    #     }
    #   }
    #   out <- private$aggregate_ts(tss, weights_ts)
    #   return(out)
    # },
    aggregate = function(components, weights, which = "ts") {
      # For each component, return the appropriate series (ts / adjusted / forecasts).
      # If a component is itself a hierarchicalTimeSeries, compute its aggregated
      # series recursively by calling private$aggregate on its subcomponents.
      tss <- lapply(components, function(component) {
        # If component is hierarchical, compute aggregate from its children
        if ("hierarchicalTimeSeries" %in% class(component)) {
          # component$components and component$weights are public, use them
          return(private$aggregate(component$components, component$weights, which = which))
        }

        # For leaf-level persephoneSingle objects, handle special which-cases:
        if (which == "adjustedIndirect" && "persephoneSingle" %in% class(component)) {
          return(component[["adjusted"]])
        }
        if (which == "forecastsIndirect" && "persephoneSingle" %in% class(component)) {
          return(component[["forecasts"]])
        }

        # Default: ask the component for the requested field
        component[[which]]
      })

      # Build weights series (if provided) to match tss lengths/tsp
      weights_ts <- NULL
      if (!is.null(weights)) {
        weights_ts <- lapply(tss, function(x) {
          x <- x * 0 + 1
          x[is.na(x)] <- 1
        })
        for (i in seq_along(tss)) {
          # ensure we compare end times with the correct frequencies
          if (startEndAsDecimal(end(weights[, i]), freq = frequency(weights[, i])) <
              startEndAsDecimal(end(tss[[i]]), freq = frequency(tss[[i]]))) {
            weights_ts[[i]] <- ts(tail(weights[, i], 1),
                                  start = start(tss[[i]]),
                                  end = end(tss[[i]]), frequency = frequency(tss[[i]]))
          } else {
            weights_ts[[i]] <- window(weights[, i], start = start(tss[[i]]),
                                      end = end(tss[[i]]))
          }
        }
      }

      out <- private$aggregate_ts(tss, weights_ts)
      return(out)
    },
    aggregate_ts = function(ts_vec, weights_ts) {
      if (!"mts" %in% class(ts_vec[[1]])) {
        return(private$aggregate_ts0(ts_vec, weights_ts))
      }
      # combine by columns for multivariate series
      out <- do.call("cbind", lapply(
        lapply(1:ncol(ts_vec[[1]]),
               function(ind) lapply(ts_vec, function(x) x[, ind])),
        private$aggregate_ts0, weights_ts = weights_ts))
      colnames(out) <- colnames(ts_vec[[1]])
      return(out)
    },

    aggregate_ts0 = function(ts_vec, weights_ts) {
      if (length(ts_vec) == 1) {
        return(ts_vec[[1]])
      }
      if (!is.null(weights_ts)) {
        ts_vec <- do.call("cbind", ts_vec)
        weights_ts <- do.call("cbind", weights_ts)
        out <- ts_vec[, 1] * 0
        for (i in 1:nrow(ts_vec)) {
          out[i] <- weighted.mean(ts_vec[i, ], weights_ts[i, ])
        }
      } else {
        ts_vecX <- do.call("cbind", ts_vec)
        out <- ts(rowSums(ts_vecX),
                  start = start(ts_vecX),
                  frequency = frequency(ts_vecX))
      }
      return(out)
    },

    coerce_component_names = function(components) {
      lapply(seq_along(components), function(i) {
        parname <- names(components)[i]
        if (!is.null(parname) && parname != "")
          return(parname)
        else
          stop("all components in 'hierarchicalTimeSeries' must be named")
      })
    },

    # update specification helper: centralizes spec creation / update logic
    updateFun = function(name = NULL, freq, init_spec = NULL, spec_list = NULL, ...) {
      # if init_spec is NULL, fall back to saved params
      if (is.null(init_spec)) {
        init_spec <- private$params_internal
      }
      # if still NULL -> create a new spec using template
      if (is.null(init_spec)) {
        tmpl <- if (!is.null(name)) name else private$template
        init_spec <- if (private$method == "x13") {
          rjd3x13::x13_spec(name = tmpl)
        } else {
          rjd3tramoseats::tramoseats_spec(name = tmpl)
        }
      }
      # if spec_list not provided, construct from ... to allow passing named spec args
      if (is.null(spec_list)) {
        spec_list <- list(...)
      }
      if (length(spec_list) > 0) {
        spec_new <- update_spec(
          spec      = init_spec,
          method    = if (private$method == "x13") "x13" else "tramoseats",
          freq      = freq,
          spec_list = spec_list
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
      private$updateFun(name = private$template,
                        freq = freq,
                        init_spec = private$params_internal,
                        spec_list = if (exists("spec_list")) spec_list else NULL,
                        ...)
      invisible(NULL)
    },

    runDirect = function(ts) {
      spec <- private$params_internal
      if (is.null(spec)) {
        spec <- if (private$method == "x13") {
          rjd3x13::x13_spec(name = private$template)
        } else {
          rjd3tramoseats::tramoseats_spec(name = private$template)
        }
        private$params_internal <- spec
      }

      methodFunction <- switch(private$method,
                               tramoseats = rjd3tramoseats::tramoseats_fast,
                               x13 = rjd3x13::x13_fast)

      private$output_internal <- methodFunction(
        ts = private$ts_internal,
        spec = private$params_internal,
        userdefined = private$userdefined,
        context = private$context
      )
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
#' @param weights  either a vector if the same weight is used for all time
#'   points or a list of ts objects or a mts object if the weight varies for
#'   different time points. They must have the same length as the number of
#'   components.
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
perHts <- function( ..., list = NULL, method = c("tramoseats", "x13"),
                    template = "rsa3",
                    context = NULL, userdefined = NULL, speclist = NULL, spec = NULL,
                    weights = NULL, iterate = TRUE) {
  hierarchicalTimeSeries$new(..., method = method, template = template,
                             context = context,
                             userdefined = userdefined, speclist = speclist, spec = spec,
                             list = list, weights = weights,
                             iterate = iterate)
}

# TO DO: für Quartalsdaten erweitern - mit freq parameter statt 12 hardcoded
# startEndAsDecimal <- function(x){
#   x[1] + (x[2] - 1) / 12
# }
# generalisierte start/end -> Dezimal-Position (funktioniert für beliebige Frequenz)
startEndAsDecimal <- function(x, freq = 12) {
  # x: numeric vector like end(ts) or start(ts) -> c(year, period)
  if (is.null(x) || length(x) < 2) {
    stop("startEndAsDecimal: 'x' must be a numeric vector of length >= 2 (as returned by start()/end()).")
  }
  if (!is.numeric(freq) || length(freq) != 1 || is.na(freq) || freq <= 0) {
    stop("startEndAsDecimal: 'freq' must be a single positive number (e.g. 12 for monthly, 4 for quarterly).")
  }
  year <- as.numeric(x[1])
  period <- as.numeric(x[2])
  year + (period - 1) / freq
}
