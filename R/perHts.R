#' R6 Class for hierarchical time series
#'
#' @description Combine mutliple objects of persephone objects into a new
#'   hierachical persephone object. On the resulting time series the user can perform direct and
#'   indirect seasonal adjustments.
#' @examples
#' \dontrun{
#' objX13 <- perX13(AirPassengers, "rsa3")
#'
#' ht <- perHts(a = objX13, b = objX13, method = "x13", spec = "rsa1")
#'
#' ht$updateParams(easter.enabled = TRUE)
#'
#' ht$updateParams(component = "a", usrdef.outliersEnabled = TRUE,
#'                      usrdef.outliersType = c("AO","LS","LS"),
#'                      usrdef.outliersDate=c("1950-01-01","1955-04-01","1959-10-01"))
#' ht$run()
#' ht$adjustedDirect
#' ht$adjustedIndirect
#'
#' ht2 <- perHts(a = ht, b = objX13)
#' ht2$run()
#' ht2$adjustedDirect
#' ht2$adjustedIndirect
#' }
#' @export
hierarchicalTimeSeries <- R6::R6Class(
  "hierarchicalTimeSeries",
  inherit = multipleTimeSeries,
  # ---- Public Methods ----
  public = list(
    #' @description create a new hierarchical time series
    #' @param ... one or more objects which are either of class persephone or
    #'   can be coerced to persephone objects with asPersephone. If more than
    #'   one element is supplied, the underlying time series must have the same
    #'   time instances. All elements supplied in ... must be named.
    #' @param method specifies the method to be used for the direct adjustment
    #'   of the aggregate series. tramoseats or x13
    #' @param template the name of the predefined specification template, passed
    #' as the `name` argument to [rjd3x13::x13_spec()].
    #' Must be one of:
    #' `"rsa0"`, `"rsa1"`, `"rsa2c"`, `"rsa3"`, `"rsa4"`, `"rsa5c"`.
    #' Defaults to `"rsa3"`.
    #' @param context passed as the `context` argument of [x13_fast()] or
    #'   [tramoseats_fast()], a list of external regressors (calendar or other) to be used for estimation.
    #' @param userdefined passed as the userdefined argument to x13_fast() or
    #'   tramoseats_fast()
    #' @param spec a model specification returned by x13_spec() or
    #'   tramoseats_spec()
    #' @param list a list of persephone objects as alternative input to `...`.
    #'   This argument can also handle mts objects
    #' @param weights either a vector if the same weight is used for all time
    #'   points or a list of ts objects or a mts object if the weight varies
    #'   for different time points. They must have the same length as the
    #'   number of components.
    #' @param recursive logical, if TRUE, apply this setting to all subseries as well. If FALSE,
    #'   only apply to aggregate
    initialize = function(method = c("tramoseats", "x13"),
                          template = c("rsa3", "rsa0", "rsa1", "rsa2c", "rsa4","rsa5c"),
                          context = NULL,
                          userdefined = NULL,
                          spec = NULL,
                          list = NULL,
                          weights = NULL,
                          recursive = TRUE,
                          ...) {
      # browser()
      private$method <- match.arg(method)
      private$template <- match.arg(template)
      # zusätzlich im eigenen privaten State speichern (für runDirect)
      private$context_internal <- context
      private$userdefined <- union(userdefined, userdefined_default)
      private$params_internal <- spec
      if (!is.null(list)) {
        components <- list
        if (is.null(names(components))) {
          names(components) <- paste0("ts", seq_along(components))
        }
        if (length(list(...)) > 0) {
          # warning("If the list argument is specified, additional arguments ",
          #         " as ... will be ignored.") <- gilt nicht mehr
          # rjd3 specs für updateParams
        }
      } else {
        components <- list(...)
        if (is.null(names(components)) || "" %in% names(components)) {
          stop("all arguments must be named")
        }
        # reference by position -> each argument as ..1, ..2, ..3, ..n
        # ...length() tells us how many arguments are passed in
        # hier unterscheiden zwischen components und rjd3-specs
        sel_spec <- vector()
        for(c in paste0(names(specfun_list),".")) {
          sel_spec <- c(sel_spec, which(startsWith(names(components), c)))
        }
        # remaining dots
        spec_list <- components[sel_spec]
        # "real" components
        if(length(spec_list) >0){
          components <- components[-sel_spec]
        }
      }
      components <- lapply(components, as.persephone)
      componentsHts <- sapply(
        components,
        function(x) "hierarchicalTimeSeries" %in% class(x))
      if (!is.null(weights)) {
        if (ifelse(is.list(weights) || is.vector(weights),
                   length(weights), ncol(weights)) != sum(!componentsHts)) {
          stop("If the weights argument is provided,
             its length must be equal to the number of components.")
        }
      }
      weights_ts <- list()
      if (any(componentsHts)) {
        weightsNull <- sapply(components[componentsHts],
                              function(x) is.null(x$weights))
        if (any(!weightsNull)) {
          if (any(weightsNull)) {
            stop("At the moment it is only supported to use either weights ",
                 "for all components or none.")
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
      if ("mts" %in% class(weights)) {
        weights_ts[!componentsHts] <- as.list(weights)
      }
      if (length(weights_ts) == 0 && !is.list(weights)) {
        weights_ts <- NULL
      } else if (length(weights_ts) == 0 && is.list(weights)) {
        weights_ts <- do.call("cbind", weights)
        colnames(weights_ts) <- names(components)
      }else if (length(weights_ts) > 0 && is.list(weights)){
        weights_ts[!componentsHts] <- weights
        weights_ts <- do.call("cbind", weights_ts)
        colnames(weights_ts) <- names(components)
      }else{
        weights_ts <- do.call("cbind", weights_ts)
        colnames(weights_ts) <- names(components)
      }
      self$weights <- weights_ts
      private$ts_internal <- private$aggregate(components, self$weights)
      # super$super2()$updateParams(context = context, userdefined = userdefined, spec = spec)
      # Kein super$super2() mehr, sondern direkt setzen:
      private$context_internal <- context
      private$userdefined <- union(userdefined, userdefined_default)
      # Spezifikation erzeugen/aktualisieren: init_spec = 'spec' aus Argumenten,
      # und ggf. zusätzliche ... Parameter beachten (falls gewünscht, hier ohne ...)
      private$params_internal <- private$updateFun(
        name      = private$template,
        freq      = frequency(private$ts_internal),
        init_spec = private$params_internal,
        spec_list = spec_list,
        ...)

      invisible(NULL)
    },
    #' @description run the model
    #' @param verbose if `FALSE` (the default), the results of the run will
    #'   be returned invisibly
    #' @examples perX13(AirPassengers)$run()
    run = function(verbose = FALSE) {
      ## indirect
      lapply(self$components, function(component) {
        component$run(verbose = verbose)
      })
      ## direct
      #     private$runDirect(self$ts)
      # Je nach Oberklasse gibt es self$ts. Falls nicht, verwende private$ts_internal.
      ts_to_run <- if (!is.null(self$ts)) self$ts else private$ts_internal
      private$runDirect(ts_to_run)

      invisible(NULL)

    },
    #' @field components the sub series of the hierarchical time series
    components = NULL,
    #' @field weights the weights used for aggregating components
    weights = NULL,
    #' @field indirect wether to use direct or indirect adjustement
    indirect = NA,
    #' @description print a hierarchical timeseries to screen
    print = function() {
      tbl <- private$print_table()
      if (all(!tbl$run))
        tbl <- tbl[, 1:3]
      print(tbl, right = FALSE, row.names = FALSE)
    },
    #' #' @description sets options for all entries of the dependency tree
    #' #'   recursively (if recursive = TRUE). See
    #' #'   vignette("persephone-hierarchical") for more details.
    #' #' @param context passed as the `context` argument of [x13_fast()] or
    #' #'   [tramoseats_fast()], a list of external regressors (calendar or other) to be used for estimation.
    #' #' @param userdefined additional outputs to generate while running. See
    #' #'   [x13_fast()] and [tramoseats_fast()].
    #' #' @param spec specifications generated by `x13_spec()` or
    #' #'   `tramoseats_spec()`.
    #' #' @param recursive apply this setting to all subseries as well?
    #' #' @param component which component to modify.
    #' setOptions = function(context = NA, userdefined = NA, spec = NA, recursive = TRUE,
    #'                       component = "") {
    #'   if (component != "") {
    #'     root <- self$getComponent(component)
    #'     return(root$setOptions(context, userdefined, spec, recursive))
    #'   }
    #'   super$super2()$setOptions(context, userdefined, spec, recursive)
    #'   if (recursive)
    #'     lapply(self$components, function(x) {
    #'       x$setOptions(context, userdefined, spec, recursive)
    #'     })
    #'   invisible(NULL)
    #' },
    #' @description iterate over all components
    #' @details this functin is similar to `lapply()` in the sense that it
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
      res <- c(super$super2()$iterate(fun), comp)
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
    #' @description change all or some parameters of components
    #' @details this functions provides the possibility to update
    #' parameters of one or more persephone single objects
    #'
    #' @param component character vector with names of the components
    #' for which the parameters should be changed. If NULL (default) the
    #' parameters of all components will be changed
    #' @param ... named arguments to be changed
    updateParams = function(component = NULL, iterate = FALSE, ...) {
      # if(is.null(component) || component ==""){
      #   private$updateParamsDirect(...)
      # }
      # super$updateParams(component,...)
      # Gezielt ein Sub-Component updaten
      if (nzchar(component)) {
        root <- self$getComponent(component)
        root$updateParams(iterate = iterate, ...)   # rekursives Verhalten übernimmt der Teilbaum selbst
        return(invisible(NULL))
      }
      # Direkt für die Aggregatreihe:
      # brauche updateParamsDirect nicht mehr
      private$updateFun(
        name      = private$template,
        freq      = frequency(private$ts_internal),
        init_spec = private$params_internal,
        spec_list = NULL,      # wird aus ... erzeugt
        ...
      )
      # Optional rekursiv:
      if (isTRUE(iterate)) {
        lapply(self$components, function(node) {
          node$updateParams(   iterate = TRUE,
                               ...)
          invisible(NULL)
        })
      }
      invisible(NULL)

    },
    #' #' @description fix the arima model(s)
    #' #' @param component character vector with names of the components
    #' #' for which the parameters should be changed. If NULL (default) the
    #' #' parameters of all components will be changed
    #' #' @param verbose if TRUE the changed parameters will be reported
    #' fixModel = function(component = NULL, verbose = FALSE) {
    #'   if(is.null(component) || component ==""){
    #'     super$super2()$fixModel(verbose = verbose)
    #'   }
    #'   if(!is.null(component)){
    #'     lapply(self$components[component],function(x)x$fixModel(verbose = verbose))
    #'   }else{
    #'     lapply(self$components,function(x)x$fixModel(verbose = verbose))
    #'   }
    #'   return(invisible(NULL))
    #' },
    #' #' @description fix the automatically detected outliers and the
    #' #' span to find new automatically detected outliers
    #' #' @param component character vector with names of the components
    #' #' for which the parameters should be changed. If NULL (default) the
    #' #' parameters of all components will be changed
    #' #' @param timespan number of months from the end of the time series
    #' #' where outliers are not fixed
    #' #' @param verbose if TRUE the changed parameters will be reported
    #' fixOutlier = function(component = NULL, timespan =12, verbose = FALSE) {
    #'   if(is.null(component) || component ==""){
    #'     super$super2()$fixOutlier(timespan = timespan,
    #'                               verbose = verbose)
    #'   }
    #'   if(!is.null(component)){
    #'     lapply(self$components[component],function(x)x$fixOutlier(timespan = timespan,
    #'                                                               verbose = verbose))
    #'   }else{
    #'     lapply(self$components,function(x)x$fixOutlier(timespan = timespan,
    #'                                                    verbose = verbose))
    #'   }
    #'   return(invisible(NULL))
    #' },
    #' @description Generate a table for the eurostat quality report
    #' @param component (optional) a sub-component to create the report for
    generateQrTable = function(component = "") {
      self$iterate(generate_Qr_List, asTable = TRUE, component = component)
    }
  ),
  # ---- Active Bindings ----
  active = list(
    #' @field params of all components and the aggregated series
    params = function() {
      # c(self$private$params_internal,super$params)
      c(list(aggregate = private$params_internal), super$params)
    },
    #' @field paramsDirect params of the aggregated series
    paramsDirect = function() {
      # super$super2()$private$params_internal
      #
      # nur die Direkt-Spec der Aggregatreihe
      private$params_internal
    },
    #' @field adjustedIndirect results from the indirect adjustment where
    #'   all components are adjusted and then aggregated
    adjustedIndirect = function() {
      if (is.null(self$output))
        return(NULL)
      private$aggregate(self$components, self$weights,
                        which = "adjustedIndirect")
    },
    #' @field adjusted results from the seasonal adjustment
    adjusted = function() {
      if (is.na(self$indirect)) {
        warning("The decision between direct and indirect adjustment was not ",
                "recorded yet. \nDirect adjustment is returned.")
      } else if (self$indirect) {
        return(private$adjusted_indirect_one_step())
      }
      return(self$adjustedDirect)
    },
    #' @field forecasts get forecasts
    forecasts = function() {
      if (is.na(self$indirect)) {
        warning("The decision between direct and indirect adjustment was not ",
                "recorded yet. \nDirect forecasts are returned.")
      } else if (self$indirect) {
        return(private$forecasts_indirect_one_step())
      }
      return(self$forecastsDirect)
    },
    #' @field forecastsIndirect get forecasts according to indirect adjustments
    forecastsIndirect = function() {
      if (is.null(self$output))
        return(NULL)
      private$aggregate(self$components, self$weights,
                        which = "forecastsIndirect")
    }
  ),
  # ---- Private Methods ----
  private = list(
    template = NULL, # one of c("rsa3", "rsa0", "rsa1", "rsa2c", "rsa4", "rsa5c") -> in private
    method = NULL,# one of c("rjdemetra","x13") -> in private
    spec = NULL, # rjd3 spec object
    context = NULL,
    userdefined = NULL,
    ts_internal = NULL,       # aggregate series (sum or weighted sum)
    output_internal = NULL,   # output for direct aggregate
    params_internal = NULL,   # spec for aggregate series
    forecasts_indirect_one_step = function() {
      if (is.null(self$output))
        return(NULL)
      private$aggregate(self$components, self$weights, which = "forecasts")
    },
    adjusted_indirect_one_step = function() {
      if (is.null(self$output))
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
    aggregate = function(components, weights, which = "ts") {
      tss <- lapply(components, function(component) {
        if (which == "adjustedIndirect" &
            "persephoneSingle" %in% class(component)) {
          return(component[["adjusted"]])
        }
        if (which == "forecastsIndirect" &
            "persephoneSingle" %in% class(component)) {
          return(component[["forecasts"]])
        }
        component[[which]]
      })
      weights_ts <- NULL
      if (!is.null(weights)) {
        weights_ts <- lapply(tss, function(x) {
          x <- x * 0 + 1
          x[is.na(x)] <- 1
        })
        for (i in seq_along(tss)) {
          if (startEndAsDecimal(end(weights[, i])) <
              startEndAsDecimal(end(tss[[i]]))) {
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
      # TODO: Maybe a do.call on 4 lapplys is not the easiest option here
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
    # update parameters for direct adjustment of aggregated series
    # updateParamsDirect = function(...) {
    #
    #   methodFunction <- switch(private$method,
    #                            tramoseats = rjd3tramoseats::tramoseats_fast,
    #                            x13 = rjd3x13::x13_fast)
    #   if (is.null(self$spec)){
    #     spec <- switch(private$method,
    #                    tramoseats = tramoseats_spec(...),
    #                    x13 = x13_spec( ...))
    #   }else{
    #     if("X13"%in%class(self$spec)){
    #       spec <- x13_spec(self$spec,...)
    #     }else{
    #       spec <- tramoseats_spec(self$spec,...)
    #     }
    #
    #   }
    #  super$super2()$setOptions(spec=spec)
    #  private$params_internal <- spec
    # },

    updateFun = function(name = NULL, freq, init_spec = NULL, spec_list = NULL, ...) {
      # browser()
      # initialize basic spec
      # Wenn init_spec NULL ist, nimm das gespeicherte Spec
      if (is.null(init_spec)) {
        init_spec <- private$params_internal
      }
      # Falls das auch NULL ist (z. B. beim ersten Mal), erzeuge neues Spec
      if (is.null(init_spec)) {
        # nutze Template; falls name übergeben, verwende den
        tmpl <- if (!is.null(name)) name else private$template
        init_spec <- if (private$method == "x13") {
          rjd3x13::x13_spec(name = tmpl)
        } else {
          rjd3tramoseats::tramoseats_spec(name = tmpl)
        }
      }
      # zusätzliche Argumente aus ... holen und anwenden falls keine spec_list (bei Initialisierung von hts)
      # übergeben wird
      if(is.null(spec_list)) {
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

      # 3) Speichern & zurückgeben
      private$params_internal <- spec_new
      return(spec_new)
    },
    updateParamsDirect = function(...) {

      freq <- frequency(private$ts_internal)

      init_spec <- private$params_internal

      if (is.null(init_spec)) {
        init_spec <- if (private$method == "x13") {
          rjd3x13::x13_spec(name = private$template)
        } else {
          rjd3tramoseats::tramoseats_spec(name = private$template)
        }
      }
      # extract additional parameters from ...
      spec_list <- list(...) # extra arguments

      if(length(spec_list) > 0) {
        spec_new <- update_spec(spec = init_spec, method = private$method,
                                freq = freq, spec_list = spec_list)
      } else {
        spec_new <- init_spec
      }
      # direkt speichern, kein super$super2()$updateParams(...) mehr
      private$params_internal <- spec_new
      #super$super2()$updateParams(spec = spec_new)
      invisible(NULL)
    },
    runDirect = function(ts) {
      # if (is.null(self$spec))
      #   spec <- switch(private$method, tramoseats = tramoseats_spec(),
      #                  x13 = x13_spec())
      # else
      #   spec <- self$spec

      spec <- private$params_internal

      # Falls das auch NULL ist (z. B. beim ersten Mal), erzeuge neues Spec
      if (is.null(spec)) {
        spec <- if (private$method == "x13") {
          rjd3x13::x13_spec(name = private$template)
        } else {
          rjd3tramoseats::tramoseats_spec(name = private$template)
        }
        private$params_internal <- spec
      }

      # super$super2()$updateParams(spec=spec)

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
#' @param ... one or more objects which are either of class persephone or can be
#'   coerced to persephone objects with as_persephone. If more than one element
#'   is supplied, the underlying time series must have the same time instances.
#'   All elements supplied in ... must be named.
#' @param method specifies the method to be used for the direct adjustment of
#'   the aggregate series, i.e. "tramoseats" or "x13".
#' @param template the name of the predefined specification, i.e. one of
#' c("rsa3", "rsa0", "rsa1", "rsa2c", "rsa4","rsa5c")
#' @param context a list of external regressors (calendar or other) to be used for estimation,
#' passed as the `context` argument of [rjd3x13::x13_fast()] or [rjd3tramoseats::tramoseats_fast()]..
#' @param userdefined a character vector of user-defined variables to be included in the output,
#' passed as the `userdefined` argument to [rjd3x13::x13_fast()] or
#' [rjd3tramoseats::tramoseats_fast()].
#' @param spec  a model specification object returned by [rjd3x13::x13_spec()] or
#'   [rjd3tramoseats::tramoseats_spec()].
#' @param list a list of persephone objects as alternative input to `...`. This
#'   argument can also handle mts objects.
#' @param weights  either a vector if the same weight is used for all time
#'   points or a list of ts objects or a mts object if the weight varies for
#'   different time points. They must have the same length as the number of
#'   components.
#' @examples
#' \dontrun{
#' objX13 <- perX13(AirPassengers, "rsa3")
#'
#' ht <- perHts(a = objX13, b = objX13, method = "x13")
#'
#' ht$updateParams(easter.enabled = TRUE)
#'
#' ht$updateParams(component = "a", usrdef.outliersEnabled = TRUE,
#'                      usrdef.outliersType = c("AO","LS","LS"),
#'                      usrdef.outliersDate=c("1950-01-01","1955-04-01","1959-10-01"))
#' ht$run()
#' ht$adjustedDirect
#' ht$adjustedIndirect
#'
#' ht2 <- perHts(a = ht, b = objX13)
#' ht2$run()
#' ht2$adjustedDirect
#' ht2$adjustedIndirect
#' }
#' @export
perHts <- function(..., method = c("tramoseats", "x13"),
                   template = c("rsa3", "rsa0", "rsa1", "rsa2c", "rsa4","rsa5c"),
                   context = NULL, userdefined = NULL, spec = NULL,
                   list = NULL, weights = NULL) {
  hierarchicalTimeSeries$new(..., method = method, template = template,
                             context = context,
                             userdefined = userdefined, spec = spec,
                             list = list, weights = weights)
}

# TO DO: für Quartalsdaten erweitern - mit freq parameter statt 12 hardcoded
startEndAsDecimal <- function(x){
  x[1] + (x[2] - 1) / 12
}
#Und im aggregate()-Code:
# or (i in seq_along(tss)) {
#   # ...
#   if (endAsDecimal(end(weights[, i]), frequency(weights[, i])) <
#       endAsDecimal(end(tss[[i]]), frequency(tss[[i]]))) {
#     weights_ts[[i]] <- ts(tail(weights[, i], 1),
#                           start = start(tss[[i]]),
#                           end = end(tss[[i]]),
#                           frequency = frequency(tss[[i]]))
#   } else {
#     weights_ts[[i]] <- window(weights[, i],
#                               start = start(tss[[i]]),
#                               end = end(tss[[i]]))
#   }
#
# Wenn weights eine Liste von ts ist, setzt du ohnehin
# weights_ts <- do.call("cbind", weights) → mts,
# was frequency(weights[, i]) korrekt macht.
#
# Funktioniert template setzen schon??
