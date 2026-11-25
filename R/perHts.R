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
    #' The shorthands `outliers` and `arima` of [perX13()] are accepted as
    #' well and reach all components.
    #' @param spec a model specification object of class JD3_X13_SPEC or JD3_TRAMOSEATS_SPEC
    #'   returned by [rjd3x13::x13_spec()] or [rjd3tramoseats::tramoseats_spec()].
    #' @param weights either a vector if the same weight is used for all time
    #'   points or a list of ts objects or a mts object if the weight varies
    #'   for different time points. They must have the same length as the
    #'   number of components. In `groups` mode a named list with one entry per
    #'   aggregate is expected.
    #' @param groups named list that defines the aggregation structure: one
    #'   entry per aggregate series with the names of its sub-series, e.g.
    #'   `list(C20_21 = c("C20", "C21"), C = c("C20", "C21", "C22"))`. All
    #'   series (the sub-series and the aggregates) have to be supplied in
    #'   `...`/`list`. Aggregates are always built from their leaf series, so
    #'   an aggregate that is itself a sub-series of another aggregate is
    #'   replaced by its own sub-series; pass only the series that should be
    #'   aggregated directly to get another structure.
    #' @param td trading day regressors, see [perTd()] and the `td` argument of
    #'   [perX13()]. Either a single regressor set (a calendar, precomputed
    #'   regressors, the output of [genTd()] or a [td_context()] list) that is
    #'   used for all series, or a named list of them with one entry per series.
    #'   Sub-series that are not named in the list are adjusted without
    #'   user-defined trading day regressors.
    #' @param user further user-defined regressors of the regression model (no
    #'   trading days), see the `user` argument of [perX13()]. Either a single
    #'   set of regressors (a `ts`/`mts` object or a named list of them) used
    #'   for the aggregate series and all sub-series, or a named list with one
    #'   set per series. As in `td`, the names of the list are read as series
    #'   names when one of them names a series.
    #' @param cl_weights chain linking factors of the chain linked series, an
    #'   `mts` object with one column per series (or a named list of `ts`
    #'   objects). The factors are the levels of the chain linking, i.e. the
    #'   chain linked series is the unchained series multiplied by its factor.
    #'   Series without a column are treated as not chain linked (factor 1).
    #' @param ref_year the base year the indirectly adjusted chain linked
    #'   aggregates are re-referenced to (their mean in this year is set to 100).
    #'   Required when `cl_weights` is given.
    #' @param adjustment how the aggregate series are adjusted: `"both"` (the
    #'   default) performs the direct and the indirect adjustment, `"direct"`
    #'   only the direct adjustment and `"indirect"` only the indirect one (the
    #'   seasonal adjustment of the aggregate series themselves is skipped,
    #'   which saves a lot of computing time for many aggregates).
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
      groups = NULL,
      td = NULL,
      user = NULL,
      cl_weights = NULL,
      ref_year = NULL,
      adjustment = c("both", "direct", "indirect"),
      iterate = TRUE,
      speclist = NULL
    ) {
      #browser()
      private$method <- match.arg(method)
      adjustment <- match.arg(adjustment)
      private$adjustment <- adjustment
      self$indirect <- switch(
        adjustment,
        indirect = TRUE,
        direct = FALSE,
        NA
      )
      # template default and validation depending on method
      if (missing(template) || is.null(template)) {
        template <- "rsa3"
      }
      check_template(template, private$method)
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
          template = private$template
        )
        check_component_method(components, private$method, iterate)
      } else {
        components <- lapply(components, as.persephone)
      }
      componentsHts <- sapply(
        components,
        function(x) "hierarchicalTimeSeries" %in% class(x)
      )

      private$check_classes(components)
      names(components) <- private$coerce_component_names(components)

      # ------------------------------------------------------------
      # groups mode: several aggregates at one level, all series once
      # ------------------------------------------------------------
      if (!is.null(groups)) {
        if (any(componentsHts)) {
          stop(
            "with 'groups', all series must be single series (ts or persephone ",
            "objects), no hierarchical time series: ",
            paste(sQuote(names(components)[componentsHts]), collapse = ", "),
            call. = FALSE
          )
        }
        private$init_groups(
          components = components,
          groups = groups,
          weights = weights,
          td = td,
          user = user,
          cl_weights = cl_weights,
          ref_year = ref_year,
          spec_list = spec_list,
          iterate = iterate
        )
        invisible(NULL)
        return(invisible(NULL))
      }

      # ------------------------------------------------------------
      # classic mode: one aggregate built from the components
      # ------------------------------------------------------------
      if (!is.null(weights)) {
        if (n_weight_cols(weights) != length(components)) {
          stop(
            "If the weights argument is provided, its length must be equal to the number of components."
          )
        }
      }
      weights_ts <- list()
      if (any(componentsHts)) {
        weightsNull <- sapply(components[componentsHts], function(x) {
          is.null(x$weights)
        })
        if (any(!weightsNull)) {
          if (any(weightsNull)) {
            stop(
              "At the moment it is only supported to use weights for all components or none."
            )
          }
          for (i in which(componentsHts)) {
            weights_ts[[i]] <- ts(
              rowSums(components[[i]]$weights),
              start = start(components[[i]]$weights[, 1]),
              end = end(components[[i]]$weights[, 1]),
              frequency = frequency(components[[i]]$weights[, 1])
            )
          }
        }
      }

      if (!is.list(weights) & !is.null(weights)) {
        which_components_not_Hts <- which(!componentsHts)
        for (i in seq_along(which_components_not_Hts)) {
          j <- which_components_not_Hts[i]
          weights_ts[[j]] <- ts(
            weights[i],
            start = start(components[[j]]$ts),
            end = end(components[[j]]$ts),
            frequency = frequency(components[[j]]$ts)
          )
        }
      }

      private$tsp_internal <- private$check_time_instances(components)
      private$nodes <- components
      self$components <- components
      if (!is.null(weights) && "mts" %in% class(weights)) {
        # one column of the mts per component, kept as ts columns so that the
        # aggregation can align them to the adjusted series
        weights_ts <- do.call("cbind", lapply(seq_along(components), function(j) {
          ts(
            as.numeric(weights[, j]),
            start = start(weights),
            end = end(weights),
            frequency = frequency(weights)
          )
        }))
      }
      if (is.null(weights_ts) || length(weights_ts) == 0) {
        weights_ts <- NULL
      } else if (is.list(weights) && !"mts" %in% class(weights)) {
        # weights given as a list of ts objects: the entries of the components
        # that are no hierarchical objects are taken from that list
        notHts <- which(!componentsHts)
        if (length(weights) == length(notHts)) {
          weights_ts[notHts] <- weights
        } else {
          weights_ts[!componentsHts] <- weights
        }
        names(weights_ts) <- names(components)
        weights_ts <- do.call("cbind", weights_ts)
      } else if (!"mts" %in% class(weights)) {
        weights_ts <- do.call("cbind", weights_ts)
      }
      if (!is.null(weights_ts)) {
        colnames(weights_ts) <- names(components)
      }

      self$weights <- weights_ts
      private$ts_internal <- private$aggregate(components, self$weights)

      # the trading day regressors: a single input is used for the aggregate and
      # for all sub-series, a named list only for the series it names
      private$apply_td_classic(components, td)
      private$apply_user_classic(components, user)
      if (!is.null(private$td_internal)) {
        # create/initialise the spec of the aggregate series with the trading
        # day spec block
        private$params_internal <- private$updateFun(
          name = private$template,
          freq = frequency(private$ts_internal),
          init_spec = private$params_internal,
          spec_list = modifyList(
            private$td_internal$speclist,
            if (is.null(spec_list)) list(...) else spec_list
          )
        )
      } else {
        # create/initialise spec for aggregate series using updateFun
        private$params_internal <- private$updateFun(
          name = private$template,
          freq = frequency(private$ts_internal),
          init_spec = private$params_internal,
          spec_list = if (exists("spec_list")) spec_list else NULL,
          ...
        )
      }
      if (!is.null(private$user_internal)) {
        # the regressors of the `user` argument for the aggregate series
        private$params_internal <- spec_add_usrdefvars(
          private$params_internal,
          private$user_internal$vars
        )
      }

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
      if (!is.null(private$struct)) {
        return(private$run_groups(verbose = verbose))
      }
      ## indirect: run child components first (they fill their own outputs)
      lapply(self$components, function(component) {
        component$run(verbose = verbose)
      })
      ## direct: run aggregate using private$ts_internal (aggregated series)
      ts_to_run <- if (!is.null(self$ts)) self$ts else private$ts_internal
      if (!identical(private$adjustment, "indirect")) {
        private$runDirect(ts_to_run)
      }
      private$agg_list <- NULL
      invisible(NULL)
    },

    #' @field components the sub series of the hierarchical time series; in
    #'   `groups` mode all series of the structure, sub-series and aggregates
    components = NULL,
    #' @field weights the weights used for aggregating components
    weights = NULL,
    #' @field groups the aggregation structure of `groups` mode
    groups = NULL,
    #' @field cl_weights the chain linking factors of the chain linked series
    cl_weights = NULL,
    #' @field ref_year the base year of the indirectly adjusted chain linked
    #'   aggregates
    ref_year = NULL,
    #' @field indirect whether to use direct or indirect adjustment
    indirect = NA,

    #' @description print a hierarchical timeseries to screen
    print = function() {
      tbl <- private$print_table()
      if (all(!tbl$run)) {
        tbl <- tbl[, 1:5]
      } # hier 1:4 wenn class statt method + type
      print(tbl, right = FALSE, row.names = FALSE)
    },

    #' @description quality overview of all series as a data.frame (the table
    #'   printed by the object): one row per series, aggregates get a row with
    #'   `agg = "direct"` for their own adjustment and, if the indirect
    #'   adjustment is computed, a second row with `agg = "indirect"`; series
    #'   that are no aggregates have `agg = "none"`. `seasTest` and `tdTest`
    #'   are the p-values of the tests on residual seasonality (Qs) and on
    #'   trading day effects of the adjusted series of the row.
    qualityTable = function() {
      private$print_table()
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
      if (!is.null(private$struct)) {
        # groups mode: every series is a component and is visited exactly once
        res <- lapply(private$nodes, function(node) {
          list(value = fun(node))
        })
        return(private$convert_list(res, asTable, unnest))
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
      if (!is.null(private$struct)) {
        if (!componentId %in% names(private$nodes)) {
          stop("component '", componentId, "' not found", call. = FALSE)
        }
        return(private$nodes[[componentId]])
      }
      component_path <- strsplit(componentId, "/")[[1]]
      direct_child <- component_path[1]
      if (length(component_path) == 1) {
        return(self$components[[direct_child]])
      }
      rest <- paste(component_path[-1], collapse = "/")
      self$components[[direct_child]]$getComponent(rest)
    },

    #' @description names of the series of the object
    #' @param aggregates if TRUE, only the names of the aggregate series
    getSeries = function(aggregates = FALSE) {
      if (is.null(private$struct)) {
        return(names(private$nodes))
      }
      if (aggregates) {
        return(private$struct$aggregates)
      }
      names(private$nodes)
    },

    #' @description the directly adjusted aggregate series
    #' @param agg the name of an aggregate series (`groups` mode). If it is
    #'   omitted, the single aggregate of the object is returned.
    getAdjustedDirect = function(agg = NULL) {
      if (is.null(agg)) {
        return(self$adjustedDirect)
      }
      private$check_agg(agg)
      private$nodes[[agg]]$adjusted
    },

    #' @description the indirectly adjusted aggregate series (the chain linking
    #'   factors and the re-referencing are taken into account if they are given)
    #' @param agg the name of an aggregate series (`groups` mode). If it is
    #'   omitted, the single aggregate of the object is returned.
    getAdjustedIndirect = function(agg = NULL) {
      if (is.null(agg)) {
        return(self$adjustedIndirect)
      }
      private$check_agg(agg)
      private$indirect_agg(agg, "adjusted")
    },

    #' @description the unchained indirectly adjusted aggregate series, i.e. the
    #'   indirectly adjusted aggregate before it is chain linked again (step 3 of
    #'   the indirect adjustment of chain linked series)
    #' @param agg the name of an aggregate series
    getUnchainedIndirect = function(agg = NULL) {
      if (is.null(agg)) {
        stop(
          "'agg' is needed: the unchained aggregate is only defined in ",
          "'groups' mode",
          call. = FALSE
        )
      }
      private$check_agg(agg)
      private$indirect_agg(agg, "unchained")
    },

    #' @description the indirectly adjusted forecasts of the aggregate series
    #' @param agg the name of an aggregate series (`groups` mode). If it is
    #'   omitted, the single aggregate of the object is returned.
    getForecastsIndirect = function(agg = NULL) {
      if (is.null(agg)) {
        return(self$forecastsIndirect)
      }
      private$check_agg(agg)
      private$indirect_agg(agg, "forecasts")
    },

    #' @description change parameters of components or aggregate
    #' @param component character scalar name of the component to change; if empty, operate on aggregate
    #' @param iterate logical: if TRUE, apply parameter changes to all components recursively
    #' @param speclist list with named arguments to be changed
    #' @param spec an existing model specification (class `JD3_X13_SPEC` or
    #'   `JD3_TRAMOSEATS_SPEC`, e.g. the `params` of a run of this object):
    #'   it replaces the current specification, still updated by
    #'   `speclist`/`...`
    #' @param td trading day regressors, see the `td` argument of [perHts()]
    #' @param user further user-defined regressors, see the `user` argument of
    #'   [perHts()]
    #' @param ... named arguments to be changed (spec options forwarded to updateFun)
    updateParams = function(
      component = "",
      iterate = TRUE,
      speclist = NULL,
      spec = NULL,
      td = NULL,
      user = NULL,
      ...
    ) {
      # If a specific subcomponent is requested, forward the call to that subcomponent
      # nzchar is a fast way to find out if elements of a character vector are non-empty strings.

      if (nzchar(component)) {
        root <- self$getComponent(component)
        root$updateParams(
          iterate = iterate,
          speclist = speclist,
          spec = spec,
          td = td,
          user = user,
          ...
        ) # subtree handles recursion itself
        return(invisible(NULL))
      }

      if (!is.null(private$struct)) {
        # groups mode: the specification changes apply to all series
        if (!is.null(td) && is.list(td) && !is.null(names(td)) &&
            any(names(td) %in% names(private$nodes))) {
          private$apply_td(private$nodes, td, names(private$nodes))
        } else if (!is.null(td)) {
          private$apply_td(
            private$nodes,
            stats::setNames(
              rep(list(td), length(private$nodes)),
              names(private$nodes)
            ),
            names(private$nodes)
          )
        }
        private$apply_user(
          private$nodes,
          private$user_list(user, names(private$nodes)),
          names(private$nodes)
        )
        # the spec updates: the plain block.param arguments as well as the
        # speclist (both are combined by the nodes' own updateParams()), for
        # all series or - with iterate = FALSE - for the aggregates only
        dots <- list(...)
        if (length(speclist) > 0 || length(dots) > 0 || !is.null(spec)) {
          targets <- if (isTRUE(iterate)) {
            private$nodes
          } else {
            private$nodes[private$root_aggregates()]
          }
          lapply(targets, function(node) {
            do.call(
              node$updateParams,
              c(
                list(
                  iterate = iterate, speclist = speclist, spec = spec
                ),
                dots
              )
            )
            invisible(NULL)
          })
        }
        private$agg_list <- NULL
        return(invisible(NULL))
      }

      # Update aggregate spec using updateFun (applies ... to aggregate spec)
      if (!is.null(td)) {
        private$apply_td_classic(self$components, td)
        speclist <- modifyList(
          private$td_internal$speclist,
          if (is.null(speclist)) list() else speclist
        )
      }
      if (!is.null(user)) {
        private$apply_user_classic(self$components, user)
      }
      private$updateFun(
        name = private$template,
        freq = frequency(private$ts_internal),
        init_spec = if (is.null(spec)) private$params_internal else spec,
        spec_list = speclist,
        ...
      )
      if (!is.null(user)) {
        private$params_internal <- spec_add_usrdefvars(
          private$params_internal,
          private$user_internal$vars
        )
      }

      # If requested, apply the same changes to all components recursively
      if (isTRUE(iterate)) {
        lapply(self$components, function(node) {
          node$updateParams(iterate = TRUE, speclist = speclist,
                            spec = spec, ...)
          invisible(NULL)
        })
      }
      private$agg_list <- NULL
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
      if (!is.null(private$struct)) {
        roots <- private$root_aggregates()
        if (length(roots) == 1L) {
          return(private$nodes[[roots]]$ts)
        }
        return(do.call(
          "cbind",
          lapply(roots, function(r) private$nodes[[r]]$ts)
        ))
      }
      private$ts_internal
    },
    #' @field params of all components and the aggregated series
    params = function() {
      if (!is.null(private$struct)) {
        return(lapply(private$nodes, function(node) node$params))
      }
      c(list(aggregate = private$params_internal), super$params)
    },
    # # ' @field methodDirect SA method used for the aggregated series
    # methodDirect = function() {
    #   private$method
    # },
    #' @field paramsDirect params of the aggregated series
    paramsDirect = function() {
      if (!is.null(private$struct)) {
        roots <- private$root_aggregates()
        if (length(roots) == 1L) {
          return(private$nodes[[roots]]$params)
        }
        return(lapply(roots, function(r) private$nodes[[r]]$params))
      }
      private$params_internal
    },

    #' @field output the rjd3 output of the aggregated series
    output = function() {
      if (!is.null(private$struct)) {
        roots <- private$root_aggregates()
        if (length(roots) == 1L) {
          return(private$nodes[[roots]]$output)
        }
        return(NULL)
      }
      private$output_internal
    },

    #' @field adjustedDirect results from the direct adjustment of the
    #'   aggregate series (all aggregates as a list in `groups` mode)
    adjustedDirect = function() {
      if (!is.null(private$struct)) {
        return(stats::setNames(
          lapply(
            private$struct$aggregates,
            function(agg) private$nodes[[agg]]$adjusted
          ),
          private$struct$aggregates
        ))
      }
      if (is.null(private$output_internal)) {
        return(NULL)
      }
      private$output_internal$user_defined$sa
    },

    #' @field adjustedIndirect results from the indirect adjustment where
    #'   all components are adjusted and then aggregated
    adjustedIndirect = function() {
      if (!is.null(private$struct)) {
        return(stats::setNames(
          lapply(
            private$struct$aggregates,
            function(agg) private$indirect_agg(agg, "adjusted")
          ),
          private$struct$aggregates
        ))
      }
      private$adjusted_indirect_one_step()
    },

    #' @field adjusted results from the seasonal adjustment
    adjusted = function() {
      if (!is.null(private$struct)) {
        if (identical(private$adjustment, "direct")) {
          return(self$adjustedDirect)
        }
        return(self$adjustedIndirect)
      }
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

    #' @field forecastsDirect get forecasts of the aggregate series
    forecastsDirect = function() {
      if (!is.null(private$struct)) {
        return(lapply(
          private$struct$aggregates,
          function(agg) private$nodes[[agg]]$forecasts
        ))
      }
      private$output_internal$final$forecasts
    },

    #' @field forecastsIndirect get forecasts according to indirect adjustments
    forecastsIndirect = function() {
      if (!is.null(private$struct)) {
        return(lapply(
          private$struct$aggregates,
          function(agg) private$indirect_agg(agg, "forecasts")
        ))
      }
      private$forecasts_indirect_one_step()
    }
  ),

  # ---- Private Methods ----
  private = list(
    template = NULL, # one of c("rsa3", "rsa0", "rsa1", "rsa2c", "rsa4", "rsa5c") for x13, others for tramoseats
    method = NULL, # one of c("tramoseats","x13")
    spec_list = NULL, # specification parameters
    spec = NULL, # rjd3 spec object (if used directly)
    context = NULL, # list of external regressors (calendar or other) for
    userdefined = NULL, # additional output values from rjd3
    ts_internal = NULL, # aggregate series (sum or weighted sum)
    output_internal = NULL, # output for direct aggregate
    params_internal = NULL, # spec for aggregate series
    td_internal = NULL, # trading day regressors of the aggregate series
    user_internal = NULL, # further regressors of the aggregate series
    adjustment = "both", # one of c("both", "direct", "indirect")
    # groups mode
    nodes = NULL, # all series (sub-series and aggregates), in topological order
    struct = NULL, # list(aggregates = , leaves = ) of the groups argument
    group_weights = NULL, # list of weights, one entry per aggregate
    cl_factors = NULL, # list of chain linking factors, one entry per series
    ref_year_cl = NULL, # base year of the indirect aggregates
    agg_list = NULL, # cache of the indirect aggregates

    # -----------------------------------------------------------------------
    # groups mode
    # -----------------------------------------------------------------------
    init_groups = function(components, groups, weights, td, user, cl_weights,
                           ref_year, spec_list, iterate) {
      series <- names(components)
      if (anyDuplicated(series)) {
        stop("all series must have unique names", call. = FALSE)
      }
      struct <- cl_groups_structure(groups, series)
      # cl_groups_structure() has checked that every aggregate and every
      # sub-series is one of the supplied series
      private$nodes <- private$topological_nodes(components, struct)
      private$struct <- struct
      self$components <- private$nodes
      self$groups <- groups
      private$ref_year_cl <- private$check_ref_year(ref_year, cl_weights)
      private$cl_factors <- cl_factor_list(cl_weights, series)
      self$cl_weights <- cl_weights
      self$ref_year <- ref_year
      private$group_weights <- private$check_group_weights(
        weights,
        struct,
        private$nodes
      )
      # for one single aggregate the weights are kept in the public field as
      # in the classic mode
      self$weights <- if (length(struct$aggregates) == 1L &&
                          !is.null(private$group_weights)) {
        private$group_weights[[1L]]
      } else {
        NULL
      }
      private$tsp_internal <- private$check_time_instances(private$nodes)
      # the base year must lie inside the span of the series (tsp_internal is
      # the tsp vector c(start, end, frequency))
      if (!is.null(private$ref_year_cl)) {
        years <- floor(private$tsp_internal[1]):floor(private$tsp_internal[2])
        if (!private$ref_year_cl %in% years) {
          stop(
            "ref_year ", private$ref_year_cl, " is outside the span of the ",
            "series (", years[1], "-", utils::tail(years, 1), ")",
            call. = FALSE
          )
        }
      }

      # trading day regressors and the specification for every series
      private$apply_td(private$nodes, private$td_list(td, series), series)
      private$apply_user(private$nodes, private$user_list(user, series), series)
      if (!is.null(spec_list) && length(spec_list) > 0) {
        # as in the classic mode, the specification changes reach all series
        # only if iterate is TRUE, otherwise they apply to the aggregates only
        targets <- if (isTRUE(iterate)) {
          private$nodes
        } else {
          private$nodes[struct$aggregates]
        }
        lapply(targets, function(node) {
          do.call(node$updateParams, c(list(iterate = TRUE), spec_list))
          invisible(NULL)
        })
      }
      invisible(NULL)
    },

    # the series ordered such that every aggregate comes before the aggregates
    # it is a sub-series of (which never happens: aggregates are built from
    # leaves only), so the order of the leaves of an aggregate is respected
    topological_nodes = function(components, struct) {
      aggs <- struct$aggregates
      # an aggregate may not be a sub-series of another aggregate (they are
      # built from their leaves), so the only ordering condition is that the
      # series of the structure are all present
      missing <- setdiff(aggs, names(components))
      if (length(missing) > 0) {
        stop(
          "the aggregate series ", paste(sQuote(missing), collapse = ", "),
          " must be supplied as a series as well",
          call. = FALSE
        )
      }
      unused <- setdiff(names(components), c(aggs, unlist(struct$leaves)))
      if (length(unused) > 0) {
        warning(
          "neither sub-series nor aggregates of 'groups', and adjusted on ",
          "their own: ", paste(sQuote(unused), collapse = ", "),
          call. = FALSE
        )
      }
      components
    },

    check_agg = function(agg) {
      if (is.null(private$struct)) {
        stop(
          "'agg' is only supported in 'groups' mode",
          call. = FALSE
        )
      }
      if (!agg %in% private$struct$aggregates) {
        stop(
          "'", agg, "' is not an aggregate series of this object. Aggregates ",
          "are: ", paste(private$struct$aggregates, collapse = ", "),
          call. = FALSE
        )
      }
      invisible(TRUE)
    },

    # the aggregates that are not a sub-series of another aggregate
    root_aggregates = function() {
      if (is.null(private$struct)) {
        return(character(0))
      }
      leaves <- unlist(private$struct$leaves, use.names = FALSE)
      roots <- setdiff(private$struct$aggregates, leaves)
      if (length(roots) == 0) {
        return(private$struct$aggregates)
      }
      roots
    },

    check_ref_year = function(ref_year, cl_weights) {
      if (is.null(ref_year)) {
        if (!is.null(cl_weights)) {
          stop(
            "'ref_year' is needed when 'cl_weights' is given: the indirectly ",
            "adjusted chain linked aggregates are re-referenced to their mean ",
            "in this year",
            call. = FALSE
          )
        }
        return(NULL)
      }
      if (!is.numeric(ref_year) || length(ref_year) != 1L || is.na(ref_year)) {
        stop("'ref_year' must be a single year (a number)", call. = FALSE)
      }
      as.integer(ref_year)
    },

    # weights for every aggregate: a named list with one entry per aggregate
    # (a vector, a ts or an mts with one column per sub-series), a single
    # vector/mts used for all aggregates, or NULL for equal weights
    check_group_weights = function(weights, struct, nodes) {
      aggs <- struct$aggregates
      if (is.null(weights)) {
        return(NULL)
      }
      per_agg <- is.list(weights) && !stats::is.ts(weights) &&
        !stats::is.mts(weights) && !is.null(names(weights)) &&
        any(names(weights) %in% aggs)
      if (!per_agg) {
        # one weight set for all aggregates (only sensible for equal numbers of
        # sub-series)
        return(stats::setNames(
          rep(list(weights), length(aggs)),
          aggs
        ))
      }
      missing <- setdiff(aggs, names(weights))
      if (length(missing) > 0) {
        warning(
          "no weights are given for the aggregate(s) ",
          paste(sQuote(missing), collapse = ", "),
          "; they are aggregated with equal weights",
          call. = FALSE
        )
      }
      w <- stats::setNames(lapply(aggs, function(a) weights[[a]]), aggs)
      for (a in aggs) {
        if (is.null(w[[a]])) {
          next
        }
        n <- length(struct$leaves[[a]])
        k <- n_weight_cols(w[[a]])
        if (k != n) {
          stop(
            "the weights of the aggregate '", a, "' have ", k,
            " columns/values, but it is built from ", n,
            " sub-series (", paste(struct$leaves[[a]], collapse = ", "),
            "); aggregates are always built from their leaf series, so the ",
            "weights are the weights of the leaves",
            call. = FALSE
          )
        }
        target <- nodes[[struct$leaves[[a]][1]]]$ts
        if (stats::is.mts(w[[a]]) && !is.null(colnames(w[[a]]))) {
          cn <- unname(colnames(w[[a]]))
          leaves <- struct$leaves[[a]]
          if (identical(cn, leaves)) {
            # the columns are already in the order of the sub-series
          } else if (all(cn %in% leaves)) {
            # the columns are named by the sub-series, but in another order
            w[[a]] <- w[[a]][, leaves, drop = FALSE]
          } else if (all(grepl("^V[0-9]+$", cn))) {
            # generic column names, the order of the columns is used
          } else {
            stop(
              "the columns of the weights mts of the aggregate '", a,
              "' must be named by its sub-series (",
              paste(leaves, collapse = ", "), ")",
              call. = FALSE
            )
          }
        }
        wm <- suppressWarnings(cl_weight_matrix(w[[a]], n, target))
        rs <- if (is.null(wm)) rep(1, length(target)) else rowSums(wm)
        if (any(abs(rs - 1) > 0.02, na.rm = TRUE)) {
          warning(
            "the weights of the aggregate '", a,
            "' do not sum to 1 for all periods (range ",
            paste(round(range(rs, na.rm = TRUE), 4), collapse = "-"),
            "); they are used as given",
            call. = FALSE
          )
        }
      }
      w
    },

    # a single td input, or a named list of them, as a named list of the series
    td_list = function(td, series) {
      if (is.null(td)) {
        return(NULL)
      }
      if (is.list(td) && !is.null(names(td)) && !stats::is.ts(td) &&
          !inherits(td, "JD3_CALENDARDEFINITION")) {
        if (any(names(td) %in% series)) {
          unknown <- setdiff(names(td), series)
          if (length(unknown) > 0) {
            stop(
              "'td' refers to series that are not provided: ",
              paste(sQuote(unknown), collapse = ", "),
              call. = FALSE
            )
          }
          return(td)
        }
        # no name of the list is a series: it can only be one regressor set for
        # all series, which is a td_context() list or the output of genTd()
        is_single <- all(c("td", "context", "speclist") %in% names(td)) ||
          (length(td) == 3L && all(vapply(td, stats::is.ts, TRUE)))
        if (!is_single) {
          stop(
            "the names of the list 'td' must be the names of the series; ",
            "unknown: ", paste(sQuote(setdiff(names(td), series)),
                               collapse = ", "),
            call. = FALSE
          )
        }
      }
      stats::setNames(rep(list(td), length(series)), series)
    },

    # the `user` argument of perHts() as a named list with one set of
    # regressors per series: one set (a ts/mts object or a list of such objects)
    # is used for every series, a named list of sets only for the series it
    # names. As in td_list(), a name that names a series is read as a series
    # name, so a set of regressors whose names are series names has to be
    # passed as a ts/mts object with those column names instead.
    user_list = function(user, series) {
      if (is.null(user)) {
        return(NULL)
      }
      is_set <- function(u) {
        stats::is.ts(u) ||
          (is.list(u) && length(u) > 0 &&
             all(vapply(u, stats::is.ts, TRUE)))
      }
      if (!stats::is.ts(user) && any(names(user) %in% series)) {
        unknown <- setdiff(names(user), series)
        if (length(unknown) > 0) {
          stop(
            "'user' refers to series that are not provided: ",
            paste(sQuote(unknown), collapse = ", "),
            call. = FALSE
          )
        }
        if (!all(vapply(user, is_set, TRUE))) {
          stop(
            "every entry of the list 'user' must be a ts/mts object or a ",
            "list of such objects",
            call. = FALSE
          )
        }
        return(user[names(user) %in% series])
      }
      if (!is_set(user)) {
        stop(
          "'user' must be a ts/mts object, a list of such objects (one set of ",
          "regressors for all series) or a named list of those with one entry ",
          "per series",
          call. = FALSE
        )
      }
      stats::setNames(rep(list(user), length(series)), series)
    },

    # apply the trading day regressors of the named list `td` to the matching
    # nodes; `td` must be a named list of the series (see td_list), anything
    # else is ignored
    apply_td = function(nodes, td, names = names(nodes)) {
      private$apply_regressors(nodes, td, names, "td")
    },

    # the further user-defined regressors of the named list `user` to the
    # matching nodes; `user` must be a named list of the series (see user_list)
    apply_user = function(nodes, user, names = names(nodes)) {
      private$apply_regressors(nodes, user, names, "user")
    },

    # the regressors of one kind (`arg` is "td" or "user") from a named list of
    # the series to the nodes that the list names; a node gets them through its
    # own updateParams(), which registers them in its context and specification
    apply_regressors = function(nodes, values, names = names(nodes), arg) {
      if (is.null(values) || !is.list(values) || is.null(names(values))) {
        return(invisible(NULL))
      }
      if (!any(names(values) %in% names(nodes))) {
        return(invisible(NULL))
      }
      for (nm in intersect(names(values), names(nodes))) {
        if (is.null(values[[nm]])) {
          next
        }
        args <- list(values[[nm]])
        names(args) <- arg
        do.call(nodes[[nm]]$updateParams, args)
      }
      invisible(NULL)
    },

    run_groups = function(verbose = FALSE) {
      private$agg_list <- NULL
      indirect_only <- identical(private$adjustment, "indirect")
      for (nm in names(private$nodes)) {
        if (indirect_only && nm %in% private$struct$aggregates) {
          # the aggregate series do not need to be adjusted themselves
          next
        }
        private$nodes[[nm]]$run(verbose = verbose)
      }
      invisible(NULL)
    },

    # the indirect aggregate `agg`, `which` one of "adjusted", "forecasts" or
    # "unchained". The result is cached until the object is run or changed
    # again.
    #
    #   1. the seasonally adjusted sub-series (already computed by run())
    #   2. unchained: sa_i / factor_i
    #   3. weighted sum of the unchained sub-series
    #   4. chain linked again: * factor_agg
    #   5. re-referenced to ref_year (= 100)
    indirect_agg = function(agg, which = "adjusted") {
      key <- paste(agg, which, sep = "|")
      if (!is.null(private$agg_list[[key]])) {
        return(private$agg_list[[key]])
      }
      leaves <- private$struct$leaves[[agg]]
      src <- if (identical(which, "forecasts")) "forecasts" else "adjusted"
      sa <- lapply(leaves, function(nm) {
        out <- private$nodes[[nm]][[src]]
        if (is.null(out)) {
          stop(
            "the series '", nm, "' has no ", src,
            " series; call $run() first",
            call. = FALSE
          )
        }
        out
      })
      names(sa) <- leaves
      unchained <- Map(
        function(x, nm) cl_unchain(x, private$cl_factors[[nm]]),
        sa,
        leaves
      )
      w <- private$group_weights[[agg]]
      agg_ul <- cl_weighted_sum(
        unchained,
        w,
        warn_prefix = paste0("the weights of the aggregate '", agg, "'")
      )
      if (identical(which, "unchained")) {
        private$agg_list[[key]] <- agg_ul
        return(agg_ul)
      }
      out <- cl_rechain(agg_ul, private$cl_factors[[agg]])
      if (!is.null(private$ref_year_cl) && !is.null(private$cl_factors)) {
        out <- cl_rereference(out, private$ref_year_cl)
      }
      private$agg_list[[key]] <- out
      out
    },

    # -----------------------------------------------------------------------
    # classic mode
    # -----------------------------------------------------------------------
    # have all leaf series been run? (a nested object in "indirect" mode has no
    # own output, but its sub-series do)
    all_components_run = function(components) {
      all(vapply(components, function(component) {
        if ("hierarchicalTimeSeries" %in% class(component)) {
          if (!is.null(component$groups) && isTRUE(component$indirect)) {
            # groups mode in indirect adjustment: the aggregates are not
            # adjusted themselves, so only the other series need to be run
            run_ids <- setdiff(
              component$getSeries(),
              component$getSeries(aggregates = TRUE)
            )
            return(all(vapply(
              run_ids,
              function(id) !is.null(component$getComponent(id)$output),
              TRUE
            )))
          }
          private$all_components_run(component$components)
        } else {
          !is.null(component$output)
        }
      }, TRUE))
    },

    forecasts_indirect_one_step = function() {
      if (!private$all_components_run(self$components)) {
        return(NULL)
      }
      private$aggregate(self$components, self$weights, which = "forecasts")
    },

    adjusted_indirect_one_step = function() {
      if (!private$all_components_run(self$components)) {
        return(NULL)
      }
      private$aggregate(self$components, self$weights, which = "adjusted")
    },

    # the trading day regressors of the classic mode: a single input is used for
    # the aggregate series and for all sub-series, a named list only for the
    # sub-series it names
    apply_td_classic = function(components, td) {
      if (is.null(td)) {
        return(invisible(NULL))
      }
      if (is.list(td) && !is.null(names(td)) && !stats::is.ts(td) &&
          any(names(td) %in% names(components))) {
        unknown <- setdiff(names(td), names(components))
        if (length(unknown) > 0) {
          stop(
            "'td' refers to components that are not provided: ",
            paste(sQuote(unknown), collapse = ", "),
            call. = FALSE
          )
        }
        private$apply_td(components, td, names(components))
        return(invisible(NULL))
      }
      private$td_internal <- resolve_td_input(private$ts_internal, td, private$method)
      private$apply_td(
        components,
        stats::setNames(rep(list(td), length(components)), names(components)),
        names(components)
      )
      invisible(NULL)
    },

    # the further user-defined regressors of the classic mode: a single set is
    # used for the aggregate series and for all sub-series, a named list only
    # for the sub-series it names. The regressors of the aggregate series are
    # kept in user_internal, which adds them to the specification of the
    # aggregate series (see runDirect and the initialize method)
    apply_user_classic = function(components, user) {
      if (is.null(user)) {
        return(invisible(NULL))
      }
      if (!stats::is.ts(user) && is.list(user) && !is.null(names(user)) &&
          any(names(user) %in% names(components))) {
        unknown <- setdiff(names(user), names(components))
        if (length(unknown) > 0) {
          stop(
            "'user' refers to components that are not provided: ",
            paste(sQuote(unknown), collapse = ", "),
            call. = FALSE
          )
        }
        private$apply_user(components, user, names(components))
        return(invisible(NULL))
      }
      private$user_internal <- user_context(private$ts_internal, user)
      private$context <- context_add(
        private$context,
        private$user_internal$variables
      )
      if (!is.null(private$td_internal)) {
        # the aggregate series itself is estimated with both kinds of
        # regressors, which tramoseats does not do
        check_user_td_combination(private$method, td = TRUE, user = TRUE)
      }
      private$apply_user(
        components,
        stats::setNames(rep(list(user), length(components)), names(components)),
        names(components)
      )
      invisible(NULL)
    },

    check_classes = function(components) {
      lapply(components, function(component) {
        stopifnot(is.persephone(component))
      })
    },

    print_table = function() {
      private$agg_table(self$iterate(printDiagnostics, asTable = TRUE))
    },

    # quality overview with the agg column and the tests on the adjusted
    # series: series that are no aggregates get agg = "none"; an aggregate
    # gets a row with agg = "direct" (its own adjustment) and, if the indirect
    # adjustment is computed, a second row with agg = "indirect" that is
    # tested on residual seasonality and trading day effects. With
    # adjustment = "indirect" the aggregates are not adjusted themselves and
    # get only the indirect row.
    agg_table = function(tbl) {
      aggs <- directAggIds(self)
      only_indirect <- identical(private$adjustment, "indirect")
      tbl$agg <- ifelse(
        tbl$component %in% aggs,
        if (only_indirect) "indirect" else "direct",
        "none"
      )
      tbl$type[tbl$component %in% aggs] <- "aggregate"
      series <- directSeriesMap(self)
      idx <- match(tbl$component, names(series))
      tests <- lapply(idx, function(i) {
        if (is.na(i)) {
          return(c(seasTest = NA_real_, tdTest = NA_real_))
        }
        adjustedTests(series[[i]])
      })
      tbl$seasTest <- vapply(tests, function(t) unname(t[["seasTest"]]), 0)
      tbl$tdTest <- vapply(tests, function(t) unname(t[["tdTest"]]), 0)
      tbl <- tbl[, c(
        "component", "run", "method", "type", "agg", "seasonality",
        "logTransform", "arimaMdl", "nOutliers", "nAutoout", "nRecentout",
        "Q", "Q2", "TotalQ",
        "seasTest", "tdTest"
      ), drop = FALSE]
      if (identical(private$adjustment, "direct")) {
        return(tbl)
      }
      ind_series <- indirectSeriesMap(self)
      jdx <- match(tbl$component, names(ind_series))
      rows <- lapply(seq_len(nrow(tbl)), function(i) {
        row <- tbl[i, , drop = FALSE]
        s <- if (is.na(jdx[i])) NULL else ind_series[[jdx[i]]]
        if (is.null(s)) {
          # the indirect aggregate is not available (the sub-series are not
          # run yet): keep the direct row
          return(row)
        }
        ind <- indirectAggDiagnostics(tbl$component[i], s)
        if (only_indirect) {
          return(ind)
        }
        rbind(row, ind)
      })
      do.call(rbind, rows)
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
      # For each component, return the appropriate series (ts / adjusted / forecasts).
      # If a component is itself a hierarchicalTimeSeries, compute its aggregated
      # series recursively by calling private$aggregate on its subcomponents.
      tss <- lapply(components, function(component) {
        # If component is hierarchical, compute aggregate from its children
        if ("hierarchicalTimeSeries" %in% class(component)) {
          if (!is.null(component$groups)) {
            # groups mode: the aggregate of the object is the (sum of the)
            # root aggregates; for the adjusted and forecast series the
            # indirect versions of the roots are used
            roots <- cl_groups_roots(component$groups)
            src <- switch(
              which,
              adjusted = "adjustedIndirect",
              forecasts = "forecastsIndirect",
              adjustedIndirect = "adjustedIndirect",
              forecastsIndirect = "forecastsIndirect",
              NULL
            )
            vals <- if (is.null(src)) {
              lapply(roots, function(r) component$getComponent(r)[["ts"]])
            } else {
              all_vals <- component[[src]]
              lapply(roots, function(r) all_vals[[r]])
            }
            if (length(vals) == 1L) {
              return(vals[[1L]])
            }
            return(private$aggregate_ts0(vals, NULL))
          }
          # component$components and component$weights are public, use them
          return(private$aggregate(
            component$components,
            component$weights,
            which = which
          ))
        }

        # For leaf-level persephoneSingle objects, handle special which-cases:
        if (
          which == "adjustedIndirect" &&
            "persephoneSingle" %in% class(component)
        ) {
          return(component[["adjusted"]])
        }
        if (
          which == "forecastsIndirect" &&
            "persephoneSingle" %in% class(component)
        ) {
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
          if (
            startEndAsDecimal(
              end(weights[, i]),
              freq = frequency(weights[, i])
            ) <
              startEndAsDecimal(end(tss[[i]]), freq = frequency(tss[[i]]))
          ) {
            weights_ts[[i]] <- ts(
              tail(weights[, i], 1),
              start = start(tss[[i]]),
              end = end(tss[[i]]),
              frequency = frequency(tss[[i]])
            )
          } else {
            weights_ts[[i]] <- window(
              weights[, i],
              start = start(tss[[i]]),
              end = end(tss[[i]])
            )
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
      out <- do.call(
        "cbind",
        lapply(
          lapply(1:ncol(ts_vec[[1]]), function(ind) {
            lapply(ts_vec, function(x) x[, ind])
          }),
          private$aggregate_ts0,
          weights_ts = weights_ts
        )
      )
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
        out <- ts(
          rowSums(ts_vecX),
          start = start(ts_vecX),
          frequency = frequency(ts_vecX)
        )
      }
      return(out)
    },

    coerce_component_names = function(components) {
      lapply(seq_along(components), function(i) {
        parname <- names(components)[i]
        if (!is.null(parname) && parname != "") {
          return(parname)
        } else {
          stop("all components in 'hierarchicalTimeSeries' must be named")
        }
      })
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
          spec = init_spec,
          method = if (private$method == "x13") "x13" else "tramoseats",
          freq = freq,
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
      spec <- private$params_internal
      if (is.null(spec)) {
        spec <- if (private$method == "x13") {
          rjd3x13::x13_spec(name = private$template)
        } else {
          rjd3tramoseats::tramoseats_spec(name = private$template)
        }
        private$params_internal <- spec
      }

      methodFunction <- switch(
        private$method,
        tramoseats = rjd3tramoseats::tramoseats_fast,
        x13 = rjd3x13::x13_fast
      )

      # the trading day regressors of the aggregate series (classic mode with
      # td) are registered in their own modelling context, which is combined
      # with the context given by the user (which also holds the regressors of
      # the `user` argument)
      context <- context_add(
        private$context,
        private$td_internal$context$variables
      )

      private$output_internal <- check_td_output(
        methodFunction(
          ts = private$ts_internal,
          spec = private$params_internal,
          userdefined = private$userdefined,
          context = context
        ),
        !is.null(private$td_internal)
      )
    }
  )
)

#' Define a hierarchical time series
#'
#' Combine mutliple objects of persephone objects into a new persephone object.
#' The resulting time series can perform direct and indirect adjustments.
#'
#' Two structures are supported. Without `groups`, the object holds the
#' sub-series and builds their aggregate itself; hierarchies with more than one
#' level are created by nesting `perHts()` calls. With `groups`, all series
#' (the sub-series and the aggregates) are supplied at one level and their
#' aggregation structure is given explicitly, which allows several aggregates
#' and sub-aggregates, each with its own weights and chain linking factors.
#'
#' If the series are chain linked, the indirect adjustment cannot be obtained by
#' summing up the adjusted chain linked sub-series. It is computed in the
#' unchained space instead: the adjusted sub-series are unchained by their chain
#' linking factors, weighted together, chain linked again with the factor of the
#' aggregate and finally re-referenced to the base year (`ref_year`). See the
#' examples.
#'
#' Printing an object shows a quality overview of all series (also available as
#' a data.frame via `$qualityTable()`). Series that are no aggregates have
#' `agg = "none"`; an aggregate is shown with a row `agg = "direct"` for its
#' own adjustment and, if the indirect adjustment is computed, with a second
#' row `agg = "indirect"` (with `adjustment = "indirect"` only the indirect
#' row is shown). On the indirect row the indicators of a model fit are not
#' available, but `seasTest` and `tdTest` give the p-values of the tests on
#' residual seasonality (Qs) and on trading day effects of the adjusted series
#' of the row; these two tests are also shown for the directly adjusted
#' aggregates and for the series that are no aggregates. They allow choosing
#' between the direct and the indirect adjustment for every aggregate.
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
#' The shorthands `outliers` and `arima` described in [perX13()] are accepted
#' as well and reach all components.
#' @param spec a model specification object of class JD3_X13_SPEC or JD3_TRAMOSEATS_SPEC
#'   returned by [rjd3x13::x13_spec()] or [rjd3tramoseats::tramoseats_spec()].
#' @param weights either a vector if the same weight is used for all time
#'   points or a list of ts objects or a mts object if the weight varies for
#'   different time points. They must have the same length as the number of
#'   components.
#'
#'   In `groups` mode, one weight set per aggregate is expected: a named list
#'   with the names of the aggregates, every entry a vector of constant weights,
#'   a `ts` or an `mts` with one column per sub-series (in the order of
#'   `groups`) for time varying weights. Weights of a lower frequency (e.g.
#'   annual weights for monthly series) are repeated within their periods, and
#'   weights that do not cover the whole span of the series are extended by
#'   their first and last value. The weights are **not** normalized: the
#'   indirect aggregate is the weighted sum of its (unchained) sub-series, so
#'   they should sum to 1.
#' @param groups named list that defines the aggregation structure of `groups`
#'   mode, one entry per aggregate series with the names of its sub-series, e.g.
#'   `list(C20_21 = c("C20", "C21"), C = c("C20", "C21", "C22"))`. Every series
#'   has to be supplied as a series of the object, the aggregates as well: they
#'   are needed for the direct adjustment and for their chain linking factors.
#'   Aggregates are always built from their leaf series, i.e. an aggregate that
#'   is listed as a sub-series of another aggregate is replaced by its own
#'   sub-series. To aggregate an aggregate directly instead, do not list its own
#'   sub-series in `groups` (then it is a leaf of the structure).
#' @param td trading day regressors, see [perTd()] and the `td` argument of
#'   [perX13()]. Either one regressor set for all series, or a named list with
#'   one entry per series (only the series named in it get trading day
#'   regressors). Every entry can be a national calendar
#'   ([rjd3toolkit::national_calendar()]), precomputed regressors as a `mts`
#'   object, the output of [genTd()] or a [td_context()] list.
#' @param user further user-defined regressors of the regression model (no
#'   trading days), see the `user` argument of [perX13()]. Either one set of
#'   regressors for all series (a `ts`/`mts` object with one regressor per
#'   column, or a named list of such objects), or a named list with one set per
#'   series. In the classic mode (without `groups`) a single set is also used
#'   for the aggregate series itself.
#' @param cl_weights chain linking factors of the chain linked series, as an
#'   `mts` object with one column per series or as a named list of `ts` objects.
#'   The factor is the level of the chain linking: the chain linked series is
#'   the unchained series multiplied by its factor. Series without an entry are
#'   treated as not chain linked (factor 1). The factors are only used for the
#'   indirect adjustment.
#' @param ref_year the base year of the indirectly adjusted chain linked
#'   aggregates: they are re-referenced such that their mean in this year is
#'   100. Required when `cl_weights` is given, ignored otherwise.
#' @param adjustment how the aggregate series are adjusted. `"both"` (the
#'   default) computes the direct and the indirect adjustment, `"direct"` only
#'   the direct adjustment and `"indirect"` only the indirect adjustment; in the
#'   latter case the aggregate series are not adjusted themselves, which saves a
#'   lot of computing time if there are many aggregates.
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
#'
#' # several aggregates with their own weights and chain linking factors:
#' # C20 and C21 build C20_21, and together with C22 they build C
#' hts <- perHts(
#'   list = cbind(C20 = y20, C21 = y21, C22 = y22, C20_21 = y2021, C = yC),
#'   method = "x13", template = "rsa3",
#'   groups = list(C20_21 = c("C20", "C21"), C = c("C20", "C21", "C22")),
#'   weights = list(C20_21 = w20_21, C = wC),
#'   cl_weights = cbind(C20 = lf20, C21 = lf21, C22 = lf22,
#'                      C20_21 = lf2021, C = lfC),
#'   ref_year = 2021,
#'   td = list(C20 = td20, C21 = td21),
#'   adjustment = "indirect"
#' )
#' hts$run()
#' hts$getAdjustedIndirect("C20_21")
#' hts$getUnchainedIndirect("C")
#' hts$adjustedIndirect
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
  groups = NULL,
  td = NULL,
  user = NULL,
  cl_weights = NULL,
  ref_year = NULL,
  adjustment = c("both", "direct", "indirect"),
  iterate = TRUE
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
    groups = groups,
    td = td,
    user = user,
    cl_weights = cl_weights,
    ref_year = ref_year,
    adjustment = adjustment,
    iterate = iterate
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
