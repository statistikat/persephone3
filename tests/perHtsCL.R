# =============================================================================
#  perHtsCL.R  —  batch chain-linked hierarchical SA
#
#  series     : mts — ALL series (components + aggregates + standalone)
#               columns: a, b, c, d, e, ..., m, n, ..., y, z
#
#  weights    : mts — component weights ONLY (aggregates excluded).
#               Aggregate weight = sum of its component weights.
#               columns: a, b, c, e, ..., l, m, ..., y  (no d, no z)
#               Weights should sum to 1 across components of each aggregate.
#
#  cl_weights : mts — chain-linking factors for ALL CL-involved series
#               (components + aggregates).
#               columns: a, b, c, d, m, n, ..., y, z
#
#  cl_groups  : named list — maps aggregate name -> component names
#               list(d = c("a","b","c"), z = c("m","n",...,"y"))
#
#  All column names must be consistent across series/weights/cl_weights
#  for the columns they share.
# =============================================================================

.get_userdefined_default <- function() {
  ns <- tryCatch(asNamespace("persephone3"), error = function(e) NULL)
  if (!is.null(ns) &&
      exists("userdefined_default", envir = ns, inherits = FALSE))
    return(get("userdefined_default", envir = ns, inherits = FALSE))
  if (exists("userdefined_default", inherits = TRUE))
    return(get("userdefined_default", inherits = TRUE))
  character(0)
}

# =============================================================================
#  R6 Class
# =============================================================================
hierarchicalTimeSeriesCL <- R6::R6Class(
  "hierarchicalTimeSeriesCL",
  inherit = multipleTimeSeries,

  public = list(

    #' @field components Named list of persephone objects (one per series column).
    components = NULL,
    #' @field weights Component weights mts (NO aggregate columns).
    #'   Aggregate weight = sum of its component weights.
    weights    = NULL,
    #' @field indirect Whether indirect adjustment is preferred over direct.
    indirect   = NA,
    #' @field cl_weights Chain-linking factors mts (components + aggregates).
    cl_weights = NULL,
    #' @field cl_groups Named list: aggregate_name -> character vector of
    #'   component names.
    cl_groups  = NULL,

    # -------------------------------------------------------------------------
    initialize = function(series       = NULL,
                          weights      = NULL,
                          cl_weights   = NULL,
                          cl_groups    = NULL,
                          method       = c("tramoseats", "x13"),
                          template     = NULL,
                          context      = NULL,
                          userdefined  = NULL,
                          spec         = NULL,
                          ref_year     = NULL,
                          cal_adjusted = FALSE,
                          iterate      = TRUE,
                          speclist     = NULL,
                          ...) {

      # ---- SA method & template -------------------------------------------
      private$method <- match.arg(method)
      if (missing(template) || is.null(template)) template <- "rsa3"

      x13_tmpl <- c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c")
      ts_tmpl  <- c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")

      if (private$method == "x13") {
        if (!template %in% x13_tmpl)
          stop(sprintf("Invalid template '%s' for x13. Allowed: %s",
                       template, paste(x13_tmpl, collapse = ", ")))
      } else { # tramoseats
        if (!template %in% ts_tmpl)
          stop(sprintf("Invalid template '%s' for tramoseats. Allowed: %s",
                       template, paste(ts_tmpl, collapse = ", ")))
      }

      private$template    <- template
      # keep context and userdefined in private state for runDirect
      private$context     <- context
      # userdefined_default expected to be defined elsewehere in the package
      private$userdefined <- union(userdefined, .get_userdefined_default())

      # ---- Build components from mts or named ... -------------------------
      if (!is.null(series)) {
        if (!("mts" %in% class(series)))
          stop("'series' must be an 'mts' object.")
        if (is.null(colnames(series)))
          stop("'series' must have column names.")

        components <- setNames(
          lapply(seq_len(ncol(series)), function(j) {
            as.persephone(ts(series[, j],
                             start     = start(series),
                             frequency = frequency(series)))
          }),
          colnames(series)
        )

      } else {
        dots <- base::list(...)
        if (length(dots) == 0)
          stop("Either 'series' (mts) or named components via '...' required.")
        if (is.null(names(dots)) || "" %in% names(dots))
          stop("All '...' arguments must be named.")

        # Spec argument detection (guarded for interactive source())
        sel_spec <- integer(0)
        tryCatch({
          ns <- asNamespace("persephone3")
          if (exists("specfun_list", envir = ns, inherits = FALSE) &&
              exists("p3_spec_names", envir = ns, inherits = FALSE)) {
            sf_list  <- get("specfun_list", envir = ns, inherits = FALSE)
            p3_names <- get("p3_spec_names", envir = ns, inherits = FALSE)
            for (cp in paste0(names(sf_list), "."))
              sel_spec <- c(sel_spec, which(startsWith(names(dots), cp)))
            for (cp in p3_names)
              sel_spec <- c(sel_spec, which(startsWith(names(dots), cp)))
          }
        }, error = function(e) invisible(NULL))

        sel_spec <- unique(sel_spec)
        if (length(sel_spec) > 0) dots <- dots[-sel_spec]
        if (!is.null(speclist) && length(speclist) > 0 && isTRUE(iterate)) {
          lapply(dots, function(node) {
            tryCatch(do.call(node$updateParams,
                             c(base::list(iterate = TRUE), speclist)),
                     error = function(e) invisible(NULL))
          })
        }
        components <- lapply(dots, as.persephone)
      }

      private$check_classes(components)
      private$tsp_internal <- private$check_time_instances(components)
      self$components      <- components

      # ---- Validate and store weights -------------------------------------
      # weights covers only component columns (NOT aggregate columns).
      # Validation: every component that appears in cl_groups must be in weights
      # (aggregates are intentionally absent from weights).
      if (!is.null(weights)) {
        if (!("mts" %in% class(weights)))
          stop("'weights' must be an 'mts' object.")

        # Aggregate names (keys of cl_groups) must NOT be in weights
        if (!is.null(cl_groups)) {
          agg_in_weights <- intersect(names(cl_groups), colnames(weights))
          if (length(agg_in_weights) > 0)
            warning(sprintf(
              paste0("Aggregate column(s) found in 'weights': %s. ",
                     "Aggregate weights are computed automatically as the ",
                     "sum of their component weights and will be ignored."),
              paste(agg_in_weights, collapse = ", ")))
        }

        # All weight columns must exist in series
        unknown_w <- setdiff(colnames(weights), names(components))
        if (length(unknown_w) > 0)
          stop(sprintf(
            "'weights' contains columns not in 'series': %s",
            paste(unknown_w, collapse = ", ")))

        self$weights <- weights
      }

      # ---- Validate and store cl_weights / cl_groups ----------------------
      if (!is.null(cl_weights)) {
        if (!("mts" %in% class(cl_weights)))
          stop("'cl_weights' must be an 'mts' object.")
        if (is.null(cl_groups) || !is.list(cl_groups))
          stop("'cl_groups' must be a named list when 'cl_weights' is provided.")
        if (is.null(names(cl_groups)) || any(nchar(names(cl_groups)) == 0))
          stop("All entries in 'cl_groups' must be named (aggregate = key).")

        agg_names  <- names(cl_groups)
        comp_names <- unique(unlist(cl_groups))

        # All CL component names must exist in series
        missing_comp <- setdiff(comp_names, names(components))
        if (length(missing_comp) > 0)
          stop(sprintf(
            "Components in 'cl_groups' not in 'series': %s",
            paste(missing_comp, collapse = ", ")))

        # All CL aggregate names must exist in series
        missing_agg <- setdiff(agg_names, names(components))
        if (length(missing_agg) > 0)
          stop(sprintf(
            "Aggregates in 'cl_groups' not in 'series': %s",
            paste(missing_agg, collapse = ", ")))

        # cl_weights must contain both component and aggregate columns
        all_cl_needed <- c(comp_names, agg_names)
        missing_cl <- setdiff(all_cl_needed, colnames(cl_weights))
        if (length(missing_cl) > 0)
          stop(sprintf(
            "'cl_weights' missing columns: %s",
            paste(missing_cl, collapse = ", ")))

        # cl_weights must NOT contain columns outside series
        unknown_cl <- setdiff(colnames(cl_weights), names(components))
        if (length(unknown_cl) > 0)
          stop(sprintf(
            "'cl_weights' contains columns not in 'series': %s",
            paste(unknown_cl, collapse = ", ")))

        if (is.null(ref_year))
          stop("'ref_year' must be supplied when 'cl_weights' is provided.")
        if (!is.numeric(ref_year) || length(ref_year) != 1 || is.na(ref_year))
          stop("'ref_year' must be a single numeric value.")

        private$cl_weights_mts  <- cl_weights
        private$cl_groups_list  <- cl_groups
        private$ref_year_cl     <- as.integer(ref_year)
        private$cal_adjusted_cl <- isTRUE(cal_adjusted)
        self$cl_weights         <- cl_weights
        self$cl_groups          <- cl_groups
      }

      # ---- Initialise one spec per aggregate ------------------------------
      if (!is.null(cl_groups)) {
        private$agg_specs <- setNames(
          lapply(names(cl_groups), function(nm) private$make_spec()),
          names(cl_groups)
        )
      }

      invisible(NULL)
    }, # end initialize

    # -------------------------------------------------------------------------
    run = function(verbose = FALSE) {

      # SA all individual series (components + aggregates + standalone)
      lapply(self$components, function(comp) comp$run(verbose = verbose))

      # Dieser Teil kann optional durchgeführt werden, wenn in series, die Aggregatsreihen
      # nicht enthalten sind.
      ####################################################################################
      # For each aggregate group: build aggregate ts and SA it directly
      #if (!is.null(private$cl_groups_list)) {
      #  for (agg_nm in names(private$cl_groups_list)) {
      #    comp_nms <- private$cl_groups_list[[agg_nm]]
      #    agg_ts   <- private$build_group_aggregate(comp_nms)

      #    mf <- switch(private$method,
      #                 tramoseats = rjd3tramoseats::tramoseats_fast,
      #                 x13        = rjd3x13::x13_fast)

      #    private$agg_outputs[[agg_nm]] <- mf(
      #      ts          = agg_ts,
      #      spec        = private$agg_specs[[agg_nm]],
      #      userdefined = private$userdefined,
      #      context     = private$context)
      #  }
      #}
      invisible(NULL)
    },

    # -------------------------------------------------------------------------
    print = function() {
      tbl <- private$print_table()
      print(tbl, right = FALSE, row.names = FALSE)
    },

    # -------------------------------------------------------------------------
    iterate = function(fun, asTable = FALSE, component = "", unnest = FALSE) {
      if (nzchar(component)) {
        return(self$getComponent(component)$iterate(fun, asTable))
      }
      comp <- lapply(self$components, function(comp) comp$iterate(fun))
      private$convert_list(comp, asTable, unnest)
    },

    # -------------------------------------------------------------------------
    getComponent = function(componentId) {
      if (!componentId %in% names(self$components))
        stop(sprintf("Component '%s' not found.", componentId))
      self$components[[componentId]]
    },

    # -------------------------------------------------------------------------
    updateParams = function(component = NULL, iterate = FALSE,
                            speclist = NULL, ...) {
      if (!is.null(component) && nzchar(component)) {
        self$components[[component]]$updateParams(
          iterate = iterate, speclist = speclist, ...)
        return(invisible(NULL))
      }
      # Update specs for all aggregate SA runs
      freq <- frequency(self$components[[1]]$ts)
      for (agg_nm in names(private$agg_specs)) {
        private$agg_specs[[agg_nm]] <- private$updateFun(
          name      = private$template,
          freq      = freq,
          init_spec = private$agg_specs[[agg_nm]],
          spec_list = speclist, ...)
      }
      if (isTRUE(iterate)) {
        lapply(self$components, function(node) {
          node$updateParams(iterate = TRUE, speclist = speclist, ...)
        })
      }
      invisible(NULL)
    },

    # -------------------------------------------------------------------------
    generateQrTable = function(component = "") {
      self$iterate(generate_Qr_List, asTable = TRUE, component = component)
    },

# =============================================================================
# In public = list(...) — replace the three conflicting method definitions:
# =============================================================================

    #' @description Get the directly adjusted series for an aggregate.
    #' @param agg_nm Aggregate name (key in cl_groups).
    #' @return A ts object with the directly SA'd aggregate.
    #getAdjustedDirect = function(agg_nm) {
    #  out <- private$agg_outputs[[agg_nm]]
    #  if (is.null(out)) {
    #    # Fallback: single-series mode (x/y API)
    #    if (!is.null(private$output_internal))
    #      return(private$output_internal$user_defined$sa)
    #    return(NULL)
    #  }
    #  out$user_defined$sa
    #},

    #' @description Get the indirectly adjusted series for an aggregate.
    #' @param agg_nm Aggregate name (key in cl_groups). In single-aggregate
    #'   mode (x/y API) this argument can be omitted.
    #' @return A ts object with the indirectly SA'd aggregate.
    getAdjustedIndirect = function(agg_nm = NULL) {
      # Single-aggregate mode (x/y API — no cl_groups)
      if (is.null(private$cl_groups_list)) {
        if (!is.null(private$cl_weights_mts))
          return(private$aggregate_cl_indirect_single("adjusted"))
        return(private$simple_indirect("adjusted"))
      }
      # Batch mode
      if (is.null(agg_nm))
        stop("'agg_nm' must be supplied in batch mode.")
      if (!agg_nm %in% names(private$cl_groups_list))
        stop(sprintf("'%s' not found in cl_groups.", agg_nm))
      private$aggregate_cl_indirect(agg_nm, "adjusted")
    },

    #' @description Get the indirect SA forecasts for an aggregate.
    #' @param agg_nm Aggregate name (key in cl_groups).
    getForecastsIndirect = function(agg_nm = NULL) {
      if (is.null(private$cl_groups_list)) {
        if (!is.null(private$cl_weights_mts))
          return(private$aggregate_cl_indirect_single("forecasts"))
        return(private$simple_indirect("forecasts"))
      }
      if (is.null(agg_nm))
        stop("'agg_nm' must be supplied in batch mode.")
      private$aggregate_cl_indirect(agg_nm, "forecasts")
    },


    # -------------------------------------------------------------------------
    #' @description Quality diagnostics table.
    qualityTable = function() {

      classify_norm_indep <- function(p) {
        if (is.null(p) || length(p) == 0 || is.na(p)) return("Undefined")
        p <- as.numeric(p[1])
        if      (p < 0.001) "Severe"
        else if (p < 0.01)  "Bad"
        else if (p < 0.1)   "Uncertain"
        else                 "Good"
      }
      classify_seas_td <- function(p) {
        if (is.null(p) || length(p) == 0 || is.na(p)) return("Undefined")
        p <- as.numeric(p[1])
        if      (p < 0.01) "Severe"
        else if (p < 0.05) "Bad"
        else if (p < 0.1)  "Uncertain"
        else                "Good"
      }

      all_rows <- list()

      # ---- One row per individual series ----------------------------------
      for (nm in names(self$components)) {
        type <- if (!is.null(private$cl_groups_list) &&
                    nm %in% names(private$cl_groups_list)) "direct" else "single"
        all_rows[[length(all_rows) + 1]] <- private$quality_row(
          series_name         = nm,
          type                = type,
          method              = self$components[[nm]]$method,
          out                 = self$components[[nm]]$output,
          classify_norm_indep = classify_norm_indep,
          classify_seas_td    = classify_seas_td)
      }

      # ---- Direct (aggregate SA) + indirect rows per cl_group ------------
      if (!is.null(private$cl_groups_list)) {
        for (agg_nm in names(private$cl_groups_list)) {
          comp_nms <- private$cl_groups_list[[agg_nm]]

          # Direct: SA of the built aggregate ts
          #all_rows[[length(all_rows) + 1]] <- private$quality_row(
          #  series_name         = agg_nm,
          #  type                = "direct",
          #  method              = private$method,
          #  out                 = private$agg_outputs[[agg_nm]],
          #  classify_norm_indep = classify_norm_indep,
          #  classify_seas_td    = classify_seas_td)

          # Indirect: 5-step CL procedure
          all_rows[[length(all_rows) + 1]] <- private$quality_row_indirect(
            agg_nm              = agg_nm,
            comp_nms            = comp_nms,
            classify_norm_indep = classify_norm_indep,
            classify_seas_td    = classify_seas_td)
        }
      }

      # ---- Align columns --------------------------------------------------
      all_cols <- unique(unlist(lapply(all_rows, names)))
      all_rows <- lapply(all_rows, function(df) {
        for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
        df[, all_cols, drop = FALSE]
      })

      result <- do.call(rbind, all_rows)
      rownames(result) <- NULL
      result
    }

  ), # end public

  # --------------------------------------------------------------------------
  # ACTIVE BINDINGS
  # --------------------------------------------------------------------------
  active = list(

    #' @field ts Named list of unadjusted ts (one per component).
    ts = function() lapply(self$components, function(comp) comp$ts),

    #' @field params Named list of specs (one per component).
    params = function() lapply(self$components, function(comp) comp$params)

    # NOTE: adjustedDirect, adjustedIndirect, forecastsIndirect are NOT
    # active bindings here because they require an agg_nm argument.
    # Use getAdjustedDirect(), getAdjustedIndirect(), getForecastsIndirect().
  ),

  # --------------------------------------------------------------------------
  # PRIVATE
  # --------------------------------------------------------------------------
  private = list(

    template        = NULL,
    method          = NULL,
    context         = NULL,
    userdefined     = NULL,
    ts_internal     = NULL,
    output_internal = NULL,
    params_internal = NULL,
    cl_weights_mts  = NULL,
    cl_groups_list  = NULL,
    ref_year_cl     = NULL,
    cal_adjusted_cl = FALSE,
    agg_specs       = list(),
    agg_outputs     = list(),

    # =========================================================================
    # ROBUST SCALAR EXTRACTORS
    # =========================================================================

    scalar = function(expr) {
      val <- tryCatch(expr, error = function(e) NULL)
      if (is.null(val) || length(val) == 0) return(NA_real_)
      val <- suppressWarnings(as.numeric(val[1]))
      if (is.nan(val)) NA_real_ else val
    },

    scalar_chr = function(expr) {
      val <- tryCatch(expr, error = function(e) NULL)
      if (is.null(val) || length(val) == 0) return(NA_character_)
      as.character(val[1])
    },

    # =========================================================================
    # SPEC HELPERS
    # =========================================================================

    make_spec = function() {
      if (private$method == "x13")
        rjd3x13::x13_spec(name = private$template)
      else
        rjd3tramoseats::tramoseats_spec(name = private$template)
    },

    updateFun = function(name = NULL, freq, init_spec = NULL,
                         spec_list = NULL, ...) {
      if (is.null(init_spec)) init_spec <- private$make_spec()
      if (is.null(spec_list)) spec_list <- base::list(...)
      if (length(spec_list) > 0) {
        update_spec(
          spec      = init_spec,
          method    = if (private$method == "x13") "x13" else "tramoseats",
          freq      = freq,
          spec_list = spec_list)
      } else {
        init_spec
      }
    },

    # =========================================================================
    # AGGREGATION HELPERS
    # =========================================================================
# =============================================================================
# In private = list(...) — add single-aggregate CL helper:
# =============================================================================

    # CL indirect for the original two-component (x/y) API
    # (when cl_groups is NULL but cl_weights_mts is set via cl_agg)
    aggregate_cl_indirect_single = function(which = "adjusted") {
      comp_names <- names(self$components)
      cl         <- private$cl_weights_mts

      sa_list <- setNames(lapply(comp_names, function(nm) {
        val <- self$components[[nm]][[which]]
        if (is.null(val))
          stop(sprintf("perHtsCL: '%s' has no '%s'. Call $run().", nm, which))
        val
      }), comp_names)

      ul_list <- setNames(lapply(comp_names, function(nm) {
        clw <- private$expand_weights(cl[, nm], sa_list[[nm]])
        sa_list[[nm]] / clw
      }), comp_names)

      s <- do.call(pmax, lapply(ul_list, start))
      e <- do.call(pmin, lapply(ul_list, end))
      f <- frequency(ul_list[[1]])

      if (is.null(self$weights)) {
        mat       <- do.call("cbind", lapply(ul_list,
                      function(x) window(x, start = s, end = e)))
        sa_ul_agg <- ts(rowSums(mat), start = s, frequency = f)
      } else {
        sa_ul_agg <- window(
          private$weighted_sum(ul_list, self$weights),
          start = s, end = e)
      }

      cl_agg   <- private$expand_weights(cl[, private$cl_agg_name], sa_ul_agg)
      sa_cl    <- sa_ul_agg * cl_agg
      private$rereference(sa_cl)
    },

    # Build aggregate ts for direct SA from component columns.
    # Aggregate weight = sum of component weights (from self$weights).
    # If no weights: simple sum of component series.
    build_group_aggregate = function(comp_nms) {
      tss <- setNames(
        lapply(comp_nms, function(nm) self$components[[nm]]$ts),
        comp_nms)

      if (is.null(self$weights)) {
        # Simple sum
        mat <- do.call("cbind", tss)
        return(ts(rowSums(mat), start = start(mat), frequency = frequency(mat)))
      }

      # Weighted sum using component weights
      # (aggregate weight = sum of component weights — no explicit agg column needed)
      private$weighted_sum(tss, self$weights)
    },

    # Weighted sum of a named list of ts using columns from self$weights mts.
    # Each component's weight is taken from the corresponding column.
    # The weights do NOT need to sum to 1 — they are used as-is.
    weighted_sum = function(tss, wts_mts) {
      comp_nms <- names(tss)
      s   <- do.call(pmax, lapply(tss, start))
      e   <- do.call(pmin, lapply(tss, end))
      f   <- frequency(tss[[1]])
      n   <- length(window(tss[[1]], start = s, end = e))
      out <- ts(rep(0, n), start = s, frequency = f)

      for (nm in comp_nms) {
        ser <- window(tss[[nm]], start = s, end = e)
        w   <- private$expand_weights(wts_mts[, nm], ser)
        out <- out + ser * w
      }
      out
    },

    # Expand annual weights to sub-annual frequency if needed.
    expand_weights = function(w_ts, target_ts) {
      if (frequency(w_ts) == frequency(target_ts))
        return(window(w_ts, start = start(target_ts), end = end(target_ts)))
      freq   <- frequency(target_ts)
      yr_s   <- start(target_ts)[1]
      yr_e   <- end(target_ts)[1]
      w_s    <- start(w_ts)[1]
      w_e    <- end(w_ts)[1]
      w_vals <- sapply(yr_s:yr_e, function(yr) {
        idx <- max(w_s, min(w_e, yr))
        as.numeric(window(w_ts, start = idx, end = idx))
      })
      ts(rep(w_vals, each = freq), start = c(yr_s, 1), frequency = freq)
    },

    # =========================================================================
    # CHAIN-LINKING  (steps 1-5)
    # =========================================================================

    aggregate_cl_indirect = function(agg_nm, which = "adjusted") {
      comp_nms <- private$cl_groups_list[[agg_nm]]
      cl       <- private$cl_weights_mts

      # Step 1 — retrieve SA component series
      sa_list <- setNames(lapply(comp_nms, function(nm) {
        val <- self$components[[nm]][[which]]
        if (is.null(val))
          stop(sprintf(
            "perHtsCL: '%s' has no '%s'. Call $run() first.", nm, which))
        val
      }), comp_nms)

      # Step 2 — unchain: sa_ul = sa / cl_comp
      ul_list <- setNames(lapply(comp_nms, function(nm) {
        clw <- private$expand_weights(cl[, nm], sa_list[[nm]])
        sa_list[[nm]] / clw
      }), comp_nms)

      # Step 3 — aggregate unchained SA components
      s <- do.call(pmax, lapply(ul_list, start))
      e <- do.call(pmin, lapply(ul_list, end))
      f <- frequency(ul_list[[1]])

      if (is.null(self$weights)) {
        # Simple sum
        mat       <- do.call("cbind", lapply(ul_list,
                      function(x) window(x, start = s, end = e)))
        sa_ul_agg <- ts(rowSums(mat), start = s, frequency = f)
      } else {
        # Weighted sum using component weights from self$weights
        sa_ul_agg <- window(
          private$weighted_sum(ul_list, self$weights),
          start = s, end = e)
      }

      # Step 4 — rechain: multiply by aggregate chain-linking factor
      cl_agg   <- private$expand_weights(cl[, agg_nm], sa_ul_agg)
      sa_cl    <- sa_ul_agg * cl_agg

      # Step 5 — re-reference to ref_year = 100
      private$rereference(sa_cl)
    },

    rereference = function(sa_cl) {
      ref_year   <- private$ref_year_cl
      freq       <- frequency(sa_cl)
      ref_window <- tryCatch(
        window(sa_cl, start = c(ref_year, 1), end = c(ref_year, freq)),
        error = function(e)
          stop(sprintf("perHtsCL: ref_year %d outside series range.", ref_year)))
      ref_avg <- mean(ref_window, na.rm = TRUE)
      if (is.na(ref_avg) || ref_avg == 0)
        stop(sprintf("perHtsCL: annual avg in ref_year %d is 0 or NA.", ref_year))
      sa_cl / ref_avg * 100
    },

    # =========================================================================
    # QUALITY TABLE HELPERS
    # =========================================================================

    get_n_outliers = function(out) {
      if (is.null(out)) return(NA_integer_)
      variables <- tryCatch(
        out$preprocessing$description$variables, error = function(e) NULL)
      if (is.null(variables) || length(variables) == 0) return(0L)
      vartype <- sapply(variables, function(v) v$type)
      as.integer(sum(vartype %in% c("AO", "TC", "LS")))
    },

    get_arima_mdl = function(out) {
      if (is.null(out)) return(NA_character_)
      ud <- out$user_defined
      vals <- c(private$scalar(ud$arima.p),  private$scalar(ud$arima.d),
                private$scalar(ud$arima.q),  private$scalar(ud$arima.bp),
                private$scalar(ud$arima.bd), private$scalar(ud$arima.bq))
      if (any(is.na(vals))) return(NA_character_)
      sprintf("(%g %g %g)(%g %g %g)", vals[1], vals[2], vals[3],
              vals[4], vals[5], vals[6])
    },

    quality_row = function(series_name, type, method, out,
                           classify_norm_indep, classify_seas_td) {
      na_row <- data.frame(
        series = series_name, type = type, method = method,
        logTransform = NA_character_, arimaMdl = NA_character_,
        nOutliers = NA_integer_, td.ftest = NA_character_,
        normality = NA_character_, independence = NA_character_,
        seas.qs = NA_character_, seas.f = NA_character_,
        comb.seas.si = NA_character_, td.sa.all = NA_character_,
        quality.summary = NA_character_, stringsAsFactors = FALSE)
      if (is.null(out)) return(na_row)

      ud <- out$user_defined

      log_val <- tryCatch(out$preprocessing$description$log,
                          error = function(e) NA)
      log_str <- if (is.na(log_val)) NA_character_
                 else if (log_val)   "TRUE" else "FALSE"

      comb_si_raw <- private$scalar_chr(ud$`diagnostics.seas-si-combined`)
      comb_si <- if (is.na(comb_si_raw))                          "Undefined"
                 else if (comb_si_raw %in% c("Present","Severe")) "Bad"
                 else if (comb_si_raw == "Present at 10% level")  "Uncertain"
                 else                                               "Good"

      data.frame(
        series          = series_name,
        type            = type,
        method          = method,
        logTransform    = log_str,
        arimaMdl        = private$get_arima_mdl(out),
        nOutliers       = private$get_n_outliers(out),
        td.ftest        = classify_seas_td(
          private$scalar(ud$`regression.td-ftest`$pvalue)),
        normality       = classify_norm_indep(
          private$scalar(ud$residuals.doornikhansen$pvalue)),
        independence    = classify_norm_indep(
          private$scalar(ud$residuals.lb$pvalue)),
        seas.qs         = classify_seas_td(
          private$scalar(ud$`diagnostics.seas-sa-qs`$pvalue)),
        seas.f          = classify_seas_td(
          private$scalar(ud$`diagnostics.seas-sa-f`$pvalue)),
        comb.seas.si    = comb_si,
        td.sa.all       = classify_seas_td(
          private$scalar(ud$`diagnostics.td-sa-all`$pvalue)),
        quality.summary = private$scalar_chr(ud$quality.summary),
        stringsAsFactors = FALSE)
    },

    quality_row_indirect = function(agg_nm, comp_nms,
                                    classify_norm_indep, classify_seas_td) {
      # logTransform
      log_known <- Filter(Negate(is.na), sapply(comp_nms, function(nm) {
        tryCatch(
          self$components[[nm]]$output$preprocessing$description$log,
          error = function(e) NA)
      }))
      log_summary <- if (length(log_known) == 0)      NA_character_
                     else if (all(log_known == TRUE))  "TRUE"
                     else if (all(log_known == FALSE)) "FALSE"
                     else                               "mixed"

      # nOutliers: unique names across components
      all_names <- unlist(lapply(comp_nms, function(nm) {
        out_obj <- tryCatch(getOutliers(self$components[[nm]]),
                            error = function(e) NULL)
        if (is.data.frame(out_obj) && "name" %in% names(out_obj))
          as.character(out_obj$name)
        else
          character(0)
      }))
      n_unique <- if (length(all_names) == 0) NA_integer_
                  else as.integer(length(unique(all_names)))

      # Tests on indirect SA series
      seas_qs_flag <- "Undefined"
      seas_f_flag  <- "Undefined"
      td_f_flag    <- "Undefined"

      ind_sa <- tryCatch(
        private$aggregate_cl_indirect(agg_nm, "adjusted"),
        error = function(e) NULL)

      if (!is.null(ind_sa) && !all(is.na(ind_sa))) {
        freq <- frequency(ind_sa)
        qs_r <- tryCatch(rjd3toolkit::seasonality_qs(ind_sa, period = freq),
                         error = function(e) NULL)
        if (!is.null(qs_r))
          seas_qs_flag <- classify_seas_td(private$scalar(qs_r$pvalue))
        f_r <- tryCatch(rjd3toolkit::seasonality_f(ind_sa, period = freq),
                        error = function(e) NULL)
        if (!is.null(f_r))
          seas_f_flag <- classify_seas_td(private$scalar(f_r$pvalue))
        td_r <- tryCatch(rjd3toolkit::td_f(ind_sa, nyears = 0),
                         error = function(e) NULL)
        if (!is.null(td_r))
          td_f_flag <- classify_seas_td(private$scalar(td_r$pvalue))
      }

      data.frame(
        series = agg_nm, type = "indirect", method = private$method,
        logTransform = log_summary, arimaMdl = NA_character_,
        nOutliers = n_unique, td.ftest = NA_character_,
        normality = NA_character_, independence = NA_character_,
        seas.qs = seas_qs_flag, seas.f = seas_f_flag,
        comb.seas.si = NA_character_, td.sa.all = td_f_flag,
        quality.summary = NA_character_, stringsAsFactors = FALSE)
    },

    # =========================================================================
    # MISC HELPERS
    # =========================================================================

    check_classes = function(components) {
      lapply(components, function(comp) stopifnot(is.persephone(comp)))
    },

    print_table = function() {
      self$iterate(printDiagnostics, asTable = TRUE)
    },

    check_time_instances = function(components) {
      tsps <- lapply(components, function(comp) comp$tsp)
      if (length(unique(tsps)) != 1)
        stop("All components must have the same time instances.")
      tsps[[1]]
    },

    convert_list = function(res, asTable = FALSE, unnest = FALSE) {
      if (asTable) return(asTable_nested_list(res))
      if (unnest)  return(unnest_nested_list(res))
      res
    }

  ) # end private
) # end R6Class


# =============================================================================
#  Constructor wrapper
# =============================================================================

#' Define a batch chain-linked hierarchical time series
#'
#' @param series mts with ALL series (components + aggregates + standalone).
#' @param weights Optional mts with component weights ONLY (no aggregate
#'   columns). Aggregate weight = sum of its component weights.
#'   Column names must match the corresponding component names in series.
#'   NULL = equal weights (simple sum).
#' @param cl_weights Optional mts with chain-linking factors for ALL
#'   CL-involved series (components + aggregates). Column names must match
#'   series column names.
#' @param cl_groups Named list: aggregate_name -> character vector of
#'   component names. E.g. \code{list(d = c("a","b","c"), z = c("m","n","y"))}.
#' @param method SA method: "tramoseats" or "x13".
#' @param template Predefined spec template.
#' @param context External regressors.
#' @param userdefined Additional output keys.
#' @param spec Pre-built rjd3 spec object.
#' @param ref_year Integer reference year. Required when cl_weights given.
#' @param cal_adjusted Logical (default FALSE).
#' @param iterate Logical: propagate spec to all components.
#' @param speclist Named list of spec arguments.
#' @param ... Additional named ts/persephone objects (alternative to series).
#' @return An R6 object of class hierarchicalTimeSeriesCL.
#' @export
perHtsCL <- function(series       = NULL,
                     weights      = NULL,
                     cl_weights   = NULL,
                     cl_groups    = NULL,
                     method       = c("tramoseats", "x13"),
                     template     = "rsa3",
                     context      = NULL,
                     userdefined  = NULL,
                     spec         = NULL,
                     ref_year     = NULL,
                     cal_adjusted = FALSE,
                     iterate      = TRUE,
                     speclist     = NULL,
                     ...) {
  hierarchicalTimeSeriesCL$new(
    series       = series,
    weights      = weights,
    cl_weights   = cl_weights,
    cl_groups    = cl_groups,
    method       = method,
    template     = template,
    context      = context,
    userdefined  = userdefined,
    spec         = spec,
    ref_year     = ref_year,
    cal_adjusted = cal_adjusted,
    iterate      = iterate,
    speclist     = speclist,
    ...
  )
}

