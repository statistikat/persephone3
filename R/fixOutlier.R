#' Fix detected outlier locations in the model
#'
#' Adds eligible detected AO, LS and TC effects to the regression specification,
#' preserving existing manual outliers. Their locations and types become
#' explicit; their coefficients are still estimated when the model is rerun.
#'
#' @param x A fitted Persephone single series, hierarchy or batch.
#' @param timespan Number of months at the end of the observed series in which
#'   automatic detection remains allowed. Converted to observations with
#'   `ceiling(timespan * frequency / 12)`; a positive fraction of a quarterly
#'   observation protects the whole quarter. Zero fixes all detected locations
#'   and disables automatic detection. A window covering the complete series
#'   leaves the specification unchanged.
#' @param verbose If TRUE, report newly specified outliers and the detection span.
#' @param iterate If TRUE, also process hierarchy components recursively. The
#'   default changes only the direct aggregate. A batch has no aggregate, so
#'   its immediate members are always processed; this flag controls recursion
#'   within hierarchical members.
#' @return `x`, invisibly, after updating specifications in place. If any selected
#'   model has not been run, warns and returns `NULL` without changing models.
#' @details
#' The protected tail is intersected with the current estimation and detection
#' spans. The function never enables disabled detection or broadens those spans.
#' If that intersection is empty, automatic detection is disabled. Existing
#' manually specified outliers, names and fixed coefficients are preserved.
#'
#' Native RSA3 requires automatic detection over the full span. Requesting a
#' shorter detection window raises an error during preflight. Native RSA0 and
#' restricted X-13 use specifications with automatic detection disabled.
#'
#' Results remain the results of the previous fit and become stale after a
#' specification changes. Call `$run()` again to estimate the new specification.
#' Repeating the operation does not add duplicate type/date pairs. All selected
#' updates are prepared before any object is changed.
#' @export
fixOutlier <- function(x, timespan = 12, verbose = FALSE, iterate = FALSE) {
  if (!is.numeric(timespan) || length(timespan) != 1L || is.na(timespan) ||
      !is.finite(timespan) || timespan < 0) {
    stop("'timespan' must be one finite nonnegative number of months.", call. = FALSE)
  }
  if (!is.logical(iterate) || length(iterate) != 1L || is.na(iterate) ||
      !is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("'iterate' and 'verbose' must each be TRUE or FALSE.", call. = FALSE)
  }
  if (!inherits(x, "persephone")) {
    stop("'x' must be a Persephone object.", call. = FALSE)
  }

  # Deduplicate shared R6 nodes, and avoid revisiting a component cycle.
  seen <- list()
  targets <- list()
  collect <- function(node) {
    if (any(vapply(seen, identical, logical(1), y = node))) return(invisible(NULL))
    seen[[length(seen) + 1L]] <<- node
    hierarchical <- inherits(node, "hierarchicalTimeSeries")
    batch <- inherits(node, "multipleTimeSeries") && !hierarchical
    if (!batch) targets[[length(targets) + 1L]] <<- node
    if (batch || (hierarchical && iterate)) {
      for (child in node$components) collect(child)
    }
    invisible(NULL)
  }
  collect(x)
  if (any(vapply(targets, function(node) is.null(node$output), logical(1)))) {
    warning("not run yet; no outlier specifications were changed.", call. = FALSE)
    return(invisible(NULL))
  }

  prepared <- lapply(targets, prepare_fixed_outliers, timespan = timespan)
  # Public updateParams() uses the same adapter as preflight. Explicitly disable
  # its hierarchy recursion: each target has its own dates and fitted results.
  for (i in seq_along(targets)) {
    changes <- prepared[[i]]$changes
    if (length(changes)) targets[[i]]$updateParams(speclist = changes, iterate = FALSE)
    if (verbose) {
      for (text in prepared[[i]]$messages) message(text)
    }
  }
  invisible(x)
}

prepare_fixed_outliers <- function(x, timespan) {
  unchanged <- list(changes = list(), messages = character())
  frq <- stats::frequency(x$ts)
  periods <- round(as.numeric(stats::time(x$ts)) * frq)
  # Compare first to avoid overflowing the multiplication for a large but
  # otherwise valid finite window.
  protected <- if (timespan >= length(periods) * 12 / frq) length(periods) else ceiling(timespan * frq / 12)
  if (protected == length(periods)) return(unchanged)
  current <- if (inherits(x, "hierarchicalTimeSeries")) x$paramsDirect else x$params
  if (inherits(current, "persephone_native_auto_spec"))
    backend_native_refusal("fixing detected outlier locations", x$method, "automatic_spec_pinned",
                           "RSA3 requires its full detection span and has no manual outlier inputs;")
  if (inherits(current, "persephone_native_spec")) return(unchanged)
  regression_spec <- if (x$method == "x13") current$regarima else current$tramo
  detection <- regression_spec$outlier
  enabled <- if (x$method == "x13") length(detection$outliers) > 0L else isTRUE(detection$enabled)
  if (!enabled) return(unchanged)
  variable_types <- vapply(x$output$preprocessing$description$variables,
                           function(variable) variable$type, "")
  if (any(variable_types == "SO")) {
    stop("fixOutlier currently supports AO, LS and TC; a fitted SO effect requires an explicit specification update.",
         call. = FALSE)
  }

  first_protected <- if (protected == 0) tail(periods, 1L) + 1 else periods[length(periods) - protected + 1L]
  outliers <- getOutliers(x)
  old_manual <- manual_outlier_table(regression_spec)
  old_keys <- paste(old_manual$type, outlier_period_index(old_manual$date, frq))
  outlier_periods <- outlier_period_index(outliers$date, frq)
  keys <- paste(outliers$type, outlier_periods)
  add <- outliers[outliers$autoout & outlier_periods < first_protected & !keys %in% old_keys, , drop = FALSE]
  if (nrow(add)) add <- add[!duplicated(paste(add$type, add$date)), , drop = FALSE]
  changes <- list()
  messages <- character()
  if (nrow(add)) {
    changes$addout.type <- add$type
    changes$addout.date <- as.character(add$date)
    messages <- sprintf("Specified %s outlier at %s.", add$type, add$date)
  }

  estimation <- outlier_estimation_periods(x$ts, regression_spec)
  detection_span <- list(basic = regression_spec$basic, estimate = list(span = detection$span))
  allowed <- intersect(estimation, outlier_estimation_periods(x$ts, detection_span))
  allowed <- allowed[allowed >= first_protected]
  if (!length(allowed)) {
    changes$setout.outliers.type <- character()
    messages <- c(messages, "Disabled automatic outlier detection: no protected observations are eligible.")
  } else {
    from <- as.character(outlier_period_dates(min(allowed), frq))
    if (max(allowed) == tail(periods, 1L)) {
      changes$setout.span.type <- "From"
      changes$setout.d0 <- from
      messages <- c(messages, paste("Automatic outlier detection from", from))
    } else {
      until <- as.character(outlier_period_dates(max(allowed) + 1L, frq))
      changes$setout.span.type <- "Between"
      changes$setout.d0 <- from
      changes$setout.d1 <- until
      messages <- c(messages, sprintf("Automatic outlier detection from %s until %s (exclusive).", from, until))
    }
  }
  proposed <- backend_update_spec(current, x$method, frq, changes, x$backend)
  if (identical(proposed, current)) return(unchanged)
  list(changes = changes, messages = messages)
}
