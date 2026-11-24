#' Fix outliers in the model
#'
#' @description Fixes the outliers of a run to prevent their re-detection in
#'   the next run: they become pre-specified outliers of the specification and
#'   the automatic outlier detection is restricted to the last observations of
#'   the series. This is the production pattern for the monthly re-estimation:
#'   the outliers found so far are kept as they are, only the end of the
#'   series is checked for new ones.
#'
#' @details
#' Every outlier of the estimated model (an automatically detected one as well
#' as one that was already fixed in an earlier call or given to the `outliers`
#' argument) is written to the specification as a pre-specified outlier
#' (`addout.type`/`addout.date`); such outliers are part of the regression
#' model of every later run and are never dropped by the outlier test. The
#' automatic detection is then restricted to the last `timespan` observations
#' (`setout.span.type = "From"` and `setout.d0`); pre-specified outliers
#' outside this span are not affected. Called again after a run with new data,
#' the function fixes exactly the outliers of that run - the historical ones
#' (which are pre-specified) together with the new ones found at the end.
#'
#' `fixModel()` fixes the ARIMA model in the same spirit, see [fixModel()].
#'
#' @param x a persephone object; for an [perHts()] object every component
#'   that has been adjusted (the sub-series as well as the directly adjusted
#'   aggregates) is fixed on its own
#' @param timespan number of months from the end of the time series where
#'   outliers are not fixed (they are searched again in the next run)
#' @param verbose if TRUE the changed parameters will be reported
#'
#' @export
fixOutlier = function(x, timespan = 12, verbose = FALSE) {
  if (is.null(x$output)) {
    warning("not run yet.")
    return(invisible(NULL))
  }
  if (inherits(x, "hierarchicalTimeSeries")) {
    # every node that was adjusted itself (sub-series, directly adjusted
    # aggregates, nested objects) is fixed on its own; the aggregates that
    # are only built indirectly have no output and are skipped
    for (nm in x$getSeries()) {
      component <- x$getComponent(nm)
      if (!is.null(component$output)) {
        fixOutlier(component, timespan = timespan, verbose = verbose)
      }
    }
    return(invisible(NULL))
  }
  otl <- getOutliers(x)
  from <- detection_from(x$ts, timespan)
  if (is.data.frame(otl) && nrow(otl) > 0) {
    if (verbose) {
      for (i in seq_len(nrow(otl))) {
        message(otl$type[i], " outlier saved at ", format(otl$date[i]), ".")
      }
    }
    x$updateParams(
      addout.type = otl$type,
      addout.date = otl$date,
      setout.span.type = "From",
      setout.d0 = from
    )
  } else {
    if (verbose) {
      message("No automatic outliers found.")
    }
    x$updateParams(setout.span.type = "From", setout.d0 = from)
  }
  invisible(NULL)
}

# the date the automatic outlier detection starts at: the observation
# timespan periods before the end of the series (the last timespan
# observations stay open for re-detection)
detection_from = function(ts, timespan) {
  tt <- stats::time(ts)
  freq <- stats::frequency(ts)
  from <- tt[max(1L, length(tt) - timespan + 1L)]
  year <- floor(from)
  period <- round((from - year) * freq) + 1  # 1 .. freq
  month <- if (freq == 12) {
    period
  } else if (freq == 4) {
    3 * (period - 1) + 1
  } else {
    1
  }
  as.Date(sprintf("%04d-%02d-01", year, month), format = "%Y-%m-%d")
}
