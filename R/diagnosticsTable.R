#' Generate diagnostics table for persephone objects (internal)
#'
#' @param x an object of class \code{\link{persephone}}
#' @return A data.frame with one row containing diagnostic measures
#'
#' @details
#' The function classifies various diagnostic statistics into quality categories:
#' \itemize{
#'   \item \strong{Normality/Independence tests:} "Good" (p >= 0.1), "Uncertain" (0.01 <= p < 0.1),
#'         "Bad" (0.001 <= p < 0.01), "Severe" (p < 0.001)
#'   \item \strong{Seasonality/TD tests:} "Good" (p >= 0.1), "Uncertain" (0.05 <= p < 0.1),
#'         "Bad" (0.01 <= p < 0.05), "Severe" (p < 0.01)
#'   \item \strong{TD regression F-test (\code{TD_status}):} small p indicates a
#'         significant trading day effect and thus justified trading-day
#'         regressors, i.e. the reverse direction of the residual tests:
#'         "Good" (p < 0.05), "Uncertain" (0.05 <= p < 0.1), "Bad" (p >= 0.1)
#'   \item \strong{Combined seasonality:} categorical backend outcome,
#'         preserved as text rather than interpreted as a p-value.
#'   \item \strong{Outliers:} "Good" (< 3\%), "Uncertain" (3\% to below 5\%), "Severe" (>= 5\%)
#' }
#'
#' @section Returned Columns:
#' \describe{
#'   \item{run}{Logical indicating if the model has been run}
#'   \item{method}{Seasonal adjustment method used (x13 or tramoseats)}
#'   \item{type}{Type of series ("single" or "aggregate")}
#'   \item{seasonality}{Result of combined seasonality test}
#'   \item{normality}{Classification of Doornik-Hansen normality test}
#'   \item{independence}{Classification of Ljung-Box test for independence}
#'   \item{QS_seas}{Classification of QS test for residual seasonality}
#'   \item{F_seas}{Classification of F-test for residual seasonality}
#'   \item{combSA_all}{Combined test for calendar effects in SA (all observations)}
#'   \item{combSA_last}{Combined test for calendar effects in SA (last year)}
#'   \item{combI_all}{Combined test for calendar effects in irregular (all observations)}
#'   \item{combI_last}{Combined test for calendar effects in irregular (last year)}
#'   \item{fTestSA_td}{F-test for trading day effects in SA series}
#'   \item{TD_Ftest}{P-value of regression F-test for trading days. \code{NA}
#'         if the final model contains no trading-day regressors — e.g.
#'         templates without calendar effects (\code{"rsa3"}) or when the
#'         automatic pre-test (\code{td.test = "Remove"}, as in \code{"rsa5c"})
#'         dropped them; use \code{td.test = "None"} to force their inclusion}
#'   \item{TD_status}{Classification of the regression F-test for trading days:
#'         "Good" if the trading day effect is significant (small p), "Bad" if
#'         the trading-day regressors are not justified by the data}
#'   \item{logTransform}{Logical indicating if log transformation was applied}
#'   \item{arimaMdl}{ARIMA model specification as string}
#'   \item{nOutliers_Q}{Classification of outlier proportion}
#'   \item{nOutliers}{Number of outliers in the model}
#'   \item{nAutoout}{Number of automatically detected outliers (see the
#'         \code{autoout} column of \code{\link{getOutliers}})}
#'   \item{nRecentout}{Number of outliers falling into the last observations
#'         of the series (see the \code{recentout} column of
#'         \code{\link{getOutliers}})}
#'   \item{nSigRecentout}{Number of \emph{significant} recent outliers: those
#'         recent outliers whose absolute t-statistic exceeds the model's
#'         critical value, i.e. \code{recentout} and \code{abs(t) > crit.value}
#'         in \code{\link{getOutliers}}. This is the same threshold JDemetra+
#'         uses to decide whether an effect enters the model at all, so
#'         automatically detected recent outliers essentially always count.
#'         The value differs from \code{nRecentout} mainly for manually
#'         specified recent outliers that do not reach the threshold, and for
#'         outliers with a non-estimable t-statistic (e.g. \code{coef.type =
#'         "FIXED"}), which are never counted as significant.}
#'   \item{Q}{Composite quality indicator built from the X-11 M-statistics
#'         (M1 to M11): a weighted mean of the individual statistics, each of
#'         which ranges from 0 to 3 and is accepted below 1; a value below 1
#'         indicates a satisfactory quality}
#'   \item{Q2}{The same composite computed without the M2 statistic
#'         (\code{Q-M2})}
#'   \item{nMstatOutside}{Number of X-11 M-statistics outside their acceptance
#'         limits (value above 1); statistics that could not be computed do
#'         not count, and the value is \code{NA} when no M-statistics are
#'         available at all (e.g. TRAMO-SEATS)}
#'   \item{TotalQ}{Overall quality summary (JDemetra+ node
#'         \code{quality.summary}): an aggregate of all standard diagnostic
#'         tests (residual, forecasting, outlier, spectral, seasonality and
#'         trading-day tests, plus the M-statistics Q/Q2 for X-13 or the
#'         Wiener-Kolmogorov tests for SEATS), classified as \code{Good},
#'         \code{Uncertain}, \code{Bad}, \code{Severe} or \code{Undefined}.
#'         See the
#'         [diagnostics vignette](https://statistikat.github.io/persephone3/articles/persephone-diagnostics.html)
#'         for details}
#' }
#'
#' @keywords internal
generate_diagnostics_table <- function(x) {
  type <- ifelse(grepl("Single", class(x)[1]), "single", "aggregate")

  if (is.null(x$output) | length(x$output) == 0) {
    return(data.frame(
      run = !is.null(x$output),
      method = x$method,
      type = type,
      seasonality = NA,
      normality = NA,
      independence = NA,
      QS_seas = NA,
      F_seas = NA,
      combSA_all = NA,
      combSA_last = NA,
      combI_all = NA,
      combI_last = NA,
      fTestSA_td = NA,
      TD_Ftest = NA,
      TD_status = NA,
      logTransform = NA,
      arimaMdl = NA,
      nOutliers_Q = NA,
      nOutliers = NA,
      nAutoout = NA,
      nRecentout = NA,
      nSigRecentout = NA,
      Q = NA,
      Q2 = NA,
      nMstatOutside = NA,
      TotalQ = NA
    ))
  }

  metrics <- diagnostic_metrics(x)
  out <- diagnostic_outliers(x)
  n_out <- if (is.null(out)) NA_integer_ else nrow(out)
  out_p <- if (length(x$ts)) n_out / length(x$ts) else NA_real_
  # m-statistic acceptance counts (m1..m11, accepted below 1; NA entries such as
  # M6 without a 3x5 filter are not counted). Java-lane only: native objects
  # carry no rjd3 mstats block, so mvals is empty and the count falls back to NA.
  mstats <- x$output$mstats
  mvals <- suppressWarnings(as.numeric(
    unlist(mstats[grepl("^m[0-9]{1,2}$", names(mstats))], use.names = FALSE)
  ))
  data.frame(
    run = TRUE,
    method = x$method,
    type = type,
    seasonality = metrics$seasonality$value,
    normality = metrics$normality$status,
    independence = metrics$independence$status,
    QS_seas = metrics$residual_qs$status,
    F_seas = metrics$residual_f$status,
    combSA_all = metrics$td_sa_all$status,
    combSA_last = metrics$td_sa_last$status,
    combI_all = metrics$td_i_all$status,
    combI_last = metrics$td_i_last$status,
    fTestSA_td = metrics$td_sa_f$status,
    TD_Ftest = signif(metrics$td_regression$pvalue, 3),
    TD_status = metrics$td_regression$status,
    logTransform = metrics$log,
    arimaMdl = metrics$arima,
    nOutliers_Q = diagnostic_classify(out_p, "outliers"),
    nOutliers = n_out,
    nAutoout = if (is.null(out)) NA_integer_ else sum(out$autoout),
    nRecentout = if (is.null(out)) NA_integer_ else sum(out$recentout),
    nSigRecentout = if (is.null(out)) NA_integer_ else sum(
      out$recentout & abs(out$t) > out$crit.value, na.rm = TRUE
    ),
    Q = round(metrics$q$value, 2),
    Q2 = round(metrics$q2$value, 2),
    nMstatOutside = if (all(is.na(mvals))) NA else sum(mvals > 1, na.rm = TRUE),
    TotalQ = metrics$quality$value,
    stringsAsFactors = FALSE
  )
}

#' Diagnostics table for persephone objects
#'
#' Creates a comprehensive table of diagnostic statistics and quality indicators
#' for seasonal adjustment results. For hierarchical persephone objects, the
#' function uses the iterate mechanism to process all components.
#'
#' @param x a persephone object created with \code{\link{perX13}},
#'   \code{\link{perTramo}}, or \code{\link{perHts}}
#'
#' @return A data.frame with diagnostic statistics. For hierarchical objects,
#'   includes a 'component' column identifying each series.
#'
#' @details
#' The diagnostics table provides an overview of key quality indicators:
#' \itemize{
#'   \item \strong{Residual diagnostics:} normality and independence tests
#'   \item \strong{Seasonality tests:} QS and F-tests for residual seasonality
#'   \item \strong{Calendar effects:} tests for trading day and other calendar effects
#'   \item \strong{Model information:} ARIMA specification, transformation, outliers
#'   \item \strong{Quality measures:} composite M-statistics indicators
#'   (Q, Q2), number of M-statistics outside their acceptance limits
#'   (nMstatOutside) and overall quality
#' }
#'
#' All p-values are classified into quality categories (Good, Uncertain, Bad, Severe)
#' based on commonly used thresholds in seasonal adjustment diagnostics.
#'
#' @seealso
#' \code{\link{generateQrTable}} for the Eurostat quality report
#'
#' @examples
#' \dontrun{
#' # Single series
#' obj <- perX13(AirPassengers, template = "rsa3")
#' obj$run()
#' diagnosticsTable(obj)
#'
#' # Hierarchical series
#' obj1 <- perX13(AirPassengers, "rsa3")
#' obj2 <- perTramo(AirPassengers, "rsafull")
#' ht <- perHts(x13_series = obj1, tramo_series = obj2)
#' ht$run()
#' diagnosticsTable(ht)
#'
#' # R6 method
#' obj$diagnosticsTable()
#' }
#'
#' @export
diagnosticsTable <- function(x) {
  if (!inherits(x, "persephone")) {
    stop("x must be a persephone object", call. = FALSE)
  }
  x$iterate(generate_diagnostics_table, asTable = TRUE)
}
