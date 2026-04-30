#' Generate diagnostics table for persephone objects (internal)
#'
#' @param x an object of class \code{\link{persephone}}
#' @return A data.frame with one row containing diagnostic measures
#'
#' @details
#' The function classifies various diagnostic statistics into quality categories:
#' \itemize{
#'   \item \strong{Normality/Independence tests:} "Good" (p ≥ 0.1), "Uncertain" (0.01 ≤ p < 0.1),
#'         "Bad" (0.001 ≤ p < 0.01), "Severe" (p < 0.001)
#'   \item \strong{Seasonality/TD tests:} "Good" (p ≥ 0.1), "Uncertain" (0.05 ≤ p < 0.1),
#'         "Bad" (0.01 ≤ p < 0.05), "Severe" (p < 0.01)
#'   \item \strong{Outliers:} "Good" (< 3\%), "Uncertain" (3-5\%), "Severe" (> 5\%)
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
#'   \item{TD_Ftest}{P-value of regression F-test for trading days}
#'   \item{logTransform}{Logical indicating if log transformation was applied}
#'   \item{arimaMdl}{ARIMA model specification as string}
#'   \item{nOutliers}{Classification of outlier proportion}
#'   \item{Q}{M-statistics Q value}
#'   \item{Q2}{M-statistics Q-M2 value}
#'   \item{TotalQ}{Overall quality summary}
#' }
#'
#' @keywords internal
generate_diagnostics_table <- function(x) {
  #browser()
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
      combSA_last = NA,
      combSA_all = NA,
      combI_all = NA,
      combI_last = NA,
      fTestSA_td = NA,
      TD_Ftest = NA,
      logTransform = NA,
      arimaMdl = NA,
      nOutliers = NA,
      Q = NA,
      Q2 = NA,
      TotalQ = NA
    ))
  }

  userdef <- x$output$user_defined
  preproc <- x$output$preprocessing

  bpbdbq <- paste0(
    "(",
    userdef$arima.p,
    " ",
    userdef$arima.d,
    " ",
    userdef$arima.q,
    ")",
    "(",
    userdef$arima.bp,
    " ",
    userdef$arima.bd,
    " ",
    userdef$arima.bq,
    ")"
  )

  classify_norm_indep <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) {
      return("Undefined")
    }
    p <- as.numeric(p[1])
    if (p < 0.001) {
      "Severe"
    } else if (p < 0.01) {
      "Bad"
    } else if (p < 0.1) {
      "Uncertain"
    } else {
      "Good"
    }
  }

  classify_seas_td <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) {
      return("Undefined")
    }
    p <- as.numeric(p[1])
    if (p < 0.01) {
      "Severe"
    } else if (p < 0.05) {
      "Bad"
    } else if (p < 0.1) {
      "Uncertain"
    } else {
      "Good"
    }
  }

  classify_outl <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) {
      return("Undefined")
    }
    p <- as.numeric(p[1])
    if (p < 0.03) {
      "Good"
    } else if (p < 0.05) {
      "Uncertain"
    } else {
      "Severe"
    }
  }

  classify_p <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) {
      return("Undefined")
    }
    p <- as.numeric(p[1])
    if (p < 0.05) {
      "Good"
    } else {
      "Bad"
    }
  }

  qStat <- x$output$mstats$q
  qStat2 <- x$output$mstats$qm2
  out_p <- userdef$regression.nout / length(x$ts)
  fTest <- userdef$`regression.td-ftest`$pvalue[1]

  data.frame(
    run = !is.null(x$output),
    method = x$method,
    type = type,
    seasonality = userdef$`diagnostics.seas-si-combined`,
    normality = classify_norm_indep(userdef$residuals.doornikhansen$pvalue),
    independence = classify_norm_indep(userdef$residuals.lb$pvalue),
    QS_seas = classify_seas_td(userdef$`diagnostics.seas-sa-qs`$pvalue),
    F_seas = classify_seas_td(userdef$`diagnostics.seas-sa-f`$pvalue),
    combSA_all = classify_p(userdef$`diagnostics.td-sa-all`$pvalue),
    combSA_last = classify_p(userdef$`diagnostics.td-sa-last`$pvalue),
    combI_all = classify_p(userdef$`diagnostics.td-i-all`$pvalue),
    combI_last = classify_p(userdef$`diagnostics.td-i-last`$pvalue),
    fTestSA_td = classify_norm_indep(x$output$diagnostics$td.ftest.sa$pvalue),
    TD_Ftest = ifelse(is.null(fTest), NA, round(fTest, 2)),
    logTransform = preproc$description$log,
    arimaMdl = bpbdbq,
    nOutliers = classify_outl(out_p),
    Q = ifelse(is.null(qStat), NA, round(qStat, digits = 2)),
    Q2 = ifelse(is.null(qStat2), NA, round(qStat2, digits = 2)),
    TotalQ = userdef$quality.summary
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
#'   \item \strong{Quality measures:} M-statistics (Q, Q2) and overall quality
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
