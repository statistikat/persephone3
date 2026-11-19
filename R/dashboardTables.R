# coefficient tables for the diagnostic dashboard: trading days (with the
# joint F test), outliers / mean / leap year and the quality indicators.
# the coefficient extraction reuses the helpers of summary.persephone()
# (sumCoefTable(), sumWaldF(), sumVarLabels(), sumTdSunday()) so that the
# tables of the dashboard and of summary() are always consistent.

# empty result of the getters when the regression does not contain the
# requested group of variables
emptyCoefTable <- function() {
  data.frame(
    Variable = character(0), Coef = numeric(0), Std.Error = numeric(0),
    t.value = numeric(0), P.value = numeric(0), Test = character(0),
    stringsAsFactors = FALSE
  )
}

# coefficient table of the variables of the given types with the column
# names used by the exported getters
coefTable <- function(x, types, also = NULL) {
  tbl <- sumCoefTable(x, types, function(x, v) sumVarLabels(x, v), also = also)
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(emptyCoefTable())
  }
  data.frame(
    Variable = rownames(tbl),
    Coef = tbl$coef,
    Std.Error = tbl$stderr,
    t.value = tbl$t,
    P.value = tbl$p,
    Test = rep("t", nrow(tbl)),
    stringsAsFactors = FALSE
  )
}

# label "F(q, df2)" of a joint Wald F test result
ftestLabel <- function(f) {
  paste0("F(", f$df1, ", ", f$df2, ")")
}

# validate the input of the exported getters: a run single series object
checkRunSingle <- function(x, caller) {
  if (!inherits(x, "persephoneSingle")) {
    stop(
      caller, "() is only available for single series objects ",
      "(perX13() and perTramo()). For hierarchical objects use ",
      "diagnosticsTable().",
      call. = FALSE
    )
  }
  if (is.null(x$output)) {
    stop(caller, "(): no results from run available, call run() first.",
         call. = FALSE)
  }
  invisible(x)
}

#' Trading day coefficient table
#'
#' Extracts the coefficients of the trading day regression of a run seasonal
#' adjustment with their standard errors, t values and p-values, together
#' with the joint F test of the trading day regressors.
#'
#' @details
#' The regressors are labelled as in [summary.persephone()]: the column names
#' of the user-defined regressors (see [perTd()]), or the day names Monday to
#' Saturday for the built-in calendar regressors. When the trading day
#' regression consists of the six day regressors, the effect of the reference
#' day Sunday is derived from the six effects and shown as an additional row
#' "Sunday (derived)", see [summary.persephone()].
#'
#' @param x a `persephone` object of a single series (created with [perX13()]
#'   or [perTramo()]) for which [run][persephone] has been called.
#'
#' @return A `data.frame` with one row per trading day regressor and one row
#'   for the joint F test, with the columns `Variable` (name of the regressor
#'   or "Trading days - FTest"), `Coef` (coefficient; F statistic for the F
#'   test row), `Std.Error`, `t.value`, `P.value` and `Test` ("t" for the
#'   regressor rows, "F(q, df)" for the F test row). A data.frame with no
#'   rows when the model has no trading day regressors.
#'
#' @examples
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' getTradingDaysTable(obj)
#'
#' @export
getTradingDaysTable <- function(x) {
  checkRunSingle(x, "getTradingDaysTable")
  td <- coefTable(x, "TD")
  if (nrow(td) == 0) {
    return(emptyCoefTable())
  }
  if (nrow(td) == 6 && !any(grepl("^sun", tolower(td$Variable)))) {
    sunday <- sumTdSunday(x)
    if (!is.null(sunday)) {
      td <- rbind(
        td,
        data.frame(
          Variable = rownames(sunday),
          Coef = sunday$coef,
          Std.Error = sunday$stderr,
          t.value = sunday$t,
          P.value = sunday$p,
          Test = "t",
          stringsAsFactors = FALSE
        )
      )
    }
  }
  f_td <- sumWaldF(x, "TD")
  if (!is.null(f_td)) {
    td <- rbind(
      td,
      data.frame(
        Variable = "Trading days - FTest",
        Coef = f_td$value,
        Std.Error = NA_real_,
        t.value = NA_real_,
        P.value = f_td$pvalue,
        Test = ftestLabel(f_td),
        stringsAsFactors = FALSE
      )
    )
  }
  rownames(td) <- NULL
  td
}

#' Outlier and mean / leap year coefficient table
#'
#' Extracts the coefficients of the outlier regressors (AO, LS, TC) and of
#' the mean and leap year part of the regression of a run seasonal adjustment
#' with their standard errors, t values and p-values.
#'
#' @details
#' The rows of the outlier regressors carry their outlier name as in
#' [summary.persephone()] (e.g. "LS1985.03"); the rows of the mean / leap
#' year part are the constant and the leap year regressor (the leap year
#' regressor of the specification and a user-defined regressor named like a
#' leap year, see [perTd()]).
#'
#' @param x a `persephone` object of a single series (created with [perX13()]
#'   or [perTramo()]) for which [run][persephone] has been called.
#'
#' @return A `data.frame` with the columns `Variable`, `Coef`, `Std.Error`,
#'   `t.value`, `P.value` and `Test`. Rows are absent for the groups the
#'   model does not contain; a data.frame with no rows when both groups are
#'   empty.
#'
#' @examples
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' getOutlierTable(obj)
#'
#' @export
getOutlierTable <- function(x) {
  checkRunSingle(x, "getOutlierTable")
  outl <- coefTable(x, c("AO", "LS", "TC"))
  meanly <- coefTable(x, c("MEAN", "LP"), also = isUserLeapVar)
  res <- rbind(outl, meanly)
  rownames(res) <- NULL
  res
}

#' Quality indicators table
#'
#' Builds a tidy table of the diagnostic statistics and quality indicators of
#' a run seasonal adjustment: the M-statistics, the overall quality summary,
#' the residual tests (normality, independence), the seasonality tests and
#' the combined tests for calendar effects left in the components (the
#' "combined test on SA" and "combined test on irregular" tests of
#' JDemetra+), together with their classification into the quality classes.
#'
#' @details
#' The values and classes are the ones of [diagnosticsTable()], reshaped into
#' one row per indicator (the wide form of [diagnosticsTable()] is better
#' suited to compare several series). The four combined tests show the
#' p-value as value; they test whether the calendar effects that are left in
#' the seasonally adjusted series and in the irregular component (all
#' observations / last year) are significant, and a "Good" result means
#' p < 0.05. For the normality and independence tests a "Good" result means
#' p >= 0.1, for the seasonality tests p >= 0.1 and for the outlier share
#' less than 3% of the observations.
#'
#' @param x a `persephone` object of a single series (created with [perX13()]
#'   or [perTramo()]) for which [run][persephone] has been called.
#'
#' @return A `data.frame` with one row per indicator and the columns `Item`,
#'   `Value` (formatted as a string) and `Status` (the quality class
#'   "Good" / "Uncertain" / "Bad" / "Severe" / "Undefined", the empty string
#'   for the indicators that are not classified).
#'
#' @examples
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' getQualityTable(obj)
#'
#' @export
getQualityTable <- function(x) {
  checkRunSingle(x, "getQualityTable")
  d <- generate_diagnostics_table(x)
  # order of the indicators: overall quality, model, residual and
  # seasonality tests, combined calendar tests, outliers, M-statistics
  items <- c(
    "TotalQ", "Q", "Q2", "arimaMdl", "logTransform", "seasonality",
    "normality", "independence", "QS_seas", "F_seas", "TD_Ftest",
    "fTestSA_td", "combSA_all", "combSA_last", "combI_all", "combI_last",
    "nOutliers"
  )
  labels <- c(
    TotalQ = "Total quality", Q = "Q", Q2 = "Q-M2",
    arimaMdl = "ARIMA model", logTransform = "Log transformation",
    seasonality = "Seasonality", normality = "Normality",
    independence = "Independence", QS_seas = "QS seasonality",
    F_seas = "F seasonality", TD_Ftest = "TD F-test (p-value)",
    fTestSA_td = "TD in SA",
    combSA_all = "Combined test on SA",
    combSA_last = "Combined test on SA (last y.)",
    combI_all = "Combined test on irregular",
    combI_last = "Combined test on irregular (last y.)",
    nOutliers = "Outliers"
  )
  classified <- c(
    "normality", "independence", "QS_seas", "F_seas", "combSA_all",
    "combSA_last", "combI_all", "combI_last", "fTestSA_td", "nOutliers",
    "TotalQ"
  )
  # p-values of the combined calendar tests (diagnosticsTable() keeps only
  # their classification)
  ud <- x$output$user_defined
  pvals <- list(
    combSA_all = "diagnostics.td-sa-all", combSA_last = "diagnostics.td-sa-last",
    combI_all = "diagnostics.td-i-all", combI_last = "diagnostics.td-i-last"
  )
  value <- function(v) {
    if (is.logical(v)) {
      return(if (is.na(v)) "Undefined" else if (v) "Yes" else "No")
    }
    if (is.numeric(v)) {
      if (is.na(v)) {
        return("Undefined")
      }
      return(format(v, digits = 4))
    }
    if (length(v) == 0 || all(is.na(v))) {
      return("Undefined")
    }
    as.character(v[1])
  }
  rows <- lapply(items, function(it) {
    v <- d[[it]]
    pnm <- pvals[[it]]
    if (!is.null(pnm)) {
      p <- ud[[pnm]]$pvalue
      v <- if (is.null(p) || length(p) == 0) NA else p
    }
    data.frame(
      Item = labels[[it]],
      Value = value(v),
      Status = if (it %in% classified) value(d[[it]]) else "",
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, rows)
  rownames(res) <- NULL
  res
}
