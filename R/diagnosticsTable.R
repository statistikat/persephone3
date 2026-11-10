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

# labels of the columns of the diagnostics table: friendly names (the DT
# column headers) and, in the header tooltip, a short explanation of what a
# "Good" result means for the test
diagColumnMeta <- function() {
  list(
    component = list(label = "Component", title = ""),
    method = list(label = "Method", title = ""),
    arimaMdl = list(
      label = "ARIMA",
      title = "reg-ARIMA model of the pre-transformed series"
    ),
    logTransform = list(
      label = "Log", title = "the model was estimated in logs"
    ),
    seasonality = list(
      label = "Seasonality",
      title = "combined seasonality test (SI ratios): Present / Absent"
    ),
    normality = list(
      label = "Normality",
      title = "Doornik-Hansen test on the residuals; Good: p >= 0.1"
    ),
    independence = list(
      label = "Independence",
      title = "Ljung-Box test on the residuals; Good: p >= 0.1"
    ),
    QS_seas = list(
      label = "QS seasonality",
      title = "QS test of residual seasonality in the s.a. series;
      Good: p >= 0.1"
    ),
    F_seas = list(
      label = "F seasonality",
      title = "F test of residual seasonality in the s.a. series;
      Good: p >= 0.1"
    ),
    combSA_all = list(
      label = "Calendar in SA (all)",
      title = "combined test of the significance of the calendar effects
      left in the s.a. series, all observations; Good: p < 0.05"
    ),
    combSA_last = list(
      label = "Calendar in SA (last y.)",
      title = "combined test of the calendar effects left in the s.a.
      series, last year; Good: p < 0.05"
    ),
    combI_all = list(
      label = "Calendar in Irregular (all)",
      title = "combined test of the calendar effects left in the
      irregular component, all observations; Good: p < 0.05"
    ),
    combI_last = list(
      label = "Calendar in Irregular (last y.)",
      title = "combined test of the calendar effects left in the
      irregular component, last year; Good: p < 0.05"
    ),
    fTestSA_td = list(
      label = "TD in SA",
      title = "F test of the significance of the trading day effects left
      in the s.a. series; Good: p >= 0.1"
    ),
    TD_Ftest = list(
      label = "TD p-value",
      title = "p-value of the regression F test for the trading day
      regressors"
    ),
    nOutliers = list(
      label = "Outliers",
      title = "share of observations flagged as outliers;
      Good: < 3%, Uncertain: 3-5%, Severe: > 5%"
    ),
    Q = list(label = "Q", title = "M-statistic Q (0 = perfect model)"),
    Q2 = list(label = "Q2", title = "modified M-statistic Q-M2"),
    TotalQ = list(label = "Quality", title = "overall quality summary")
  )
}

# order of the columns of the viewer table (grouped by topic); columns not
# listed keep their position at the end
diagColumnOrder <- c(
  "component", "method", "arimaMdl", "logTransform",
  "seasonality", "normality", "independence", "QS_seas", "F_seas",
  "combSA_all", "combSA_last", "combI_all", "combI_last",
  "fTestSA_td", "TD_Ftest", "nOutliers",
  "Q", "Q2", "TotalQ"
)

# colours of the quality classes: font colours on the dark page (dark =
# TRUE) or their darker counterparts for a light page
diagClassColors <- function(dark = TRUE) {
  if (dark) {
    c(
      Good = "#4bd464", Uncertain = "#ffb340",
      Bad = "#ff6a3d", Severe = "#ff4040", Undefined = "#9e9e9e"
    )
  } else {
    c(
      Good = "#1a803c", Uncertain = "#b26a00",
      Bad = "#c93c00", Severe = "#cc0000", Undefined = "#888888"
    )
  }
}

# style sheet of the page of the widget when dark = TRUE: black background
# with bright text, DT's controls (search field, filter inputs, sorting
# arrows, info text) adapted to the dark page
diagDarkCss <- function() {
  paste(
    "body { background: #141414; color: #dddddd; }",
    ".dataTables_wrapper { color: #dddddd; }",
    "table.dataTable th, table.dataTable td { color: #dddddd;",
    "  border-bottom-color: #3a3a3a !important; }",
    "table.dataTable thead th, table.dataTable thead td,",
    "table.dataTable tfoot th, table.dataTable tfoot td { color: #dddddd;",
    "  border-bottom-color: #5a5a5a !important;",
    "  border-top-color: #5a5a5a !important; }",
    "table.dataTable tbody tr { background-color: #141414; }",
    "table.dataTable tbody tr.odd, table.dataTable.stripe tbody tr.odd,",
    "table.dataTable.display tbody tr.odd { background-color: #1c1c1c; }",
    "table.dataTable.hover tbody tr:hover,",
    "table.dataTable.display tbody tr:hover { background-color: #2a2a2a; }",
    ".dataTables_wrapper .dataTables_filter input,",
    ".dataTables_wrapper .dataTables_length select,",
    "input[type=\"search\"], select.form-control {",
    "  background: #262626 !important; color: #dddddd !important;",
    "  border: 1px solid #555 !important; border-radius: 3px; }",
    ".dataTables_wrapper .dataTables_info,",
    ".dataTables_wrapper .dataTables_filter, .dataTables_wrapper label {",
    "  color: #cccccc; }",
    ".dataTables_wrapper .dataTables_paginate .paginate_button { color:",
    "  #cccccc !important; }",
    "::-webkit-scrollbar { background: #1c1c1c; }",
    "::-webkit-scrollbar-thumb { background: #555; border-radius: 4px; }",
    "::-webkit-scrollbar-corner { background: #1c1c1c; }",
    sep = " "
  )
}

# interactive viewer table of the diagnostics table of diagnosticsTable():
# readable column headers with explanatory tooltips, the quality classes in
# traffic light font colours, columns grouped by topic; DT shows it in the
# viewer pane (RStudio) or the browser
#' @importFrom DT datatable formatStyle
diagnosticsDatatable <- function(df, dark = TRUE, ...) {
  meta <- diagColumnMeta()
  # drop the columns that are uninformative in the viewer: run (rows are
  # only shown for objects that were run) and type (all series of one call
  # have the same type)
  drop <- intersect(c("run", "type"), names(df))
  for (dc in drop) {
    if (all(!is.na(df[[dc]])) && all(df[[dc]] %in% c(TRUE, "single"))) {
      df[[dc]] <- NULL
    }
  }
  ord <- c(intersect(diagColumnOrder, names(df)),
           setdiff(names(df), diagColumnOrder))
  df <- df[, ord, drop = FALSE]

  labels <- vapply(ord, function(cn) {
    if (is.null(meta[[cn]])) cn else meta[[cn]]$label
  }, "")
  titles <- vapply(ord, function(cn) {
    if (is.null(meta[[cn]]) || is.null(meta[[cn]]$title)) {
      ""
    } else {
      gsub("[[:space:]]+", " ", trimws(meta[[cn]]$title))
    }
  }, "")

  # one tooltip per column: DataTables' "title" option replaces the column
  # header on the client side and is inserted as HTML (DT itself escapes
  # colnames), so the span with the tooltip has to be built here
  colDefs <- unname(lapply(which(titles != ""), function(i) {
    list(
      targets = i - 1L,
      title = paste0(
        '<span title="', htmltools::htmlEscape(titles[i], attribute = TRUE),
        '">', labels[i], "</span>"
      )
    )
  }))
  # style sheet injected into the page of the widget for a black background
  # (dark = TRUE); the widget remains self contained, it also works when
  # saved to a file or embedded in a dashboard
  container <- paste0(
    if (dark) paste0("<style>", diagDarkCss(), "</style>") else "",
    "<table class=\"display\">",
    "<thead><tr>",
    paste0(
      "<th>", htmltools::htmlEscape(unname(labels)), "</th>",
      collapse = ""
    ),
    "</tr></thead><tbody></tbody></table>"
  )
  dt <- DT::datatable(
    df,
    rownames = FALSE,
    container = container,
    filter = if (nrow(df) > 1) "top" else "none",
    colnames = unname(labels),
    options = list(
      scrollX = TRUE,
      paging = FALSE,
      searching = nrow(df) > 1,
      info = nrow(df) > 1,
      ordering = TRUE,
      columnDefs = colDefs
    )
  )
  # quality classes as coloured fonts
  classes <- diagClassColors(dark)
  coloured <- c(
    "seasonality", "normality", "independence", "QS_seas", "F_seas",
    "combSA_all", "combSA_last", "combI_all", "combI_last",
    "fTestSA_td", "nOutliers", "TotalQ"
  )
  for (cn in intersect(coloured, ord)) {
    dt <- dt %>% DT::formatStyle(
      cn, color = DT::styleEqual(
        names(classes), unname(classes)
      )
    )
  }
  # right alignment of the numeric columns
  for (cn in ord) {
    if (is.numeric(df[[cn]])) {
      dt <- dt %>% DT::formatStyle(cn, textAlign = "right")
    }
  }
  dt
}

#' Diagnostics table for persephone objects
#'
#' Creates a comprehensive table of diagnostic statistics and quality indicators
#' for seasonal adjustment results. For hierarchical persephone objects, the
#' function uses the iterate mechanism to process all components.
#'
#' @param x a persephone object created with \code{\link{perX13}},
#'   \code{\link{perTramo}}, or \code{\link{perHts}}
#' @param view logical. If `TRUE`, the table is shown as an interactive
#'   widget (sortable columns, explanatory tooltips in the headers and the
#'   quality classes in coloured fonts), displayed in the RStudio
#'   viewer pane or the browser, instead of being returned as a data.frame.
#' @param dark if `view = TRUE`: logical, draw the page of the widget with a
#'   black background and bright text (the colours of the quality classes
#'   are adapted accordingly). Use `dark = FALSE` for the standard light
#'   page.
#' @param ... passed to [DT::datatable()] if `view = TRUE`
#'
#' @return A data.frame with diagnostic statistics if `view = FALSE` (for
#'   hierarchical objects, includes a 'component' column identifying each
#'   series), otherwise a `datatables` htmlwidget.
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
#' # interactive table with tooltips and coloured quality classes on a
#' # black page (dark = FALSE for a light page), shown in the RStudio
#' # viewer pane
#' diagnosticsTable(obj, view = TRUE)
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
#' obj$diagnosticsTable(view = TRUE)
#' }
#'
#' @export
diagnosticsTable <- function(x, view = FALSE, dark = TRUE, ...) {
  if (!inherits(x, "persephone")) {
    stop("x must be a persephone object", call. = FALSE)
  }
  res <- x$iterate(generate_diagnostics_table, asTable = TRUE)
  if (view) {
    diagnosticsDatatable(res, dark = dark, ...)
  } else {
    res
  }
}
