# ============================================================================
# Compare Series Module - Side-by-Side Comparison of Two Series
# ============================================================================
# This module provides a side-by-side comparison of two hierarchical time
# series dashboards, highlighting key differences in diagnostics and metrics.
# ============================================================================

#' Compare two single series dashboards
#'
#' Creates a side-by-side comparison view of two hierarchical time series,
#' displaying their individual diagnostics alongside a comparison panel.
#'
#' @param x First hierarchical time series object
#' @param y Second hierarchical time series object
#' @param title Main title for the comparison (default: "Series Comparison")
#' @param file Optional file path to save HTML output
#' @return browsable HTML UI for comparison
#'
#' @examples
#' # compare two series
#' # dashboard_compare(hts1, hts2)
#'
#' @export
dashboard_compare <- function(x, y, title = "Series Comparison", file = NULL) {
  # ---- required packages ----
  pkgs <- c("htmltools", "htmlwidgets", "base64enc")
  miss <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(miss) > 0) {
    stop("Please install required packages first: ", paste(miss, collapse = ", "))
  }

  # ---- helpers (copied from dashboard_single) ----
  safe_get <- function(obj, path, default = NULL) {
    cur <- obj
    for (nm in path) {
      if (is.null(cur)) return(default)

      nxt <- tryCatch({
        if (is.list(cur) || is.data.frame(cur)) {
          if (nm %in% names(cur)) cur[[nm]] else NULL
        } else if (is.environment(cur)) {
          if (exists(nm, envir = cur, inherits = FALSE)) {
            get(nm, envir = cur, inherits = FALSE)
          } else {
            NULL
          }
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (is.null(nxt)) return(default)
      cur <- nxt
    }
    cur
  }

  extract_scalar <- function(z, default = NA) {
    if (is.null(z) || length(z) == 0) return(default)
    if (is.list(z) && !is.null(z$pvalue)) return(extract_scalar(z$pvalue, default))
    z <- unname(z[1])
    if (length(z) == 0 || is.na(z)) return(default)
    z
  }

  fmt_num <- function(z, digits = 3) {
    if (is.null(z) || length(z) == 0 || all(is.na(z))) return("NA")
    if (is.logical(z)) return(ifelse(z, "Yes", "No"))
    if (is.numeric(z)) return(formatC(z[1], digits = digits, format = "f"))
    as.character(z[1])
  }

  fmt_p <- function(z, digits = 4) {
    if (is.null(z) || length(z) == 0 || all(is.na(z))) return("NA")
    if (!is.numeric(z)) z <- suppressWarnings(as.numeric(z[1]))
    if (is.na(z)) return("NA")
    formatC(z, digits = digits, format = "f")
  }

  classify_norm_indep <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) return("Undefined")
    p <- as.numeric(p[1])
    if      (p < 0.001) "Severe"
    else if (p < 0.01)  "Bad"
    else if (p < 0.1)   "Uncertain"
    else                "Good"
  }

  classify_seas_td <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) return("Undefined")
    p <- as.numeric(p[1])
    if      (p < 0.01) "Severe"
    else if (p < 0.05) "Bad"
    else if (p < 0.1)  "Uncertain"
    else               "Good"
  }

  classify_td_ftest <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) return("Undefined")
    p <- as.numeric(p[1])
    if (p >= 0.05) "Good" else "Bad"
  }

  classify_qstat <- function(q) {
    if (is.null(q) || length(q) == 0 || is.na(q)) return("Undefined")
    q <- as.numeric(q[1])
    if (q < 1) "Good"
    else if (q < 2) "Bad"
    else "Severe"
  }

  status_color <- function(status) {
    status <- tolower(as.character(status))
    switch(
      status,
      "good"      = "#1b8a3c",
      "uncertain" = "#e38b16",
      "bad"       = "#cc2f2f",
      "severe"    = "#cc2f2f",
      "undefined" = "#8a8a8a",
      "#8a8a8a"
    )
  }

  metric_box_comp <- function(label, val_x, val_y, status_x = NULL, status_y = NULL, small = FALSE) {
    color_x <- if (is.null(status_x)) "#4f46e5" else status_color(status_x)
    color_y <- if (is.null(status_y)) "#4f46e5" else status_color(status_y)
    cls <- if (small) "mini-box" else "value-box"

    htmltools::tags$div(
      class = "compare-metric",
      htmltools::tags$div(class = "metric-label", label),
      htmltools::tags$div(
        class = "metric-values",
        htmltools::tags$div(
          class = cls,
          style = paste0("border-left: 4px solid ", color_x, ";"),
          htmltools::tags$div(class = "box-value", val_x)
        ),
        htmltools::tags$div(
          class = cls,
          style = paste0("border-left: 4px solid ", color_y, ";"),
          htmltools::tags$div(class = "box-value", val_y)
        )
      )
    )
  }

  card <- function(title, ...) {
    htmltools::tags$div(
      class = "card",
      htmltools::tags$div(class = "card-title", title),
      ...
    )
  }

  # ---- extract data for both series ----
  extract_metrics <- function(series_obj) {
    userdef <- safe_get(series_obj, c("output", "user_defined"), list())

    q1 <- suppressWarnings(as.numeric(extract_scalar(safe_get(series_obj, c("output", "mstats", "q"), NA))))
    q2 <- suppressWarnings(as.numeric(extract_scalar(safe_get(series_obj, c("output", "mstats", "qm2"), NA))))

    log_flag <- safe_get(series_obj, c("output", "preprocessing", "description", "log"), NA)
    log_txt <- if (isTRUE(log_flag)) "Yes" else if (identical(log_flag, FALSE)) "No" else fmt_num(log_flag)

    arima_txt <- paste0(
      "(",
      fmt_num(safe_get(userdef, "arima.p", NA), 0), " ",
      fmt_num(safe_get(userdef, "arima.d", NA), 0), " ",
      fmt_num(safe_get(userdef, "arima.q", NA), 0), ")",
      "(",
      fmt_num(safe_get(userdef, "arima.bp", NA), 0), " ",
      fmt_num(safe_get(userdef, "arima.bd", NA), 0), " ",
      fmt_num(safe_get(userdef, "arima.bq", NA), 0), ")"
    )

    p_norm <- suppressWarnings(as.numeric(extract_scalar(
      safe_get(series_obj, c("output", "preprocessing", "diagnostics", "doornikhansen", "pvalue"), NA)
    )))

    p_indep <- suppressWarnings(as.numeric(extract_scalar(
      safe_get(series_obj, c("output", "preprocessing", "diagnostics", "lb", "pvalue"), NA)
    )))

    p_seas <- suppressWarnings(as.numeric(extract_scalar(
      safe_get(userdef, "diagnostics.seas-si-combined", NULL)
    )))

    p_td <- suppressWarnings(as.numeric(extract_scalar(
      safe_get(userdef, c("regression.td-ftest", "pvalue"), NULL)
    )))

    list(
      q1 = q1,
      q2 = q2,
      log_txt = log_txt,
      arima_txt = arima_txt,
      p_norm = p_norm,
      p_indep = p_indep,
      p_seas = p_seas,
      p_td = p_td,
      st_norm = classify_norm_indep(p_norm),
      st_indep = classify_norm_indep(p_indep),
      st_seas = classify_seas_td(p_seas),
      st_td = classify_td_ftest(p_td)
    )
  }

  metrics_x <- extract_metrics(x)
  metrics_y <- extract_metrics(y)

  # ---- comparison table ----
  comp_df <- data.frame(
    Metric = c("Normality (p)", "Independence (p)", "Seasonality (p)", "TD F-test (p)", "Q1", "Q2"),
    Series_A = c(
      fmt_p(metrics_x$p_norm),
      fmt_p(metrics_x$p_indep),
      fmt_p(metrics_x$p_seas),
      fmt_p(metrics_x$p_td),
      fmt_num(metrics_x$q1),
      fmt_num(metrics_x$q2)
    ),
    Series_B = c(
      fmt_p(metrics_y$p_norm),
      fmt_p(metrics_y$p_indep),
      fmt_p(metrics_y$p_seas),
      fmt_p(metrics_y$p_td),
      fmt_num(metrics_y$q1),
      fmt_num(metrics_y$q2)
    ),
    Status_A = c(metrics_x$st_norm, metrics_x$st_indep, metrics_x$st_seas, metrics_x$st_td,
                 classify_qstat(metrics_x$q1), classify_qstat(metrics_x$q2)),
    Status_B = c(metrics_y$st_norm, metrics_y$st_indep, metrics_y$st_seas, metrics_y$st_td,
                 classify_qstat(metrics_y$q1), classify_qstat(metrics_y$q2)),
    stringsAsFactors = FALSE
  )

  # ---- HTML ----
  ui <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML("
        body {
          margin: 0;
          background: #f5f7fb;
          font-family: Inter, Segoe UI, Roboto, Helvetica, Arial, sans-serif;
          color: #1f2937;
        }
        .comp-header {
          padding: 14px;
        }
        .comp-title {
          font-size: 24px;
          font-weight: 700;
          margin: 0 0 12px 0;
        }
        .comp-grid {
          display: grid;
          grid-template-columns: 1fr 1fr 380px;
          gap: 14px;
          padding: 0 14px 14px 14px;
        }
        .series-panel {
          display: flex;
          flex-direction: column;
          gap: 14px;
        }
        .card {
          background: #ffffff;
          border: 1px solid #e5e7eb;
          border-radius: 14px;
          box-shadow: 0 1px 3px rgba(0,0,0,.05);
          padding: 12px 12px 10px 12px;
          overflow: hidden;
        }
        .card-title {
          font-size: 15px;
          font-weight: 700;
          margin-bottom: 10px;
          color: #111827;
        }
        .comparison-panel {
          background: #ffffff;
          border: 1px solid #e5e7eb;
          border-radius: 14px;
          box-shadow: 0 1px 3px rgba(0,0,0,.05);
          padding: 12px;
          height: fit-content;
          position: sticky;
          top: 14px;
        }
        .compare-metric {
          margin-bottom: 10px;
          padding-bottom: 10px;
          border-bottom: 1px solid #ececec;
        }
        .compare-metric:last-child {
          border-bottom: none;
        }
        .metric-label {
          font-size: 12px;
          color: #6b7280;
          margin-bottom: 6px;
          text-transform: uppercase;
          letter-spacing: .02em;
        }
        .metric-values {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 8px;
        }
        .value-box, .mini-box {
          background: #fafafa;
          border: 1px solid #ececec;
          border-radius: 12px;
          padding: 10px 12px;
          min-height: 62px;
        }
        .box-value {
          font-size: 18px;
          line-height: 1.15;
          font-weight: 700;
          color: #111827;
        }
        .compact-table {
          width: 100%;
          border-collapse: collapse;
          margin-top: 10px;
          font-size: 12px;
        }
        .compact-table th, .compact-table td {
          border-bottom: 1px solid #ececec;
          padding: 6px 4px;
          text-align: left;
          vertical-align: top;
        }
        .compact-table th {
          background: #fafafa;
          font-weight: 700;
          font-size: 11px;
        }
        .legend {
          margin-top: 12px;
          font-size: 11px;
          color: #6b7280;
        }
        .legend span {
          margin-right: 12px;
        }
        .status-dot {
          display: inline-block;
          width: 10px;
          height: 10px;
          border-radius: 50%;
          margin-right: 4px;
        }
      "))
    ),
    htmltools::tags$div(
      class = "comp-header",
      htmltools::tags$div(class = "comp-title", title)
    ),
    htmltools::tags$div(
      class = "comp-grid",

      # Left panel: Series A
      htmltools::tags$div(
        class = "series-panel",
        card(
          paste("Series A - ARIMA", metrics_x$arima_txt),
          htmltools::tags$div(
            class = "value-box",
            style = "border-left: 6px solid #4f46e5;",
            htmltools::tags$div(class = "box-label", "Log transformation"),
            htmltools::tags$div(class = "box-value", metrics_x$log_txt)
          )
        ),
        card(
          "Diagnostics",
          htmltools::tags$div(
            class = "status-box-grid",
            metric_box_comp("Normality", fmt_p(metrics_x$p_norm), metrics_x$st_norm, small = TRUE),
            metric_box_comp("Independence", fmt_p(metrics_x$p_indep), metrics_x$st_indep, small = TRUE),
            metric_box_comp("Seasonality", fmt_p(metrics_x$p_seas), metrics_x$st_seas, small = TRUE),
            metric_box_comp("TD F-test", fmt_p(metrics_x$p_td), metrics_x$st_td, small = TRUE)
          )
        )
      ),

      # Middle panel: Series B
      htmltools::tags$div(
        class = "series-panel",
        card(
          paste("Series B - ARIMA", metrics_y$arima_txt),
          htmltools::tags$div(
            class = "value-box",
            style = "border-left: 6px solid #4f46e5;",
            htmltools::tags$div(class = "box-label", "Log transformation"),
            htmltools::tags$div(class = "box-value", metrics_y$log_txt)
          )
        ),
        card(
          "Diagnostics",
          htmltools::tags$div(
            class = "status-box-grid",
            metric_box_comp("Normality", fmt_p(metrics_y$p_norm), metrics_y$st_norm, small = TRUE),
            metric_box_comp("Independence", fmt_p(metrics_y$p_indep), metrics_y$st_indep, small = TRUE),
            metric_box_comp("Seasonality", fmt_p(metrics_y$p_seas), metrics_y$st_seas, small = TRUE),
            metric_box_comp("TD F-test", fmt_p(metrics_y$p_td), metrics_y$st_td, small = TRUE)
          )
        )
      ),

      # Right panel: Comparison summary
      htmltools::tags$div(
        class = "comparison-panel",
        htmltools::tags$div(class = "card-title", "Comparison Summary"),
        htmltools::tags$table(
          class = "compact-table",
          htmltools::tags$thead(
            htmltools::tags$tr(
              htmltools::tags$th("Metric"),
              htmltools::tags$th("A"),
              htmltools::tags$th("B"),
              htmltools::tags$th("Status A"),
              htmltools::tags$th("Status B")
            )
          ),
          htmltools::tags$tbody(
            lapply(seq_len(nrow(comp_df)), function(i) {
              htmltools::tags$tr(
                htmltools::tags$td(comp_df$Metric[i]),
                htmltools::tags$td(comp_df$Series_A[i]),
                htmltools::tags$td(comp_df$Series_B[i]),
                htmltools::tags$td(comp_df$Status_A[i]),
                htmltools::tags$td(comp_df$Status_B[i])
              )
            })
          )
        ),
        htmltools::tags$div(
          class = "legend",
          htmltools::tags$span(
            htmltools::tags$span(class = "status-dot", style = "background: #1b8a3c;"), "Good"
          ),
          htmltools::tags$span(
            htmltools::tags$span(class = "status-dot", style = "background: #e38b16;"), "Uncertain"
          ),
          htmltools::tags$span(
            htmltools::tags$span(class = "status-dot", style = "background: #cc2f2f;"), "Bad/Severe"
          )
        )
      )
    )
  )

  out <- htmltools::browsable(ui)

  if (!is.null(file)) {
    htmltools::save_html(out, file = file, background = "white")
  }

  out
}
