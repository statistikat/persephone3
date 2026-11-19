#' Generate diagnostic dashboard for single time series
#'
#' Creates an interactive HTML diagnostic dashboard for a single hierarchical
#' time series object, showing model diagnostics, plots, and quality metrics.
#' The dashboard is organised in tabs: Overview (model summary and
#' diagnostics), Plots (the series, the SI ratios, the overlaid spectra and
#' the residual overview), Trading days (coefficients and the joint F test,
#' see [getTradingDaysTable()]), Outliers (outlier and mean/leap year
#' coefficients, see [getOutlierTable()]) and Quality (quality indicators,
#' see [getQualityTable()]).
#'
#' @param x A single time series object (class `x13Single` or `tramoseatsSingle`)
#' @param title Main title for the dashboard (default: "Diagnostics")
#' @param file Optional file path to save HTML output
#' @return Browsable HTML UI containing the diagnostic dashboard
#' @export
#' @note This is an internal function; use [createDashboard()] instead for the public API.
#' @examples
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' createDashboard(obj)  # preferred
dashboard_single <- function(x, file = NULL, title = "Diagnostics") {
  # ---- required packages ----
  pkgs <- c("htmltools", "htmlwidgets", "base64enc")
  miss <- pkgs[
    !vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  ]
  if (length(miss) > 0) {
    stop(
      "Please install required packages first: ",
      paste(miss, collapse = ", ")
    )
  }

  # ---- helpers ----
  safe_get <- function(obj, path, default = NULL) {
    cur <- obj
    for (nm in path) {
      if (is.null(cur)) {
        return(default)
      }

      nxt <- tryCatch(
        {
          # list/data.frame style
          if (is.list(cur) || is.data.frame(cur)) {
            if (nm %in% names(cur)) cur[[nm]] else NULL

            # environment style
          } else if (is.environment(cur)) {
            if (exists(nm, envir = cur, inherits = FALSE)) {
              get(nm, envir = cur, inherits = FALSE)
            } else {
              NULL
            }
          } else {
            NULL
          }
        },
        error = function(e) NULL
      )

      if (is.null(nxt)) {
        return(default)
      }
      cur <- nxt
    }
    cur
  }

  first_non_null <- function(...) {
    vals <- list(...)
    for (v in vals) {
      if (!is.null(v) && length(v) > 0 && !all(is.na(v))) return(v)
    }
    NULL
  }

  extract_scalar <- function(z, default = NA) {
    if (is.null(z) || length(z) == 0) {
      return(default)
    }
    if (is.list(z) && !is.null(z$pvalue)) {
      return(extract_scalar(z$pvalue, default))
    }
    z <- unname(z[1])
    if (length(z) == 0 || is.na(z)) {
      return(default)
    }
    z
  }

  fmt_num <- function(z, digits = 3) {
    if (is.null(z) || length(z) == 0 || all(is.na(z))) {
      return("Undefined")
    }
    if (is.logical(z)) {
      return(ifelse(z, "Yes", "No"))
    }
    if (is.numeric(z)) {
      return(formatC(z[1], digits = digits, format = "f"))
    }
    as.character(z[1])
  }

  fmt_p <- function(z, digits = 4) {
    if (is.null(z) || length(z) == 0 || all(is.na(z))) {
      return("Undefined")
    }
    if (!is.numeric(z)) {
      z <- suppressWarnings(as.numeric(z[1]))
    }
    if (is.na(z)) {
      return("Undefined")
    }
    formatC(z, digits = digits, format = "f")
  }

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

  # a significant F test (p < 0.05) means that the trading day effects are
  # significant, which is the expected ("Good") result for a model that
  # estimates trading day regressors
  classify_td_ftest <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) {
      return("Undefined")
    }
    p <- as.numeric(p[1])
    if (p < 0.05) "Good" else "Bad"
  }

  classify_qstat <- function(q) {
    if (is.null(q) || length(q) == 0 || is.na(q)) {
      return("Undefined")
    }
    q <- as.numeric(q[1])
    if (q < 1) {
      "Good"
    } else if (q < 2) {
      "Bad"
    } else {
      "Severe"
    }
  }

  status_color <- function(status) {
    status <- tolower(as.character(status))
    switch(
      status,
      "good" = "#1b8a3c",
      "uncertain" = "#e38b16",
      "bad" = "#cc2f2f",
      "severe" = "#cc2f2f",
      "undefined" = "#8a8a8a",
      "#8a8a8a"
    )
  }

  metric_box <- function(label, value, status = NULL, small = FALSE) {
    color <- if (is.null(status)) "#4f46e5" else status_color(status)
    cls <- if (small) "mini-box" else "value-box"
    htmltools::tags$div(
      class = cls,
      style = paste0("border-left: 6px solid ", color, ";"),
      htmltools::tags$div(class = "box-label", label),
      htmltools::tags$div(class = "box-value", value),
      if (!is.null(status)) {
        htmltools::tags$div(class = "box-status", status)
      }
    )
  }

  make_table <- function(df) {
    hdr <- htmltools::tags$tr(
      lapply(names(df), function(nm) htmltools::tags$th(nm))
    )
    rows <- lapply(seq_len(nrow(df)), function(i) {
      htmltools::tags$tr(
        lapply(df[i, , drop = TRUE], function(cell) {
          htmltools::tags$td(as.character(cell))
        })
      )
    })
    htmltools::tags$table(
      class = "compact-table",
      htmltools::tags$thead(hdr),
      htmltools::tags$tbody(rows)
    )
  }

  # ---- PNG renderer only for custom base plots ----
  plot_to_img <- function(
    fun,
    width = 1000,
    height = 600,
    res = 120,
    bg = "white"
  ) {
    tf <- tempfile(fileext = ".png")

    grDevices::png(
      filename = tf,
      width = width,
      height = height,
      res = res,
      bg = bg,
      type = if (capabilities("cairo")) "cairo" else "Xlib"
    )

    tryCatch(
      {
        fun()
      },
      error = function(e) {
        graphics::par(mar = c(0, 0, 0, 0))
        plot.new()
        graphics::text(
          0.5,
          0.60,
          "Plot could not be created",
          cex = 1.2,
          font = 2
        )
        graphics::text(0.5, 0.46, conditionMessage(e), cex = 0.9)
      }
    )

    grDevices::dev.off()

    if (
      !file.exists(tf) || is.na(file.info(tf)$size) || file.info(tf)$size == 0
    ) {
      return(
        htmltools::tags$div(
          style = "padding:1rem;color:#b91c1c;font-weight:600;",
          "Plot unavailable"
        )
      )
    }

    uri <- base64enc::dataURI(file = tf, mime = "image/png")
    unlink(tf)

    htmltools::tags$img(src = uri, class = "plot-img")
  }

  # ---- render Persephone plots directly if htmlwidgets ----
  render_persephone_plot <- function(expr, height = "320px") {
    obj <- tryCatch(
      eval.parent(substitute(expr)),
      error = function(e) e
    )

    if (inherits(obj, "error")) {
      return(
        htmltools::tags$div(
          class = "plot-message",
          paste("Plot error:", conditionMessage(obj))
        )
      )
    }

    if (inherits(obj, "htmlwidget")) {
      return(
        htmltools::tags$div(
          class = "widget-wrap",
          style = paste0("height:", height, ";"),
          obj
        )
      )
    }

    if (inherits(obj, "ggplot")) {
      return(plot_to_img(function() print(obj)))
    }

    if (inherits(obj, "grob") || inherits(obj, "gTree")) {
      return(plot_to_img(function() grid::grid.draw(obj)))
    }

    if (is.null(obj)) {
      return(
        htmltools::tags$div(
          class = "plot-message",
          "Plot returned NULL and could not be embedded."
        )
      )
    }

    htmltools::tags$div(
      class = "plot-message",
      paste("Unsupported plot object:", paste(class(obj), collapse = ", "))
    )
  }

  card <- function(title, ...) {
    htmltools::tags$div(
      class = "card",
      htmltools::tags$div(class = "card-title", title),
      ...
    )
  }

  # ---- extract data ----
  userdef <- safe_get(x, c("output", "user_defined"), list())
  resids <- safe_get(x, c("output", "preprocessing", "estimation", "res"), NULL)

  out <- tryCatch(getOutliers(x), error = function(e) NULL)
  n_out <- if (is.null(out)) {
    NA
  } else if (is.null(nrow(out))) {
    NA
  } else {
    nrow(out)
  }

  q1 <- suppressWarnings(as.numeric(extract_scalar(safe_get(
    x,
    c("output", "mstats", "q"),
    NA
  ))))
  q2 <- suppressWarnings(as.numeric(extract_scalar(safe_get(
    x,
    c("output", "mstats", "qm2"),
    NA
  ))))

  log_flag <- safe_get(
    x,
    c("output", "preprocessing", "description", "log"),
    NA
  )
  log_txt <- if (isTRUE(log_flag)) {
    "Yes"
  } else if (identical(log_flag, FALSE)) {
    "No"
  } else {
    fmt_num(log_flag)
  }

  arima_txt <- paste0(
    "(",
    fmt_num(safe_get(userdef, "arima.p", NA), 0),
    " ",
    fmt_num(safe_get(userdef, "arima.d", NA), 0),
    " ",
    fmt_num(safe_get(userdef, "arima.q", NA), 0),
    ")",
    "(",
    fmt_num(safe_get(userdef, "arima.bp", NA), 0),
    " ",
    fmt_num(safe_get(userdef, "arima.bd", NA), 0),
    " ",
    fmt_num(safe_get(userdef, "arima.bq", NA), 0),
    ")"
  )

  p_norm <- suppressWarnings(as.numeric(extract_scalar(
    safe_get(
      x,
      c("output", "preprocessing", "diagnostics", "doornikhansen", "pvalue"),
      NA
    )
  )))

  p_indep <- suppressWarnings(as.numeric(extract_scalar(
    safe_get(x, c("output", "preprocessing", "diagnostics", "lb", "pvalue"), NA)
  )))

  p_seas <- suppressWarnings(as.numeric(extract_scalar(
    first_non_null(
      safe_get(
        x,
        c("output", "user_defined", "diagnostics.seas-si-combined"),
        NULL
      ),
      safe_get(userdef, "diagnostics.seas-si-combined", NULL)
    )
  )))

  p_td <- suppressWarnings(as.numeric(extract_scalar(
    first_non_null(
      safe_get(
        x,
        c("output", "user_defined", "regression.td-ftest", "pvalue"),
        NULL
      ),
      safe_get(userdef, c("regression.td-ftest", "pvalue"), NULL)
    )
  )))

  p_seas_qs <- suppressWarnings(as.numeric(extract_scalar(
    first_non_null(
      safe_get(
        x,
        c("output", "user_defined", "diagnostics.seas-sa-qs", "pvalue"),
        NULL
      ),
      safe_get(userdef, c("diagnostics.seas-sa-qs", "pvalue"), NULL)
    )
  )))

  p_seas_f <- suppressWarnings(as.numeric(extract_scalar(
    first_non_null(
      safe_get(
        x,
        c("output", "user_defined", "diagnostics.seas-sa-f", "pvalue"),
        NULL
      ),
      safe_get(
        x,
        c("output", "user_defined", "diagnostics.seas-sa-f'", "pvalue"),
        NULL
      ),
      safe_get(userdef, c("diagnostics.seas-sa-f", "pvalue"), NULL),
      safe_get(userdef, c("diagnostics.seas-sa-f'", "pvalue"), NULL)
    )
  )))

  q_summary <- first_non_null(
    safe_get(userdef, "quality.summary", NULL),
    safe_get(x, c("output", "user_defined", "quality.summary"), NULL),
    NA
  )

  # ---- statuses ----
  st_norm <- classify_norm_indep(p_norm)
  st_indep <- classify_norm_indep(p_indep)
  st_seas <- classify_seas_td(p_seas)
  st_td <- classify_td_ftest(p_td)
  st_qs <- classify_seas_td(p_seas_qs)
  st_sf <- classify_seas_td(p_seas_f)
  st_q1 <- classify_qstat(q1)
  st_q2 <- classify_qstat(q2)

  # ---- Persephone native plots as plotly widgets ----
  # main = "": the card titles above the plots replace the plot titles
  main_plot <- render_persephone_plot(
    plot(x, plotEngine = "plotly", main = ""), height = "400px"
  )
  si_plot <- render_persephone_plot(
    plotSiRatios(x, main = ""), height = "360px"
  )
  spectrum_plot <- render_persephone_plot(
    plotSpectrum(x, plotEngine = "plotly", tsType = c("original", "sa"),
                 main = ""),
    height = "400px"
  )
  resid_plot <- render_persephone_plot(
    plotResiduals(x, which = "overview", main = ""), height = "520px"
  )

  # ---- dashboard tables ----
  td_df <- tryCatch(getTradingDaysTable(x), error = function(e) NULL)
  otl_df <- tryCatch(getOutlierTable(x), error = function(e) NULL)
  q_df <- tryCatch(getQualityTable(x), error = function(e) NULL)

  # coefficient table (trading days, outliers) as formatted HTML
  fmt_coef_df <- function(df) {
    blank <- function(v, d) {
      ifelse(is.na(v), "", formatC(v, digits = d, format = "f"))
    }
    out <- data.frame(
      Variable = df$Variable,
      Coef = blank(df$Coef, 4),
      `Std. Error` = blank(df$Std.Error, 4),
      `t value` = blank(df$t.value, 3),
      `P-value` = vapply(
        df$P.value,
        function(p) {
          if (is.na(p)) "" else format.pval(p, digits = 3, eps = 1e-10)
        },
        ""
      ),
      Test = df$Test,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    out
  }

  coef_card <- function(title, df, empty_msg) {
    if (is.null(df)) {
      return(card(title, htmltools::tags$div(
        class = "plot-message", "Table could not be created."
      )))
    }
    if (nrow(df) == 0) {
      return(card(title, htmltools::tags$div(
        class = "plot-message", empty_msg
      )))
    }
    card(title, make_table(fmt_coef_df(df)))
  }

  # quality table: the status column coloured with the traffic light rule
  quality_card <- function(df) {
    if (is.null(df)) {
      return(card("Quality indicators", htmltools::tags$div(
        class = "plot-message", "Table could not be created."
      )))
    }
    card(
      "Quality indicators",
      htmltools::tags$table(
        class = "compact-table",
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th("Indicator"),
          htmltools::tags$th("Value"),
          htmltools::tags$th("Status")
        )),
        htmltools::tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            status <- df$Status[i]
            htmltools::tags$tr(
              htmltools::tags$td(df$Item[i]),
              htmltools::tags$td(df$Value[i]),
              if (status == "") {
                htmltools::tags$td("")
              } else {
                htmltools::tags$td(htmltools::tags$span(
                  status,
                  style = paste0(
                    "color:", status_color(status), ";font-weight:600;"
                  )
                ))
              }
            )
          })
        )
      )
    )
  }

  diag_df <- data.frame(
    Diagnostic = c(
      "Normality",
      "Independence",
      "Seasonality",
      "TD F-test",
      "SeasQS",
      "SeasF",
      "Q1",
      "Q2"
    ),
    Value = c(
      fmt_p(p_norm),
      fmt_p(p_indep),
      fmt_p(p_seas),
      fmt_p(p_td),
      fmt_p(p_seas_qs),
      fmt_p(p_seas_f),
      fmt_num(q1),
      fmt_num(q2)
    ),
    Status = c(st_norm, st_indep, st_seas, st_td, st_qs, st_sf, st_q1, st_q2),
    stringsAsFactors = FALSE
  )

  # ---- HTML ----
  ui <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML(
        "
        body {
          margin: 0;
          background: #f5f7fb;
          font-family: Inter, Segoe UI, Roboto, Helvetica, Arial, sans-serif;
          color: #1f2937;
        }
        .dash-wrap {
          padding: 14px;
        }
        .dash-title {
          font-size: 24px;
          font-weight: 700;
          margin: 0 0 12px 0;
        }
        .dash-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 14px;
        }
        .card {
          background: #ffffff;
          border: 1px solid #e5e7eb;
          border-radius: 14px;
          box-shadow: 0 1px 3px rgba(0,0,0,.05);
          padding: 12px 12px 10px 12px;
          overflow: hidden;
          display: flex;
          flex-direction: column;
          min-height: 260px;
        }
        .card-title {
          font-size: 15px;
          font-weight: 700;
          margin-bottom: 10px;
          color: #111827;
        }
        .plot-img {
          width: 100%;
          height: auto;
          border-radius: 10px;
          display: block;
        }
        .widget-wrap {
          width: 100%;
          min-height: 260px;
          flex: 1 1 auto;
          overflow: hidden;
        }
        .widget-wrap .html-widget {
          width: 100% !important;
          height: 100% !important;
        }
        .plot-message {
          padding: 1rem;
          color: #92400e;
          background: #fff7ed;
          border: 1px solid #fed7aa;
          border-radius: 10px;
        }
        .value-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 10px;
          margin-top: 8px;
        }
        .value-box, .mini-box {
          background: #fafafa;
          border: 1px solid #ececec;
          border-radius: 12px;
          padding: 10px 12px;
        }
        .value-box {
          min-height: 76px;
        }
        .mini-box {
          min-height: 62px;
        }
        .box-label {
          font-size: 12px;
          color: #6b7280;
          margin-bottom: 4px;
          text-transform: uppercase;
          letter-spacing: .02em;
        }
        .box-value {
          font-size: 20px;
          line-height: 1.15;
          font-weight: 700;
          color: #111827;
          word-break: break-word;
        }
        .mini-box .box-value {
          font-size: 16px;
        }
        .box-status {
          margin-top: 4px;
          font-size: 12px;
          font-weight: 600;
          color: #374151;
        }
        .arima-box {
          margin-bottom: 10px;
        }
        .arima-big {
          font-size: 30px;
          font-weight: 800;
          line-height: 1.1;
          margin-top: 2px;
          color: #111827;
        }
        .compact-table {
          width: 100%;
          border-collapse: collapse;
          margin-top: 10px;
          font-size: 13px;
        }
        .compact-table th, .compact-table td {
          border-bottom: 1px solid #ececec;
          padding: 7px 6px;
          text-align: left;
          vertical-align: top;
        }
        .compact-table th {
          background: #fafafa;
          font-weight: 700;
        }
        .status-box-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 10px;
        }
        .footer-note {
          margin-top: 8px;
          color: #6b7280;
          font-size: 12px;
        }

        /* inner tabs: see p3TabsCss(), added as a separate style tag */
      "
      ))
    ),
    htmltools::tags$style(htmltools::HTML(p3TabsCss())),
    htmltools::tags$div(
      class = "dash-wrap",
      htmltools::tags$div(class = "dash-title", title),
      p3_tabs_html(list(
        overview = list(
          label = "Overview",
          content = htmltools::tags$div(
            class = "dash-grid",

            # 1) model summary
            card(
              "Model summary",
              htmltools::tags$div(
                class = "value-box arima-box",
                style = "border-left: 6px solid #4f46e5;",
                htmltools::tags$div(class = "box-label", "ARIMA model"),
                htmltools::tags$div(class = "arima-big", arima_txt)
              ),
              htmltools::tags$div(
                class = "value-grid",
                metric_box("Log transformation", log_txt, small = TRUE),
                metric_box(
                  "Outliers",
                  ifelse(is.na(n_out), "Undefined", as.character(n_out)),
                  small = TRUE
                ),
                metric_box("Q1", fmt_num(q1), st_q1, small = TRUE),
                metric_box("Q2", fmt_num(q2), st_q2, small = TRUE),
                metric_box("Quality summary", fmt_num(q_summary), small = TRUE),
                metric_box(
                  "Residuals available",
                  if (!is.null(resids)) "Yes" else "No",
                  small = TRUE
                )
              )
            ),

            # 2) diagnostics
            card(
              "Diagnostics",
              htmltools::tags$div(
                class = "status-box-grid",
                metric_box("Normality", fmt_p(p_norm), st_norm, small = TRUE),
                metric_box("Independence", fmt_p(p_indep), st_indep, small = TRUE),
                metric_box("Seasonality", fmt_p(p_seas), st_seas, small = TRUE),
                metric_box("TD F-test", fmt_p(p_td), st_td, small = TRUE),
                metric_box("SeasQS", fmt_p(p_seas_qs), st_qs, small = TRUE),
                metric_box("SeasF", fmt_p(p_seas_f), st_sf, small = TRUE)
              ),
              make_table(diag_df),
              htmltools::tags$div(
                class = "footer-note",
                "Color rule: Good = green, Uncertain = orange, Bad/Severe = red, Undefined = grey."
              )
            )
          )
        ),
        plots = list(
          label = "Plots",
          content = htmltools::tags$div(
            class = "stack",
            card("Original series", main_plot),
            card("SI ratios", si_plot),
            card("Spectrum: original vs. seasonally adjusted", spectrum_plot),
            card("Residual diagnostics", resid_plot)
          )
        ),
        tradingdays = list(
          label = "Trading days",
          content = htmltools::tags$div(
            class = "stack",
            coef_card(
              "Trading day regressors and joint F test",
              td_df,
              "The model has no trading day regressors."
            )
          )
        ),
        outliers = list(
          label = "Outliers",
          content = htmltools::tags$div(
            class = "stack",
            coef_card(
              "Outlier, mean and leap year regressors",
              otl_df,
              "The model has no outlier regressors and no mean / leap year coefficients."
            )
          )
        ),
        quality = list(
          label = "Quality",
          content = htmltools::tags$div(
            class = "stack",
            quality_card(q_df)
          )
        )
      ))
    ),
    p3TabsJs()
  )

  out <- htmltools::browsable(ui)

  if (!is.null(file)) {
    htmltools::save_html(out, file = file, background = "white")
  }

  out
}
