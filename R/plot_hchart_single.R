add_ts <- function(hc, x, name, ..., type = "spline") {
  highcharter::hc_add_series(hc, x, name = name, ..., type = type)
}

#' Interactive Highcharts plot for single persephone objects
#'
#' Creates an interactive time series plot showing the original series, trend,
#' seasonally adjusted series, and forecast with confidence intervals.
#' Outliers are marked with vertical lines.
#'
#' @param object A persephone object of class \code{persephoneSingle}
#' @param ... Additional arguments (currently unused)
#'
#' @return A \code{highchart} object
#'
#' @details
#' The plot includes:
#' \itemize{
#'   \item Original series (blue)
#'   \item Trend (orange)
#'   \item Seasonally adjusted series (green)
#'   \item Forecast with 95\% confidence interval (grey)
#'   \item Outliers marked with pink dashed lines
#' }
#'
#' The plot is interactive with zoom functionality (double-click to reset zoom).
#'
#' @examples
#' \dontrun{
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' hchart(obj)
#' }
#'
#' @export
hchart.persephoneSingle <- function(object, ...) {
  output <- object$output
  if (is.null(output)) {
    stop("Model has not been run yet. Please run object$run() first.", call. = FALSE)
  }

  ud <- object$output$user_defined

  outliers <- tryCatch({getOutliers(object)$date},
                       error = function(e) { NULL })

  hc <- highcharter::highchart(type = "stock") %>%
    highcharter::hc_yAxis_multiples(
      highcharter::create_axis(2, height = c(4, 1), turnopposite = TRUE)) %>%
    add_ts(ud$y, "original", id = "original") %>%
    add_ts(ud$t, "trend") %>%
    add_ts(ud$sa, "seasonally adjusted") %>%
    highcharter::hc_tooltip(
      fixed = TRUE,
      formatter = highcharter::JS("function () {
        return `${Highcharts.dateFormat('%Y-%m', this.x)}`;
      }"),
      positioner = highcharter::JS("function (labelWidth, labelHeight, point) {
        return { x: point.plotX, y: -5 };
      }")
    ) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_rangeSelector(buttons = list(
      list(type = "all", text = "all"),
      list(text = "10y", count = 10, type = "year"),
      list(text = "5y", count = 5, type = "year")
    )) %>%
    highcharter::hc_navigator(enabled = FALSE) %>%
    highcharter::hc_scrollbar(enabled = FALSE) %>%
    highcharter::hc_chart(zoomType = "x") %>%
    highcharter::hc_chart(events = list(
      load = highcharter::JS(
        "function() { this.container.ondblclick = _ => this.zoomOut(); }"
      )
    ))

  # Add outlier markers if available
  if (!is.null(outliers) && length(outliers) > 0) {
    hc <- hc %>%
      highcharter::hc_xAxis(plotLines = lapply(outliers, function(o) {
        list(
          value = highcharter::datetime_to_timestamp(o),
          color = "#FF83FA",
          dashStyle = "dash",
          width = 1
        )
      }))
  }

  # Add forecast
  hc <- hc_add_forecast(hc, output)

  return(hc)
}
# https://www.highcharts.com/docs/chart-design-and-style/colors
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
# https://www.farb-tabelle.de/en/rgb2hex.htm?q=plum3
# hc_add_forecast <- function(hc, x) {
#   forecast <- x$user_defined$y_f
#   forecast_sd <- x$user_defined$y_ef
#   forecast_lower <- forecast - 1.96 * forecast_sd
#   forecast_upper <- forecast + 1.96 * forecast_sd
#   forecast_x <- zoo::as.Date(forecast) %>% highcharter::datetime_to_timestamp()
#   hc %>% highcharter::hc_add_series(
#     forecast, type = "spline",   name = "forecast",
#     dashStyle = "shortdash", color = "grey", id = "forecast") %>%
#     highcharter::hc_add_series(
#       type = "areasplinerange", opacity = 0.15, color = "blue",
#       linkedTo = "forecast", name = "confidence interval",
#       data = lapply(seq_along(forecast_x), function(i) {
#         list(x = forecast_x[i], low = forecast_lower[i],
#              high = forecast_upper[i])
#       }))
# }
#' Add forecast to highchart (internal helper)
#'
#' @param hc A highchart object
#' @param x Output object from seasonal adjustment
#' @return A highchart object with forecast added
#' @keywords internal
hc_add_forecast <- function(hc, x) {
  forecast <- x$user_defined$y_f
  forecast_sd <- x$user_defined$y_ef

  if (is.null(forecast) || length(forecast) == 0) {
    return(hc)
  }

  forecast_lower <- forecast - 1.96 * forecast_sd
  forecast_upper <- forecast + 1.96 * forecast_sd
  forecast_x <- zoo::as.Date(forecast) %>% highcharter::datetime_to_timestamp()

  hc %>%
    highcharter::hc_add_series(
      forecast,
      type = "spline",
      name = "forecast",
      dashStyle = "shortdash",
      color = "grey",
      id = "forecast"
    ) %>%
    highcharter::hc_add_series(
      type = "areasplinerange",
      opacity = 0.15,
      color = "blue",
      linkedTo = "forecast",
      name = "confidence interval",
      data = lapply(seq_along(forecast_x), function(i) {
        list(
          x = forecast_x[i],
          low = forecast_lower[i],
          high = forecast_upper[i]
        )
      })
    )
}
