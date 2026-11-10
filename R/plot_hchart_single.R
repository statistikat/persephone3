add_ts <- function(hc, x, name, ..., type = "spline") {
  highcharter::hc_add_series(hc, x, name = name, ..., type = type)
}

#' Highcharts plot for single persephone objects (internal workhorse)
#'
#' Interactive time series plot in the style of [plotSeasIrrCal()]: Highcharts
#' stock chart, series drawn with the Highcharts spline smoothing (or plain
#' lines if `smooth = FALSE`), default Highcharts colours, forecasts as dashed
#' lines with the confidence band. Outliers are marked with coloured dots.
#'
#' @param object A persephone object
#' @param main plot title (NULL: no title)
#' @param forecasts logical, plot forecasts with confidence band
#' @param showOutliers logical, mark detected outliers with dots
#' @param rangeSelector logical, show the range selector at the bottom
#' @param smooth logical, smooth the lines (Highcharts spline)
#' @param drawPoints logical, draw a marker at each observation
#' @param annualComparison integer, month or quarter marked in every year
#' @param ... passed to [highcharter::highchart()]
#' @return A `highchart` object
#' @keywords internal
plotSingleHc <- function(
  object,
  main = NULL,
  forecasts = TRUE,
  showOutliers = TRUE,
  rangeSelector = FALSE,
  smooth = TRUE,
  drawPoints = FALSE,
  annualComparison = NULL,
  ...
) {
  seriesType <- if (smooth) "spline" else "line"
  marker <- list(enabled = drawPoints, radius = 2)

  hc <- highcharter::highchart(type = "stock", ...)

  ud <- object$output$user_defined
  if (is.null(ud)) {
    # before the run: only the original series
    hc <- hc %>%
      add_ts(object$ts, "Original", id = "original",
             type = seriesType, marker = marker)
  } else {
    hc <- hc %>%
      add_ts(ud$y, "Original", id = "original",
             type = seriesType, marker = marker) %>%
      add_ts(ud$t, "Trend", type = seriesType, marker = marker) %>%
      add_ts(ud$sa, "Seasonally Adjusted", type = seriesType, marker = marker)
    if (forecasts && !is.null(ud$y_f) && !is.null(ud$y_ef)) {
      hc <- hc_add_forecast(hc, object$output)
    }
    if (showOutliers && !all(is.na(getOutliers(object)))) {
      otl <- gettsout(object)
      for (otlType in c("AO", "LS", "TC")) {
        dates <- otl[[2]][grepl(otlType, names(otl[[2]]))]
        if (length(dates) == 0) {
          next
        }
        if (is.null(dim(otl[[1]]))) {
          values <- stats::na.omit(as.numeric(otl[[1]]))
        } else {
          values <- stats::na.omit(
            as.numeric(otl[[1]][, paste0("otl", otlType), drop = TRUE])
          )
        }
        hc <- hc %>%
          highcharter::hc_add_series(
            data.frame(
              x = highcharter::datetime_to_timestamp(as.Date(unlist(dates))),
              y = values
            ),
            type = "scatter",
            name = otlType,
            color = p3_outlier_colors[[otlType]],
            marker = list(radius = 3),
            enableMouseTracking = TRUE
          )
      }
    }
  }

  if (!is.null(main)) {
    hc <- hc %>% highcharter::hc_title(text = main)
  }

  if (!is.null(annualComparison)) {
    freq <- stats::frequency(object$ts)
    if (freq == 12) {
      mons <- annualComparison
    } else if (freq == 4) {
      mons <- 3 * (annualComparison - 1) + 1
    } else {
      mons <- 1
    }
    yrs <- seq(floor(stats::start(object$ts)[1]),
               ceiling(stats::end(object$ts)[1]))
    annDates <- as.Date(sprintf("%04d-%02d-01", yrs, mons))
    hc <- hc %>%
      highcharter::hc_xAxis(plotLines = lapply(annDates, function(d) {
        list(
          value = highcharter::datetime_to_timestamp(d),
          color = "#d3d3d3", width = 1
        )
      }))
  }

  hc <- hc %>%
    # the legend below the plot, like in plotSeasIrrCal()
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_chart(zoomType = "x") %>%
    highcharter::hc_chart(events = list(
      load = highcharter::JS(
        "function() { this.container.ondblclick = _ => this.zoomOut(); }"
      )
    )) %>%
    highcharter::hc_rangeSelector(enabled = rangeSelector) %>%
    highcharter::hc_navigator(enabled = FALSE) %>%
    highcharter::hc_scrollbar(enabled = FALSE)

  hc
}

#' Interactive Highcharts plot for single persephone objects
#'
#' Creates an interactive time series plot showing the original series, trend,
#' seasonally adjusted series, and forecast with confidence interval, in the
#' style of [plotSeasIrrCal()] (Highcharts splines and colours, see
#' [plotSingleHc()]). Outliers are marked with coloured dots.
#'
#' @param object A persephone object of class \code{persephoneSingle}
#' @param ... passed to [plotSingleHc()]
#'
#' @return A \code{highchart} object
#'
#' @details
#' The plot includes:
#' \itemize{
#'   \item Original series, trend and seasonally adjusted series
#'     (default Highcharts colours, smoothed with Highcharts splines)
#'   \item Forecast with 95\% confidence interval (grey dashed line)
#'   \item Outliers marked with coloured dots
#' }
#'
#' The plot is interactive with zoom functionality (double-click to reset
#' zoom).
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
  if (is.null(object$output)) {
    stop("Model has not been run yet. Please run object$run() first.",
         call. = FALSE)
  }
  plotSingleHc(object, ...)
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
