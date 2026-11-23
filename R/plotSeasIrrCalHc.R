#' Interactive Seasonal, Irregular, and Calendar Effects Plot
#'
#' The function generates an interactive time series plot of the seasonal
#' component, irregular component and calendar effects
#' for a [persephone] object, including the components and forecasts supplied by the fit.
#'
#' @param x an object of class [persephone]
#'
#' @returns An object of class `highchart`
#'
#' @export
plotSeasIrrCalHc <- function(x) {
  series <- effect_chart_series(x)
  hc <- highcharter::highchart(type = "stock")
  for (key in names(series)) {
    name <- unname(effect_chart_labels[key])
    if (endsWith(key, "_f")) {
      observed <- sub("_f$", "", key)
      hc <- add_ts(hc, series[[key]], name, dashStyle = "shortdash",
                   linkedTo = if (observed %in% names(series)) observed)
    } else {
      hc <- add_ts(hc, series[[key]], name, id = key)
    }
  }
  hc %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_chart(zoomType = "x") %>%
    highcharter::hc_chart(events = list(load = highcharter::JS("function() { this.container.ondblclick = _ => this.zoomOut(); }"))) %>%
    highcharter::hc_rangeSelector(buttons = list(list(
      type = "all", text = "all"), list(text = "20y", count = 20, type = "year"),
      list(text = "10y", count = 10, type = "year"), list(text = "5y",
                                                          count = 5, type = "year")))
}
