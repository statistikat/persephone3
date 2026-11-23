#' Interactive Seasonal, Irregular, and Calendar Effects Plot
#'
#' Produces a dygraph (see the
#' [online documentation](https://rstudio.github.io/dygraphs/) for more detail).
#' The function generates an interactive time series plot of the seasonal
#' component, irregular component and calendar effects
#' for a [persephone] object, including the components and forecasts supplied by the fit.
#'
#' @param x an object of class [persephone]
#' @param main plot title
#' @param forecasts logical flag indicating if forecasts should be plotted
#' @param rangeSelector logical flag specifying if a range selector should be
#'   included in the plot
#' @param drawPoints logical flag indicating if a small dot should be drawn at
#'   each point, in addition to a line going through the point.
#' @param annualComparison integer corresponding to the month or quarter which
#'   should be highlighted in the plot for every year
#'
#' @return Returns an object of class `dygraphs`
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#' # Generate a persephone object, in this case an x13Single object
#' obj <- perX13(AirPassengers, "rsa1")
#'
#' obj$run()
#' # Plot after run
#' plotSeasIrrCalDygraph(obj)
#'
#' @export
plotSeasIrrCalDygraph <- function(x, main=NULL, forecasts=TRUE, rangeSelector=TRUE,
                    drawPoints=FALSE, annualComparison=NULL){

  series <- effect_chart_series(x, forecasts)

  # Helper function for annual comparison
  annComp <- function(ts, annualComparison){
    annCompVec <- format(time(ts)[cycle(ts) == annualComparison])
    if (frequency(ts) == 12) {
      annCompLab <- month.abb[annualComparison]
      #annCompLab <- month.name[annualComparison]
      annCompVec <- paste0(substr(annCompVec, 1, 4), "-",
                           stringfix(annualComparison, 2, "0"), "-01")
    } else if (frequency(ts) == 4) {
      annCompLab <- paste0("Q", annualComparison)
      #annCompLab <- paste0(annualComparison,". Quarter")
      annCompVec <- paste0(
        substr(annCompVec, 1, 4), "-",
        stringfix(c(1, 4, 7, 10)[annualComparison], 2, "0"), "-01")
    }
    return(list(annCompVec = annCompVec, annCompLab = annCompLab))
  }

  if (is.null(main)) {
    main <- if (length(series) == 1L) unname(effect_chart_labels[names(series)]) else
      "Seasonal Comp., Irregular Comp. and Calendar Eff."
  }

  colors <- c(s = "rgb(0,128,0)", i = "rgb(0,0,128)", cal = "rgb(128,0,0)")
  ts <- do.call(cbind, series)
  graphObj <- dygraph(ts, main = main)
  for (key in names(series)) {
    graphObj <- dySeries(graphObj, if (length(series) == 1L) "V1" else key,
                         label = unname(effect_chart_labels[key]),
                         drawPoints = drawPoints,
                         color = unname(colors[sub("_f$", "", key)]),
                         strokePattern = if (endsWith(key, "_f")) "dashed")
  }

  if (rangeSelector) {
    graphObj <- graphObj %>%
      dyRangeSelector(height = 20)
  }

  if (!is.null(annualComparison)) {
    annCompRes <- annComp(ts, annualComparison)
    for (i in seq_along(annCompRes[["annCompVec"]])) {
      graphObj <- graphObj %>%
        dyEvent(annCompRes[["annCompVec"]][i], annCompRes[["annCompLab"]],
                labelLoc = "bottom", color = "lightgrey")
    }
  }

  # graphObj <- graphObj %>%
  #   dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
  #               highlightCircleSize = 4,
  #               highlightSeriesBackgroundAlpha = 0.5)
  # graphObj <- graphObj %>%
  #   dyOptions(titleHeight=3)
  #
  cssString <- textConnection(".dygraph-title {
font-size: 14px;
}
.dygraph-legend > span {
font-size: 10px;
}")

  on.exit(close(cssString))
  graphObj <- graphObj %>%
    dyCSS(cssString)

  graphObj

}
