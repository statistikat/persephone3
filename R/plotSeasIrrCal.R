#' Interactive Seasonal, Irregular, and Calendar Effects Plot
#'
#' Create interactive plots of seasonal, irregular, and calendar effects using
#' either highcharter or dygraphs. This is a unified interface for
#' [plotSeasIrrCalHc] and [plotSeasIrrCalDygraph].
#'
#' @param x an object of class [persephone].
#' @param plotEngine character. Either `"hc"` (default, highcharter) or
#'   `"dygraph"` (dygraphs).
#' @param main if `plotEngine="dygraph"`: plot title
#' @param forecasts if `plotEngine="dygraph"`: logical flag indicating if forecasts should be plotted
##' @param rangeSelector if `plotEngine="dygraph"`: logical flag specifying if a range selector should be
#'   included in the plot
#' @param drawPoints if `plotEngine="dygraph"`: logical flag indicating if a small dot should be drawn at
#'   each point, in addition to a line going through the point.
#' @param annualComparison if `plotEngine="dygraph"`: integer corresponding to the month or quarter which
#'   should be highlighted in the plot for every year. If NULL (default), no
#'   annual comparison is shown.
#'
#' @return A highchart object (if `plotEngine="hc"`) or a dygraph object
#'   (if `plotEngine="dygraph"`).
#'
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSeasIrrCal(x, plotEngine = "hc")
#' plotSeasIrrCal(x, plotEngine = "dygraph")
#'
#' @export
plotSeasIrrCal <- function(
    x,
    plotEngine = c("hc", "dygraph"),
    main = NULL,
    forecasts = TRUE,
    rangeSelector=TRUE,
    drawPoints=FALSE,
    annualComparison = NULL
) {
  stopifnot(inherits(x, "persephone"))
  plotEngine <- match.arg(plotEngine)

  if (plotEngine == "hc") {
    plotSeasIrrCalHc(
      x
     )
  } else {
    plotSeasIrrCalDygraph(
      x,
      main = main,
      forecasts = forecasts,
      rangeSelector=rangeSelector,
      drawPoints=drawPoints,
      annualComparison = annualComparison
    )
  }
}
