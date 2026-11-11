#' Interactive Seasonal, Irregular, and Calendar Effects Plot
#'
#' Create interactive plots of seasonal, irregular, and calendar effects using
#' either dygraphs or plotly. This is a unified interface for
#' [plotSeasIrrCalDygraph] and [plotSeasIrrCalPlotly].
#'
#' @param x an object of class [persephone].
#' @param plotEngine character. Either `"dygraph"` (default, dygraphs) or
#'   `"plotly"` (plotly).
#' @param main plot title
#' @param forecasts logical flag indicating if forecasts should be plotted
#' @param rangeSelector if `plotEngine="dygraph"`: logical flag specifying if a range selector should be
#'   included in the plot; if `plotEngine="plotly"`: logical flag specifying if
#'   a range slider should be shown below the plot
#' @param smooth logical flag or `"spline"` specifying if the series should be
#'   drawn as smoothed lines (monotone piecewise cubic interpolation, or the
#'   overshooting not-a-knot cubic spline for `"spline"`)
#' @param drawPoints if `plotEngine="dygraph"`: logical flag indicating if a small dot should be drawn at
#'   each point, in addition to a line going through the point.
#' @param annualComparison integer corresponding to the month or quarter which
#'   should be highlighted in the plot for every year. If NULL (default), no
#'   annual comparison is shown.
#' @param ... passed to the underlying plot function (for
#'   `plotEngine="plotly"` to [plotly::plot_ly()])
#'
#' @return A dygraph object (if `plotEngine="dygraph"`) or a plotly object
#'   (if `plotEngine="plotly"`).
#'
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSeasIrrCal(x, plotEngine = "dygraph")
#' plotSeasIrrCal(x, plotEngine = "plotly")
#'
#' @export
plotSeasIrrCal <- function(
    x,
    plotEngine = c("dygraph", "plotly"),
    main = NULL,
    forecasts = TRUE,
    rangeSelector=TRUE,
    smooth = TRUE,
    drawPoints=FALSE,
    annualComparison = NULL,
    ...
) {
  stopifnot(inherits(x, "persephone"))
  plotEngine <- match.arg(plotEngine)

  if (plotEngine == "plotly") {
    plotSeasIrrCalPlotly(
      x,
      main = main,
      forecasts = forecasts,
      rangeSelector = rangeSelector,
      smooth = smooth,
      drawPoints = drawPoints,
      annualComparison = annualComparison,
      ...
    )
  } else {
    plotSeasIrrCalDygraph(
      x,
      main = main,
      forecasts = forecasts,
      rangeSelector=rangeSelector,
      smooth = smooth,
      drawPoints=drawPoints,
      annualComparison = annualComparison
    )
  }
}
