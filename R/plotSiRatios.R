#' SI-Ratio Plot
#'
#' Create SI-ratio plots using either plotly (interactive) or ggplot2 (static).
#' This is a unified interface for [plotSiRatiosPlotly] and
#' [plotSiRatiosGgplot].
#'
#' @param x an object of class [persephone].
#' @param plotEngine character. Either `"plotly"` (default, interactive plotly
#'   plots) or `"ggplot"` (static ggplot2 plots).
#' @param main plot title.
#' @param interactive If `plotEngine="ggplot"`, whether to wrap in plotly for
#'   basic interactivity. Ignored for `plotEngine="plotly"`.
#' @param ... other parameters passed to the underlying plot function.
#'
#' @details
#' For `x13Single` objects, displays SI-ratios, replaced SI-ratios, seasonal
#' factors, and their mean. For `tramoseatsSingle` objects, shows SI-ratios
#' and their mean.
#'
#' @return A plotly object (if `plotEngine="plotly"`) or a ggplot/plotly
#'   object (if `plotEngine="ggplot"`).
#'
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSiRatios(x, plotEngine = "plotly")
#' plotSiRatios(x, plotEngine = "ggplot")
#' plotSiRatios(x, plotEngine = "ggplot", interactive = TRUE)
#'
#' @export
plotSiRatios <- function(
    x,
    plotEngine = c("plotly", "ggplot"),
    main = NULL,
    interactive = FALSE,
    ...
) {
  stopifnot(inherits(x, "persephone"))
  plotEngine <- match.arg(plotEngine)

  if (plotEngine == "ggplot") {
    plotSiRatiosGgplot(x, main = main, interactive = interactive, ...)
  } else {
    plotSiRatiosPlotly(x, main = main, ...)
  }
}
