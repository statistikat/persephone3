#' SI-Ratio Plot
#'
#' Create SI-ratio plots using either highcharter (interactive) or ggplot2 (static).
#' This is a unified interface for [plotSiRatiosHc] and [plotSiRatiosGgplot].
#'
#' @param x an object of class [persephone].
#' @param plotEngine character. Either `"hc"` (default, interactive highcharter plots)
#'   or `"ggplot"` (static ggplot2 plots).
#' @param main if `plotEngine="ggplot"`, plot title. Ignored for highcharter plots.
#' @param interactive If `plotEngine="ggplot"`, whether to wrap in plotly for
#'   basic interactivity. Ignored for highcharter plots.
#' @param ... other parameters passed to the underlying plot function. Not currently used.
#'
#' @details
#' For `x13Single` objects, displays SI-ratios, replaced SI-ratios, seasonal
#' factors, and their mean. For `tramoseatsSingle` objects, shows SI-ratios
#' and their mean.
#'
#' @return A highchart object (if `plotEngine="hc"`) or a ggplot/plotly object
#'   (if `plotEngine="ggplot"`).
#'
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSiRatios(x, plotEngine = "hc")
#' plotSiRatios(x, plotEngine = "ggplot")
#' plotSiRatios(x, plotEngine = "ggplot", interactive = TRUE)
#'
#' @export
plotSiRatios <- function(
    x,
    plotEngine = c("hc", "ggplot"),
    main = NULL,
    interactive = FALSE,
    ...
) {
  stopifnot(inherits(x, "persephone"))
  plotEngine <- match.arg(plotEngine)

  if (plotEngine == "hc") {
    plotSiRatiosHc(x)
  } else {
    plotSiRatiosGgplot(x, main = main, interactive = interactive, ...)
  }
}
