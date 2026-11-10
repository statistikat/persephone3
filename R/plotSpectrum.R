#' Spectral Plots
#'
#' Create spectral plots using either highcharter (interactive) or ggplot2 (static).
#' This is a unified interface for [plotSpectrumHc] and [plotSpectrumGgplot].
#'
#' @param x an object of class [persephone].
#' @param plotEngine character. Either `"hc"` (default, interactive highcharter plots)
#'   or `"ggplot"` (static ggplot2 plots).
#' @param tsType character vector with one or more of
#'   (`"original"`,`"sa"`,`"irregular"`,`"residuals"`)
#'   selecting the preferred type(s) of time series to plot. If more than one
#'   type is given, the spectra are overlaid in one plot (not available for
#'   plotType `"arSpecBars"`). A numeric
#'   value (`1:4`) corresponding to one of these characters is also accepted.
#' @param plotType character (`"arSpec"`,`"arSpecBars"`,`"periodogram"`)
#'   selecting the preferred type of plot. A numeric value (`1:3`)
#'   corresponding to one of these characters is also accepted.
#' @param maxobs maximum number of observations of the time series. If `NULL`,
#'   `maxobs` equals the length of the time series for plotTypes
#'   `"arSpec"` and `"periodogram"` and `maxobs=96` for plotType `"arSpecBars"`.
#' @param main plot title
#' @param n.freq the number of frequencies, i.e. the number of points at which
#'   to plot. If `NULL`, `n.freq=301` for plotTypes `"arSpec"` and `"periodogram"`
#'   and `n.freq=61` for plotType `"arSpecBars"`.
#' @param order order of the AR model.
#' @param interactive if `plotEngine="ggplot"`, whether to wrap in plotly for
#'   basic interactivity. Ignored for highcharter plots.
#' @param ... other parameters passed to the underlying plot function.
#'
#' @return A highchart object (if `plotEngine="hc"`) or a ggplot/plotly object
#'   (if `plotEngine="ggplot"`).
#'
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSpectrum(x, plotEngine = "hc")
#' plotSpectrum(x, plotEngine = "ggplot")
#' plotSpectrum(x, plotEngine = "ggplot", interactive = TRUE)
#' # all three spectra (s.a., irregular and residuals) in one plot
#' plotSpectrum(x, tsType = c("sa", "irregular", "residuals"))
#'
#' @export
plotSpectrum <- function(
    x,
    plotEngine = c("hc", "ggplot"),
    tsType = "original",
    plotType = c("arSpec", "arSpecBars", "periodogram"),
    maxobs = NULL,
    main = NULL,
    n.freq = NULL,
    order = 30,
    interactive = FALSE,
    ...
) {
  stopifnot(inherits(x, "persephone"))
  plotEngine <- match.arg(plotEngine)

  if (plotEngine == "hc") {
    plotSpectrumHc(
      x,
      tsType = tsType,
      plotType = plotType,
      maxobs = maxobs,
      main = main,
      n.freq = n.freq,
      order = order
    )
  } else {
    plotSpectrumGgplot(
      x,
      tsType = tsType,
      plotType = plotType,
      maxobs = maxobs,
      main = main,
      n.freq = n.freq,
      order = order,
      interactive = interactive,
      ...
    )
  }
}
