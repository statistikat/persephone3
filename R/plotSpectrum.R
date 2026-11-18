#' Spectral Plots
#'
#' Create spectral plots using plotly (interactive) or ggplot2 (static).
#' This is a unified interface for [plotSpectrumPlotly] and
#' [plotSpectrumGgplot].
#'
#' @param x an object of class [persephone].
#' @param plotEngine character. Either `"plotly"` (default, interactive
#'   plotly plots) or `"ggplot"` (static ggplot2 plots).
#' @param tsType character vector with one or more of
#'   (`"original"`,`"sa"`,`"irregular"`,`"residuals"`)
#'   selecting the preferred type(s) of time series to plot. If more than one
#'   type is given, all spectra are overlaid in a single plot (with a legend);
#'   except for plotType `"arSpecBars"`, which only supports a single series
#'   type. A numeric value (`1:4`) corresponding to one of these characters is
#'   also accepted.
#' @param plotType character vector with one or more of
#'   (`"arSpec"`,`"arSpecBars"`,`"periodogram"`)
#'   selecting the preferred type(s) of plot. If more than one type is given,
#'   a named list with one plot per plot type is returned (each one overlaying
#'   the requested series types). `"arSpecBars"` is only available for a single
#'   plotType and series type. A numeric value (`1:3`) corresponding to one of
#'   these characters is also accepted.
#' @param layout character. `"overplot"` (default) overlays the spectra of all
#'   requested series types in a single plot (one plot per plot type).
#'   `"panels"` places every combination of plot type and series type in a
#'   panel of its own. The panels form a square-ish grid of
#'   `ceiling(sqrt(n))` equally wide columns (two spectra side by side,
#'   three in two rows, six in two rows of three), each panel with a frame
#'   and with the height:width ratio a plot of a single panel would have;
#'   a single plot object is returned (not a list). Panels need more than
#'   one plot: with a single series type and plot type there is only one
#'   panel, so `"panels"` only adds the frame.
#' @param maxobs maximum number of observations of the time series. If `NULL`,
#'   `maxobs` equals the length of the time series for plotTypes
#'   `"arSpec"` and `"periodogram"` and `maxobs=96` for plotType `"arSpecBars"`.
#' @param main plot title (for several plot types, it is extended by the name
#'   of the plot type of each plot)
#' @param n.freq the number of frequencies, i.e. the number of points at which
#'   to plot. If `NULL`, `n.freq=301` for plotTypes `"arSpec"` and `"periodogram"`
#'   and `n.freq=61` for plotType `"arSpecBars"`.
#' @param order order of the AR model.
#' @param interactive if `plotEngine="ggplot"`, whether to wrap in plotly for
#'   basic interactivity. Ignored for `plotEngine="plotly"`.
#' @param ... other parameters passed to the underlying plot function.
#'
#' @return A plotly object (if `plotEngine="plotly"`) or a ggplot/plotly
#'   object (if `plotEngine="ggplot"`), or a named list of such plots if
#'   several plot types are requested and `layout = "overplot"`.
#'
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSpectrum(x, plotEngine = "plotly")
#' plotSpectrum(x, plotEngine = "ggplot")
#' plotSpectrum(x, plotEngine = "ggplot", interactive = TRUE)
#' # the spectra of the original, s.a. and irregular series overlaid in one
#' # plot
#' plotSpectrum(x, tsType = c("original", "sa", "irregular"))
#' # one plot per plot type (a named list): AR spectrum and periodogram
#' plotSpectrum(x, tsType = c("original", "sa"),
#'              plotType = c("arSpec", "periodogram"))
#' # the spectra of the original, the s.a. and the irregular series in
#' # panels (three spectra give two rows of two panels)
#' plotSpectrum(x, tsType = c("original", "sa", "irregular"),
#'              layout = "panels")
#'
#' @export
plotSpectrum <- function(
    x,
    plotEngine = c("plotly", "ggplot"),
    tsType = "original",
    plotType = "arSpec",
    layout = c("overplot", "panels"),
    maxobs = NULL,
    main = NULL,
    n.freq = NULL,
    order = 30,
    interactive = FALSE,
    ...
) {
  stopifnot(inherits(x, "persephone"))
  plotEngine <- match.arg(plotEngine)
  layout <- match.arg(layout)

  if (plotEngine == "plotly") {
    plotSpectrumPlotly(
      x,
      tsType = tsType,
      plotType = plotType,
      layout = layout,
      maxobs = maxobs,
      main = main,
      n.freq = n.freq,
      order = order,
      ...
    )
  } else {
    plotSpectrumGgplot(
      x,
      tsType = tsType,
      plotType = plotType,
      layout = layout,
      maxobs = maxobs,
      main = main,
      n.freq = n.freq,
      order = order,
      interactive = interactive,
      ...
    )
  }
}
