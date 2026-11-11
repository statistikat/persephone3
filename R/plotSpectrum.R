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
#'   type is given, the spectra are shown in separate panels (one per series
#'   type; see `layout`), except for plotType `"arSpecBars"`, which only
#'   supports a single series type. A numeric
#'   value (`1:4`) corresponding to one of these characters is also accepted.
#' @param plotType character vector with one or more of
#'   (`"arSpec"`,`"arSpecBars"`,`"periodogram"`)
#'   selecting the preferred type(s) of plot. If more than one type is given,
#'   the plot types are the rows and the series types the columns of the
#'   panel grid (e.g. `tsType = c("original", "sa", "irregular")` and
#'   `plotType = c("arSpec", "periodogram")` gives 6 panels: the
#'   autoregressive spectra above the periodograms). `"arSpecBars"` is only
#'   available for a single plotType and series type. A numeric value
#'   (`1:3`) corresponding to one of these characters is also accepted.
#' @param layout character (`"panels"`, `"overlay"`) how to show several
#'   spectra: `"panels"` (default) arranges one spectrum per panel in a
#'   single figure, `"overlay"` draws all spectra into the same panel
#'   (only for a single plotType; not available for plotType
#'   `"arSpecBars"`).
#' @param maxobs maximum number of observations of the time series. If `NULL`,
#'   `maxobs` equals the length of the time series for plotTypes
#'   `"arSpec"` and `"periodogram"` and `maxobs=96` for plotType `"arSpecBars"`.
#' @param main plot title
#' @param n.freq the number of frequencies, i.e. the number of points at which
#'   to plot. If `NULL`, `n.freq=301` for plotTypes `"arSpec"` and `"periodogram"`
#'   and `n.freq=61` for plotType `"arSpecBars"`.
#' @param order order of the AR model.
#' @param interactive if `plotEngine="ggplot"`, whether to wrap in plotly for
#'   basic interactivity. Ignored for `plotEngine="plotly"`.
#' @param ... other parameters passed to the underlying plot function.
#'
#' @return A plotly object (if `plotEngine="plotly"`) or a ggplot/plotly
#'   object (if `plotEngine="ggplot"`).
#'
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSpectrum(x, plotEngine = "plotly")
#' plotSpectrum(x, plotEngine = "ggplot")
#' plotSpectrum(x, plotEngine = "ggplot", interactive = TRUE)
#' # the spectra of the original, s.a. and irregular series in one figure
#' plotSpectrum(x, tsType = c("original", "sa", "irregular"))
#' # six panels: the AR spectra above the periodograms
#' plotSpectrum(x, tsType = c("original", "sa", "irregular"),
#'              plotType = c("arSpec", "periodogram"))
#' # ... overlaid in a single panel
#' plotSpectrum(x, tsType = c("sa", "irregular"), layout = "overlay")
#'
#' @export
plotSpectrum <- function(
    x,
    plotEngine = c("plotly", "ggplot"),
    tsType = "original",
    plotType = "arSpec",
    layout = c("panels", "overlay"),
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
