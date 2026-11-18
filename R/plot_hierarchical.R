# the aggregate series a plot of a hierarchicalTimeSeries object shows: in
# 'groups' mode it is the series given by `series`, or the single aggregate of
# the object if it has only one. The returned y, sa_dir and sa_indir are the
# original, the directly and the indirectly adjusted series of that aggregate;
# an adjustment that is not available (not run yet, or adjustment = "indirect",
# where the aggregate series are not adjusted themselves) is NULL and is left
# out of the plot.
hierFocus <- function(x, series = NULL) {
  if (is.null(x$groups)) {
    return(list(
      name = NULL,
      y = x$ts,
      sa_dir = x$adjustedDirect,
      sa_indir = x$adjustedIndirect
    ))
  }
  aggs <- x$getSeries(aggregates = TRUE)
  agg <- series
  if (is.null(agg)) {
    if (length(aggs) != 1L) {
      stop(
        "this object has more than one aggregate (",
        paste(aggs, collapse = ", "), "), so 'series' must be given",
        call. = FALSE
      )
    }
    agg <- aggs
  } else if (!agg %in% aggs) {
    stop(
      "'", agg, "' is not an aggregate series of this object. Aggregates are: ",
      paste(aggs, collapse = ", "),
      call. = FALSE
    )
  }
  list(
    name = agg,
    y = x$getComponent(agg)$ts,
    sa_dir = tryCatch(x$getAdjustedDirect(agg), error = function(e) NULL),
    sa_indir = tryCatch(x$getAdjustedIndirect(agg), error = function(e) NULL)
  )
}

# plot title for an aggregate series with the name of the aggregate in brackets
hierPlotTitle <- function(title, name) {
  if (is.null(name)) title else paste0(title, " (", name, ")")
}

#' Interactive time series plot for a persephone object
#'
#' Produces an interactive time series plot for objects of class [persephone]
#' with the dygraphs engine (`plotEngine = "dygraph"`, see the [online
#' documentation](https://rstudio.github.io/dygraphs/) for more detail) or
#' with the plotly engine (`plotEngine = "plotly"`).
#' In case of ``persephoneSingle`` objects,
#' the function generates an interactive time series plot
#' of the original series, the seasonally adjusted (SA) series and the trend as
#' well as one year forecasts with prediction intervals of the original series.
#' For [perHts()] objects, the function plots the original
#' aggregate series
#' as well as the directly and indirectly seasonally adjusted aggregate series.
#'
#' If no run has been performed on the [persephone] object,
#' only the original (aggregate) time series is plotted.
#'
#' @details
#'
#' The interactive plots fill the plot pane of RStudio and Positron: a plot
#' shown on its own gets the whole pane and is redrawn when the pane is
#' resized, so a small pane gives a small plot and nothing has to be scrolled.
#' In a knitr document (reports, vignettes) and in the dashboards the plots
#' keep their own size, `options(persephone3.plotFill = FALSE)` switches the
#' filling off everywhere, and `options(persephone3.plotHeight = 300)` (with
#' `persephone3.plotWidth`) gives a plot a size of its own. While the mouse
#' moves over a plot, a vertical line marks its position and the values of all
#' plotted series at this date are shown together; no dots are drawn at the
#' hovered points.
#'
#' @name plot
#' @param x an object of class [persephone].
#' @param main plot title
#' @param forecasts logical flag indicating if forecasts should be plotted
#' @param showOutliers logical flag specifying if outliers should be highlighted
#'   in the plot
#' @param rangeSelector logical flag specifying if a range selector should be
#'   included in the plot (default: `FALSE`)
#' @param smooth logical flag or `"spline"` specifying if the series should be
#'   drawn as smoothed lines instead of straight line segments (default:
#'   `TRUE`, monotone piecewise cubic interpolation between the observations,
#'   which cannot overshoot the data); `"spline"` uses the overshooting
#'   not-a-knot cubic spline instead. Outlier markers and
#'   forecast bands are never smoothed.
#' @param drawPoints logical flag indicating if a small dot should be drawn at
#'   each point, in
#'   addition to a line going through the point.
#' @param annualComparison integer corresponding to the month or quarter which
#'   should be highlighted in the plot for every year.
#' @param series for [perHts()] objects created with `groups`: the name of the
#'   aggregate series that is plotted. It is needed when the object has more
#'   than one aggregate; if it has only one, that one is plotted by default.
#' @param plotEngine character. Either `"dygraph"` (default, interactive
#'   dygraphs plots) or `"plotly"` (plotly plots); both in the style of
#'   [plotSeasIrrCal()] with smoothed lines and Highcharts colours.
#' @param ... other plotting parameters to affect the plot. Not currently used.
#'
#' @return Returns a `dygraphs` (if `plotEngine="dygraph"`) or `plotly`
#'   object.
#'
#' @examples
#' # Plotting hierarchicalTimeSeries objects
#'
#' # Monthly data
#' data(AirPassengers, package = "datasets")
#' # Generate two persephoneSingle objects, in this case two tramoseatsSingle
#' # objects
#' tsAir1 <- perTramo(AirPassengers, "rsa1")
#' tsAir2 <- perTramo(AirPassengers, "rsa1")
#' # Generate a hierarchicalTimeSeries object
#' htAir <- perHts(tsAir1=tsAir1, tsAir1=tsAir1)
#' # Plot before run of persephone object
#' plot(htAir, drawPoints = TRUE)
#' htAir$run()
#' # Plot after run
#' plot(htAir, drawPoints=TRUE)
#'
#' # Quarterly data
#' data(UKgas, package = "datasets")
#' tsUKgas1 <- perTramo(UKgas, "rsa3")
#' tsUKgas2 <- perTramo(UKgas, "rsa3")
#' htUKgas <- perHts(tsUKgas1=tsUKgas1, tsUKgas2=tsUKgas2)
#' plot(htUKgas)
#' htUKgas$run()
#' plot(htUKgas)
#'
#' @importFrom stats time cycle dnorm frequency lag acf qnorm pacf
#' @importFrom dygraphs dyCSS dyLegend dyRangeSelector dySeries dygraph
#'   dyHighlight dyAnnotation dyPlotter dyEvent
#' @import ggplot2
#' @export
plot.hierarchicalTimeSeries <- function(x, main=NULL,
                            rangeSelector=FALSE, smooth=TRUE, drawPoints=FALSE,
                            annualComparison=NULL,
                            series=NULL,
                            plotEngine = c("dygraph", "plotly"), ...){

  plotEngine <- match.arg(plotEngine)
  if (plotEngine == "plotly") {
    return(plotHierPlotly(
      x,
      main = main,
      rangeSelector = rangeSelector,
      smooth = smooth,
      drawPoints = drawPoints,
      annualComparison = annualComparison,
      series = series,
      ...
    ))
  }

  # Helper function for annual comparison
  annComp <- function(ts, annualComparison){

    annCompVec <- format(time(ts)[cycle(ts) == annualComparison])
    if (frequency(ts) == 12) {
      annCompLab <- month.abb[annualComparison]
      #annCompLab <- month.name[annualComparison]
      annCompVec <- paste0(substr(annCompVec, 1, 4), "-",
                           stringfix(annualComparison, 2, "0"),
                           "-01")
    }else if (frequency(ts) == 4) {
      annCompLab <- paste0("Q", annualComparison)
      #annCompLab <- paste0(annualComparison,". Quarter")
      annCompVec <- paste0(substr(annCompVec, 1, 4), "-",
                           stringfix(c(1, 4, 7, 10)[annualComparison],
                                   2, "0"), "-01")
    }
    return(list(annCompVec = annCompVec, annCompLab = annCompLab))

  }

  preRunPlot <- function(ts, main = main, rangeSelector=rangeSelector,
                         drawPoints=drawPoints,
                         annualComparison=annualComparison){

    if (is.null(main)) {
      main <- "Original Aggregate Time Series"
    }

    dp <- drawPoints && !isSmoothed(smooth)
    graphObj <- dygraph(
      dyData(ts, smooth = smooth, smooth_cols = "V1"),
      main = main
    ) %>%
      dySeries(
        "V1",
        label = "Original",
        color = p3_series_colors[["Original"]],
        drawPoints = dp
      )

    if (rangeSelector) {
      graphObj <- graphObj %>%
        dyRangeSelector(height = 20)
    }
    if (!is.null(annualComparison)) {
      annCompRes <- annComp(ts, annualComparison)
      for (i in 1:length(annCompRes[["annCompVec"]])) {
        graphObj <- graphObj %>%
          #dyEvent(annCompRes[["annCompVec"]][i],
          # paste(substr(annCompRes[["annCompVec"]][i],1,4),
          # annCompRes[["annCompLab"]]),labelLoc = "bottom",color="lightgrey")
          dyEvent(annCompRes[["annCompVec"]][i], annCompRes[["annCompLab"]],
                  labelLoc = "bottom", color = "lightgrey")
      }
    }

    graphObj

  }

  postRunPlot <- function(
    focus, main = main, rangeSelector = rangeSelector,
    drawPoints = drawPoints, annualComparison = annualComparison) {

    focus <- hierFocus(x, series)
    if (is.null(main)) {
      main <- hierPlotTitle(
        "Original, Directly and Indirectly Adjusted Aggregate Series",
        focus$name
      )
    }

    y <- focus$y
    sa_dir <- focus$sa_dir
    sa_indir <- focus$sa_indir

    # Forecasts not implemented yet

    # ppm_y_f <- x$output$user_defined$preprocessing.model.y_f # nolint
    # ppm_y_ef <- x$output$user_defined$preprocessing.model.y_ef # nolint

    # Initialize Graph Object
    # # Back-/Forecasts
    # if (forecasts & !is.null(ppm_y_f) & !is.null(ppm_y_ef)) {
    #   lowerci <- ppm_y_f - 1.96 * ppm_y_ef
    #   upperci <- ppm_y_f + 1.96 * ppm_y_ef
    #   ts <- cbind(y, t, sa, ppm_y_f, lowerci, upperci)
    #
    #   graphObj <- dygraph(ts, main = main) %>%
    #     dySeries("y", label = "Original", drawPoints = drawPoints) %>%
    #     dySeries("sa", label = "Seasonally Adjusted") %>%
    #     dySeries("t", label = "Trend") %>%
    #     dySeries(c("lowerci", "ppm_y_f", "upperci"), label = "Forecasts",
    #              strokePattern = "dashed", drawPoints = drawPoints) %>%
    #     dyLegend(width = 400)
    #
    # }else{
      # only the adjustments that are available are plotted
      series_names <- "y"
      labels <- c(y = "Original")
      ts <- y
      if (!is.null(sa_dir)) {
        ts <- cbind(ts, sa_dir)
        series_names <- c(series_names, "sa_dir")
        labels <- c(labels, sa_dir = "Direct SA")
      }
      if (!is.null(sa_indir)) {
        ts <- cbind(ts, sa_indir)
        series_names <- c(series_names, "sa_indir")
        labels <- c(labels, sa_indir = "Indirect SA")
      }
      colnames(ts) <- series_names

      dp <- drawPoints && !isSmoothed(smooth)
      colors <- c(
        y = p3_series_colors[["Original"]],
        sa_dir = p3_series_colors[["Direct SA"]],
        sa_indir = p3_series_colors[["Indirect SA"]]
      )
      graphObj <- dygraph(
        dyData(ts, smooth = smooth, smooth_cols = series_names),
        main = main
      )
      for (sn in series_names) {
        graphObj <- graphObj %>%
          dySeries(
            sn,
            label = unname(labels[sn]),
            color = unname(colors[sn]),
            drawPoints = dp && sn == "y"
          )
      }
      graphObj <- graphObj %>% dyLegend(width = 290)
    # }

    # Outliers not implemented

    if (rangeSelector) {
      graphObj <- graphObj %>%
        dyRangeSelector(height = 20)
    }

    if (!is.null(annualComparison)) {
      annCompRes <- annComp(ts, annualComparison)
      for (i in 1:length(annCompRes[["annCompVec"]])) {
        graphObj <- graphObj %>%
          dyEvent(annCompRes[["annCompVec"]][i], annCompRes[["annCompLab"]],
                  labelLoc = "bottom", color = "lightgrey")
      }
    }

    graphObj <- graphObj %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
                  highlightCircleSize = 4, highlightSeriesBackgroundAlpha = 0.5)

    graphObj
  }

  # the plot shows the adjustments that are available; in 'groups' mode with
  # adjustment = "indirect" the aggregates have no own output, but the indirect
  # adjustment is available
  focus <- hierFocus(x, series)
  if (!is.null(focus$sa_dir) || !is.null(focus$sa_indir)) {

    graphObj <- postRunPlot(
      focus = focus, main = main, rangeSelector = rangeSelector,
      drawPoints = drawPoints, annualComparison = annualComparison
    )
    p3_fluid(graphObj)

  }else{
    ts <- if (is.null(x$groups)) x$ts else focus$y
    p3_fluid(preRunPlot(
      ts = ts,
      main = hierPlotTitle("Original Aggregate Time Series", focus$name),
      rangeSelector = rangeSelector,
      drawPoints = drawPoints,
      annualComparison = annualComparison
    ))
  }
}
