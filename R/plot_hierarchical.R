#' Interactive time series plot for a persephone object
#'
#' Produces a dygraph (see the
#' [online documentation](https://rstudio.github.io/dygraphs/) for more detail)
#' for objects of class [persephone].
#' In case of ``persephoneSingle`` objects,
#' the function generates an interactive time series plot
#' of the original series, the seasonally adjusted (SA) series and the trend as
#' well as one year forecasts with prediction intervals of the original series.
#' For [perHts()] objects, the function plots the original
#' aggregate series
#' as well as the directly and indirectly seasonally adjusted aggregate series.
#' Forecasts and 95% prediction intervals refer to the original aggregate's
#' direct model. If forecast standard errors are unavailable, only the forecast
#' line is shown. Outlier markers (AO, LS, TC) refer to that same aggregate
#' model, not to outliers in individual components.
#'
#' If no run has been performed on the [persephone] object,
#' only the original (aggregate) time series is plotted.
#'
#' @name plot
#' @param x an object of class [persephone].
#' @param main plot title
#' @param forecasts logical flag indicating if forecasts should be plotted
#' @param showOutliers logical flag specifying if outliers should be highlighted
#'   in the plot
#' @param rangeSelector logical flag specifying if a range selector should be
#'   included in the plot
#' @param drawPoints logical flag indicating if a small dot should be drawn at
#'   each point, in
#'   addition to a line going through the point.
#' @param annualComparison integer corresponding to the month or quarter which
#'   should be highlighted in the plot for every year.
#' @param ... other plotting parameters to affect the plot. Not currently used.
#'
#' @return Returns an object of class `dygraphs`.
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
#' htAir <- perHts(tsAir1 = tsAir1, tsAir2 = tsAir2)
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
                            rangeSelector=TRUE, drawPoints=FALSE,
                            annualComparison=NULL, forecasts=TRUE,
                            showOutliers=TRUE, ...){

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

  preRunPlot <- function(ts, rangeSelector=rangeSelector, drawPoints=drawPoints,
                         annualComparison=annualComparison){

    if (is.null(main)) {
      main <- "Original Aggregate Time Series"
    }

    graphObj <- dygraph(ts, main = main) %>%
      dySeries("V1", label = "Original", drawPoints = drawPoints)

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
    main = main, rangeSelector = rangeSelector, drawPoints = drawPoints,
    annualComparison = annualComparison) {

    if (is.null(main)) {
      main <- "Original, Directly and Indirectly Adjusted Aggregate Series"
    }

    y <- x$ts
    sa_dir <- x$adjustedDirect
    sa_indir <- x$adjustedIndirect

    series <- cbind(y, sa_dir, sa_indir)
    forecast <- x$output$user_defined$y_f
    forecast_se <- x$output$user_defined$y_ef
    has_forecast <- isTRUE(forecasts) && !is.null(forecast) &&
      length(forecast) > 0L && any(is.finite(forecast))
    has_interval <- has_forecast && !is.null(forecast_se) &&
      length(forecast_se) == length(forecast) &&
      identical(tsp(forecast_se), tsp(forecast))

    if (has_forecast) {
      series_names <- colnames(series)
      series <- cbind(series, forecast)
      colnames(series) <- c(series_names, "forecast")
      if (has_interval) {
        lowerci <- forecast - 1.96 * forecast_se
        upperci <- forecast + 1.96 * forecast_se
        series_names <- colnames(series)
        series <- cbind(series, lowerci, upperci)
        colnames(series) <- c(series_names, "lowerci", "upperci")
      }
    }

    outliers <- if (isTRUE(showOutliers)) getOutliers(x) else NULL
    colors <- c(AO = "red", LS = "orange", TC = "dodgerblue")
    marker_names <- character(0)
    if (is.data.frame(outliers) && nrow(outliers) > 0L) {
      # Match calendar dates to observation periods (including quarterly data).
      period <- floor(as.numeric(zoo::as.yearmon(outliers$date)) *
                        frequency(y) + 1e-7)
      index <- match(period, round(as.numeric(time(y)) * frequency(y)))
      keep <- !is.na(index) & outliers$type %in% names(colors)
      outliers <- outliers[keep, , drop = FALSE]
      index <- index[keep]
      for (type in unique(outliers$type)) {
        marker <- y
        marker[] <- NA_real_
        positions <- index[outliers$type == type]
        marker[positions] <- y[positions]
        series_names <- colnames(series)
        series <- cbind(series, marker)
        marker_name <- paste0("otl", type)
        colnames(series) <- c(series_names, marker_name)
        marker_names[type] <- marker_name
      }
    }

    graphObj <- dygraph(series, main = main) %>%
      dySeries("y", label = "Original", drawPoints = drawPoints) %>%
      dySeries("sa_dir", label = "Direct SA") %>%
      dySeries("sa_indir", label = "Indirect SA") %>%
      dyLegend(width = if (has_forecast) 400 else 290)

    for (type in names(marker_names)) {
      graphObj <- dySeries(graphObj, marker_names[[type]], label = type,
                           drawPoints = TRUE, strokeWidth = 0,
                           color = colors[[type]], pointShape = "dot",
                           pointSize = 2.5)
      for (date in as.character(outliers$date[outliers$type == type])) {
        graphObj <- dyEvent(graphObj, date, label = NULL, labelLoc = "bottom",
                            strokePattern = "dashed", color = colors[[type]])
      }
    }

    # Configure colors before collapsing the three forecast columns into
    # custom bars: dySeries otherwise initializes colors from the raw columns.
    if (has_forecast) {
      forecast_columns <- if (has_interval) {
        c("lowerci", "forecast", "upperci")
      } else {
        "forecast"
      }
      graphObj <- dySeries(graphObj, forecast_columns, label = "Forecasts",
                           strokePattern = "dashed", drawPoints = drawPoints)
    }
    if (rangeSelector) {
      graphObj <- graphObj %>%
        dyRangeSelector(height = 20)
    }

    if (!is.null(annualComparison)) {
      annCompRes <- annComp(series, annualComparison)
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

  if (!is.null(x$output$user_defined)) {

    graphObj <- postRunPlot(
      main = main, rangeSelector = rangeSelector, drawPoints = drawPoints,
      annualComparison = annualComparison
    )
    graphObj

  }else{
    ts <- x$ts
    preRunPlot(
      ts = ts,
      rangeSelector = rangeSelector,
      drawPoints = drawPoints,
      annualComparison = annualComparison
    )
  }
}
