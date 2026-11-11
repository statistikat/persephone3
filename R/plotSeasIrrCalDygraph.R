#' Interactive Seasonal, Irregular, and Calendar Effects Plot
#'
#' Produces a dygraph (see the
#' [online documentation](https://rstudio.github.io/dygraphs/) for more detail).
#' The function generates an interactive time series plot of the seasonal
#' component, irregular component and calendar effects
#' for a [persephone] object together with one year forecasts.
#'
#' @param x an object of class [persephone]
#' @param main plot title
#' @param forecasts logical flag indicating if forecasts should be plotted
#' @param rangeSelector logical flag specifying if a range selector should be
#'   included in the plot
#' @param smooth logical flag or `"spline"` specifying if the series should be
#'   drawn as smoothed lines (monotone piecewise cubic interpolation, or the
#'   overshooting not-a-knot cubic spline for `"spline"`)
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
                    smooth=TRUE, drawPoints=FALSE, annualComparison=NULL){

  if (is.null(x$output$user_defined)) {
    stop("No results from run available.\n")
  }

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
    main <- "Seasonal Comp., Irregular Comp. and Calendar Eff."
  }

  dp <- drawPoints && !isSmoothed(smooth)

  s <- x$output$user_defined$s
  s_f <- x$output$user_defined$s_f
  i <- x$output$user_defined$i
  i_f <- x$output$user_defined$i_f
  cal <- x$output$user_defined$cal
  cal_f <- x$output$user_defined$cal_f

  # Initialize Graph Object
  # Back-/Forecasts
  if (forecasts & !is.null(s_f) & !is.null(i_f)) {

    ts <- cbind(s, i, s_f, i_f, cal, cal_f)
    smooth_cols <- c("s", "i", "s_f", "i_f")
    if (!is.null(cal)) {
      smooth_cols <- c(smooth_cols, "cal", "cal_f")
    }

    graphObj <- dygraph(
      dyData(ts, smooth = smooth, smooth_cols = smooth_cols),
      main = main
    ) %>%
      dySeries("s", label = "Seasonal Component",
               drawPoints = dp,
               color = p3_series_colors[["Seasonal Component"]]) %>%
      dySeries("i",
               label = "Irregular Component",
               drawPoints = dp,
               color = p3_series_colors[["Irregular Component"]]) %>%
      dySeries(
        "s_f",
        label = "Forecasts Seasonal Comp.",
        strokePattern = "dashed",
        drawPoints = dp,
        color = p3_series_colors[["Seasonal Component"]]
      ) %>%
      dySeries(
        "i_f",
        label = "Forecasts Irregular Comp.",
        strokePattern = "dashed",
        drawPoints = dp,
        color = p3_series_colors[["Irregular Component"]]
      )
    if (!is.null(cal) & !is.null(cal_f)) {
      graphObj <- graphObj %>%
        dySeries("cal",
                 label = "Calendar Effects",
                 drawPoints = dp,
                 color = p3_series_colors[["Calendar Effects"]]) %>%
        dySeries(
          "cal_f",
          label = "Forecasts Calendar Eff.",
          strokePattern = "dashed",
          drawPoints = dp,
          color = p3_series_colors[["Calendar Effects"]]
        )
    }
    graphObj <- graphObj %>% dyLegend(width = 470)

  }else{
    ts <- cbind(s, i, cal)

    graphObj <- dygraph(
      dyData(ts, smooth = smooth,
             smooth_cols = intersect(c("s", "i", "cal"), colnames(ts))),
      main = main
    ) %>%
      dySeries("s",
               label = "Seasonal Component",
               drawPoints = dp,
               color = p3_series_colors[["Seasonal Component"]]) %>%
      dySeries("i",
               label = "Irregular Component",
               drawPoints = dp,
               color = p3_series_colors[["Irregular Component"]])
    if (!is.null(cal)) {
      graphObj <- graphObj %>%
        dySeries("cal",
                 label = "Calendar Effects",
                 drawPoints = dp,
                 color = p3_series_colors[["Calendar Effects"]])
    }
    graphObj <- graphObj %>% dyLegend(width = 400)

  }


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

  p3_fluid(graphObj)

}
