# Older native fits keep the observed series on the object only.
fitted_plot_series <- function(x) {
  if (identical(x$result$backend, "native")) {
    components <- x$result$components
    if (is.null(components$y)) components$y <- x$ts
    forecast <- x$result$forecasts
    if (!is.null(forecast)) {
      for (field in names(forecast$points)) components[[paste0(field, "_f")]] <- forecast$points[[field]]
      components$y_ef <- forecast$standard_errors$y
    }
    return(components)
  }
  series <- x$output$user_defined
  if (inherits(x$result, "persephone_result") && "forecasts" %in% names(x$result)) {
    for (field in forecast_component_names())
      series[[paste0(field, "_f")]] <- x$result$forecasts$points[[field]]
    series$y_ef <- x$result$forecasts$standard_errors$y
  }
  series
}

#' @name plot
#' @examples
#' # Plotting persephoneSingle objects
#'
#' # Monthly data
#' data(AirPassengers, package = "datasets")
#' # Generate a persephoneSingle object, in this case an x13Single object
#' # The addout.* spec-block updates below drive rjd3 specification functions,
#' # so this example selects the reference backend explicitly
#' obj <- perX13(AirPassengers, "rsa1", backend = "rjd3")
#' # Plot before run of persephoneSingle object
#' plot(obj, drawPoints = TRUE)
#' obj$run()
#' # Plot after run
#' plot(obj, drawPoints=TRUE)
#' # Update some parameters of the persephoneSingle object
#' # (randomly specify some outliers for illustrative purposes only)
#' obj$updateParams(addout.type = c("AO","LS","LS"),
#'                  addout.date = c("1950-01-01","1955-04-01","1959-10-01"))
#' # Perform run to make updateParams take effect
#' obj$run()
#' plot(obj)
#'
#' # Quarterly data
#' data(UKgas, package = "datasets")
#' obj2 <- perX13(UKgas, "rsa3", backend = "rjd3")
#' plot(obj2)
#' obj2$run()
#' plot(obj2)
#'
#' # Generate a persephoneSingle object, in this case a tramoseatsSingle object
#' obj3 <- perTramo(UKgas, "rsa3", backend = "rjd3")
#' plot(obj3)
#' obj3$run()
#' plot(obj3)
#'
#' @importFrom stats time cycle dnorm frequency lag acf qnorm pacf
#' @importFrom dygraphs dyCSS dyLegend dyRangeSelector dySeries dygraph
#'   dyHighlight dyAnnotation dyPlotter dyEvent
#' @import ggplot2
#' @importFrom stats window<-
#' @export
plot.persephoneSingle <- function(
  x,
  main = NULL,
  forecasts = TRUE,
  showOutliers = TRUE,
  rangeSelector = TRUE,
  drawPoints = FALSE,
  annualComparison = NULL,
  ...
) {
  # Helper function for annual comparison
  annComp <- function(ts, annualComparison) {
    annCompVec <- format(time(ts)[cycle(ts) == annualComparison])
    if (frequency(ts) == 12) {
      annCompLab <- month.abb[annualComparison]
      #annCompLab <- month.name[annualComparison]
      annCompVec <- paste0(
        substr(annCompVec, 1, 4),
        "-",
        stringfix(annualComparison, 2, "0"),
        "-01"
      )
    } else if (frequency(ts) == 4) {
      annCompLab <- paste0("Q", annualComparison)
      #annCompLab <- paste0(annualComparison,". Quarter")
      annCompVec <- paste0(
        substr(annCompVec, 1, 4),
        "-",
        stringfix(c(1, 4, 7, 10)[annualComparison], 2, "0"),
        "-01"
      )
    }
    return(list(annCompVec = annCompVec, annCompLab = annCompLab))
  }

  preRunPlot <- function(
    ts,
    rangeSelector = rangeSelector,
    drawPoints = drawPoints,
    annualComparison = annualComparison
  ) {
    if (is.null(main)) {
      main <- "Original Time Series"
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
          dyEvent(
            annCompRes[["annCompVec"]][i],
            annCompRes[["annCompLab"]],
            labelLoc = "bottom",
            color = "lightgrey"
          )
      }
    }

    graphObj
  }

  # gettsout <- function(outliers, y) {
  #
  #   names(outliers) <- sapply(outliers, function(x) x[1])
  #
  #   if (frequency(x$ts) == 12) {
  #     #Date format für dyevent
  #     dateout <- lapply(sapply(outliers, function(x) strsplit(x[[2]], "-")),
  #                       function(z) paste0(
  #                         z[[2]], "-", stringfix(z[[1]], 2, "0"), "-01"))
  #
  #     outliers <- lapply(sapply(outliers, function(x) strsplit(x[[2]], "-")),
  #                        function(z) as.numeric(c( z[[2]],z[[1]])))
  #   } else {
  #     dateout <- lapply(
  #       sapply(outliers,function(x) strsplit(x[[2]], "-")),
  #       function(z) paste0(
  #         z[[2]], "-",
  #         stringfix(c(1, 4, 7, 10)[as.numeric(utils::as.roman(z[[1]]))],
  #                   2, "0"), "-01"))
  #     outliers <- lapply(sapply(outliers, function(x) strsplit(x[[2]], "-")),
  #                        function(z) as.numeric(c( z[[2]], as.numeric(utils::as.roman(z[[1]])))))
  #   }
  #
  #   outliersAO <- outliers[names(outliers) %in% "AO"]
  #   outliersLS <- outliers[names(outliers) %in% "LS"]
  #   outliersTC <- outliers[names(outliers) %in% "TC"]
  #
  #   tsout <- list()
  #   if(length(outliersAO)>0){
  #     otlAO <- ts(start = start(y), end = end(y), frequency = frequency(y))
  #     for(i in seq_along(outliersAO)) {
  #       window(otlAO, start=outliersAO[[i]], end=outliersAO[[i]]) <- window(y, start=outliersAO[[i]], end=outliersAO[[i]])
  #     }
  #     tsout[[length(tsout)+1]] <- otlAO
  #     names(tsout)[length(tsout)] <- "otlAO"
  #   }
  #
  #   if(length(outliersLS)>0){
  #     otlLS <- ts(start = start(y), end = end(y), frequency = frequency(y))
  #     for(i in seq_along(outliersLS)) {
  #       window(otlLS, start=outliersLS[[i]], end=outliersLS[[i]]) <- window(y, start=outliersLS[[i]], end=outliersLS[[i]])
  #     }
  #     tsout[[length(tsout)+1]] <- otlLS
  #     names(tsout)[length(tsout)] <- "otlLS"
  #   }
  #
  #   if(length(outliersTC)>0){
  #     otlTC <- ts(start = start(y), end = end(y), frequency = frequency(y))
  #     for(i in seq_along(outliersTC)) {
  #       window(otlTC, start=outliersTC[[i]], end=outliersTC[[i]]) <- window(y, start=outliersTC[[i]], end=outliersTC[[i]])
  #     }
  #     tsout[[length(tsout)+1]] <- otlTC
  #     names(tsout)[length(tsout)] <- "otlTC"
  #   }
  #
  #   tsout <- do.call(cbind, tsout)
  #
  #   return(list(tsout, dateout))
  # }

  includeOutGraphically <- function(otlType, otlColor = NULL, otl, graphObj) {
    if (any(grepl(otlType, names(otl[[2]])))) {
      if (is.null(otlColor)) {
        otlColors <- list(AO = "red", LS = "orange", TC = "dodgerblue")
        otlColor <- otlColors[[otlType]]
      }
      seriesName <- paste0("otl", otlType)
      graphObj <- graphObj %>%
        dySeries(
          seriesName,
          label = otlType,
          drawPoints = TRUE,
          color = otlColor,
          pointShape = "dot",
          pointSize = 2.5
        )
      dateout <- otl[[2]][grepl(otlType, names(otl[[2]]))]
      for (i in seq_along(dateout)) {
        graphObj <- graphObj %>%
          dyEvent(
            dateout[[i]],
            label = NULL,
            labelLoc = "bottom",
            strokePattern = "dashed",
            color = otlColor
          )
      }
    }
    return(graphObj)
  }

  postRunPlot <- function(
    main = main,
    forecasts = forecasts,
    showOutliers = showOutliers,
    rangeSelector = rangeSelector,
    drawPoints = drawPoints,
    annualComparison = annualComparison
  ) {
    if (is.null(main)) {
      main <- "Original, SA and Trend Series"
    }

    columns <- series[c("y", "t", "sa")]
    types <- character()
    if (isTRUE(showOutliers)) {
      otl <- gettsout(x)
      types <- intersect(c("AO", "LS", "TC"), names(otl[[2L]]))
      for (k in seq_along(types)) {
        points <- otl[[1L]]
        columns[[paste0("otl", types[k])]] <- if (is.null(dim(points))) points else points[, k]
      }
    }

    show_forecast <- isTRUE(forecasts) && length(series$y_f) > 0L
    show_interval <- show_forecast && length(series$y_ef) > 0L
    if (show_forecast) columns$y_f <- series$y_f
    if (show_interval) {
      columns$lowerci <- series$y_f - 1.96 * series$y_ef
      columns$upperci <- series$y_f + 1.96 * series$y_ef
    }
    ts <- do.call(cbind, columns)
    graphObj <- dygraph(ts, main = main) %>%
      dySeries("y", label = "Original", drawPoints = drawPoints) %>%
      dySeries("sa", label = "Seasonally Adjusted") %>%
      dySeries("t", label = "Trend")
    for (type in types) graphObj <- includeOutGraphically(type, otl = otl, graphObj = graphObj)
    if (show_forecast) {
      fields <- if (show_interval) c("lowerci", "y_f", "upperci") else "y_f"
      graphObj <- dySeries(graphObj, fields, label = "Forecasts", strokePattern = "dashed",
                           drawPoints = drawPoints)
    }
    legend_width <- if (!show_forecast) 290 else if (length(types)) 300 else 400
    graphObj <- dyLegend(graphObj, width = legend_width)

    if (rangeSelector) {
      graphObj <- graphObj %>%
        dyRangeSelector(height = 20)
    }

    if (!is.null(annualComparison)) {
      annCompRes <- annComp(ts, annualComparison)
      for (i in 1:length(annCompRes[["annCompVec"]])) {
        graphObj <- graphObj %>%
          dyEvent(
            annCompRes[["annCompVec"]][i],
            annCompRes[["annCompLab"]],
            labelLoc = "bottom",
            color = "lightgrey"
          )
      }
    }

    graphObj <- graphObj %>%
      dyHighlight(
        highlightSeriesOpts = list(strokeWidth = 2),
        highlightCircleSize = 4,
        highlightSeriesBackgroundAlpha = 0.5
      )

    graphObj
  }

  series <- fitted_plot_series(x)
  if (!is.null(series)) {
    graphObj <- postRunPlot(
      main = main,
      forecasts = forecasts,
      showOutliers = showOutliers,
      rangeSelector = rangeSelector,
      drawPoints = drawPoints,
      annualComparison = annualComparison
    )
    graphObj
  } else {
    ts <- x$ts
    preRunPlot(
      ts = ts,
      rangeSelector = rangeSelector,
      drawPoints = drawPoints,
      annualComparison = annualComparison
    )
  }
}

stringfix <- function(x, l, fill = " ") {
  x <- sapply(x, function(x) {
    if (is.na(x)) {
      return("")
    }
    paste0(paste0(rep(fill, l - nchar(x)), collapse = ""), x)
  })
  return(x)
}
