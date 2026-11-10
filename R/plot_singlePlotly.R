# interactive time series plots with plotly, in the style of the highcharter
# plots (plotSeasIrrCal()): smoothed lines, default Highcharts colours and
# the forecast with a confidence band

# common layout of the plotly time series plots: legend above the plot,
# optional range slider instead of the range selector of the dygraph plots,
# no y axis title
plotlyTsLayout <- function(p, main = NULL, rangeSelector = FALSE) {
  p %>%
    plotly::layout(
      title = main,
      legend = list(orientation = "h", x = 0, y = 1.1, traceorder = "normal"),
      margin = list(l = 50, r = 20, t = if (is.null(main)) 30 else 60, b = 40),
      hovermode = "x unified",
      plot_bgcolor = "white",
      xaxis = list(
        title = "",
        rangeslider = list(visible = rangeSelector),
        showgrid = FALSE
      ),
      yaxis = list(title = "", zeroline = FALSE)
    )
}

# vertical lines at the annual comparison month/quarter (one per year),
# like the dyEvent() marks of the dygraph plots
plotlyAnnualLines <- function(p, dates) {
  if (length(dates) == 0) {
    return(p)
  }
  shapes <- lapply(dates, function(d) {
    list(
      type = "line", xref = "x", yref = "paper",
      x0 = d, x1 = d, y0 = 0, y1 = 1,
      line = list(color = "lightgrey", width = 1)
    )
  })
  p %>% plotly::layout(shapes = shapes)
}

# dates of the annual comparison (one per year), for the plotly plots
p3_ann_dates <- function(ts, annualComparison) {
  freq <- stats::frequency(ts)
  if (freq == 12) {
    mons <- annualComparison
  } else if (freq == 4) {
    mons <- 3 * (annualComparison - 1) + 1
  } else {
    return(as.Date(character(0)))
  }
  yrs <- seq(floor(stats::start(ts)[1]), ceiling(stats::end(ts)[1]))
  as.Date(sprintf("%04d-%02d-01", yrs, mons))
}

# a single line series: smoothed between the observations if smooth is on
# (dyData() evaluated the spline on a finer grid), markers at the
# observations if drawPoints is on
plotlyLine <- function(p, d, col, name, color,
                       drawPoints = FALSE, dashed = FALSE) {
  if (drawPoints) {
    p %>%
      plotly::add_trace(
        data = d,
        x = ~Date, y = stats::as.formula(paste0("~`", col, "`")),
        name = name, type = "scatter", mode = "lines+markers",
        line = list(
          color = color, width = 1.6, shape = "linear",
          dash = if (dashed) "dash" else "solid"
        ),
        marker = list(color = color, size = 3),
        connectgaps = FALSE
      )
  } else {
    p %>%
      plotly::add_trace(
        data = d,
        x = ~Date, y = stats::as.formula(paste0("~`", col, "`")),
        name = name, type = "scatter", mode = "lines",
        line = list(
          color = color, width = 1.6, shape = "linear",
          dash = if (dashed) "dash" else "solid"
        ),
        connectgaps = FALSE
      )
  }
}

#' Interactive plotly plot for single persephone objects
#'
#' Creates an interactive time series plot in the style of [plotSeasIrrCal()]
#' and of the dygraphs plots of [plot()]: smoothed lines with the default
#' Highcharts colours, the forecast as a dashed line with the confidence
#' band, outliers marked with coloured dots.
#'
#' @param object a persephone object of class `persephoneSingle`
#' @param main plot title (NULL: default title)
#' @param forecasts logical, plot the forecasts with confidence band
#' @param showOutliers logical, mark detected outliers with dots
#' @param rangeSelector logical, show the range slider below the plot
#' @param smooth logical, smooth the lines (cubic spline)
#' @param drawPoints logical, draw a marker at each observation
#' @param annualComparison integer, month or quarter marked in every year
#' @param ... passed to [plotly::plot_ly()]
#' @return A `plotly` htmlwidget
#' @keywords internal
plotSinglePlotly <- function(
  object,
  main = NULL,
  forecasts = TRUE,
  showOutliers = TRUE,
  rangeSelector = FALSE,
  smooth = TRUE,
  drawPoints = FALSE,
  annualComparison = NULL,
  ...
) {
  # as in the dygraph plots: markers only when the line is not smoothed
  dp <- drawPoints && !smooth
  ud <- object$output$user_defined
  if (is.null(ud)) {
    # before the run: only the original series
    if (is.null(main)) {
      main <- "Original Time Series"
    }
    d <- dyData(object$ts, smooth = smooth, smooth_cols = "V1")
    p <- plotly::plot_ly(...) %>%
      plotlyLine(d, "V1", "Original",
        color = p3_series_colors[["Original"]],
        drawPoints = dp
      )
  } else {
    if (is.null(main)) {
      main <- "Original, SA and Trend Series"
    }
    y <- ud$y
    t_ <- ud$t
    sa <- ud$sa
    ppm_y_f <- ud$y_f # nolint
    ppm_y_ef <- ud$y_ef # nolint

    otlTF <- showOutliers && !all(is.na(getOutliers(object)))

    withFc <- forecasts && !is.null(ppm_y_f) && !is.null(ppm_y_ef)
    otl <- if (otlTF) gettsout(object) else NULL
    mats <- list(y = y, t = t_, sa = sa)
    cols <- c("y", "t", "sa")
    if (withFc) {
      lowerci <- ppm_y_f - 1.96 * ppm_y_ef
      upperci <- ppm_y_f + 1.96 * ppm_y_ef
      mats <- c(mats, list(
        ppm_y_f = ppm_y_f, lowerci = lowerci, upperci = upperci
      ))
      cols <- c(cols, "ppm_y_f", "lowerci", "upperci")
    }
    ts <- do.call(cbind, mats)
    d <- dyData(ts, smooth = smooth, smooth_cols = cols)

    p <- plotly::plot_ly(...)
    for (nm in c("y", "t", "sa")) {
      label <- c(y = "Original", t = "Trend", sa = "Seasonally Adjusted")[nm]
      p <- p %>% plotlyLine(
        d, nm, label, color = p3_series_colors[[label]],
        drawPoints = dp
      )
    }
    if (!is.null(otl)) {
      # the outliers as coloured dots at the observations
      otlmat <- otl[[1]]
      for (otlType in c("AO", "LS", "TC")) {
        dates <- unlist(otl[[2]][grepl(otlType, names(otl[[2]]))])
        if (length(dates) == 0) {
          next
        }
        if (is.null(dim(otlmat))) {
          vals <- stats::na.omit(as.numeric(otlmat))
        } else {
          vals <- stats::na.omit(
            as.numeric(otlmat[, paste0("otl", otlType), drop = TRUE])
          )
        }
        p <- p %>%
          plotly::add_trace(
            x = as.Date(dates), y = vals, type = "scatter",
            mode = "markers", name = otlType,
            marker = list(
              color = p3_outlier_colors[[otlType]], size = 6
            ),
            hovertemplate = paste0(
              otlType, " %{x|%Y-%m-%d}<extra></extra>"
            )
          )
      }
    }
    if (withFc) {
      p <- p %>%
        plotlyLine(
          d, "ppm_y_f", "Forecasts",
          color = p3_series_colors[["Forecasts"]],
          drawPoints = dp, dashed = TRUE
        )
      # the confidence band: filled between lowerci and upperci
      band <- stats::na.omit(d[, c("Date", "lowerci", "upperci")])
      if (nrow(band) > 1) {
        p <- p %>%
          plotly::add_trace(
            data = band, x = ~Date, y = ~upperci,
            type = "scatter", mode = "lines", showlegend = FALSE,
            hoverinfo = "skip", line = list(width = 0)
          ) %>%
          plotly::add_trace(
            data = band, x = ~Date, y = ~lowerci,
            type = "scatter", mode = "lines", showlegend = FALSE,
            hoverinfo = "skip", line = list(width = 0),
            fill = "tonexty", fillcolor = "rgba(124,181,236,0.25)"
          )
      }
    }
  }

  if (!is.null(annualComparison)) {
    p <- p %>% plotlyAnnualLines(p3_ann_dates(object$ts, annualComparison))
  }
  plotlyTsLayout(p, main = main, rangeSelector = rangeSelector)
}

#' Interactive plotly plot for hierarchicalTimeSeries objects
#' @inheritParams plotSinglePlotly
#' @param x a `hierarchicalTimeSeries` object
#' @keywords internal
plotHierPlotly <- function(
  x,
  main = NULL,
  rangeSelector = FALSE,
  smooth = TRUE,
  drawPoints = FALSE,
  annualComparison = NULL,
  ...
) {
  dp <- drawPoints && !smooth
  if (is.null(x$output$user_defined)) {
    if (is.null(main)) {
      main <- "Original Aggregate Series"
    }
    d <- dyData(x$ts, smooth = smooth, smooth_cols = "V1")
    p <- plotly::plot_ly(...) %>%
      plotlyLine(d, "V1", "Original",
        color = p3_series_colors[["Original"]],
        drawPoints = dp
      )
  } else {
    if (is.null(main)) {
      main <- "Original, Directly and Indirectly Adjusted Aggregate Series"
    }
    ts <- cbind(x$ts, x$adjustedDirect, x$adjustedIndirect)
    colnames(ts) <- c("y", "sa_dir", "sa_indir")
    d <- dyData(
      ts, smooth = smooth,
      smooth_cols = c("y", "sa_dir", "sa_indir")
    )
    p <- plotly::plot_ly(...) %>%
      plotlyLine(d, "y", "Original",
        color = p3_series_colors[["Original"]],
        drawPoints = dp
      ) %>%
      plotlyLine(d, "sa_dir", "Direct SA",
        color = p3_series_colors[["Direct SA"]],
        drawPoints = dp
      ) %>%
      plotlyLine(d, "sa_indir", "Indirect SA",
        color = p3_series_colors[["Indirect SA"]],
        drawPoints = dp
      )
  }
  if (!is.null(annualComparison)) {
    p <- p %>% plotlyAnnualLines(p3_ann_dates(x$ts, annualComparison))
  }
  plotlyTsLayout(p, main = main, rangeSelector = rangeSelector)
}
