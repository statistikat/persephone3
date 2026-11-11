# interactive time series plots with plotly, in the style of the dygraph
# plots: smoothed lines, default Highcharts colours and the forecast with a
# confidence band

# common layout of the plotly time series plots: legend above the plot,
# optional range slider instead of the range selector of the dygraph plots,
# no y axis title; the space above the plot is sized to the (horizontally
# wrapped) legend so that it does not overlap the title
plotlyTsLayout <- function(p, main = NULL, rangeSelector = FALSE) {
  nms <- vapply(p$x$data, function(s) {
    if (isFALSE(s$showlegend)) "" else paste0(s$name, "   ")
  }, "")
  # roughly 110 characters of legend labels of average length fit in one row
  nRows <- max(1, ceiling(sum(nchar(nms)) / 110))
  p %>%
    plotly::layout(
      title = if (is.null(main)) {
        NULL
      } else {
        list(text = main, yref = "paper", y = 1, yanchor = "top")
      },
      legend = list(
        orientation = "h", x = 0, y = 1.1 + 0.05 * (nRows - 1),
        traceorder = "normal"
      ),
      margin = list(
        l = 50, r = 20, b = 40,
        t = (if (is.null(main)) 30 else 60) + 20 * (nRows - 1)
      ),
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
#' @param smooth logical or `"spline"`, smooth the lines (monotone piecewise
#'   cubic interpolation; `"spline"`: the overshooting not-a-knot cubic spline)
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
  dp <- drawPoints && !isSmoothed(smooth)
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
  dp <- drawPoints && !isSmoothed(smooth)
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

#' Interactive plotly plot of the seasonal, irregular and calendar components
#'
#' Interactive time series plot of the seasonal component, the irregular
#' component and the calendar effects with plotly, smoothed lines with the
#' default Highcharts colours, the forecasts as dashed lines. For the
#' dygraphs version see [plotSeasIrrCalDygraph()].
#'
#' @inheritParams plotSeasIrrCalDygraph
#' @param ... passed to [plotly::plot_ly()]
#' @return A `plotly` htmlwidget
#' @export
plotSeasIrrCalPlotly <- function(
  x,
  main = NULL,
  forecasts = TRUE,
  rangeSelector = FALSE,
  smooth = TRUE,
  drawPoints = FALSE,
  annualComparison = NULL,
  ...
) {
  if (is.null(x$output$user_defined)) {
    stop("No results from run available.\n")
  }
  dp <- drawPoints && !isSmoothed(smooth)
  if (is.null(main)) {
    main <- "Seasonal Comp., Irregular Comp. and Calendar Eff."
  }

  ud <- x$output$user_defined
  # the forecasts are drawn in the colour of their component, as a dashed
  # line; colsBase maps every column to the colour of its component
  matCols <- c(
    s = "Seasonal Component", i = "Irregular Component",
    cal = "Calendar Effects", s_f = "Seasonal Component",
    i_f = "Irregular Component", cal_f = "Calendar Effects"
  )
  labels <- c(
    s = "Seasonal Component", i = "Irregular Component",
    cal = "Calendar Effects", s_f = "Forecasts Seasonal Comp.",
    i_f = "Forecasts Irregular Comp.", cal_f = "Forecasts Calendar Eff."
  )
  keep <- c("s", "i")
  if (!is.null(ud$cal)) {
    keep <- c(keep, "cal")
  }
  if (forecasts && !is.null(ud$s_f) && !is.null(ud$i_f)) {
    keep <- c(keep, "s_f", "i_f")
    if (!is.null(ud$cal) && !is.null(ud$cal_f)) {
      keep <- c(keep, "cal_f")
    }
  }
  ud <- ud[intersect(keep, names(ud))]
  keep <- intersect(keep, names(ud))
  ts <- do.call(cbind, ud[keep])
  colnames(ts) <- keep
  d <- dyData(ts, smooth = smooth, smooth_cols = keep)

  p <- plotly::plot_ly(...)
  for (nm in keep) {
    p <- p %>% plotlyLine(
      d, nm, labels[[nm]],
      color = p3_series_colors[[matCols[[nm]]]],
      drawPoints = dp,
      dashed = grepl("^Forecasts", labels[[nm]])
    )
  }
  if (!is.null(annualComparison)) {
    p <- p %>% plotlyAnnualLines(p3_ann_dates(x$ts, annualComparison))
  }
  plotlyTsLayout(p, main = main, rangeSelector = rangeSelector)
}

# labels of the periods of a seasonal series ("Jan".."Dec", "Q1".."Q4")
p3_cycleName <- function(ts) {
  if (stats::frequency(ts) == 12) {
    res <- month.abb[stats::cycle(ts)]
    res <- ordered(res, levels = month.abb)
  } else if (stats::frequency(ts) == 4) {
    res <- c("Q1", "Q2", "Q3", "Q4")[stats::cycle(ts)]
    res <- ordered(res, levels = c("Q1", "Q2", "Q3", "Q4"))
  } else if (stats::frequency(ts) == 2) {
    res <- c("H1", "H2")[stats::cycle(ts)]
    res <- ordered(res, levels = c("H1", "H2"))
  } else {
    stop("plotSiRatios() needs a seasonal series", call. = FALSE)
  }
  return(res)
}

# data of the SI-ratio plot as in the interactive plots: one row per
# observation with the abscissa cycle + (year - first year) / number of
# years, so that the cycles are next to each other on the x axis
siRatiosData <- function(vectors, cyc) {
  years <- floor(stats::time(vectors[[1]]))
  nyears <- length(unique(years))
  xnum <- as.integer(cyc) + (years - min(years)) / nyears
  dat <- data.frame(
    x = xnum,
    year = years,
    cycleName = as.character(cyc)
  )
  for (nm in names(vectors)) {
    dat[[nm]] <- as.numeric(vectors[[nm]])
  }
  dat[order(dat$x), , drop = FALSE]
}

# a "by period" line plot in the style of the interactive SI-ratio plots:
# cols are drawn as one line per period (one trace per period, one legend
# entry per series), dots as small unconnected dots
plotlySiLines <- function(p, dat, cols, dots = NULL,
                          colors, pointSize = 3) {
  cycleNames <- levels(droplevels(factor(dat$cycleName,
    levels = unique(dat$cycleName)
  )))
  ncol <- length(cycleNames)
  ticktext <- switch(
    as.character(ncol),
    "12" = month.abb,
    "4" = c("Q1", "Q2", "Q3", "Q4"),
    "2" = c("H1", "H2"),
    as.character(seq_len(ncol))
  )
  for (nm in setdiff(cols, dots)) {
    first <- TRUE
    for (cnm in cycleNames) {
      dcn <- dat[dat$cycleName == cnm, , drop = FALSE]
      p <- p %>%
        plotly::add_trace(
          data = dcn, x = ~x,
          y = stats::as.formula(paste0("~`", nm, "`")),
          name = nm, type = "scatter", mode = "lines",
          line = list(color = colors[[nm]], width = 1.6),
          connectgaps = FALSE,
          legendgroup = nm,
          showlegend = first,
          customdata = ~ paste0(cycleName, " ", year),
          hovertemplate = paste0(
            "%{customdata}<br>", nm, ": %{y:.3f}<extra></extra>"
          )
        )
      first <- FALSE
    }
  }
  for (nm in dots) {
    dsub <- stats::na.omit(dat[, c("x", "year", "cycleName", nm)])
    p <- p %>%
      plotly::add_trace(
        data = dsub,
        x = ~x, y = stats::as.formula(paste0("~`", nm, "`")),
        name = nm, type = "scatter", mode = "markers",
        marker = list(color = colors[[nm]], size = pointSize),
        legendgroup = nm,
        customdata = ~ paste0(cycleName, " ", year),
        hovertemplate = paste0(
          "%{customdata}<br>", nm, ": %{y:.3f}<extra></extra>"
        )
      )
  }
  p %>%
    plotly::layout(
      xaxis = list(
        title = "", showgrid = FALSE, zeroline = FALSE,
        tickmode = "array", tickvals = seq_len(ncol),
        ticktext = ticktext
      )
    )
}

#' SI-ratio plot (plotly)
#'
#' Interactive SI-ratio plot with plotly in the style of the previous
#' interactive plot: the periods (months or quarters) side by side on the
#' x axis, the SI-ratios as small dots, the seasonal factors and their mean
#' as lines. For static plots see [plotSiRatiosGgplot].
#'
#' @param x an object of class [persephone]
#' @param main plot title
#' @param ... passed to [plotly::plot_ly()]
#' @return A `plotly` htmlwidget
#' @export
plotSiRatiosPlotly <- function(x, main = NULL, ...) {
  if (is.null(x$output$user_defined)) {
    stop("No results from run available.\n")
  }
  if (inherits(x$output$decomposition, "JD3X11")) {
    decomp <- x$output$decomposition
    d8 <- decomp$d8 # Final unmodified SI Ratios
    d9 <- decomp$d9 # Final replacement for SI
    d10 <- decomp$d10 # Seasonal Factors
    cyc <- p3_cycleName(d10)
    d10Mean <- rep(by(d10, cyc, mean)[as.character(cyc)], length.out = length(d10))
    dat <- siRatiosData(
      list(
        `SI-Ratio` = d8, `Replaced SI-Ratio` = d9,
        `Seasonal Factor` = d10, `SF Mean` = ts(d10Mean,
          start = stats::start(d10), frequency = stats::frequency(d10)
        )
      ),
      cyc
    )
    cols <- c("Seasonal Factor", "SF Mean", "SI-Ratio", "Replaced SI-Ratio")
    dots <- c("SI-Ratio", "Replaced SI-Ratio")
    if (is.null(main)) {
      main <- "SI Ratios and Seasonal Factors by Period"
    }
  } else if (inherits(x$output$decomposition, "JD3_SEATS")) {
    sln <- x$output$user_defined$decomposition.s_cmp
    iln <- x$output$user_defined$decomposition.i_cmp
    mode <- x$output$user_defined$mode
    siRatio <- if (mode == "Additive") sln + iln else sln * iln
    cyc <- p3_cycleName(siRatio)
    m <- rep(by(siRatio, cyc, mean)[as.character(cyc)], length.out = length(siRatio))
    dat <- siRatiosData(
      list(
        `SI-Ratio` = siRatio,
        `Mean` = ts(m, start = stats::start(siRatio),
          frequency = stats::frequency(siRatio)
        )
      ),
      cyc
    )
    cols <- c("Mean", "SI-Ratio")
    dots <- "SI-Ratio"
    if (is.null(main)) {
      main <- "SI Ratios by Period"
    }
  } else {
    stop("unknown decomposition", call. = FALSE)
  }

  colors <- list(
    `SI-Ratio` = "#00643C",
    `Replaced SI-Ratio` = "red",
    `Seasonal Factor` = "#A59187",
    `SF Mean` = "#5A8CBE",
    `Mean` = "#5A8CBE"
  )

  p <- plotly::plot_ly(...) %>%
    plotlySiLines(dat, cols = cols, dots = dots, colors = colors)
  p %>%
    plotly::layout(
      title = main,
      legend = list(orientation = "h", x = 0, y = 1.1, traceorder = "normal"),
      margin = list(l = 50, r = 20, t = 60, b = 30),
      hovermode = "closest",
      plot_bgcolor = "white",
      yaxis = list(title = "", zeroline = FALSE)
    )
}
