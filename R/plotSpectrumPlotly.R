# shared internals of the spectral plots and their plotly engine (the
# static ggplot2 engine is in plotSpectrumGgplot.R)

# validate and expand the tsType argument (character or numeric codes)
spec_tsTypes <- function(tsType) {
  all_types <- c("original", "sa", "irregular", "residuals")
  if (is.numeric(tsType)) {
    tsType <- all_types[tsType]
  }
  tsType <- unique(tsType)
  bad <- setdiff(tsType, all_types)
  if (length(bad) > 0) {
    stop(
      "invalid tsType: ", paste(sQuote(bad), collapse = ", "),
      "; allowed are: ", paste(all_types, collapse = ", "),
      call. = FALSE
    )
  }
  tsType
}

# the time series of the requested type with its first difference (for the
# non stationary types) and the frequencies of the seasonal and trading day
# cycles
plotSpectrum_series <- function(
  x,
  tsType = c("original", "sa", "irregular", "residuals")
) {
  tsType <- spec_tsTypes(tsType)[1]

  if (tsType == "original") {
    tsobj <- x$ts
    # First differences -> stationary ts
    d1 <- diff(tsobj, 1)
  } else if (tsType == "sa") {
    tsobj <- x$output$user_defined$sa
    # First differences -> stationary ts
    d1 <- diff(tsobj, 1)
  } else if (tsType == "irregular") {
    tsobj <- x$output$user_defined$i
    d1 <- tsobj
  } else if (tsType == "residuals") {
    tsobj <- x$output$user_defined$residuals.tsres
    d1 <- tsobj
  }
  list(ts = tsobj, d = d1, type = tsType)
}

# spectrum of a single series prepared for plotting: data.frame(freq, spec)
specValues <- function(s, plotType, n.freq, order) {
  if (plotType == "periodogram") {
    sp <- stats::spec.pgram(s$d, plot = FALSE)
    data.frame(
      freq = sp$freq / stats::frequency(s$d),
      spec = sp$spec
    )
  } else {
    sp <- stats::spec.ar(
      s$d,
      n.freq = n.freq %||% length(s$ts),
      order = order,
      plot = FALSE
    )
    data.frame(
      freq = sp$freq / stats::frequency(s$d),
      spec = 10 * log10(2 * sp$spec)
    )
  }
}

# the seasonal and trading day frequencies of a series frequency (4 or 12)
specFreqs <- function(frequency) {
  switch(
    as.character(frequency),
    `4` = list(
      td = c(0.044375, 0.08875, 0.294375, 0.33875, 0.38125),
      seas = 1 / 4,
      seas_lab = c("1/4"),
      minobs = 60
    ),
    `12` = list(
      td = c(0.348, 0.432),
      seas = c(1 / 12, 2 / 12, 3 / 12, 4 / 12, 5 / 12, 6 / 12),
      seas_lab = c("1/12", "2/12", "3/12", "4/12", "5/12", "6/12"),
      minobs = 80
    ),
    stop("invalid frequency")
  )
}

# validate and expand the plotType argument (character or numeric codes)
spec_plotTypes <- function(plotType) {
  all_types <- c("arSpec", "arSpecBars", "periodogram")
  if (is.numeric(plotType)) {
    plotType <- all_types[plotType]
  }
  plotType <- unique(plotType)
  bad <- setdiff(plotType, all_types)
  if (length(bad) > 0 || anyNA(plotType)) {
    stop(
      "invalid plotType: ", paste(sQuote(bad), collapse = ", "),
      "; allowed are: ", paste(all_types, collapse = ", "),
      call. = FALSE
    )
  }
  plotType
}

# short panel titles for the plot types
specPlotTypeLabel <- function(plotType) {
  c(
    arSpec = "AR spectrum",
    arSpecBars = "AR spectrum (bars)",
    periodogram = "Periodogram"
  )[plotType]
}

# preparation of the series and of the plot arguments shared by all engines
# of plotSpectrum(); returns NULL if everything is fine, otherwise the data
# for the engines (as list) or an error
specPrepare <- function(x, tsType, plotType, maxobs, main, n.freq, order) {
  tsType <- spec_tsTypes(tsType)
  plotType <- spec_plotTypes(plotType)
  if ("arSpecBars" %in% plotType &&
      (length(tsType) > 1 || length(plotType) > 1)) {
    stop(
      "plotType \"arSpecBars\" is only available for a single series type",
      call. = FALSE
    )
  }
  series_list <- lapply(tsType, function(tt) plotSpectrum_series(x, tt))
  series <- series_list[[1]]
  freq <- specFreqs(stats::frequency(series$ts))

  for (s in series_list) {
    if (length(s$ts) <= freq$minobs) {
      stop(
        "The minimum number of observations needed to compute the spectrum",
        " is ",
        freq$minobs,
        "."
      )
    }
  }
  if (is.null(maxobs)) {
    if ("arSpecBars" %in% plotType) {
      # Default settings in X-13ARIMA-SEATS
      maxobs <- 96
    } else {
      # Default settings in JD+
      maxobs <- length(series$ts)
    }
  }
  windowIfLong <- function(s) {
    if (length(s$ts) > maxobs) {
      s$d <- stats::window(
        s$d,
        start = stats::start(stats::lag(
          stats::ts(
            end = stats::end(s$d),
            frequency = stats::frequency(s$d)
          ),
          maxobs - 2
        ))
      )
    }
    s
  }
  series_list <- lapply(series_list, windowIfLong)
  list(
    series = series_list,
    freq = freq,
    plotType = plotType,
    main = main,
    maxobs = maxobs,
    n.freq = n.freq,
    order = order
  )
}

# vertical lines for the seasonal (blue, as in the ggplot2 engine) and
# trading day (red) cycles (plotly shapes)
specPlotLinesPlotly <- function(p, freq) {
  shapes <- c(
    lapply(freq$seas, function(x) {
      list(
        type = "line", xref = "x", yref = "paper",
        x0 = x, x1 = x, y0 = 0, y1 = 1,
        line = list(color = p3_blue, width = 1)
      )
    }),
    lapply(freq$td, function(x) {
      list(
        type = "line", xref = "x", yref = "paper",
        x0 = x, x1 = x, y0 = 0, y1 = 1,
        line = list(color = "red", width = 1)
      )
    })
  )
  p %>% plotly::layout(shapes = shapes)
}

# the number of frequencies at which specValues() computes one spectrum
specNfreq <- function(plotType, n.freq) {
  if (plotType == "periodogram") NULL else n.freq %||% 301
}

#' Spectral Plots (plotly)
#'
#' Interactive spectral plots with plotly, with vertical lines at the
#' seasonal (blue) and trading day (red) frequencies. With
#' `layout = "overplot"` (the default) every plot type is a single plot with
#' the spectra of several series types overlaid in it (with a legend), and
#' several plot types give a named list with one plot per plot type. With
#' `layout = "panels"` a single plot is returned in which every combination
#' of plot type and series type gets a framed panel of its own in a
#' square-ish grid of equally wide panels, each panel with the height:width
#' ratio a plot of a single panel would have. For static plots with ggplot2, see
#' [plotSpectrumGgplot].
#'
#' @inheritParams plotSpectrum
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSpectrumPlotly(x)
#' plotSpectrumPlotly(x, plotType = "periodogram")
#' plotSpectrumPlotly(x, tsType = c("original", "sa", "irregular"))
#' # one plot per plot type (a named list)
#' plotSpectrumPlotly(x, tsType = c("original", "sa"),
#'                    plotType = c("arSpec", "periodogram"))
#' # the spectra in panels (three spectra give two rows of two panels)
#' plotSpectrumPlotly(x, tsType = c("original", "sa", "irregular"),
#'                    layout = "panels")
#' @export
plotSpectrumPlotly <- function(
  x,
  tsType = "original",
  plotType = "arSpec",
  layout = c("overplot", "panels"),
  maxobs = NULL,
  main = NULL,
  n.freq = NULL,
  order = 30,
  ...
) {
  stopifnot(inherits(x, "persephone"))
  layout <- match.arg(layout)
  args <- specPrepare(x, tsType, plotType, maxobs, main, n.freq, order)
  series_labels <- vapply(args$series, function(s) s$type, "")

  if ("arSpecBars" %in% args$plotType) {
    # the spectrum of the first series as bars, the bars at the seasonal and
    # trading day frequencies highlighted in blue and red (as in the other
    # engines)
    d1 <- args$series[[1]]$d
    nf <- args$n.freq %||% 61
    d3s <- stats::spec.ar(d1, n.freq = nf, order = order, plot = FALSE)
    d3s <- data.frame(
      freq = d3s$freq / stats::frequency(d1),
      spec = 10 * log10(d3s$spec / sum(d3s$spec))
    )
    d3s$spec_new1 <- scales::rescale(
      d3s$spec,
      from = range(d3s$spec),
      to = c(1, max(d3s$spec - min(d3s$spec)))
    )
    td_freq_approx <- vapply(args$freq$td, function(f) {
      d3s$freq[which.min(abs(d3s$freq - f))]
    }, 0)
    col <- rep("grey", nrow(d3s))
    col[which(d3s$freq %in% args$freq$seas)] <- p3_blue
    col[which(d3s$freq %in% td_freq_approx)] <- "red"
    main <- args$main %||%
      paste0("Autoregressive spectrum of the ", args$series[[1]]$type, " series")
    p <- plotly::plot_ly(...) %>%
      plotly::add_trace(
        x = d3s$freq, y = abs(d3s$spec_new1), type = "bar",
        marker = list(color = col), showlegend = FALSE,
        name = "AR spectrum"
      ) %>%
      plotly::layout(
        title = main,
        margin = list(l = 50, r = 20, t = 60, b = 40),
        plot_bgcolor = "white",
        xaxis = list(title = "Frequency", showgrid = FALSE),
        yaxis = list(title = "Spectrum", zeroline = FALSE)
      ) %>%
      p3_frame() %>%
      # the vertical line at the position of the mouse, the highlighting of
      # the hovered series, and a plot that fills the plot pane
      p3_hover() %>%
      p3_hover_highlight() %>%
      p3_fluid_plotly()
    return(p)
  }

  ptypes <- args$plotType
  spec_main <- function(ptype) {
    if (ptype == "periodogram") {
      paste0(
        "Periodogram of the ",
        paste(series_labels, collapse = ", "),
        " series"
      )
    } else {
      paste0(
        "Autoregressive spectrum of the ",
        paste(series_labels, collapse = ", "),
        " series"
      )
    }
  }
  # the title of the plot of one plot type: for several plot types a given
  # main is extended by the name of the plot type
  plot_main <- function(ptype) {
    if (is.null(args$main)) {
      spec_main(ptype)
    } else if (length(ptypes) == 1) {
      args$main
    } else {
      paste0(args$main, " (", specPlotTypeLabel(ptype), ")")
    }
  }

  # one spectrum line (the trace style is the same in overplots and panels)
  specLinePlotly <- function(p, vals, label, k, showlegend) {
    p %>%
      plotly::add_trace(
        x = vals$freq, y = vals$spec,
        type = "scatter", mode = "lines", name = label,
        showlegend = showlegend,
        line = list(
          width = 1.6,
          color = p3_spec_colors[[(k - 1) %% length(p3_spec_colors) + 1]]
        ),
        hovertemplate = paste0(label, ": %{y:.3f}<extra></extra>")
      )
  }

  if (layout == "panels") {
    # one panel per combination of plot type and series type, side by side
    # in the order of the arguments and equally wide (subplot widths)
    figs <- list()
    titles <- character(0)
    for (ptype in ptypes) {
      for (k in seq_along(args$series)) {
        vals <- specValues(
          args$series[[k]],
          plotType = ptype,
          n.freq = specNfreq(ptype, args$n.freq),
          order = args$order
        )
        fig <- plotly::plot_ly(...) %>%
          specLinePlotly(vals, series_labels[k], k, showlegend = FALSE) %>%
          specPlotLinesPlotly(args$freq) %>%
          plotly::layout(
            showlegend = FALSE,
            margin = list(l = 50, r = 20, t = 30, b = 40),
            plot_bgcolor = "white",
            xaxis = list(title = "", showgrid = FALSE),
            yaxis = list(title = "", zeroline = FALSE)
          )
        figs[[length(figs) + 1]] <- fig
        titles <- c(titles, if (length(ptypes) == 1) {
          series_labels[k]
        } else {
          paste(specPlotTypeLabel(ptype), series_labels[k], sep = " / ")
        })
      }
    }
    panels_main <- args$main %||% {
      if (length(ptypes) == 1) {
        spec_main(ptypes[1])
      } else {
        paste0(
          "Spectra of the ", paste(series_labels, collapse = ", "), " series"
        )
      }
    }
    # a square-ish grid of panels: ceil(sqrt(n)) columns, rows as needed,
    # the last row may be partially empty (3 panels give 2 columns and
    # 2 rows). The gap between the panels is wide enough for the titles of
    # the lower rows when there is more than one row.
    ncols <- ceiling(sqrt(length(figs)))
    nrows <- ceiling(length(figs) / ncols)
    p <- do.call(
      plotly::subplot,
      c(
        figs,
        list(nrows = nrows, margin = if (nrows > 1) 0.08 else 0.04)
      )
    ) %>%
      panelTitles(titles) %>%
      plotly::layout(
        title = if (is.null(panels_main)) NULL else list(text = panels_main),
        margin = list(
          l = 55, r = 20, b = 45,
          t = if (is.null(panels_main)) 50 else 75
        ),
        hovermode = "x unified",
        plot_bgcolor = "white",
        xaxis = list(title = if (nrows == 1) "Frequency" else ""),
        yaxis = list(title = "Spectrum")
      )
    if (nrows > 1) {
      # the axis titles belong to the edges of the grid: "Frequency" under
      # the bottom row, "Spectrum" beside the left column (subplot numbers
      # the panel axes in the order of the figures, row by row from top to
      # bottom)
      ups <- list()
      for (i in seq_along(figs)) {
        s <- if (i > 1L) as.character(i) else ""
        if ((i - 1L) %/% ncols + 1L == nrows) {
          ups[[paste0("xaxis", s)]] <- list(title = "Frequency")
        }
        if ((i - 1L) %% ncols == 0L) {
          ups[[paste0("yaxis", s)]] <- list(title = "Spectrum")
        }
      }
      p <- do.call(plotly::layout, c(list(p), ups))
    }
    p <- p %>%
      p3_frame() %>%
      p3_hover() %>%
      p3_hover_highlight() %>%
      p3_fluid_plotly(grid = c(nrows, ncols))
    return(p)
  }

  # one plot per plot type, the spectra of the series types overlaid in it
  plots <- lapply(ptypes, function(ptype) {
    p <- plotly::plot_ly(...)
    for (k in seq_along(args$series)) {
      vals <- specValues(
        args$series[[k]],
        plotType = ptype,
        n.freq = specNfreq(ptype, args$n.freq),
        order = args$order
      )
      p <- p %>% specLinePlotly(vals, series_labels[k], k, showlegend = TRUE)
    }
    p <- specPlotLinesPlotly(p, args$freq)
    p %>%
      plotly::layout(
        title = plot_main(ptype),
        legend = list(
          orientation = "h", x = 0, y = 1.1, traceorder = "normal"
        ),
        margin = list(l = 50, r = 20, t = 60, b = 40),
        hovermode = "x unified",
        plot_bgcolor = "white",
        xaxis = list(title = "Frequency", showgrid = FALSE),
        yaxis = list(title = "Spectrum", zeroline = FALSE)
      ) %>%
      p3_frame() %>%
      p3_hover() %>%
      p3_hover_highlight() %>%
      p3_fluid_plotly()
  })
  names(plots) <- specPlotTypeLabel(ptypes)
  if (length(plots) == 1L) {
    return(plots[[1L]])
  }
  plots
}
