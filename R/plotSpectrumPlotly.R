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

# preparation of the series and of the plot arguments shared by all engines
# of plotSpectrum(); returns NULL if everything is fine, otherwise the data
# for the engines (as list) or an error
specPrepare <- function(x, tsType, plotType, maxobs, main, n.freq, order) {
  tsType <- spec_tsTypes(tsType)
  plotType <- match.arg(plotType,
                        c("arSpec", "arSpecBars", "periodogram"))
  if (length(tsType) > 1 && plotType == "arSpecBars") {
    stop(
      "overlaid spectra are not available for plotType \"arSpecBars\"",
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
    if (plotType %in% c("arSpec", "periodogram")) {
      # Default settings in JD+
      maxobs <- length(series$ts)
    } else if (plotType == "arSpecBars") {
      # Default settings in X-13ARIMA-SEATS
      maxobs <- 96
    } else {
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

# vertical lines for the seasonal and trading day cycles (plotly shapes)
specPlotLinesPlotly <- function(p, freq) {
  shapes <- c(
    lapply(freq$seas, function(x) {
      list(
        type = "line", xref = "x", yref = "paper",
        x0 = x, x1 = x, y0 = 0, y1 = 1,
        line = list(color = "grey", width = 1)
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

#' Spectral Plots (plotly)
#'
#' Interactive spectral plots with plotly, with overlaid spectra for several
#' series types and vertical lines at the seasonal (grey) and trading day
#' (red) frequencies. For static plots with ggplot2, see [plotSpectrumGgplot].
#'
#' @inheritParams plotSpectrum
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSpectrumPlotly(x)
#' plotSpectrumPlotly(x, plotType = "periodogram")
#' plotSpectrumPlotly(x, tsType = c("sa", "irregular", "residuals"))
#' @export
plotSpectrumPlotly <- function(
  x,
  tsType = "original",
  plotType = c("arSpec", "arSpecBars", "periodogram"),
  maxobs = NULL,
  main = NULL,
  n.freq = NULL,
  order = 30,
  ...
) {
  stopifnot(inherits(x, "persephone"))
  args <- specPrepare(x, tsType, plotType, maxobs, main, n.freq, order)
  series_labels <- vapply(args$series, function(s) s$type, "")

  if (args$plotType == "arSpecBars") {
    # the spectrum of the first series as bars, the bars at the trading day
    # frequencies highlighted in red (as in the other engines)
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
    col <- rep("#7cb5ec", nrow(d3s))
    col[which(d3s$freq %in% args$freq$td)] <- "blue"
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
      )
    return(p)
  }

  if (args$plotType == "periodogram") {
    default_main <- paste0(
      "Periodogram of the ",
      paste(series_labels, collapse = ", "),
      " series"
    )
    ptype <- "periodogram"
    nfreq <- NULL
  } else {
    default_main <- paste0(
      "Autoregressive spectrum of the ",
      paste(series_labels, collapse = ", "),
      " series"
    )
    ptype <- "arSpec"
    nfreq <- args$n.freq %||% 301
  }
  vals <- lapply(
    args$series,
    specValues,
    plotType = ptype,
    n.freq = nfreq,
    order = args$order
  )
  names(vals) <- series_labels

  p <- plotly::plot_ly(...)
  for (nm in names(vals)) {
    p <- p %>%
      plotly::add_trace(
        x = vals[[nm]]$freq, y = vals[[nm]]$spec,
        type = "scatter", mode = "lines", name = nm,
        line = list(width = 1.6)
      )
  }
  p <- specPlotLinesPlotly(p, args$freq)
  p %>%
    plotly::layout(
      title = args$main %||% default_main,
      legend = list(orientation = "h", x = 0, y = 1.1, traceorder = "normal"),
      margin = list(l = 50, r = 20, t = 60, b = 40),
      hovermode = "x unified",
      plot_bgcolor = "white",
      xaxis = list(title = "Frequency", showgrid = FALSE),
      yaxis = list(title = "Spectrum", zeroline = FALSE)
    )
}
