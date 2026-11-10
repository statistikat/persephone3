#' Spectral Plots (Interactive)
#'
#' Create interactive spectral plots using highcharter. This version provides
#' interactive features such as zoom, pan, and hover tooltips.
#' For static plots with ggplot2, see [plotSpectrumGgplot].
#'
#' @param x an object of class [persephone].
#' @param tsType character vector with one or more of
#' (`"original"`,`"sa"`,`"irregular"`,`"residuals"`)
#' selecting the preferred type(s) of time series to plot. If more than one
#' type is given, the spectra are overlaid in one plot (not available for
#' plotType `"arSpecBars"`). A numeric
#' value (`1:4`) corresponding to one of these characters is also accepted.
#' @param plotType character (`"arSpec"`,`"arSpecBars"`,`"periodogram"`)
#' selecting the preferred type of plot, see Details. A numeric
#' value (`1:3`) corresponding to one of these characters is also accepted.
#' @param maxobs maximum number of observations of the time series. If `NULL`,
#' `maxobs` equals the length of the time series for plotTypes
#' `"arSpec"` and `"periodogram"` and `maxobs=96` for plotType `"arSpecBars"`.
#' @param main plot title
#' @param n.freq the number of frequencies, i.e. the number of points at which
#' to plot. If `NULL`, `n.freq=301` for plotTypes `"arSpec"` and `"periodogram"`
#' and `n.freq=61` for plotType `"arSpecBars"`.
#' @param order order of the AR model.
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSpectrumHc(x)
#' plotSpectrumHc(x, plotType = "periodogram")
#' plotSpectrumHc(x, tsType = "irregular")
#' # all three spectra in one plot
#' plotSpectrumHc(x, tsType = c("sa", "irregular", "residuals"))
#' @export
plotSpectrumHc <- function(
  x,
  tsType = "original",
  plotType = c("arSpec", "arSpecBars", "periodogram"),
  maxobs = NULL,
  main = NULL,
  n.freq = NULL,
  order = 30
) {
  stopifnot(inherits(x, "persephone"))
  tsType <- spec_tsTypes(tsType)
  plotType <- match.arg(plotType)
  if (length(tsType) > 1 && plotType == "arSpecBars") {
    stop(
      "overlaid spectra are not available for plotType \"arSpecBars\"",
      call. = FALSE
    )
  }
  series_list <- lapply(tsType, function(tt) plotSpectrum_series(x, tt))
  series <- series_list[[1]]
  freq <- switch(
    as.character(stats::frequency(series$ts)),
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
  plot_args <- list(
    series = series_list,
    maxobs = maxobs,
    freq = freq,
    main = main,
    n.freq = n.freq,
    order = order
  )
  switch(
    plotType,
    periodogram = plotSpectrum_periodogram(plot_args),
    arSpec = plotSpectrum_arSpec(plot_args),
    arSpecBars = plotSpectrum_arSpecBars(plot_args),
    stop("unsupported plot type")
  )
}

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

plotSpectrum_periodogram <- function(args) {
  series_labels <- vapply(args$series, function(s) s$type, "")
  main <- args$main %||%
    paste0(
      "Periodogram of the ",
      paste(series_labels, collapse = ", "),
      " series"
    )
  vals <- lapply(
    args$series,
    specValues,
    plotType = "periodogram",
    n.freq = NULL,
    order = args$order
  )
  names(vals) <- series_labels
  specPlotLines(create_spectral_plot_multi(vals, title = main), args$freq)
}


plotSpectrum_arSpec <- function(args) {
  series_labels <- vapply(args$series, function(s) s$type, "")
  main <- args$main %||%
    paste0(
      "Autoregressive spectrum of the ",
      paste(series_labels, collapse = ", "),
      " series"
    )
  vals <- lapply(
    args$series,
    specValues,
    plotType = "arSpec",
    n.freq = args$n.freq,
    order = args$order
  )
  names(vals) <- series_labels
  specPlotLines(create_spectral_plot_multi(vals, title = main), args$freq)
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

# common axis setup with seasonal (grey) and trading day (red) markers
specPlotLines <- function(p, freq) {
  p %>%
    highcharter::hc_xAxis(
      plotLines = c(
        lapply(freq$seas, function(x) {
          list(value = x)
        }),
        lapply(freq$td, function(x) {
          list(value = x, color = "#ff000044")
        })
      )
    )
}

# one or more overlaid spectra as line series
create_spectral_plot_multi <- function(vals, title) {
  p <- highcharter::highchart() %>%
    highcharter::hc_xAxis(title = list(text = "Frequency")) %>%
    highcharter::hc_yAxis(title = list(text = "Spectrum")) %>%
    highcharter::hc_title(text = title, align = "left")
  for (nm in names(vals)) {
    p <- p %>%
      highcharter::hc_add_series(
        data.frame(x = vals[[nm]]$freq, y = vals[[nm]]$spec),
        type = "line",
        name = nm
      )
  }
  p %>% highcharter::hc_legend(align = "left", verticalAlign = "top")
}

plotSpectrum_arSpecBars <- function(args) {
  main <- args$main %||%
    paste0("Autoregressive spectrum of the ", args$series[[1]]$type, " series")
  n.freq <- args$n.freq %||% 61
  order <- args$order
  d1 <- args$series[[1]]$d
  d3s <- stats::spec.ar(d1, n.freq = n.freq, order = order, plot = FALSE)
  d3s <- data.frame(
    freq = d3s$freq / stats::frequency(d1),
    spec = 10 * log10(d3s$spec / sum(d3s$spec))
  )
  td_freq_approx <- sapply(args$freq$td, function(f) {
    d3s$freq[which(abs(d3s$freq - f) == min(abs(d3s$freq - f)))]
  })
  td_freq_approx
  d3s$spec_new1 <- rescale(d3s$spec, 1, max(d3s$spec - min(d3s$spec)))
  breaks_15 <- seq(1, max(d3s$spec_new1), length = 15)
  labels_15 <- as.character(round(
    rescale(breaks_15, min(d3s$spec), max(d3s$spec)),
    digits = 0
  ))
  breaks_15
  seas_td_col <- rep("grey", length(d3s$freq))
  seas_td_col[which(d3s$freq %in% args$freq$td)] <- "blue"
  seas_td_col[which(d3s$freq %in% td_freq_approx)] <- "red"
  seas_td_col
  d3s$color <- seas_td_col
  d3s
  highcharter::highchart() %>%
    highcharter::hc_add_series(
      data.frame(x = d3s$freq, y = abs(d3s$spec_new1), color = d3s$color),
      type = "column",
      showInLegend = FALSE,
      findNearestPointBy = "x",
      name = "AR spectrum"
    ) %>%
    highcharter::hc_xAxis(crosshair = list(width = 20, color = "#eee")) %>%
    highcharter::hc_title(text = main, align = "left") %>%
    highcharter::hc_yAxis(title = list(text = "Spectrum")) %>%
    highcharter::hc_xAxis(title = list(text = "Frequency"))
}

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
