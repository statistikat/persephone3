#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

ignore_unused_imports <- function() {
  rjd3tramoseats::tramoseats
  rjd3x13::x13
}

.onLoad <- function(...) {
  # avoid issues with missing rprotobuf descriptor which looks like this during
  # devtools::check()
  #> perX13(AirPassengers)$run()
  #> Error in h(simpleError(msg, call)) :
  #>   error in evaluating the argument 'descriptor' in selecting a method for
  #>   function 'read': object 'x13.X13Output' not found
  #> Calls: <Anonymous> ... .x13_output -> <Anonymous> -> .handleSimpleError -> h
  if (!requireNamespace("rjd3toolkit", quietly = T)) {
    stop("Loading rjd3toolkit failed")
  }
}

# Null-Coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

rescale <- function(x, min, max) {
  stopifnot(
    is.numeric(x),
    is.numeric(min),
    is.numeric(max),
    length(min) == 1,
    length(max) == 1
  )
  rg <- range(x)
  min + (x - rg[1]) / (rg[2] - rg[1]) * (max - min)
}

# interne Template-Mengen je Methode (einzig wahre Quelle)
sa_templates <- function(method = c("x13", "tramoseats")) {
  switch(
    match.arg(method),
    x13 = c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c"),
    tramoseats = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
  )
}

# expliziter Exakt-Check (match.arg wuerde partiell matchen: "rsa5" -> "rsa5c")
check_template <- function(template, method) {
  allowed <- sa_templates(method)
  if (length(template) != 1L || !(template %in% allowed)) {
    stop(sprintf(
      "Invalid template '%s' for method '%s'. Allowed values: %s",
      template,
      method,
      paste(allowed, collapse = ", ")
    ))
  }
  template
}

# Methode ist die R6-Klasse und laesst sich an bestehenden Objekten nicht
# aendern; iterate = TRUE kann sie daher nicht umschreiben.
check_component_method <- function(components, method, iterate = FALSE) {
  comp_method <- vapply(
    components,
    function(x) {
      if (is.persephone(x)) x$method else NA_character_
    },
    character(1)
  )
  conflict <- names(comp_method)[!is.na(comp_method) & comp_method != method]
  if (length(conflict) == 0) {
    return(invisible(NULL))
  }
  methods <- paste(unique(comp_method[conflict]), collapse = "/")
  msg <- sprintf(
    paste0(
      "Components %s are already '%s' objects and cannot be switched to ",
      "method = \"%s\"; they stay '%s'."
    ),
    paste(sQuote(conflict), collapse = ", "),
    methods,
    method,
    methods
  )
  if (isTRUE(iterate)) {
    msg <- paste0(
      msg,
      paste0(
        " iterate = TRUE updates their spec parameters but not their method; ",
        "pass them as plain ts/mts objects to have them created with '",
        method,
        "'."
      )
    )
  }
  warning(msg, call. = FALSE)
  invisible(NULL)
}

# spec-blocks and their corresponding functions
specfun_list <- list(
  arima = rjd3toolkit::set_arima,
  automdl = rjd3toolkit::set_automodel,
  basic = rjd3toolkit::set_basic,
  easter = rjd3toolkit::set_easter,
  estimate = rjd3toolkit::set_estimate,
  setout = rjd3toolkit::set_outlier,
  td = rjd3toolkit::set_tradingdays,
  transform = rjd3toolkit::set_transform,
  addout = rjd3toolkit::add_outlier,
  remout = rjd3toolkit::remove_outlier,
  addramp = rjd3toolkit::add_ramp,
  remramp = rjd3toolkit::remove_ramp,
  usrdefvar = rjd3toolkit::add_usrdefvar,
  x11 = rjd3x13::set_x11, # only for x13
  benchmarking = rjd3toolkit::set_benchmarking
)

# persephone3 internal spec names
p3_spec_names <- c("outliers")

# default colours for the series of the time series plots; the first five
# entries are the default colours of Highcharts, so that the plots have the
# style of the Highcharts plots of the JDmetra+ web application; named by
# the series labels of the plots
p3_series_colors <- c(
  Original = "#7cb5ec",
  Trend = "#434348",
  `Seasonally Adjusted` = "#90ed7d",
  Forecasts = "#808080",
  `Confidence Band` = "#7cb5ec",
  `Direct SA` = "#434348",
  `Indirect SA` = "#90ed7d",
  `Seasonal Component` = "#7cb5ec",
  `Irregular Component` = "#434348",
  `Calendar Effects` = "#90ed7d"
)

# default colours of the outlier types in the time series plots
p3_outlier_colors <- list(AO = "red", LS = "orange", TC = "#8055A4")

# JS plotter for the confidence band of the forecasts in the dygraph plots:
# fills the polygon between the "lowerci" column and the column of the
# series it is attached to ("upperci", its own column is opts.seriesIndex +
# 1 because the series label renames the column of getLabels()), like the
# band of the forecasts in the highcharter plots. The two columns are drawn
# with strokeWidth 0, so only the band between them is visible.
p3_band_plotter <- htmlwidgets::JS(paste0(
  "function(opts) {",
  "  var g = opts.dygraph, ctx = opts.drawingContext, area = opts.plotArea;",
  "  var labels = g.getLabels();",
  "  var loCol = labels.indexOf('lowerci');",
  "  var hiCol = opts.seriesIndex + 1;",
  "  var rows = [], i, j, lo, hi;",
  "  if (loCol >= 0) {",
  "    for (i = 0; i < g.numRows(); i++) {",
  "      lo = g.getValue(i, loCol); hi = g.getValue(i, hiCol);",
  "      if (lo === null || hi === null || isNaN(lo) || isNaN(hi)) continue;",
  "      rows.push([g.toDomXCoord(g.getValue(i, 0)),",
  "                 g.toDomYCoord(hi), g.toDomYCoord(lo)]);",
  "    }",
  "  }",
  "  if (rows.length > 1) {",
  "    ctx.save();",
  "    ctx.beginPath(); ctx.rect(area.x, area.y, area.w, area.h); ctx.clip();",
  "    ctx.fillStyle = 'rgba(124, 181, 236, 0.25)';",
  "    ctx.beginPath();",
  "    ctx.moveTo(rows[0][0], rows[0][1]);",
  "    for (j = 1; j < rows.length; j++) ctx.lineTo(rows[j][0], rows[j][1]);",
  "    for (j = rows.length - 1; j >= 0; j--) ctx.lineTo(rows[j][0], rows[j][2]);",
  "    ctx.closePath(); ctx.fill(); ctx.restore();",
  "  }",
  "}"
))

# CSS that hides the legend entries of the two confidence limit columns
# (their colour is the transparent band colour); the forecast itself has its
# own entry in the legend
p3_band_css <- paste0(
  ".dygraph-legend span[style*='124, 181, 236'] {\n",
  "  display: none !important;\n",
  "}\n"
)

# the two invisible series of the confidence band, to be added to a dygraph
# after the series of the central forecast ("ppm_y_f")
p3_band_series <- function(graphObj) {
  graphObj <- graphObj %>%
    dySeries(
      "lowerci",
      label = "lowerci",
      color = "rgba(124, 181, 236, 0.01)",
      strokeWidth = 0
    ) %>%
    dySeries(
      "upperci",
      label = "Confidence Band",
      color = "rgba(124, 181, 236, 0.01)",
      strokeWidth = 0,
      plotter = p3_band_plotter
    )
  graphObj %>% dyCSS(textConnection(p3_band_css))
}

# fluid sizing for the dygraph widgets: without it the htmlwidgets sizing
# policy keeps the fixed 960px default width of dygraphs, so in small windows
# (dashboard panels, small viewers) the chart does not shrink with its
# container and the right part of the plot is cut off
p3_fluid <- function(graphObj) {
  graphObj$width <- "100%"
  graphObj$height <- NULL
  graphObj$x$sizingPolicy <- htmlwidgets::sizingPolicy(
    defaultWidth = "100%", defaultHeight = "auto",
    viewer.defaultWidth = "100%", viewer.defaultHeight = "auto",
    browser.defaultWidth = "100%", browser.defaultHeight = "auto",
    knitr.defaultWidth = "100%", knitr.defaultHeight = "auto",
    padding = 0, browser.fill = TRUE, viewer.fill = TRUE,
    knitr.figure = FALSE
  )
  graphObj
}

# interpolation of the observations on a finer time grid; two methods:
# "monotone" (default) is a monotone piecewise cubic Hermite interpolation
# (Fritsch-Carlson, as MATLAB's pchip): smooth, but it cannot overshoot the
# data or oscillate between observations, so the line does not wobble when
# the plot is enlarged. "spline" is the not-a-knot cubic spline (the same
# family of curves as the smoothed lines of Highcharts with
# curveType = "spline"), which can slightly overshoot the sharpest peaks and
# produce small ripples
dySmooth <- function(x, y, xout, method = "monotone") {
  if (identical(method, "spline")) {
    return(stats::spline(x, y, xout = xout)$y)
  }
  n <- length(x)
  if (n < 3) {
    return(stats::approx(x, y, xout = xout)$y)
  }
  h <- diff(x)
  delta <- diff(y) / h
  # slopes at the nodes: weighted harmonic mean of the slopes of the two
  # neighbouring intervals (Fritsch-Carlson), zero at local extrema and
  # flat spots, one-sided at the ends
  d <- numeric(n)
  int <- 2:(n - 1)
  dm1 <- delta[int - 1]
  dm2 <- delta[int]
  ok <- dm1 * dm2 > 0
  w1 <- 2 * h[int] + h[int - 1]
  w2 <- h[int] + 2 * h[int - 1]
  d[int] <- ifelse(ok, (w1 + w2) / (w1 / dm1 + w2 / dm2), 0)
  d[int][!is.finite(d[int])] <- 0
  d[1] <- delta[1]
  d[n] <- delta[n - 1]
  # evaluate the piecewise cubic Hermite polynomial
  j <- pmin(pmax(findInterval(xout, x), 1), n - 1)
  s <- pmin(pmax((xout - x[j]) / h[j], 0), 1)
  h00 <- (1 + 2 * s) * (1 - s)^2
  h10 <- s * (1 - s)^2
  h01 <- s^2 * (3 - 2 * s)
  h11 <- s^2 * (s - 1)
  h00 * y[j] + h10 * h[j] * d[j] + h01 * y[j + 1] + h11 * h[j] * d[j + 1]
}

# normalize the smooth argument of the plot functions: TRUE (monotone,
# default), FALSE (no smoothing) or "spline" (the overshooting not-a-knot
# spline)
smoothMethod <- function(smooth) {
  if (is.logical(smooth)) {
    if (isTRUE(smooth)) "monotone" else FALSE
  } else if (identical(smooth, "spline")) {
    "spline"
  } else if (identical(smooth, "monotone")) {
    "monotone"
  } else if (isFALSE(smooth)) {
    FALSE
  } else {
    stop('smooth must be TRUE, FALSE or "spline"', call. = FALSE)
  }
}

# is the smoothed line drawn at all (for the smooth argument of the plot
# functions, see smoothMethod)
isSmoothed <- function(smooth) {
  !isFALSE(smoothMethod(smooth))
}

# add a title to each panel of a plotly::subplot() figure (subplot drops the
# individual figure titles, so they are re-added as axis-anchored
# annotations); titles are ordered like the panels: top row first, inside a
# row from left to right. main is the overall title of the figure.
subplotTitles <- function(p, titles, main = NULL, size = 14) {
  lay <- Reduce(
    function(a, b) utils::modifyList(a, b),
    c(list(p$x$layout), p$x$layoutAttrs)
  )
  xs <- grep("^xaxis(\\d*)$", names(lay), value = TRUE)
  # name of the layout entry of the y axis an x axis is anchored to
  yAxisKey <- function(ax) {
    an <- lay[[ax]]$anchor %||% "y"
    if (identical(an, "")) "yaxis" else sub("^y", "yaxis", an)
  }
  # panels: rows top to bottom (start of the y axis domain descending),
  # inside a row left to right (start of the x axis domain)
  ord <- order(
    -vapply(xs, function(a) lay[[yAxisKey(a)]]$domain[1], 0),
    vapply(xs, function(a) lay[[a]]$domain[1], 0)
  )
  xs <- xs[ord]
  ann <- Map(function(ax, ti) {
    list(
      x = mean(lay[[ax]]$domain), xref = "paper",
      y = 1.08, yref = lay[[ax]]$anchor %||% "y",
      text = ti, showarrow = FALSE, xanchor = "center",
      font = list(size = size)
    )
  }, xs, titles)
  p %>%
    plotly::layout(
      annotations = ann,
      title = if (is.null(main)) NULL else list(text = main),
      margin = list(t = if (is.null(main)) 50 else 75)
    )
}

# decimal time (as in time(ts)) to Date: the observation times map to the
# first day of the respective month/quarter/half year, points in between are
# interpolated linearly inside the period; needed because smoothed series are
# evaluated on a finer time grid. The period index is determined by flooring
# (except for grid points equal to a period start, where the decimals of the
# time index are not exact): rounding would send the second half of every
# period to the start of the next one, which makes the resulting dates
# non-monotonic (repeated dates at every period boundary) and produces
# vertical lines in the dygraphs/plotly plots when zooming in.
timesToDates <- function(tt, freq) {
  stopifnot(all(freq %in% c(1L, 2L, 4L, 12L)))
  months_per_period <- as.integer(12 / freq)
  v <- as.numeric(tt) * freq
  vr <- round(v)
  q <- ifelse(abs(v - vr) < 1e-6, vr, floor(v)) # index of the current period
  r <- as.numeric(tt) - q / freq    # offset inside the period
  y <- floor(q / freq)
  m <- q %% freq # 0-based period within the year
  mon <- m * months_per_period + 1L
  d0 <- as.Date(sprintf("%04d-%02d-01", y, mon))
  mon1 <- mon + months_per_period
  y1 <- y + as.integer(mon1 > 12L)
  mon1 <- ifelse(mon1 > 12L, mon1 - 12L, mon1)
  d1 <- as.Date(sprintf("%04d-%02d-01", y1, mon1))
  as.Date(d0 + pmin(pmax(r * freq, 0), 1 - 1e-9) * as.numeric(d1 - d0))
}

# Prepare input for dygraphs::dygraph() from a ts/mts object: optionally
# cubic spline interpolation of the columns in smooth_cols on a finer time
# grid (smoothed lines between the observations), and conversion of the time
# index to dates. Columns that are not smoothed (outlier markers, forecast
# bands) keep their original values, placed on the corresponding points of
# the finer grid.
# Each smoothed column is evaluated only inside its own non-missing time
# window: the interpolation is not extrapolated beyond the first and last
# observation of that column. Otherwise, columns that are shorter than the
# common time index (as the estimated series next to the forecasts, which
# extend the index by the forecast horizon) would blow up at the ends of the
# plot and determine the scaling of the y-axis.
dyData <- function(m, smooth = TRUE, smooth_cols = NULL, n = 8) {
  method <- smoothMethod(smooth)
  smooth <- !isFALSE(method)
  ti <- as.numeric(stats::time(m))
  freq <- stats::frequency(m)
  if (!stats::is.ts(m)) {
    stop("dyData() expects a ts/mts object")
  }
  if (!stats::is.mts(m)) {
    nm <- colnames(m)[1]
    if (is.null(nm) || is.na(nm)) {
      nm <- "V1"
    }
    m <- matrix(as.numeric(m), ncol = 1, dimnames = list(NULL, nm))
  }
  if (is.null(colnames(m))) {
    colnames(m) <- paste0("V", seq_len(ncol(m)))
  }
  if (smooth && length(ti) > 4) {
    tnew <- seq(ti[1], ti[length(ti)], length.out = (length(ti) - 1) * n + 1)
    grid <- 1 + n * (seq_along(ti) - 1) # index of the original points in tnew
    out <- matrix(NA_real_, nrow = length(tnew), ncol = ncol(m),
                  dimnames = list(NULL, colnames(m)))
    for (j in seq_len(ncol(m))) {
      yj <- as.numeric(m[, j])
      if (colnames(m)[j] %in% smooth_cols && sum(!is.na(yj)) > 3) {
        tj <- ti[!is.na(yj)]
        yjv <- yj[!is.na(yj)]
        # no extrapolation: keep the interpolation within the non-missing range
        inw <- tnew >= min(tj) - 1e-9 & tnew <= max(tj) + 1e-9
        out[inw, j] <- dySmooth(tj, yjv, tnew[inw], method = method)
      } else {
        out[grid, j] <- yj
      }
    }
    ti <- tnew
    m <- out
  }
  cbind(
    data.frame(Date = timesToDates(ti, freq)),
    as.data.frame(m, check.names = FALSE)
  )
}
