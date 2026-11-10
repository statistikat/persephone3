#' @importFrom highcharter hchart hc_title
#' @export
highcharter::hchart

#' @export
highcharter::hc_title

#' @importFrom highcharter %>%
#' @export
highcharter::`%>%`

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

# default colours for the series of the dygraph plots (Okabe-Ito palette,
# colour-blind friendly); named by the dySeries() labels
p3_series_colors <- c(
  Original = "#0072B2",
  `Seasonally Adjusted` = "#D55E00",
  Trend = "#009E73",
  Forecasts = "#CC79A7",
  `Direct SA` = "#D55E00",
  `Indirect SA` = "#009E73"
)

# CSS for the outlier annotation labels of the dygraph plots: the dygraphs
# annotation plugin positions the label at the data point of the series,
# here it is pinned to the top of the plot area instead (below the title);
# the border colour of the label follows the colour of its series
p3_outlier_css <- paste0(
  ".p3-outlier-ann {\n",
  "  top: 22px !important;\n",
  "  width: auto !important;\n",
  "  font-size: 10px;\n",
  "  line-height: 14px;\n",
  "  padding: 0 1px;\n",
  "  white-space: nowrap;\n",
  "}"
)

# decimal time (as in time(ts)) to Date: the observation times map to the
# first day of the respective month/quarter/half year, points in between are
# interpolated linearly inside the period; needed because smoothed series are
# evaluated on a finer time grid
timesToDates <- function(tt, freq) {
  stopifnot(all(freq %in% c(1L, 2L, 4L, 12L)))
  months_per_period <- as.integer(12 / freq)
  q <- round(as.numeric(tt) * freq) # index of the current period
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
# window: the cubic spline is not extrapolated beyond the first and last
# observation of that column. Otherwise, columns that are shorter than the
# common time index (as the estimated series next to the forecasts, which
# extend the index by the forecast horizon) would blow up at the ends of the
# plot and determine the scaling of the y-axis. Within the window the
# interpolated values are clipped to the range of the two bracketing
# observations, so that the spline does not invent maxima above (or minima
# below) the data at the seasonal peaks.
dyData <- function(m, smooth = TRUE, smooth_cols = NULL, n = 8) {
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
        # no extrapolation: keep the spline within the non-missing range
        inw <- tnew >= min(tj) - 1e-9 & tnew <= max(tj) + 1e-9
        sm <- stats::spline(tj, yjv, xout = tnew[inw])$y
        # clip to the range of the two bracketing observations
        seg <- pmax(findInterval(tnew[inw], tj), 1L)
        seg1 <- pmin(seg + 1L, length(tj))
        sm <- pmin(pmax(sm, pmin(yjv[seg], yjv[seg1])),
                   pmax(yjv[seg], yjv[seg1]))
        out[inw, j] <- sm
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
