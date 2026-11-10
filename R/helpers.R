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

# decimal time (as in time(ts)) to Date, linearly within the year; needed
# because smoothed series are evaluated on a finer time grid
timesToDates <- function(tt) {
  y <- floor(tt + 1e-9)
  frac <- pmin(pmax(tt - y, 0), 1 - 1e-9)
  d0 <- as.Date(paste0(y, "-01-01"))
  d1 <- as.Date(paste0(y + 1L, "-01-01"))
  as.Date(d0 + frac * as.numeric(d1 - d0))
}

# Prepare input for dygraphs::dygraph() from a ts/mts object: optionally
# cubic spline interpolation of the columns in smooth_cols on a finer time
# grid (smoothed lines between the observations), and conversion of the time
# index to dates. Columns that are not smoothed (outlier markers, forecast
# bands) keep their original values, placed on the corresponding points of
# the finer grid.
dyData <- function(m, smooth = TRUE, smooth_cols = NULL, n = 8) {
  ti <- as.numeric(stats::time(m))
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
        out[, j] <- stats::spline(ti[!is.na(yj)], yj[!is.na(yj)],
                                  xout = tnew)$y
      } else {
        out[grid, j] <- yj
      }
    }
    ti <- tnew
    m <- out
  }
  cbind(
    data.frame(Date = timesToDates(ti)),
    as.data.frame(m, check.names = FALSE)
  )
}
