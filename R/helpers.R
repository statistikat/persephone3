#' @importFrom highcharter hchart hc_title
#' @export
highcharter::hchart

#' @export
highcharter::hc_title

#' @importFrom highcharter %>%
#' @export
highcharter::`%>%`

#' @importFrom R6 R6Class
#' @importFrom rjd3toolkit add_outlier add_ramp add_usrdefvar remove_outlier
#'   remove_ramp set_arima set_automodel set_basic set_benchmarking
#'   set_easter set_estimate set_outlier set_tradingdays set_transform
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
  if (!requireNamespace("rjd3toolkit", quietly = T))
    stop("Loading rjd3toolkit failed")

}

# Null-Coalescing
`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else x
}

rescale <- function(x, min, max) {
  stopifnot(is.numeric(x), is.numeric(min), is.numeric(max), length(min) == 1,
            length(max) == 1)
  rg <- range(x)
  min + (x - rg[1])/(rg[2] - rg[1])*(max - min)
}

# spec-blocks and their corresponding functions
specfun_list <- list(arima = rjd3toolkit::set_arima,
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
                     benchmarking = rjd3toolkit::set_benchmarking)

# persephone3 internal spec names
p3_spec_names <- c("outliers")
