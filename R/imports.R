#' Package imports
#'
#' @useDynLib persephone3, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom jsonlite toJSON
#' @import htmlwidgets
#'
#' @importFrom stats aggregate.ts density end printCoefmat pt qqnorm setNames start tsp
#' @importFrom utils capture.output head tail
#' @importFrom graphics plot.new
#' @importFrom R6 R6Class
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom stringr str_count
#' @importFrom lubridate quarter year month
#' @keywords internal
#' @name persephone3-imports
NULL
