#' Wrapper for dashboard_compare
#'
#' This file is sourced by the Shiny app. It calls the exported function
#' from the package namespace.
#'
#' @param x First time series object
#' @param y Second time series object
#' @param title Comparison title
#' @param file Optional file path to save HTML
#' @return Browsable HTML UI
dashboard_compare <- function(x, y, title = "Series Comparison", file = NULL) {
  # Call the exported function from the package
  persephone3::dashboard_compare(x, y, title = title, file = file)
}
