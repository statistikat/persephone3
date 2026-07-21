#' Wrapper for dashboard_single
#'
#' This file is sourced by the Shiny app. It calls the exported function
#' from the package namespace.
#'
#' @param x A single time series object
#' @param title Dashboard title
#' @param file Optional file path to save HTML
#' @return Browsable HTML UI
dashboard_single <- function(x, title = "Diagnostics", file = NULL) {
  # Call the exported function from the package
  persephone3::dashboard_single(x, title = title, file = file)
}
