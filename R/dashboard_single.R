#' Generate diagnostic dashboard for single time series
#'
#' Creates an interactive HTML diagnostic dashboard for a single hierarchical
#' time series object, showing model diagnostics, plots, and quality metrics.
#'
#' @param x A single time series object (class `x13Single` or `tramoseatsSingle`)
#' @param title Main title for the dashboard (default: "Series Diagnostics")
#' @param file Optional file path to save HTML output
#' @return Browsable HTML UI containing the diagnostic dashboard
#' @export
#' @examples
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' dashboard_single(obj)
#'
#' # Save as HTML file
#' # dashboard_single(obj, file = "dashboard.html")
dashboard_single <- function(x, title = "Series Diagnostics", file = NULL) {
  # Source the internal implementation from inst/app/utils
  dashboard_file <- system.file("app/utils/dashboard_single.R", package = "persephone3")
  if (dashboard_file == "") {
    # Fallback for development with devtools::load_all()
    dashboard_file <- "inst/app/utils/dashboard_single.R"
  }
  source(dashboard_file, local = TRUE)

  # Call the internal implementation
  dashboard_single_impl(x, file = file, title = title)
}
