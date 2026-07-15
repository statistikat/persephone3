#' Compare two single series dashboards
#'
#' Creates a side-by-side comparison view of two hierarchical time series,
#' displaying their individual diagnostics alongside a comparison panel.
#'
#' @param x First single time series object (class `x13Single` or `tramoseatsSingle`)
#' @param y Second single time series object (class `x13Single` or `tramoseatsSingle`)
#' @param title Main title for the comparison (default: "Series Comparison")
#' @param file Optional file path to save HTML output
#' @return Browsable HTML UI containing the comparison dashboard
#' @export
#' @examples
#' obj1 <- perX13(AirPassengers, "rsa3")
#' obj1$run()
#' obj2 <- perX13(AirPassengers, "rsa1")
#' obj2$run()
#' dashboard_compare(obj1, obj2)
#'
#' # Save as HTML file
#' # dashboard_compare(obj1, obj2, file = "comparison.html")
dashboard_compare <- function(x, y, title = "Series Comparison", file = NULL) {
  # Source the internal implementation from inst/app/utils
  compare_file <- system.file("app/utils/dashboard_compare.R", package = "persephone3")
  if (compare_file == "") {
    # Fallback for development with devtools::load_all()
    compare_file <- "inst/app/utils/dashboard_compare.R"
  }
  source(compare_file, local = TRUE)

  # Call the internal implementation
  dashboard_compare_impl(x, y, file = file, title = title)
}
