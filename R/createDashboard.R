#' Create diagnostic dashboard for time series objects
#'
#' Creates a diagnostic dashboard for time series objects. For single series
#' objects, returns an HTML dashboard. For hierarchical time series, launches
#' an interactive Shiny application. Can also compare two single series objects.
#'
#' @param x A time series object (single series or hierarchical), or a list of two single series objects for comparison
#' @param y Second time series object (optional, for comparison of two single series)
#' @param title Main title for the dashboard (default depends on object type)
#' @param file Optional file path to save HTML output (single series only)
#' @param ... Additional arguments passed to specific methods
#' @return For single series: browsable HTML UI. For hierarchical: invisible `callr` process.
#' @export
#' @examples
#' # For single series - returns HTML dashboard
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' createDashboard(obj)
#'
#' # Compare two series
#' obj1 <- perX13(AirPassengers, "rsa3")
#' obj1$run()
#' obj2 <- perX13(AirPassengers, "rsa1")
#' obj2$run()
#' createDashboard(obj1, obj2, title = "rsa3 vs rsa1")
#'
#' # For hierarchical series - launches Shiny app
#' # createDashboard(hts_object)
createDashboard <- function(x, y = NULL, title = NULL, file = NULL, ...) {
  # Check if comparing two series
  if (!is.null(y)) {
    # Comparison mode: two single series objects
    if (!(inherits(x, "x13Single") || inherits(x, "tramoseatsSingle"))) {
      stop("First object must be x13Single or tramoseatsSingle for comparison")
    }
    if (!(inherits(y, "x13Single") || inherits(y, "tramoseatsSingle"))) {
      stop("Second object must be x13Single or tramoseatsSingle for comparison")
    }
    return(createDashboard_compare(x, y, title = title, file = file, ...))
  }

  # Single object mode
  if (inherits(x, "x13Single") || inherits(x, "tramoseatsSingle")) {
    createDashboard_single(x, title = title, file = file, ...)
  } else if (
    inherits(x, "hierarchicalTimeSeries") || inherits(x, "multipleTimeSeries")
  ) {
    # Both hts and multipleTimeSeries use the same Shiny app
    # For multipleTimeSeries, the app will show components without aggregates
    createDashboard_hts(x, title = title, ...)
  } else {
    stop(
      "Unsupported object class: ",
      class(x)[1],
      ". Supported classes: x13Single, tramoseatsSingle, multipleTimeSeries, hierarchicalTimeSeries"
    )
  }
}

#' @describeIn createDashboard Internal helper for single series dashboards
createDashboard_single <- function(
  x,
  title = "Series Diagnostics",
  file = NULL,
  ...
) {
  # Source the internal implementation from inst/app/utils
  dashboard_file <- system.file(
    "app/utils/dashboard_single.R",
    package = "persephone3"
  )
  if (dashboard_file == "") {
    # Fallback for development with devtools::load_all()
    dashboard_file <- "inst/app/utils/dashboard_single.R"
  }
  source(dashboard_file, local = TRUE)

  # Call the internal implementation
  dashboard_single(x, file = file, title = title)
}

#' @describeIn createDashboard Internal helper for hierarchical time series dashboards
createDashboard_hts <- function(x, title = NULL, file = NULL, ...) {
  # Find app directory from installed package
  app_dir <- system.file("app", package = "persephone3")
  if (app_dir == "") {
    stop("Could not find the app directory.")
  }

  # Get package directory for devtools::load_all()
  pkg_dir <- normalizePath(".")

  # Select random port for Shiny server
  port <- httpuv::randomPort()
  shiny_url <- paste0("http://127.0.0.1:", port)

  # Start Shiny app in background process
  p <- callr::r_bg(
    function(app_dir, obj, pkg_dir, port) {
      suppressPackageStartupMessages({
        library(shiny)
        devtools::load_all(pkg_dir)
      })
      options(persephone.dashboard.object = obj)
      shiny::runApp(
        app_dir,
        host = "127.0.0.1",
        port = port,
        launch.browser = FALSE
      )
    },
    args = list(app_dir = app_dir, obj = x, pkg_dir = pkg_dir, port = port),
    supervise = TRUE
  )

  # Wait for server to start
  start_time <- Sys.time()
  ready <- FALSE
  max_startup_time <- 10
  while (difftime(Sys.time(), start_time, units = "secs") < max_startup_time) {
    con <- try(
      suppressWarnings(base::url(shiny_url, open = "rb")),
      silent = TRUE
    )
    if (!inherits(con, "try-error")) {
      close(con)
      ready <- TRUE
      break
    }
    Sys.sleep(0.2)
  }

  if (ready) {
    utils::browseURL(shiny_url)
    message("Dashboard running at ", shiny_url)
  } else {
    p$kill()
    stop(
      paste("Dashboard failed to start within", max_startup_time, "seconds."),
      call. = FALSE
    )
  }

  invisible(p)
}

#' @describeIn createDashboard Internal helper for comparing two single series dashboards
createDashboard_compare <- function(
  x,
  y,
  title = "Series Comparison",
  file = NULL,
  ...
) {
  # Source the internal implementation from inst/app/utils
  compare_file <- system.file(
    "app/utils/dashboard_compare.R",
    package = "persephone3"
  )
  if (compare_file == "") {
    # Fallback for development with devtools::load_all()
    compare_file <- "inst/app/utils/dashboard_compare.R"
  }
  source(compare_file, local = TRUE)

  # Call the internal implementation
  dashboard_compare(x, y, file = file, title = title)
}
