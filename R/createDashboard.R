#' Create diagnostic dashboard for time series objects
#'
#' Creates a diagnostic dashboard for time series objects. For single series
#' objects, returns an HTML dashboard. For hierarchical time series, launches
#' an interactive Shiny application. Can also compare two single series objects.
#'
#' @param x A time series object (single series or hierarchical), for display
#' @param y Second time series object (optional, for comparison of two single series)
#' @param title Main title for the dashboard (default depends on object type)
#' @param file Optional file path to save HTML output (single series only)
#' @param launch.browser Open the batch/hierarchy dashboard in a browser.
#' @param startup.timeout Maximum seconds to wait for the Shiny server to start.
#' @param ... Additional arguments passed to specific methods
#' @return For single series: browsable HTML UI. For batch/hierarchical objects:
#'   an invisible `callr` process, with its local URL in `attr(process, "url")`.
#'   Call `process$kill()` to stop the app.
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
createDashboard <- function(x, y = NULL, title = NULL, file = NULL,
                            launch.browser = TRUE, startup.timeout = 30, ...) {
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
    createDashboard_hts(x, title = title, launch.browser = launch.browser,
                        startup.timeout = startup.timeout, ...)
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
  if (is.null(title)) title <- "Series Diagnostics"
  dashboard_single(x, file = file, title = title)
}

#' @describeIn createDashboard Internal helper for hierarchical time series dashboards
createDashboard_hts <- function(x, title = NULL, file = NULL,
                                launch.browser = TRUE, startup.timeout = 30, ...) {
  if (!is.logical(launch.browser) || length(launch.browser) != 1L ||
      is.na(launch.browser)) {
    stop("launch.browser must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.numeric(startup.timeout) || length(startup.timeout) != 1L ||
      !is.finite(startup.timeout) || startup.timeout <= 0) {
    stop("startup.timeout must be a positive number", call. = FALSE)
  }

  # Resolve from the loaded namespace, never the caller's working directory.
  pkg_dir <- getNamespaceInfo(asNamespace("persephone3"), "path")
  source_package <- dir.exists(file.path(pkg_dir, "inst", "app"))
  app_dir <- file.path(pkg_dir, if (source_package) "inst/app" else "app")
  if (!dir.exists(app_dir)) stop("Could not find the app directory.")

  port <- httpuv::randomPort()
  shiny_url <- paste0("http://127.0.0.1:", port)
  p <- callr::r_bg(
    function(app_dir, obj, pkg_dir, source_package, port, title) {
      if (source_package) {
        suppressPackageStartupMessages(devtools::load_all(pkg_dir))
      } else {
        loadNamespace("persephone3", lib.loc = dirname(pkg_dir))
      }
      options(persephone.dashboard.object = obj,
              persephone.dashboard.title = title)
      shiny::runApp(app_dir, host = "127.0.0.1", port = port,
                    launch.browser = FALSE)
    },
    args = list(app_dir = app_dir, obj = x, pkg_dir = pkg_dir,
                source_package = source_package, port = port, title = title),
    libpath = .libPaths(),
    supervise = TRUE
  )

  ready <- FALSE
  on.exit(if (!ready && p$is_alive()) p$kill(), add = TRUE)
  start_time <- Sys.time()
  while (as.numeric(difftime(Sys.time(), start_time, units = "secs")) < startup.timeout) {
    if (!p$is_alive()) {
      stop("Dashboard process exited before startup: ", p$read_error(), call. = FALSE)
    }
    con <- try(suppressWarnings(base::url(shiny_url, open = "rb")), silent = TRUE)
    if (!inherits(con, "try-error")) {
      close(con)
      ready <- TRUE
      break
    }
    Sys.sleep(0.2)
  }
  if (!ready) {
    stop("Dashboard failed to start within ", startup.timeout, " seconds.",
         call. = FALSE)
  }
  attr(p, "url") <- shiny_url
  if (launch.browser) utils::browseURL(shiny_url)
  message("Dashboard running at ", shiny_url)
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
  if (is.null(title)) title <- "Series Comparison"
  dashboard_compare(x, y, file = file, title = title)
}
