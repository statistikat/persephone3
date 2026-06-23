createDashboard <- function(x) {
  app_dir <- system.file("app", package = "persephone3")
  if (app_dir == "") stop("Could not find the app directory.")

  pkg_dir <- normalizePath(".")
  port <- httpuv::randomPort()
  shiny_url <- paste0("http://127.0.0.1:", port)

  p <- callr::r_bg(
    function(app_dir, obj, pkg_dir, port) {
      suppressPackageStartupMessages({
        library(shiny)
        devtools::load_all(pkg_dir)
      })
      options(persephone.dashboard.object = obj)
      shiny::runApp(app_dir, host = "127.0.0.1", port = port, launch.browser = FALSE)
    },
    args = list(app_dir = app_dir, pkg_dir = pkg_dir, obj = x, port = port),
    supervise = TRUE
  )

  # Warten bis Server wirklich läuft
  start_time <- Sys.time()
  ready <- FALSE
  max_startup_time <- 10
  while (difftime(Sys.time(), start_time, units = "secs") < max_startup_time) {
    con <- try(suppressWarnings(base::url(shiny_url, open = "rb")), silent = TRUE)
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
    stop(paste("Dashboard failed to start within", max_startup_time, "seconds."), call. = FALSE)
  }

  invisible(NULL)
}
