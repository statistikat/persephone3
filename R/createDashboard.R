createDashboard <- function(x) {

  app_dir <- normalizePath("inst/app")
  pkg_dir <- normalizePath(".")

  port <- httpuv::randomPort()


  callr::r_bg(
    function(app_dir, obj, pkg_dir, port) {

      suppressPackageStartupMessages({
        library(shiny)
        devtools::load_all(pkg_dir)
      })

      setwd(app_dir)   # ✅ erst NACH load_all()

      options(persephone.dashboard.object = obj)

      shiny::runApp(
        appDir = app_dir,
        host = "127.0.0.1",
        port = port,
        launch.browser = FALSE
      )

    },
    args = list(app_dir = app_dir, obj = x, pkg_dir = pkg_dir, port = port),
    stdout = "|",
    stderr = "|"
  )

  # ✅ Warten bis Server wirklich läuft
  url <- paste0("http://127.0.0.1:", port)

  for (i in 1:100) {

    con <- try(suppressWarnings(url(url, open = "rb")), silent = TRUE)

    if (!inherits(con, "try-error")) {
      close(con)
      break
    }

    Sys.sleep(0.1)
  }

  # ✅ Browser im Hauptprozess öffnen
  utils::browseURL(url)

  invisible(NULL)
}

# port anpingen
# Info:
# Da du jetzt einen neuen Prozess startest:
# options() wird nicht automatisch übernommen
# arum übergeben wir obj explizit:
#
# Du kannst dir sogar den Prozess zurückgeben lassen:
#   p <- callr::r_bg(...)
# Dann kannst du:
#   p$kill()


# # Andre Option:
# # läuft blocking, d.h.:
# #   R startet einen HTTP-Server
# # wartet aktiv auf Requests
# # kehrt nicht zurück, solange die App läuft# createDashboard <- function(x) {
# #
#   if (is.null(x)) {
#     stop("No object provided")
#   }
#
#   # Objekt global verfügbar machen (clean Lösung)
#   options(persephone.dashboard.object = x)
#
#   app_dir <- system.file("app", package = "persephone3")
#
#   if (app_dir == "") {
#     stop("App directory not found in package.")
#   }
#
#   shiny::runApp(
#     appDir = app_dir,
#     launch.browser = TRUE,  # Popup
#     quiet = TRUE
#   )
# }
