# UI

mod_series_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("content"))
  )
}

# Server

mod_series_server <- function(id, series_reactive) {

  moduleServer(id, function(input, output, session) {

    cache <- reactiveVal(NULL)

    output$content <- renderUI({

      if (!is.null(cache())) {
        return(cache())
      }

      obj <- series_reactive()

      req(obj)   # ✅ GANZ wichtig

      #ui <- dashboard_single(obj)
      ui <- tryCatch(
        dashboard_single(obj),
        error = function(e) {
          div(
            style = "color:red; padding:20px;",
            paste("Error in dashboard:", e$message)
          )
        }
      )
      cache(ui)

      ui
    })
  })
}
