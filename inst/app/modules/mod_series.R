# ============================================================================
# Series Module - Individual Series Dashboard Panel
# ============================================================================
# This module renders a complete diagnostic dashboard for a single hierarchical
# time series. It includes caching to avoid re-rendering and error handling
# for robustness.
# ============================================================================

# UI
# Creates a UI container with dynamic content
#
# @param id Module namespace ID
# @return tagList containing uiOutput for dynamic content
mod_series_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("content"))
  )
  # Alternative with loading spinner (commented out)
  # tagList(
  #   shinycssloaders::withSpinner(
  #     uiOutput(ns("content")),
  #     type = 5,
  #     caption = "Preparing data ..."
  #   )
  # )
}

# Server
# Renders dynamic UI based on the series_reactive input
#
# @param id Module namespace ID
# @param series_reactive A reactive expression that returns a hierarchical
#   time series object for the selected series
# @return moduleServer instance with cached UI rendering
mod_series_server <- function(id, series_reactive) {
  moduleServer(id, function(input, output, session) {
    # Cache to store the generated UI and avoid re-rendering
    cache <- reactiveVal(NULL)

    output$content <- renderUI({
      # Return cached UI if available
      if (!is.null(cache())) {
        return(cache())
      }

      # Get the series object from the reactive
      obj <- series_reactive()

      # Ensure the series object exists before proceeding
      req(obj)

      # Generate the dashboard UI with error handling
      ui <- tryCatch(
        dashboard_single(obj),
        error = function(e) {
          div(
            style = "color:red; padding:20px;",
            paste("Error in dashboard:", e$message)
          )
        }
      )

      # Cache the generated UI for future use
      cache(ui)

      ui
    })
  })
}
