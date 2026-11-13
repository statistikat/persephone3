library(shiny)
source(file.path("modules", "mod_table.R"), local = TRUE)
source(file.path("modules", "mod_series.R"), local = TRUE)
source(file.path("utils", "dashboard_single_wrapper.R"), local = TRUE)
source(file.path("helpers.R"), local = TRUE)

server <- function(input, output, session) {
  hts <- getOption("persephone.dashboard.object")
  if (is.null(hts)) stop("No hts/bts object passed to dashboard")

  registry <- dashboard_registry(hts)
  selected_series <- mod_table_server("table", hts, registry)
  open_tabs <- reactiveVal(character())
  panel_state <- reactiveValues()
  initialized <- new.env(parent = emptyenv())

  observeEvent(selected_series(), {
    current <- selected_series()
    if (!dashboard_valid_selection(current, registry)) return()
    previous <- open_tabs()
    added <- setdiff(current, previous)
    removed <- setdiff(previous, current)

    for (id in added) {
      tab_id <- paste0("tab_", id)
      state <- panel_state[[id]]
      generation <- if (is.null(state)) 1L else state$generation + 1L
      panel_state[[id]] <- list(open = TRUE, generation = generation)
      label <- registry$label[match(id, registry$id)]
      insertTab(
        "details_tabs",
        tabPanel(
          title = tagList(label, tags$button(
            "\u00d7", type = "button", class = "persephone-close-tab",
            `data-series-id` = id, `aria-label` = "Close series"
          )),
          value = tab_id, mod_series_ui(tab_id)
        ),
        target = "Start", position = "after", select = TRUE
      )

      # One module per visited node. Its activation generation invalidates the
      # panel on reopening, even when its R6 object was modified in place.
      if (!exists(id, envir = initialized, inherits = FALSE)) {
        local({
          series_id <- id
          active <- reactive({
            state <- panel_state[[series_id]]
            if (isTRUE(state$open)) state$generation else FALSE
          })
          mod_series_server(
            tab_id,
            reactive({
              req(active())
              dashboard_series(hts, series_id, registry)
            }),
            active = active
          )
        })
        assign(id, TRUE, envir = initialized)
      }
    }

    for (id in removed) {
      state <- panel_state[[id]]
      state$open <- FALSE
      panel_state[[id]] <- state
      removeTab("details_tabs", target = paste0("tab_", id))
      session$sendCustomMessage("table-uncheckRow", id)
    }
    open_tabs(c(intersect(previous, current), added))
  })

  observeEvent(input$close_tab, {
    id <- input$close_tab
    current <- open_tabs()
    if (!dashboard_valid_id(id, registry) || !id %in% current) return()
    idx <- match(id, current)
    next_id <- if (length(current) == 1L) NULL else if (idx < length(current)) {
      current[idx + 1L]
    } else current[idx - 1L]
    selected_series(setdiff(selected_series(), id))
    updateTabsetPanel(session, "details_tabs", selected =
                       if (is.null(next_id)) "Start" else paste0("tab_", next_id))
  })

  observeEvent(input$tab_order, {
    ids <- input$tab_order
    if (dashboard_valid_selection(ids, registry) && setequal(ids, open_tabs())) {
      open_tabs(ids)
    }
  })
}
