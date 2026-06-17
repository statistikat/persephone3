library(shiny)

# source("modules/mod_table.R", local = TRUE)
# source("modules/mod_series.R", local = TRUE)
# source("utils/dashboard_single.R", local = TRUE)
source(file.path("modules", "mod_table.R"), local = TRUE)
source(file.path("modules", "mod_series.R"), local = TRUE)
source(file.path("utils", "dashboard_single.R"), local = TRUE)

get_series_by_path <- function(hts, path) {

  parts <- strsplit(path, "/")[[1]]

  obj <- hts

  if (parts[1] != "total aggregate") {
    for (p in parts) {
      obj <- obj$components[[p]]
    }
  }

  obj
}

server <- function(input, output, session) {

  # 👉 R6 Objekt holen
  hts <- getOption("persephone.dashboard.object")

  if (is.null(hts)) {
    stop("No hts/bts object passed to dashboard")
  }

  # 👉 Table Modul
  selected_series <- mod_table_server("table", hts)

  prev_selected <- reactiveVal(character())   # ✅ HIER HIN

  # open_tabs <- reactiveVal(character())

  observeEvent(
    selected_series(),
    {

      current <- selected_series()
      if (is.null(current)) current <- character()

      previous <- prev_selected()

      added <- setdiff(current, previous)
      removed <- setdiff(previous, current)

      # ---------- hinzufügen ----------
      for (sn in added) {
        tab_id <- paste0("tab_", sn)

        insertTab(
          "details_tabs",
          tabPanel(
            title = tagList(
              sn,
              tags$span(
                " ✕",
                style = "cursor:pointer; color:#999; margin-left:6px;",
                onclick = sprintf(
                  "Shiny.setInputValue('close_tab', '%s', {priority: 'event'})",
                  tab_id
                )
              )
            ),
            value = tab_id,
            div(mod_series_ui(tab_id))   # ✅ HIER
          ),
          target = "Start",
          position = "after",
          select = TRUE
        )

        local({
          s <- sn
          id <- tab_id

          mod_series_server(
            id,
            reactive({
              #if (s == "aggregate") hts else hts$components[[s]]
              get_series_by_path(hts, s)
            })
          )
        })
      }

      # ---------- entfernen ----------
      for (sn in removed) {
        tab_id <- paste0("tab_", sn)

        removeTab("details_tabs", target = tab_id)

        # ✅ Tabelle zurücksetzen
        session$sendCustomMessage("table-uncheckRow", sn)
      }

      prev_selected(current)

    },
    ignoreInit = TRUE
  )

  observeEvent(input$close_tab, {

    tab_id <- input$close_tab
    sn <- sub("^tab_", "", tab_id)

    current <- prev_selected()

    # 👉 Position des Tabs
    idx <- match(sn, current)

    next_tab <- "Start"   # fallback

    if (!is.na(idx) && length(current) > 1) {

      if (idx < length(current)) {
        # ✅ zuerst rechts
        next_tab <- paste0("tab_", current[idx + 1])
      } else {
        # ✅ sonst links
        next_tab <- paste0("tab_", current[idx - 1])
      }
    }

    # ✅ Tab entfernen
    removeTab("details_tabs", target = tab_id)

    # ✅ Tabelle sync
    session$sendCustomMessage("table-uncheckRow", sn)

    # ✅ State updaten
    current <- setdiff(current, sn)
    prev_selected(current)

    # ✅ neues aktives Tab
    updateTabsetPanel(session, "details_tabs", selected = next_tab)

  }, ignoreInit = TRUE)

}
