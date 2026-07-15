# UI

#library(shinycssloaders)

mod_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Select series"),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("tbl")),
      type = 5,
      color = "#0066CC",
      caption = "Loading series ..."
    )
  )
}

# Server

mod_table_server <- function(id, hts) {
  cat("mod_table_server started\n")
  cat("hts class:", class(hts), "\n")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # STATE
    updating <- reactiveVal(FALSE)
    selected <- reactiveVal(character())

    cat("Getting series names...\n")
    series_names <- get_all_series(hts)
    cat("Found", length(series_names), "series\n")
    cat("Series:", paste(series_names, collapse = ", "), "\n")

    # 1. DataFrame zuerst anlegen
    df <- data.frame(
      series = series_names,
      stringsAsFactors = FALSE
    )

    # 2. Level berechnen
    df$level <- stringr::str_count(df$series, "/")

    # 3. Anzeige-Spalte (Einrückung + schöner Name)
    df$display <- paste0(
      strrep("&nbsp;&nbsp;", df$level),
      ifelse(df$level > 0, "▸ ", ""),
      sub(".*/", "", df$series)
    )

    # Checkbox-Spalte
    checkboxes <- sprintf(
      '<input type="checkbox" class="row-select" data-id="%s">',
      df$series
    )

    df$quality <- sapply(df$series, function(s) {
      tryCatch(
        get_quality(hts, s),
        error = function(e) {
          cat("Error getting quality for", s, ":", conditionMessage(e), "\n")
          NA_character_
        }
      )
    })

    df$quality <- tolower(trimws(df$quality))

    quality_map <- c(
      "good" = 3,
      "ok" = 2,
      "warning" = 1,
      "bad" = 0
    )

    df$quality_score <- quality_map[df$quality]
    df$quality_score[is.na(df$quality_score)] <- -1

    df <- data.frame(
      Series = df$display,
      Quality = df$quality,
      stringsAsFactors = FALSE
    )

    #Checkbox separat vorne einfügen
    df <- cbind(selected = checkboxes, df)

    cat("DataFrame created with", nrow(df), "rows\n")
    print(df)

    # DataTables zählt ab 0, nicht ab 1!
    output$tbl <- DT::renderDT({
      cat("Rendering DT table...\n")
      DT::datatable(
        df,
        escape = FALSE,
        selection = "none",
        colnames = c("", "Series", "Quality"),
        options = list(
          pageLength = 10,
          dom = "tip",
          columnDefs = list(
            list(orderable = FALSE, targets = 0)
          )
        ),
        callback = DT::JS(sprintf(
          "
  var tbl = table;

  // Double-click on column header to reset sorting
  $(document).on('dblclick', 'table.dataTable thead th', function() {
    tbl.order([]).draw();
  });

  // Row Click
  tbl.on('click', 'tbody tr', function(e) {

    if ($(e.target).is('input')) return;

    var cb = $(this).find('input.row-select');
    cb.prop('checked', !cb.prop('checked')).trigger('change');
  });

  // Checkbox change
  tbl.on('change', '.row-select', function() {

    var id = $(this).data('id');
    var checked = $(this).is(':checked');
    var row = $(this).closest('tr');

    if (checked) {
      row.addClass('selected');
    } else {
      row.removeClass('selected');
    }

    Shiny.setInputValue('%s', {
      id: id,
      checked: checked,
      nonce: Math.random()
    });
  });

  // From server: uncheck
  Shiny.addCustomMessageHandler('%s', function(id) {

    var el = $('input.row-select[data-id=\"' + id + '\"]');

    el.prop('checked', false)
      .closest('tr')
      .removeClass('selected');

    Shiny.setInputValue('%s', id, {priority: 'event'});
  });

",
          ns("row_event"),
          ns("uncheckRow"),
          ns("uncheckRow_event")
        )),
        class = "stripe hover compact"
      )
    })

    # User Event
    # ---------- User klickt ----------
    observeEvent(input$row_event, {
      if (updating()) {
        return()
      }

      id <- input$row_event$id
      checked <- input$row_event$checked

      current <- selected()

      if (checked) {
        current <- unique(c(current, id))
      } else {
        current <- setdiff(current, id)
      }

      selected(current)
    })

    # Server Event
    # ---------- Server zwingt uncheck ----------
    observeEvent(
      input$uncheckRow_event,
      {
        updating(TRUE)

        id <- input$uncheckRow_event

        current <- selected()
        current <- setdiff(current, id)

        selected(current)

        updating(FALSE)
      },
      ignoreInit = TRUE
    )

    return(selected)
  })
}

# bewusst vereinfacht – du kannst deine Checkbox-Logik später wieder einbauen
