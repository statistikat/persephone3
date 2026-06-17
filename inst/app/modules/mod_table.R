# UI

mod_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Select series"),
    DT::DTOutput(ns("tbl"))
  )
}

# Server
get_all_series <- function(x, prefix = NULL) {
  out <- c()

  #current_name <- if (is.null(prefix)) "aggregate" else prefix

  current_name <- if (is.null(prefix)) "total aggregate" else prefix

  out <- c(out, current_name)

  if (!is.null(x$components)) {
    for (n in names(x$components)) {
      child_name <- if (is.null(prefix)) n else paste0(prefix, "/", n)
      out <- c(out, get_all_series(x$components[[n]], child_name))
    }
  }

  out
}

mod_table_server <- function(id, hts) {
  cat("mod_table_server gestartet\n")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # STATE
    updating <- reactiveVal(FALSE) # HIER !!!
    selected <- reactiveVal(character()) # bleibt auch hier

    # series_names <- c("aggregate", names(hts$components))

    series_names <- get_all_series(hts)

    # 1. DataFrame zuerst anlegen
    df <- data.frame(
      series = series_names,
      stringsAsFactors = FALSE
    )

    # 2. Level berechnen
    df$level <- stringr::str_count(df$series, "/")

    # 3. Anzeige-Spalte (Einrückung + schöner Name)
    # df$display <- paste0(
    #   strrep("&nbsp;&nbsp;&nbsp;&nbsp;", df$level),
    #   ifelse(df$level > 0, "▸ ", ""),
    #   sub(".*/", "", df$series)
    # )
    df$display <- paste0(
      strrep("&nbsp;&nbsp;", df$level),
      ifelse(df$level > 0, "▸ ", ""),
      sub(".*/", "", df$series)
    )

    # Checkbox-Spalte
    df$selected <- sprintf(
      '<input type="checkbox" class="row-select" data-id="%s">',
      df$series
    )

    #df <- df[, c("selected", "series")]

    # df <- df[, c("selected", "display", "series")]
    # colnames(df) <- c("", "Series", "id")
    df <- data.frame(
      selected = df$selected,
      series_display = df$display, # ← NEUER NAME
      series_id = df$series,
      stringsAsFactors = FALSE
    )

    output$tbl <- DT::renderDT({
      DT::datatable(
        df,
        escape = FALSE,
        selection = "none",
        colnames = c("", "Series", "id"),
        options = list(
          pageLength = 10,
          dom = "tip",
          columnDefs = list(
            list(orderable = FALSE, targets = 0),
            list(visible = FALSE, targets = 3)
            )
        ),
        callback = DT::JS(sprintf(
          "
  var tbl = table;

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
      } # wichtig!

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
        updating(TRUE) # blockiert Loop

        id <- input$uncheckRow_event

        current <- selected()
        current <- setdiff(current, id)

        selected(current)

        updating(FALSE) # wieder freigeben
      },
      ignoreInit = TRUE
    )

    return(selected) # GANZ WICHTIG
  }) # schließt moduleServer
} # schließt mod_table_server

# bewusst vereinfacht – du kannst deine Checkbox-Logik später wieder einbauen
