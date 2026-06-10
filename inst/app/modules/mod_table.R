# UI

mod_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Select series"),
    DT::DTOutput(ns("tbl"))
  )
}

# Server

mod_table_server <- function(id, hts) {
  cat("mod_table_server gestartet\n")
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # STATE
    updating <- reactiveVal(FALSE)   # ✅ HIER !!!
    selected <- reactiveVal(character())  # bleibt auch hier

    series_names <- c("aggregate", names(hts$components))
    # Daten vorbereiten
    df <- data.frame(
      series = series_names,
      stringsAsFactors = FALSE
    )

    # ✅ Checkbox-Spalte
    df$selected <- sprintf(
      '<input type="checkbox" class="row-select" data-id="%s">',
      df$series
    )

    df <- df[, c("selected", "series")]

    output$tbl <- DT::renderDT({

      DT::datatable(
        df,
        escape = FALSE,
        selection = "none",
        options = list(
          pageLength = 10,
          dom = "tip"
        ),
        callback = DT::JS(sprintf("
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

      if (updating()) return()   # ✅ wichtig!

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
    observeEvent(input$uncheckRow_event, {

      updating(TRUE)   # ✅ blockiert Loop

      id <- input$uncheckRow_event

      current <- selected()
      current <- setdiff(current, id)

      selected(current)

      updating(FALSE)  # ✅ wieder freigeben

    }, ignoreInit = TRUE)

    return(selected)   # ✅ GANZ WICHTIG

  })   # ✅ schließt moduleServer

}      # ✅ schließt mod_table_server

# bewusst vereinfacht – du kannst deine Checkbox-Logik später wieder einbauen
