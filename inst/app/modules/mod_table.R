mod_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Select series"),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("tbl")), type = 5, color = "#0066CC",
      caption = "Loading series ..."
    )
  )
}

mod_table_server <- function(id, hts, registry = dashboard_registry(hts)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected <- reactiveVal(character())
    df <- data.frame(
      selected = sprintf(
        '<input type="checkbox" class="row-select" data-id="%s" aria-label="Select series">',
        registry$id
      ),
      Series = paste0(strrep("\u2003", registry$depth),
                      ifelse(registry$depth > 0L, "\u25b8 ", ""), registry$label),
      Quality = vapply(registry$id, function(id) {
        tryCatch(get_quality(dashboard_series(hts, id, registry)),
                 error = function(e) NA_character_)
      }, character(1)),
      stringsAsFactors = FALSE
    )

    output$tbl <- DT::renderDT({
      DT::datatable(
        df, rownames = FALSE, escape = 2:3, selection = "none",
        colnames = c("", "Series", "Quality"),
        options = list(
          pageLength = 10, dom = "tip", order = list(),
          columnDefs = list(list(orderable = FALSE, targets = 0))
        ),
        callback = DT::JS(sprintf("
  var tbl = table;
  var node = $(tbl.table().node());
  var container = document.getElementById(%s);
  var validIds = new Set(%s);
  var selected = container.persephoneSelected || new Set();
  container.persephoneSelected = selected;
  selected.forEach(function(id) { if (!validIds.has(id)) selected.delete(id); });

  function syncRows() {
    node.find('input.row-select').each(function() {
      var checked = selected.has(this.getAttribute('data-id'));
      this.checked = checked;
      $(this).closest('tr').toggleClass('selected', checked);
    });
  }

  node.off('.persephoneSelect');
  node.on('draw.dt.persephoneSelect', syncRows);
  node.on('dblclick.persephoneSelect', 'thead th', function() {
    tbl.order([]).draw();
  });
  node.on('click.persephoneSelect', 'tbody tr', function(e) {
    if ($(e.target).is('input')) return;
    var checkbox = $(this).find('input.row-select');
    checkbox.prop('checked', !checkbox.prop('checked')).trigger('change');
  });
  node.on('change.persephoneSelect', 'input.row-select', function() {
    var id = this.getAttribute('data-id');
    if (!validIds.has(id)) return;
    if (this.checked) selected.add(id); else selected.delete(id);
    syncRows();
    Shiny.setInputValue(%s, {id: id, checked: this.checked}, {priority: 'event'});
  });
  Shiny.addCustomMessageHandler(%s, function(id) {
    if (!validIds.has(id)) return;
    selected.delete(id);
    syncRows();
  });
  syncRows();
", jsonlite::toJSON(ns("tbl"), auto_unbox = TRUE),
          jsonlite::toJSON(registry$id, auto_unbox = FALSE),
          jsonlite::toJSON(ns("row_event"), auto_unbox = TRUE),
          jsonlite::toJSON(ns("uncheckRow"), auto_unbox = TRUE))),
        class = "stripe hover compact"
      )
    })

    observeEvent(input$row_event, {
      event <- input$row_event
      if (!is.list(event) || !dashboard_valid_id(event$id, registry) ||
          !is.logical(event$checked) || length(event$checked) != 1L ||
          is.na(event$checked)) return()
      current <- selected()
      selected(if (event$checked) unique(c(current, event$id)) else setdiff(current, event$id))
    })

    observeEvent(input$uncheckRow_event, {
      id <- input$uncheckRow_event
      if (dashboard_valid_id(id, registry)) selected(setdiff(selected(), id))
    })

    selected
  })
}
