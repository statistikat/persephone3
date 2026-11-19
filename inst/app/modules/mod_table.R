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

    # Diagnose-Spalten: TotalQ, Q, Q2 und die Qualitätsklassen der
    # Tests aus generate_diagnostics_table() des Pakets
    diags <- lapply(df$series, function(s) {
      tryCatch(
        get_diagnostics(hts, s),
        error = function(e) {
          cat("Error getting diagnostics for", s, ":", conditionMessage(e), "\n")
          NULL
        }
      )
    })

    diag_col <- function(field) {
      vapply(diags, function(d) {
        if (is.null(d) || is.null(d[[field]])) {
          return("")
        }
        v <- d[[field]][1]
        if (is.na(v)) "" else as.character(v)
      }, "")
    }

    df$quality <- tolower(trimws(vapply(df$series, function(s) {
      tryCatch(
        get_quality(hts, s),
        error = function(e) {
          cat("Error getting quality for", s, ":", conditionMessage(e), "\n")
          "ok"
        }
      )
    }, "")))

    df <- data.frame(
      Series = df$display,
      Quality = df$quality,
      TotalQ = diag_col("TotalQ"),
      Q = diag_col("Q"),
      Q2 = diag_col("Q2"),
      Normality = diag_col("normality"),
      Independence = diag_col("independence"),
      Seasonality = diag_col("seasonality"),
      Outliers = diag_col("nOutliers"),
      stringsAsFactors = FALSE
    )

    #Checkbox separat vorne einfügen
    df <- cbind(selected = checkboxes, df)

    cat("DataFrame created with", nrow(df), "rows\n")
    print(df)

    # DataTables zählt ab 0, nicht ab 1!
    output$tbl <- DT::renderDT({
      cat("Rendering DT table...\n")
      dt <- DT::datatable(
        df,
        escape = FALSE,
        selection = "none",
        colnames = c(
          "", "Series", "Quality", "TotalQ", "Q", "Q2",
          "Normality", "Independence", "Seasonality", "Outliers"
        ),
        options = list(
          pageLength = 10,
          dom = "tip",
          columnDefs = list(
            list(orderable = FALSE, targets = 0)
          ),
          # after every redraw (sorting, paging, filtering recreate the
          # rows and reset the checkboxes) the server is asked to push the
          # authoritative selection back to the client
          drawCallback = DT::JS(sprintf(
            paste0(
              "function() { Shiny.setInputValue('%s', Math.random(), ",
              "{priority: 'event'}); }"
            ),
            ns("redraw")
          ))
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

  // From server: the authoritative set of selected series; resyncs the
  // checkboxes after a redraw of the table (sorting, paging, filtering
  // recreate the rows and lose the DOM state of the checkboxes)
  Shiny.addCustomMessageHandler('%s', function(ids) {

    var sel = ids || [];
    if (!Array.isArray(sel)) sel = [String(sel)];

    $('input.row-select').each(function() {
      var on = sel.indexOf(String($(this).data('id'))) >= 0;
      $(this).prop('checked', on);
      $(this).closest('tr').toggleClass('selected', on);
    });
  });

",
          ns("row_event"),
          ns("uncheckRow"),
          ns("uncheckRow_event"),
          ns("setSelected")
        )),
        class = "stripe hover compact"
      )
      # quality classes in traffic light font colours (the light counterparts
      # of the colours of diagnosticsDatatable()); the Quality column holds
      # the lower case values of get_quality(), the others the classes of
      # generate_diagnostics_table()
      classes <- c(
        Good = "#1a803c", good = "#1a803c", Ok = "#1a803c",
        ok = "#1a803c", Uncertain = "#b26a00", Warning = "#b26a00",
        warning = "#b26a00", Bad = "#c93c00", bad = "#c93c00",
        Severe = "#cc0000", Undefined = "#888888"
      )
      for (cn in c(
        "Quality", "TotalQ", "Normality", "Independence", "Outliers"
      )) {
        dt <- dt %>% DT::formatStyle(
          cn, color = DT::styleEqual(names(classes), unname(classes))
        )
      }
      dt
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

    # ---------- Autoritativer Selected-Stand zurueck zum Client ----------
    # Nach jedem Redraw der Tabelle (Sortieren, Blaettern, Filtern setzt die
    # DOM-Checks der neu erzeugten Zeilen zurueck) und whenever die Auswahl
    # sich aendert, sendet der Server die aktuelle Auswahl an den Client,
    # damit Checkboxen und geoeffnete Tabs nie auseinanderlaufen.
    observe({
      sel <- selected()
      input$redraw
      session$sendCustomMessage(ns("setSelected"), as.character(sel))
    })

    return(selected)
  })
}

# bewusst vereinfacht – du kannst deine Checkbox-Logik später wieder einbauen
