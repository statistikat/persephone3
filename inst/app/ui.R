# ============================================================================
# Persephone3 Dashboard - User Interface
# ============================================================================
# This UI defines the layout for the time series diagnostic dashboard with:
# - A selectable table on the left (list of series)
# - Dynamic tabs on the right for each selected series
# - Drag-and-drop capability for tab reordering
# ============================================================================

library(shiny)
source(file.path("modules", "mod_table.R"), local = TRUE)
source(file.path("modules", "mod_series.R"), local = TRUE)
# source("modules/mod_table.R", local = TRUE)
# source("modules/mod_series.R", local = TRUE)

ui <- fluidPage(
  # -------------------------------------------------------------------------
  # HEAD: External resources, JavaScript, and CSS
  # -------------------------------------------------------------------------
  tags$head(
    # jQuery UI CSS for drag-and-drop styling
    tags$link(
      rel = "stylesheet",
      href = "https://code.jquery.com/ui/1.13.2/themes/base/jquery-ui.css"
    ),
    # jQuery UI JS for sortable functionality (tab reordering)
    tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"),

    # JavaScript for drag-and-drop of tabs
    tags$script(HTML(
      "
  $(function() {
    $(document).off('click.persephoneClose', '.persephone-close-tab')
      .on('click.persephoneClose', '.persephone-close-tab', function(event) {
        event.preventDefault();
        event.stopPropagation();
        var id = this.getAttribute('data-series-id');
        if (/^node_[0-9]+(?:_[0-9]+)*$/.test(id)) {
          Shiny.setInputValue('close_tab', id, {priority: 'event'});
        }
      });

    var tabs = $('#details_tabs');
    if (!$.fn.sortable) return;
    tabs.sortable({
      axis: 'x',
      stop: function() {

        var order = [];

        tabs.find('li a').each(function() {
          var value = $(this).attr('data-value');
          if (value && /^tab_node_[0-9]+(?:_[0-9]+)*$/.test(value)) {
            order.push(value.slice(4));
          }
        });

        // Send tab order to Shiny server
        Shiny.setInputValue('tab_order', order, {priority: 'event'});
      }
    });

    tabs.disableSelection();
  });
"
    )),
    # Custom CSS for DataTables and series display
    tags$style(HTML(
      "
  table.dataTable tbody td {
    white-space: normal !important;
    padding-left: 0 !important;
  }
  .series-cell {
    display: inline-block;
  }
  .persephone-close-tab {
    background: none;
    border: 0;
    color: #777;
    cursor: pointer;
    margin-left: 6px;
    padding: 0 2px;
  }
"
    ))
  ),

  # -------------------------------------------------------------------------
  # Title bar
  # -------------------------------------------------------------------------
  titlePanel(getOption("persephone.dashboard.title", "Persephone3 Dashboard")),

  # -------------------------------------------------------------------------
  # Main layout: Two-column layout
  # -------------------------------------------------------------------------
  fluidRow(
    # Left column (4/12): Table module for series selection
    column(
      width = 4,
      mod_table_ui("table")
    ),

    # Right column (8/12): TabsetPanel for detail views
    column(
      width = 8,
      tabsetPanel(
        id = "details_tabs", # ID for server-side tab control
        tabPanel("Start", "Please select a series") # Start tab as placeholder
      )
    )
  )
)
