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

    var tabs = $('.nav.nav-tabs');

    tabs.sortable({
      axis: 'x',
      stop: function() {

        var order = [];

        tabs.find('li a').each(function() {
          var href = $(this).attr('href');
          if (href && href.startsWith('#tab_')) {
            order.push(href.replace('#tab_', ''));
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
"
    ))
  ),

  # -------------------------------------------------------------------------
  # Title bar
  # -------------------------------------------------------------------------
  titlePanel("Persephone3 Dashboard"),

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
