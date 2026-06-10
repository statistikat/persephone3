library(shiny)
source(file.path("modules", "mod_table.R"), local = TRUE)
source(file.path("modules", "mod_series.R"), local = TRUE)
# source("modules/mod_table.R", local = TRUE)
# source("modules/mod_series.R", local = TRUE)

ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", href = "https://code.jquery.com/ui/1.13.2/themes/base/jquery-ui.css"),
    tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"),

    tags$script(HTML("
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

        Shiny.setInputValue('tab_order', order, {priority: 'event'});
      }
    });

    tabs.disableSelection();
  });
"))

  ),

  titlePanel("Persephone3 Dashboard"),

  fluidRow(

    column(
      width = 4,
      mod_table_ui("table")
    ),

    column(
      width = 8,
      tabsetPanel(
        id = "details_tabs",
        tabPanel("Start", "Please select a series")
      )
    )
  )
)
