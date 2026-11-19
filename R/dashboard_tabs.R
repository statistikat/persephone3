# inner tab widget of the dashboards (plain htmltools, no bslib: the widget
# also works inside the Bootstrap 3 fluidPage of the Shiny app and in a
# standalone saved HTML page without CSS conflicts)

# style sheet of the inner tab widget of the dashboards
p3TabsCss <- function() {
  paste(
    ".p3-tabs { display: flex; flex-direction: column; }",
    ".p3-tabbar { display: flex; gap: 6px; margin-bottom: 14px;",
    "  flex-wrap: wrap; }",
    ".p3-tab-btn { border: 1px solid #e5e7eb; background: #ffffff;",
    "  color: #374151; border-radius: 10px; padding: 8px 16px;",
    "  font-size: 14px; font-weight: 600; cursor: pointer; }",
    ".p3-tab-btn.active { background: #4f46e5; border-color: #4f46e5;",
    "  color: #ffffff; }",
    ".p3-tab-pane { display: none; }",
    ".p3-tab-pane.active { display: block; }",
    ".stack > .card { margin-bottom: 14px; }",
    sep = " "
  )
}

# tab switching of the .p3-tabs widget: event delegation on the document so
# that it also works for the several dashboards of the Shiny app; the handler
# is installed only once. After switching the tab a window resize event is
# dispatched, which makes the responsive plotly widgets (rendered hidden, so
# with a width of zero) redraw at the size of the now visible pane.
p3TabsJs <- function() {
  htmltools::tags$script(htmltools::HTML(
    "
    (function() {
      if (window.__p3TabsBound) return;
      window.__p3TabsBound = true;
      document.addEventListener('click', function(e) {
        var btn = e.target.closest('.p3-tab-btn');
        if (!btn) return;
        var root = btn.closest('.p3-tabs');
        if (!root) return;
        var target = btn.getAttribute('data-tab');
        root.querySelectorAll('.p3-tab-btn').forEach(function(b) {
          b.classList.toggle('active', b === btn);
        });
        root.querySelectorAll('.p3-tab-pane').forEach(function(p) {
          p.classList.toggle('active', p.getAttribute('data-pane') === target);
        });
        window.dispatchEvent(new Event('resize'));
      });
    })();
    "
  ))
}

# inner tab widget of the dashboards
#
# tabs: a named list of tab panels, each element a list with the entries
# `label` (text on the button) and `content` (the UI of the pane). The first
# tab is shown initially. The panes carry no ids (the dashboards appear
# several times in the Shiny app): the switching is scoped to the closest
# .p3-tabs container via the data-tab/data-pane attributes.
p3_tabs_html <- function(tabs) {
  ids <- names(tabs)
  buttons <- lapply(ids, function(id) {
    htmltools::tags$button(
      class = if (identical(id, ids[1])) "p3-tab-btn active" else "p3-tab-btn",
      `data-tab` = id,
      tabs[[id]]$label
    )
  })
  panes <- lapply(ids, function(id) {
    htmltools::tags$div(
      class = if (identical(id, ids[1])) "p3-tab-pane active" else "p3-tab-pane",
      `data-pane` = id,
      tabs[[id]]$content
    )
  })
  htmltools::tags$div(
    class = "p3-tabs",
    htmltools::tags$div(class = "p3-tabbar", buttons),
    panes
  )
}
