# Labels are display data; routing and selection use integer-path identities.
local({
  app_env <- new.env(parent = asNamespace("shiny"))
  app_dir <- system.file("app", package = "persephone3")
  old_dir <- getwd()
  setwd(app_dir)
  tryCatch(source("server.R", local = app_env), finally = setwd(old_dir))

  node <- function(text, children = NULL, quality = "ok") {
    value <- new.env(parent = emptyenv())
    value$text <- text
    value$output <- list(user_defined = list(quality.summary = quality))
    value$components <- children
    value
  }
  hostile <- "quotes '\" <img src=x onerror=alert(1)> & / \u00dc\u65e5\u672c"
  first <- node("child named Aggregate")
  left <- node("left duplicate")
  special <- node("special label", quality = "<b>warning</b>")
  right <- node("right duplicate")
  tree <- node("root aggregate", setNames(list(
    first,
    node("left branch", setNames(list(left, special), c("duplicate", hostile))),
    node("right branch", list(duplicate = right))
  ), c("Aggregate", "branch/one", "branch two")))
  registry <- app_env$dashboard_registry(tree)

  expect_equal(registry$id,
               c("node_0", "node_1", "node_2", "node_2_1", "node_2_2", "node_3", "node_3_1"))
  expect_equal(registry$label, c("Aggregate", "Aggregate", "branch/one", "duplicate",
                                  hostile, "branch two", "duplicate"))
  expect_equal(registry$depth, c(0L, 1L, 1L, 2L, 2L, 1L, 2L))
  expect_identical(registry$path[[1]], integer())
  expect_identical(registry$path[[5]], c(2L, 2L))
  expect_identical(registry, app_env$dashboard_registry(tree))
  expect_identical(app_env$dashboard_series(tree, "node_0", registry), tree)
  expect_identical(app_env$dashboard_series(tree, "node_1", registry), first)
  expect_identical(app_env$dashboard_series(tree, "node_2_1", registry), left)
  expect_identical(app_env$dashboard_series(tree, "node_3_1", registry), right)
  expect_identical(app_env$dashboard_series(tree, "node_2_2", registry), special)
  for (bad in list("Aggregate", "branch/one/duplicate", hostile, "node_99",
                   "node_2_0", NA_character_, character(), c("node_1", "node_2"), 1)) {
    expect_false(app_env$dashboard_valid_id(bad, registry))
    expect_error(app_env$dashboard_series(tree, bad, registry), "Unknown dashboard")
  }
  expect_true(app_env$dashboard_valid_selection(character(), registry))
  expect_false(app_env$dashboard_valid_selection(c("node_1", "node_1"), registry))
  expect_false(app_env$dashboard_valid_selection(c("node_1", NA_character_), registry))
  batch <- node("batch", list(Aggregate = first))
  batch$output <- NULL
  expect_equal(app_env$dashboard_registry(batch)$id, "node_1")
  expect_equal(nrow(app_env$dashboard_registry(node("empty", list()))), 1L)
  batch$components <- list()
  expect_equal(nrow(app_env$dashboard_registry(batch)), 0L)
  for (quality in list(NULL, NA_character_, "", "   ")) {
    expect_equal(app_env$get_quality(node("no quality", quality = quality)), "unavailable")
  }
  expect_equal(app_env$get_quality(node("quality", quality = " Good ")), "good")

  # Both legacy incremental events and server uncheck acknowledgements reject
  # malformed input without changing the selected set.
  shiny::testServer(app_env$mod_table_server, args = list(hts = tree, registry = registry), {
    expect_identical(selected(), character())
    expect_true(grepl(hostile, df$Series[5], fixed = TRUE))
    expect_equal(df$Quality[5], "<b>warning</b>")
    expect_false(any(grepl(hostile, df$selected, fixed = TRUE)))
    expect_true(all(grepl('data-id="node_', df$selected, fixed = TRUE)))
    session$setInputs(row_event = list(id = "node_2_1", checked = TRUE))
    session$setInputs(row_event = list(id = "node_3_1", checked = TRUE))
    expect_equal(selected(), c("node_2_1", "node_3_1"))
    session$setInputs(row_event = list(id = "node_2_1", checked = TRUE))
    expect_equal(selected(), c("node_2_1", "node_3_1"))
    for (event in list(
      list(id = hostile, checked = TRUE),
      list(id = "node_99", checked = TRUE),
      list(id = c("node_0", "node_1"), checked = TRUE),
      list(id = "node_0", checked = NA),
      list(id = "node_0", checked = "true"),
      list(id = "node_0", checked = c(TRUE, FALSE)),
      "node_0"
    )) {
      session$setInputs(row_event = event)
      expect_equal(selected(), c("node_2_1", "node_3_1"))
    }
    session$setInputs(uncheckRow_event = hostile)
    expect_equal(selected(), c("node_2_1", "node_3_1"))
    session$setInputs(row_event = list(id = "node_2_1", checked = FALSE))
    expect_equal(selected(), "node_3_1")
    session$setInputs(uncheckRow_event = "node_3_1")
    expect_identical(selected(), character())
  })

  old_options <- options(persephone.dashboard.object = tree)
  on.exit(options(old_options), add = TRUE)
  rendered_nodes <- character()
  app_env$dashboard_single <- function(obj) {
    rendered_nodes <<- c(rendered_nodes, obj$text)
    shiny::div(obj$text)
  }

  shiny::testServer(app_env$server, {
    inserted <- list()
    session$sendInsertTab <- function(inputId, liTag, divTag, ...) {
      inserted[[length(inserted) + 1L]] <<- liTag
    }
    session$setInputs("table-row_event" = list(id = "node_2_2", checked = TRUE))
    session$flushReact()
    expect_equal(open_tabs(), "node_2_2")
    expect_true(grepl("special label", output[["tab_node_2_2-content"]]$html, fixed = TRUE))
    tab_html <- inserted[[1]]$html
    expect_true(grepl("&lt;img", tab_html, fixed = TRUE))
    expect_false(grepl("<img", tab_html, fixed = TRUE))
    expect_false(grepl("onclick=", tab_html, fixed = TRUE))
    expect_true(grepl('data-series-id="node_2_2"', tab_html, fixed = TRUE))
    expect_equal(ls(initialized), "node_2_2")
    expect_equal(panel_state[["node_2_2"]]$generation, 1L)

    # Forged close and selection requests cannot address a display label.
    session$setInputs(close_tab = hostile)
    expect_equal(open_tabs(), "node_2_2")
    session$setInputs(close_tab = "tab_node_2_2")
    expect_equal(open_tabs(), "node_2_2")
    session$setInputs(close_tab = c("node_2_2", "node_1"))
    expect_equal(open_tabs(), "node_2_2")

    for (generation in 2:4) {
      session$setInputs(close_tab = "node_2_2")
      expect_identical(open_tabs(), character())
      expect_identical(selected_series(), character())
      expect_false(panel_state[["node_2_2"]]$open)
      special$text <- paste("updated panel", generation)
      before <- length(rendered_nodes)
      session$flushReact()
      expect_equal(length(rendered_nodes), before)
      session$setInputs("table-row_event" = list(id = "node_2_2", checked = TRUE))
      expect_true(grepl(special$text, output[["tab_node_2_2-content"]]$html, fixed = TRUE))
      expect_equal(length(rendered_nodes), before + 1L)
      expect_equal(ls(initialized), "node_2_2")
      expect_equal(panel_state[["node_2_2"]]$generation, as.integer(generation))
    }

    # Distinct duplicate names and Aggregate each open the correct module.
    for (id in c("node_0", "node_1", "node_2_1", "node_3_1")) {
      session$setInputs("table-row_event" = list(id = id, checked = TRUE))
      expect_true(grepl(app_env$dashboard_series(tree, id, registry)$text,
                       output[[paste0("tab_", id, "-content")]]$html, fixed = TRUE))
    }
    expect_equal(length(ls(initialized)), 5L)
    order <- rev(open_tabs())
    session$setInputs(tab_order = order)
    expect_equal(open_tabs(), order)
    session$setInputs(tab_order = c(order, "node_99"))
    expect_equal(open_tabs(), order)
    session$setInputs("table-row_event" = list(id = "node_1", checked = FALSE))
    expect_false("node_1" %in% open_tabs())
    expect_false(panel_state[["node_1"]]$open)
  })
})
