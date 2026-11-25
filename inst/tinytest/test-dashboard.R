# HTML dashboards must preserve content, titles, and export behavior.
# M7 default-backend cutover (item 27, decision 397cfe8): this file compares
# rjd3 reference output, so the rjd3 backend is pinned at every construction.
x <- perX13(AirPassengers, "rsa3", backend = "rjd3")
y <- perTramo(AirPassengers, "rsa3", backend = "rjd3")
expect_error(createDashboard(x), "Run the model")
expect_error(dashboard_single(1), "Expected an x13Single")
x$run()
y$run()
expect_error(createDashboard(1), "Unsupported object class")
expect_error(createDashboard(1, y), "First object")
expect_error(createDashboard(x, 1), "Second object")
expect_error(dashboard_compare(x, perX13(AirPassengers, backend = "rjd3")), "Run the model")

local({
  output_dir <- tempfile("dashboard-html-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE))
  for (obj in list(x, y)) {
    file <- file.path(output_dir, "single.html")
    ui <- createDashboard(obj, title = "Model <review>", file = file)
    html <- paste(readLines(file, warn = FALSE), collapse = "\n")
    expect_inherits(ui, "shiny.tag.list")
    expect_true(grepl("Model &lt;review&gt;", html, fixed = TRUE))
    expect_true(grepl("Residual diagnostics", html, fixed = TRUE))
    expect_true(grepl("dygraphs html-widget", html, fixed = TRUE))
    expect_false(grepl("Plot error:", html, fixed = TRUE))
  }
  default <- as.character(createDashboard(x))
  expect_true(grepl("Series Diagnostics", default, fixed = TRUE))
  file <- file.path(output_dir, "comparison.html")
  ui <- createDashboard(x, y, file = file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_inherits(ui, "shiny.tag.list")
  expect_true(grepl("Series Comparison", html, fixed = TRUE))
  expect_true(grepl("Comparison Summary", html, fixed = TRUE))
  expect_true(grepl("Series A", html, fixed = TRUE))
  expect_true(grepl("Series B", html, fixed = TRUE))
})

# The Shiny aggregate panel uses dashboard_single directly.
hierarchy <- perHts(a = AirPassengers, b = AirPassengers, method = "x13", backend = "rjd3")
hierarchy$run()
aggregate_html <- as.character(dashboard_single(hierarchy))
expect_true(grepl("Directly and Indirectly", aggregate_html, fixed = TRUE))
expect_false(grepl("Plot error:", aggregate_html, fixed = TRUE))
expect_error(dashboard_compare(hierarchy, x), "Expected an x13Single")

# Run real servers from a directory that is not a package checkout.
local({
  previous_dir <- getwd()
  setwd(tempdir())
  on.exit(setwd(previous_dir), add = TRUE)
  for (constructor in list(perBatch, perHts)) {
    obj <- constructor(a = AirPassengers, b = AirPassengers, method = "x13",
                       backend = "rjd3")
    obj$run()
    expect_error(createDashboard(obj, launch.browser = NA), "launch.browser")
    expect_error(createDashboard(obj, startup.timeout = 0), "startup.timeout")
    expect_error(createDashboard(obj, launch.browser = FALSE,
                                 startup.timeout = .Machine$double.eps),
                 "failed to start")
    process <- createDashboard(obj, title = "Dashboard lifecycle test",
                               launch.browser = FALSE)
    on.exit(if (process$is_alive()) process$kill(), add = TRUE)
    expect_true(process$is_alive())
    html <- paste(readLines(attr(process, "url"), warn = FALSE), collapse = "\n")
    expect_true(grepl("Dashboard lifecycle test", html, fixed = TRUE))
    expect_true(grepl("Please select a series", html, fixed = TRUE))
    process$kill()
    process$wait(timeout = 5000)
    expect_false(process$is_alive())
  }
})

# Changing the selected series must replace the panel instead of reusing stale UI.
local({
  module_env <- new.env(parent = asNamespace("shiny"))
  source(system.file("app", "modules", "mod_series.R", package = "persephone3"),
         local = module_env)
  calls <- 0L
  module_env$dashboard_single <- function(obj) {
    calls <<- calls + 1L
    htmltools::div(class(obj)[1])
  }
  selected <- shiny::reactiveVal(x)
  shiny::testServer(module_env$mod_series_server,
                    args = list(series_reactive = selected), {
    expect_true(grepl("x13Single", output$content$html, fixed = TRUE))
    session$flushReact()
    expect_equal(calls, 1L)
    selected(y)
    session$flushReact()
    expect_true(grepl("tramoseatsSingle", output$content$html, fixed = TRUE))
    expect_equal(calls, 2L)
  })
})
