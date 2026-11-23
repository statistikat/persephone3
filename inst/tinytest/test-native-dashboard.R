# Build native batches and hierarchies through the same constructors used
# with raw monthly inputs, then serve their installed Shiny application.
local({
  fixed <- list(seasonal = rep("s3x3", 12L), trend_length = 13L)
  first <- as.persephone(AirPassengers, method = "x13", template = "restricted",
                         backend = "native")
  expect_identical(first$backend, "native")
  expect_error(as.persephone(AirPassengers, "x13", "rsa0", backend = "native"),
               "fixed_coefficients_only")
  expect_error(as.persephone(AirPassengers, "x13", "restricted", backend = "rjd3"),
               "Invalid template")

  previous <- getwd()
  setwd(tempdir())
  on.exit(setwd(previous))
  for (constructor in list(perBatch, perHts)) {
    group <- constructor(first = first, second = AirPassengers,
                         method = "x13", template = "restricted",
                         speclist = fixed, backend = "native")
    group$run()
    expect_null(first$result)
    expect_identical(group$components$first$result$backend, "native")
    expect_identical(group$components$first$adjusted, group$components$second$adjusted)
    if (inherits(group, "hierarchicalTimeSeries")) {
      aggregate <- as.character(dashboard_single(group))
      expect_true(grepl("Directly and Indirectly", aggregate, fixed = TRUE))
      expect_false(grepl("Plot error:", aggregate, fixed = TRUE))
    }
    proc <- createDashboard(group, title = "Native dashboard", launch.browser = FALSE)
    tryCatch({
      page <- paste(readLines(attr(proc, "url"), warn = FALSE), collapse = "\n")
      expect_true(proc$is_alive())
      expect_true(grepl("Native dashboard", page, fixed = TRUE))
      expect_true(grepl("Please select a series", page, fixed = TRUE))
    }, finally = {
      if (proc$is_alive()) proc$kill()
      proc$wait(timeout = 5000)
    })
    expect_false(proc$is_alive())
  }
})
