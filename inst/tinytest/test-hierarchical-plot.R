# Deterministic aggregate results exercise rendering independently of Java.
make_plot_hierarchy <- function(freq = 12, with_outliers = TRUE) {
  y <- ts(seq_len(3 * freq), start = c(2020, 1), frequency = freq)
  variables <- if (with_outliers) {
    lapply(seq_along(c("AO", "LS", "TC", "AO")), function(i) {
      list(name = paste0(c("AO", "LS", "TC", "AO")[i], " (",
                         c("2020-01-01", "2021-04-01", "2022-07-01", "2010-01-01")[i], ")"),
           type = c("AO", "LS", "TC", "AO")[i],
           coef = list(value = 1, type = "FIXED"))
    })
  } else list()
  structure(list(
    ts = y, adjustedDirect = y - 1, adjustedIndirect = y - 2,
    params = list(),
    output = structure(list(
      user_defined = list(
        y_f = ts(3 * freq + seq_len(freq), start = c(2023, 1), frequency = freq),
        y_ef = ts(rep(2, freq), start = c(2023, 1), frequency = freq)),
      preprocessing = list(description = list(variables = variables),
                           estimation = list(likelihood = list(
                             neffectiveobs = length(y), nparams = 1)))
    ), class = "JD3_X13_RSLTS")
  ), class = "hierarchicalTimeSeries")
}

for (freq in c(12, 4)) {
  fixture <- make_plot_hierarchy(freq)
  graph <- plot(fixture)
  expect_inherits(graph, "dygraphs")
  expect_true(all(c("Original", "Direct SA", "Indirect SA", "Forecasts",
                    "AO", "LS", "TC") %in% graph$x$attrs$labels))
  values <- attr(graph$x, "data")
  expect_equal(tail(values$lowerci, freq), as.numeric(fixture$output$user_defined$y_f) - 3.92)
  expect_equal(tail(values$upperci, freq), as.numeric(fixture$output$user_defined$y_f) + 3.92)
  expect_equal(which(!is.na(values$otlAO)), 1L)
  expect_equal(which(!is.na(values$otlLS)), as.integer(freq + freq / 4 + 1))
  expect_equal(which(!is.na(values$otlTC)), as.integer(2 * freq + freq / 2 + 1))
  expect_equal(length(graph$x$events), 3L)
  expect_equal(length(graph$x$attrs$colors), length(graph$x$attrs$labels) - 1L)
  palette <- setNames(graph$x$attrs$colors, graph$x$attrs$labels[-1])
  expect_equal(unname(palette[c("AO", "LS", "TC")]), c("red", "orange", "dodgerblue"))

  plain <- plot(fixture, forecasts = FALSE, showOutliers = FALSE)
  expect_equal(plain$x$attrs$labels[-1], c("Original", "Direct SA", "Indirect SA"))
  expect_equal(length(plain$x$data[[1]]), length(fixture$ts))
  expect_equal(length(plain$x$events), 0L)

  fixture$output$user_defined$y_ef <- NULL
  line_only <- plot(fixture, showOutliers = FALSE)
  expect_true("Forecasts" %in% line_only$x$attrs$labels)
  expect_false(isTRUE(line_only$x$attrs$customBars))

  fixture$output$user_defined$y_f <- NULL
  expect_false("Forecasts" %in% plot(fixture)$x$attrs$labels)
  fixture$output <- NULL
  expect_equal(plot(fixture)$x$attrs$labels[-1], "Original")

  no_outliers <- plot(make_plot_hierarchy(freq, FALSE))
  expect_equal(length(no_outliers$x$events), 0L)
}
