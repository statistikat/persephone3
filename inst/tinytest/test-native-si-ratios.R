series <- AirPassengers
fixed <- function(mode) perX13(series, "restricted", backend = "native", speclist = list(
  mode = mode, log = mode != "additive", seasonal = rep("s3x3", 12L), trend_length = 13L))
modes <- c("additive", "multiplicative", "log_additive", "pseudo_additive")
objects <- c(stats::setNames(lapply(modes, fixed), modes), list(
  rsa3 = perX13(series, "rsa3", backend = "native", speclist = list(backcast = 3L, forecast = 6L)),
  rsa0 = perTramo(series, "rsa0", backend = "native")))
expect_error(objects$rsa0$plotSiRatios(), "No results from run")

for (mode in names(objects)) {
  x <- objects[[mode]]
  x$run()
  seats <- mode == "rsa0"
  if (seats) {
    si <- x$result$components$s + x$result$components$i
    expected <- list(`SI-Ratio` = si, Mean = stats::ave(as.numeric(si), cycle(si), FUN = mean))
  } else {
    d <- x$output$selection$d$tables
    factor <- x$output$results$d10
    si <- if (mode == "log_additive") exp(d$D8) else d$D8
    replacement <- if (mode == "log_additive") exp(d$D9) else d$D9
    expected <- list(`SI-Ratio` = si, `Replaced SI-Ratio` = replacement,
      `Seasonal Factor` = factor, `SF Mean` = stats::ave(as.numeric(factor), cycle(si), FUN = mean))
  }
  hc <- x$plotSiRatios()$x$hc_opts$series
  static <- x$plotSiRatios(plotEngine = "ggplot")
  for (name in names(expected)) {
    cells <- Filter(function(s) identical(s$name, name) && length(s$data) > 0L, hc)
    actual <- unlist(lapply(cells, function(s) vapply(s$data, "[[", numeric(1), "y")), use.names = FALSE)
    by_month <- order(as.numeric(cycle(si)), as.numeric(time(si)))
    expect_identical(actual, as.numeric(expected[[name]])[by_month])
    layers <- Filter(function(l) identical(unique(l$data$variable), name), static$layers)
    expect_identical(layers[[1L]]$data$value, as.numeric(expected[[name]]))
    expect_identical(layers[[1L]]$data$year, floor(as.numeric(time(si))))
    expect_identical(as.integer(layers[[1L]]$data$cycleName), as.integer(cycle(si)))
  }
  if (seats && requireNamespace("rjd3tramoseats", quietly = TRUE)) {
    ref <- rjd3tramoseats::tramoseats_fast(series, spec = "rsa0",
      userdefined = c("decomposition.s_cmp", "decomposition.i_cmp"))$user_defined
    expect_identical(as.numeric(si), as.numeric(ref$decomposition.s_cmp + ref$decomposition.i_cmp))
  }
  if (!seats && requireNamespace("rjd3x13", quietly = TRUE)) {
    if (mode == "rsa3") {
      spec <- rjd3x13::set_x11(rjd3x13::x13_spec("rsa3"), fcasts = 6, bcasts = 3)
    } else {
      spec <- rjd3toolkit::set_transform(rjd3x13::x13_spec("rsa0"),
        fun = if (mode == "additive") "None" else "Log")
      spec <- rjd3toolkit::set_tradingdays(spec, option = "None")
      spec <- rjd3x13::set_x11(spec, mode = switch(mode, additive = "Additive",
        multiplicative = "Multiplicative", log_additive = "LogAdditive", pseudo_additive = "PseudoAdditive"),
        fcasts = 0, bcasts = 0, seasonal.filter = "S3X3", henderson.filter = 13)
    }
    reference <- rjd3x13::x13_fast(series, spec = spec)$decomposition
    for (k in seq_len(3L)) {
      native <- expected[[k]]
      target <- reference[[c("d8", "d9", "d10")[k]]]
      expect_identical(c(start(native), end(native), frequency(native), length(native)),
                       c(start(target), end(target), frequency(target), length(target)))
      expect_identical(is.na(as.numeric(native)), is.na(as.numeric(target)))
      expect_true(all(abs(as.numeric(native) - as.numeric(target)) <= 1e-8, na.rm = TRUE))
    }
  }
}
