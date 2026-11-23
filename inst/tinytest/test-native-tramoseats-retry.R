# Reproducible monthly airline series whose fitted model needs a SEATS retry.
# Integer innovations and binary fractions keep the input identical on all hosts.
state <- 41
innovations <- numeric(169L)
for (j in seq_along(innovations)) {
  state <- (state * 48271) %% 2147483647
  innovations[j] <- state %% 31 - 15
}
y <- rep(2500, length(innovations))
for (j in 14:length(y)) {
  u <- innovations[j] - 0.5 * innovations[j - 1L] +
    0.5 * innovations[j - 12L] - 0.25 * innovations[j - 13L]
  y[j] <- y[j - 1L] + y[j - 12L] - y[j - 13L] + u
}
series <- ts(y[-seq_len(13L)], start = c(2002L, 3L), frequency = 12)
native <- perTramo(series, "rsa0", backend = "native")
native$run()
expect_identical(native$result$status$code, "success")
expect_identical(as.numeric(native$result$components$y), as.numeric(series))
expect_identical(as.numeric(native$adjusted[c(1L, length(series))]),
                 c(2494.5735366591171, 1131.1538582348558))
expect_identical(as.integer(start(native$forecasts$points$sa)), c(2015L, 3L))
expect_identical(length(native$forecasts$points$sa), 24L)

if (requireNamespace("rjd3tramoseats", quietly = TRUE)) {
  ref <- perTramo(series, "rsa0", backend = "rjd3", userdefined = "cal_f(24)")
  ref$run()
  grid <- function(x) c(start(x), end(x), frequency(x))
  for (key in c("y", "t", "sa", "s", "i", "cal")) {
    actual <- native$result$components[[key]]
    expected <- ref$result$components[[key]]
    expect_identical(as.numeric(actual), as.numeric(expected), info = key)
    expect_identical(grid(actual), grid(expected), info = key)
    actual <- native$forecasts$points[[key]]
    expected <- if (key == "cal") ref$output$user_defined[["cal_f(24)"]] else ref$forecasts$points[[key]]
    expect_identical(as.numeric(actual), as.numeric(expected), info = key)
    expect_identical(grid(actual), grid(expected), info = key)
  }
}
