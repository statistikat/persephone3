# Arithmetic and time alignment do not depend on a computation backend.
aggregate_values <- getFromNamespace("hts_aggregate_series", "persephone3")
prepare_weights <- getFromNamespace("hts_prepare_weights", "persephone3")

for (freq in c(12, 4)) {
  a <- ts(seq_len(2 * freq), start = c(2020, 1), frequency = freq)
  b <- a + 100
  values <- list(a = a, b = b)
  expect_equal(aggregate_values(values), a + b)
  expect_equal(aggregate_values(values, c(a = 1, b = 3)), (a + 3 * b) / 4)
  expect_equal(aggregate_values(values, c(b = 3, a = 1)), (a + 3 * b) / 4)

  short <- ts(seq_len(freq), start = start(a), frequency = freq)
  other <- short * 0 + 1
  weights <- list(a = short, b = other)
  effective <- c(seq_len(freq), rep(freq, freq))
  expected <- ts((as.numeric(a) * effective + as.numeric(b)) / (effective + 1),
                 start = start(a), frequency = freq)
  expect_equal(aggregate_values(values, weights), expected)
  expect_equal(aggregate_values(values, weights)[1], 51)
  expect_equal(aggregate_values(values, list(b = other, a = short)), expected)
  expect_equal(aggregate_values(values, cbind(b = other, a = short)), expected)

  # Input weights may start earlier; align by actual periods, not positions.
  early <- ts(seq_len(3 * freq), start = c(2019, 1), frequency = freq)
  expect_equal(aggregate_values(values, list(a = early, b = 1)),
               ts((as.numeric(a) * (freq + seq_along(a)) + as.numeric(b)) /
                    (freq + seq_along(a) + 1), start = start(a), frequency = freq))
  expect_error(aggregate_values(values, list(a = window(short, start = time(short)[2]), b = 1)),
               "beginning")
  expect_error(aggregate_values(values, list(a = ts(1:4, start = 2020, frequency = 2), b = 1)),
               "frequency")
  expect_error(aggregate_values(values, c(a = 1, b = -1)), "nonzero")
  expect_error(aggregate_values(values, c(a = 1, b = Inf)), "finite")
  expect_error(aggregate_values(values, c(a = 1, b = NA_real_)), "nonmissing")
  expect_error(aggregate_values(values, c(a = 1, absent = 2)), "names")
  expect_error(aggregate_values(values, c(a = 1, a = 2)), "names")
  expect_error(aggregate_values(values, c(a = 1)), "count")
  expect_error(aggregate_values(values, matrix(1, nrow = length(a), ncol = 2)), "mts")
  expect_error(aggregate_values(list(a = a, b = window(b, start = time(b)[2]))), "time instances")
  expect_error(aggregate_values(list(a = a), 0), "nonzero")

  with_missing <- a
  with_missing[2] <- NA_real_
  expect_equal(aggregate_values(list(a = with_missing, b = b), c(a = 0, b = 1)), b)
  expect_true(is.na(aggregate_values(list(a = with_missing, b = b))[2]))

  # Group totals preserve leaf weights through several weighted levels.
  group_one <- list(a = list(ts = a), b = list(ts = b))
  w_one <- prepare_weights(group_one, c(a = 1, b = 3))
  group_value <- aggregate_values(values, w_one)
  group <- structure(list(ts = group_value, weights = w_one), class = "hierarchicalTimeSeries")
  parent_nodes <- list(group = group, extra = list(ts = a * 2))
  inherited <- prepare_weights(parent_nodes, c(extra = 2))
  expect_equal(as.numeric(inherited[, "group"]), rep(4, length(a)))
  expect_equal(aggregate_values(list(group = group_value, extra = a * 2), inherited),
               (a + 3 * b + 4 * a) / 6)
  expect_error(prepare_weights(parent_nodes, NULL), "count")
  all_explicit <- prepare_weights(parent_nodes, c(extra = 1, group = 1))
  expect_equal(aggregate_values(list(group = group_value, extra = a * 2), all_explicit),
               (group_value + a * 2) / 2)

  future <- ts(seq_len(3 * freq), start = start(a), frequency = freq)
  retained <- prepare_weights(group_one, list(a = future, b = 1))
  expect_equal(NROW(retained), length(future))
  expect_equal(as.numeric(retained[, "a"]), as.numeric(future))
}

# Multivariate columns retain names and cannot be mixed/recycled silently.
first <- ts(cbind(level = 1:12, trend = 101:112), start = 2020, frequency = 12)
second <- first * 2
expect_equal(aggregate_values(list(a = first, b = second), c(a = 1, b = 3)), first * 1.75)
colnames(second) <- c("trend", "level")
expect_error(aggregate_values(list(a = first, b = second)), "column names")
expect_error(aggregate_values(list(a = first, b = first[, 1])), "number of columns")

if (all(vapply(c("rjd3toolkit", "rjd3x13", "rjd3tramoseats"),
               requireNamespace, logical(1), quietly = TRUE))) {
  message("Test nested hierarchy choices with actual fitted models")
  for (freq in c(12, 4)) {
    series <- if (freq == 12) AirPassengers else stats::aggregate(AirPassengers, nfrequency = 4)
    inner <- perHts(a = series, b = series * 1.1 + 20 * sin(seq_along(series)),
                    method = "x13", backend = "rjd3")
    middle <- perHts(group = inner, c = series / 2, method = "x13", iterate = FALSE,
                     backend = "rjd3")
    root <- perHts(group = middle, d = series * .7, method = "x13", iterate = FALSE,
                   backend = "rjd3")
    root$run()
    selected_middle <- root$components$group
    selected_inner <- selected_middle$components$group
    root$indirect <- TRUE
    selected_middle$indirect <- TRUE
    selected_inner$indirect <- FALSE
    expected <- selected_inner$adjustedDirect + selected_middle$components$c$adjusted +
      root$components$d$adjusted
    expect_equal(root$adjusted, expected)
    fully_indirect <- selected_inner$components$a$adjusted + selected_inner$components$b$adjusted +
      selected_middle$components$c$adjusted + root$components$d$adjusted
    expect_equal(root$adjustedIndirect, fully_indirect)
    expect_true(max(abs(expected - fully_indirect)) > 1e-6)
    selected_inner$indirect <- TRUE
    expect_equal(root$adjusted, fully_indirect)
    expect_equal(root$clone(deep = TRUE)$ts, root$ts)
  }

  a <- ts(1:24, start = 2020, frequency = 12)
  b <- a + 100
  w <- ts(1:12, start = 2020, frequency = 12)
  hierarchy <- perHts(a = a, b = b, weights = list(b = w * 0 + 1, a = w),
                      method = "x13", backend = "rjd3")
  expect_equal(hierarchy$ts[1], 51)
  expect_equal(hierarchy$ts[13], (a[13] * 12 + b[13]) / 13)

  group <- perHts(a = a, b = b, weights = c(a = 1, b = 3),
                  method = "x13", backend = "rjd3")
  upper <- perHts(group = group, c = a * 2, weights = c(c = 2),
                  method = "x13", backend = "rjd3")
  top <- perHts(group = upper, d = a * 3, weights = c(d = 4),
                method = "x13", backend = "rjd3")
  expect_equal(top$ts, (a + 3 * b + 4 * a + 12 * a) / 10)
  expect_equal(as.numeric(top$weights[, "group"]), rep(6, length(a)))
  second_group <- perHts(a = a, b = b, weights = c(a = 2, b = 4),
                         method = "x13", backend = "rjd3")
  grouped <- perHts(one = group, two = second_group, method = "x13", backend = "rjd3")
  expect_equal(grouped$ts, (3 * a + 7 * b) / 10)
  expect_equal(as.numeric(grouped$weights[1, ]), c(4, 6))
  explicit <- perHts(one = group, two = second_group, weights = c(two = 1, one = 1),
                     method = "x13", backend = "rjd3")
  expect_equal(explicit$ts, (group$ts + second_group$ts) / 2)

  # Readable public fields must not allow an old cached aggregate to be used
  # silently after mutation. Reconstruction is the supported update contract.
  changed <- hierarchy$clone(deep = TRUE)
  changed$weights[, 1] <- 2
  expect_error(changed$ts, "reconstruct")
  expect_error(changed$run(), "reconstruct")
  expect_error(changed$is_stale, "reconstruct")
  expect_equal(hierarchy$ts[1], 51)
  replaced <- hierarchy$clone(deep = TRUE)
  replaced$components$a <- perX13(a + 1, backend = "rjd3")
  expect_error(replaced$ts, "reconstruct")
}
