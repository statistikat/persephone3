local({
  old <- Sys.getenv("PERSEPHONE_NUM_THREADS", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("PERSEPHONE_NUM_THREADS") else
    Sys.setenv(PERSEPHONE_NUM_THREADS = old))
  info <- getFromNamespace(".native_parallel_info", "persephone3")
  convolve <- getFromNamespace(".native_convolve", "persephone3")
  filter <- getFromNamespace(".native_filter", "persephone3")
  transform <- getFromNamespace(".native_transform", "persephone3")
  inverse <- getFromNamespace(".native_inverse_transform", "persephone3")
  Sys.setenv(PERSEPHONE_NUM_THREADS = "1")
  single <- info()
  expect_equal(single$selected_threads, 1L)
  expect_equal(single$probe_threads, 1L)
  expect_false(single$parallel_enabled)
  n <- 150000L
  x <- exp(sin(seq_len(n) / 13))
  x[seq.int(1L, n, by = 101L)] <- NA_real_
  left <- sin(seq_len(2048) / 7)
  right <- cos(seq_len(129) / 11)
  weights <- seq_len(31)^-1
  offsets <- -15:15
  before <- serialize(list(x, left, right, weights, offsets), NULL)
  serial <- list(convolve(left, right), filter(x, weights, offsets),
                 transform(x, "log"), inverse(log(x), "log"))
  Sys.setenv(PERSEPHONE_NUM_THREADS = "4")
  threaded <- info()
  expect_identical(threaded$compiled, single$compiled)
  expect_equal(threaded$requested_threads, 4L)
  expect_true(threaded$selected_threads <= min(4L, threaded$runtime_max_threads))
  expect_true(threaded$probe_threads >= 1L && threaded$probe_threads <= threaded$selected_threads)
  expect_identical(threaded$parallel_enabled, threaded$probe_threads > 1L)
  if (!threaded$compiled) {
    expect_equal(threaded$openmp_version, 0L)
    expect_equal(threaded$selected_threads, 1L)
  }
  if (identical(Sys.getenv("PERSEPHONE_REQUIRE_OPENMP"), "1")) {
    expect_true(threaded$compiled)
    expect_true(threaded$probe_threads > 1L)
  }
  parallel <- list(convolve(left, right), filter(x, weights, offsets),
                   transform(x, "log"), inverse(log(x), "log"))
  for (i in seq_along(serial)) expect_identical(parallel[[i]], serial[[i]])
  expect_identical(serialize(list(x, left, right, weights, offsets), NULL), before)
  invalid <- x
  invalid[1000L] <- -1
  expect_error(transform(invalid, "log"), "positive observed")
  invalid[1000L] <- 1000
  expect_error(inverse(invalid, "log"), "overflowed or underflowed")
  overflow <- rep(1e308, 2048)
  expect_error(convolve(overflow, rep(10, 129)), "non-finite result")
  expect_error(filter(rep(1e308, n), c(10, 1), c(0L, 1L)), "non-finite result")
  expect_identical(transform(x, "log"), serial[[3L]])
  for (bad in c("0", "65", "-1", "+2", "2.5", " 2", "two", "999999999999")) {
    Sys.setenv(PERSEPHONE_NUM_THREADS = bad)
    expect_error(info(), "integer from 1 to 64")
    expect_error(transform(x, "log"), "integer from 1 to 64")
  }
  Sys.setenv(PERSEPHONE_NUM_THREADS = "1")
  expect_equal(convolve(c(1, 2), c(3, 4)), c(3, 10, 8))
})
