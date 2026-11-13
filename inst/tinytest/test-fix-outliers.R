if (!all(vapply(c("rjd3toolkit", "rjd3x13", "rjd3tramoseats"),
                requireNamespace, logical(1), quietly = TRUE))) {
  message("Skipping optional rjd3 outlier-fixing fixtures: reference packages unavailable")
} else {
  local({
    message("Test fitted outlier fixing for both methods and frequencies")
    saved_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (is.null(saved_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
      } else assign(".Random.seed", saved_seed, envir = .GlobalEnv)
    })
    make_series <- function(frq) {
      set.seed(721)
      n <- 20 * frq
      i <- seq_len(n)
      y <- ts(200 + .35 * i + 12 * sin(2 * pi * i / frq) + rnorm(n, sd = 2),
              start = c(2000, 1), frequency = frq)
      y[3 * frq + 2] <- y[3 * frq + 2] + 80
      y[n - 1] <- y[n - 1] + 100
      y
    }
    direct_spec <- function(x) if (inherits(x, "hierarchicalTimeSeries")) x$paramsDirect else x$params
    regression_spec <- function(x) {
      spec <- direct_spec(x)
      if (x$method == "x13") spec$regarima else spec$tramo
    }
    manual_keys <- function(x) {
      records <- persephone3:::manual_outlier_table(regression_spec(x))
      paste(records$type, records$date)
    }
    enabled <- function(x) {
      detection <- regression_spec(x)$outlier
      if (x$method == "x13") length(detection$outliers) > 0 else isTRUE(detection$enabled)
    }

    for (method in c("x13", "tramoseats")) {
      construct <- if (method == "x13") perX13 else perTramo
      for (frq in c(12, 4)) {
        y <- make_series(frq)
        fitted <- construct(y, backend = "rjd3", transform.fun = "None", setout.critical.value = 3.5,
                            addout.type = "AO", addout.date = "2001-01-01", addout.name = "existing manual",
                            addout.coef = .125)
        expect_warning(fitted$fixOutlier(), "not run yet")
        fitted$run()
        before <- fitted$getOutliers()
        old <- before[before$autoout & before$date < as.Date("2019-01-01"), , drop = FALSE]
        recent <- before[before$autoout & before$date >= as.Date("2019-01-01"), , drop = FALSE]
        expect_true(nrow(old) > 0)
        expect_true(nrow(recent) > 0)
        original_manual <- regression_spec(fitted)$regression$outliers[[1]]
        original_fit <- fitted$output

        fixed <- fitted$clone(deep = TRUE)
        expect_silent(fixed$fixOutlier(timespan = 12))
        expect_true(all(paste(old$type, old$date) %in% manual_keys(fixed)))
        expect_false(any(paste(recent$type, recent$date) %in% manual_keys(fixed)))
        expect_identical(regression_spec(fixed)$regression$outliers[[1]], original_manual)
        expect_identical(regression_spec(fixed)$outlier$span$d0, "2019-01-01")
        expect_identical(fixed$output, original_fit)
        expect_true(fixed$is_stale)
        once <- direct_spec(fixed)
        fixed$fixOutlier(timespan = 12)
        expect_identical(direct_spec(fixed), once)
        fixed$run()
        expect_false(fixed$is_stale)
        after <- fixed$getOutliers()
        specified <- paste(after$type, after$date) %in% paste(old$type, old$date)
        expect_true(all(!after$autoout[specified]))
        expect_true(all(after$coef.type[specified] == "ESTIMATED"))
        expect_true(any(after$autoout & after$date >= as.Date("2019-01-01")))
        fixed$fixOutlier(timespan = 12)
        expect_identical(direct_spec(fixed), once)

        all_fixed <- fitted$clone(deep = TRUE)
        all_fixed$fixOutlier(timespan = 0)
        expect_true(all(paste(before$type, before$date) %in% manual_keys(all_fixed)))
        expect_false(enabled(all_fixed))
        all_fixed$run()
        expect_false(any(all_fixed$getOutliers()$autoout))
        all_spec <- direct_spec(all_fixed)
        all_fixed$fixOutlier(timespan = 0)
        expect_identical(direct_spec(all_fixed), all_spec)

        # An oversized window and disabled detection must not re-enable or
        # alter settings, nor destroy existing manual coefficient constraints.
        complete <- fitted$clone(deep = TRUE)
        original <- direct_spec(complete)
        complete$fixOutlier(timespan = 1e300)
        expect_identical(direct_spec(complete), original)
        complete$updateParams(setout.outliers.type = character())
        disabled <- direct_spec(complete)
        complete$fixOutlier(timespan = 12)
        expect_identical(direct_spec(complete), disabled)

        for (invalid in list(-1, NA_real_, Inf, c(1, 2), "12")) {
          expect_error(fitted$fixOutlier(timespan = invalid), "finite nonnegative")
          expect_identical(direct_spec(fitted), original)
        }
        expect_error(fitted$fixOutlier(iterate = NA), "TRUE or FALSE")
        expect_error(fitted$fixOutlier(verbose = 1), "TRUE or FALSE")

        if (frq == 4) {
          one_month <- fitted$clone(deep = TRUE)
          one_month$fixOutlier(timespan = 1)
          expect_identical(regression_spec(one_month)$outlier$span$d0, "2019-10-01")
          expect_true(all(paste(recent$type, recent$date) %in% manual_keys(one_month)))
          four_months <- fitted$clone(deep = TRUE)
          four_months$fixOutlier(timespan = 4)
          expect_identical(regression_spec(four_months)$outlier$span$d0, "2019-07-01")
          expect_false(any(paste(recent$type, recent$date) %in% manual_keys(four_months)))
        }

        no_outliers <- construct(y, backend = "rjd3", transform.fun = "None", setout.critical.value = 99999)
        no_outliers$run()
        expect_identical(nrow(no_outliers$getOutliers()), 0L)
        no_outliers$fixOutlier()
        expect_identical(length(manual_keys(no_outliers)), 0L)
        no_outliers$run()
        expect_identical(nrow(no_outliers$getOutliers()), 0L)
      }

      constrained <- fitted$clone(deep = TRUE)
      constrained$updateParams(setout.span.type = "Between",
                               setout.d0 = "2019-07-01", setout.d1 = "2019-10-01")
      constrained$fixOutlier(timespan = 12)
      expect_identical(regression_spec(constrained)$outlier$span$d0, "2019-07-01")
      expect_identical(regression_spec(constrained)$outlier$span$d1, "2019-10-01")
      constrained$run()
      constrained_out <- constrained$getOutliers()
      expect_true(all(constrained_out$date[constrained_out$autoout] >= as.Date("2019-07-01")))
      expect_true(all(constrained_out$date[constrained_out$autoout] < as.Date("2019-10-01")))

      message("Test hierarchy aggregate-only, recursive and batch fixing")
      y <- make_series(12)
      child <- perHts(a = y, b = y * 1.1, method = method, backend = "rjd3",
                      transform.fun = "None", setout.critical.value = 3.5)
      hierarchy <- perHts(group = child, c = y / 2, method = method, backend = "rjd3",
                          transform.fun = "None", setout.critical.value = 3.5)
      hierarchy$run()
      child_spec <- hierarchy$components$group$params
      child_direct <- hierarchy$components$group$paramsDirect
      other <- hierarchy$components$c$params
      hierarchy$fixOutlier()
      expect_true(length(manual_keys(hierarchy)) > 0L)
      expect_identical(hierarchy$components$group$params, child_spec)
      expect_identical(hierarchy$components$group$paramsDirect, child_direct)
      expect_identical(hierarchy$components$c$params, other)
      hierarchy$fixOutlier(iterate = TRUE)
      expect_true(length(manual_keys(hierarchy$components$group)) > 0L)
      expect_true(length(manual_keys(hierarchy$components$group$components$a)) > 0L)
      expect_true(length(manual_keys(hierarchy$components$c)) > 0L)
      hierarchy$run()
      expect_false(hierarchy$is_stale)

      batch <- perBatch(a = y, b = y * 1.1, method = method, backend = "rjd3",
                        transform.fun = "None", setout.critical.value = 3.5)
      batch$run()
      batch$fixOutlier()
      expect_true(all(vapply(batch$components, function(x) length(manual_keys(x)) > 0L, logical(1))))
      batch$run()
      expect_false(batch$is_stale)

      partial <- perBatch(a = fitted$clone(deep = TRUE), b = construct(make_series(4), backend = "rjd3"),
                          method = method, backend = "rjd3")
      a_before <- partial$components$a$params
      expect_warning(partial$fixOutlier(), "not run yet")
      expect_identical(partial$components$a$params, a_before)
    }
  })
}
