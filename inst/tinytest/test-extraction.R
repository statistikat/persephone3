# Fitted backend fixtures check metadata and uncertainty, not merely object type.
if (!all(vapply(c("rjd3toolkit", "rjd3x13", "rjd3tramoseats"),
                requireNamespace, logical(1), quietly = TRUE))) {
  message("Skipping optional rjd3 extraction fixtures: reference packages unavailable")
} else {
  message("Test outlier dates, manual classification and zero-row results")
  for (method in c("x13", "tramoseats")) {
    construct <- if (method == "x13") perX13 else perTramo
    for (frq in c(12, 4)) {
      series <- if (frq == 12) AirPassengers else stats::aggregate(AirPassengers, nfrequency = 4)
      empty <- construct(series, "rsa0", backend = "rjd3")
      expect_error(empty$getOutliers(), "No results")
      expect_true(is.na(empty$diagnosticsTable()$nOutliers))
      empty$run()
      no_out <- empty$getOutliers()
      expect_inherits(no_out, "persephoneOutliers")
      expect_identical(nrow(no_out), 0L)
      expect_inherits(no_out$date, "Date")
      expect_identical(no_out$autoout, logical())
      expect_identical(no_out$coef.value, double())
      counts <- empty$diagnosticsTable()[, c("nOutliers", "nAutoout", "nRecentout")]
      expect_equal(unname(unlist(counts)), c(0, 0, 0))
      printed_counts <- persephone3:::printDiagnostics(empty)[, c("nOutliers", "nAutoout", "nRecentout")]
      expect_equal(unname(unlist(printed_counts)), c(0, 0, 0))
      expect_identical(persephone3:::gettsout(empty), list(NULL, list()))
      expect_inherits(plot(empty, showOutliers = TRUE), "dygraphs")

      manual <- construct(series, "rsa3", backend = "rjd3",
                          td.option = "TradingDays",
                          addout.type = c("AO", "LS", "TC"),
                          addout.date = c("1953-07-01", "1956-01-01", "1960-10-01"))
      manual$run()
      expect_silent(out <- manual$getOutliers())
      expect_true(all(!out$autoout[seq_len(3)]))
      expect_identical(out$type[seq_len(3)], c("AO", "LS", "TC"))
      expect_identical(out$date[seq_len(3)], as.Date(c("1953-07-01", "1956-01-01", "1960-10-01")))
      labels <- if (frq == 12) c("AO1953.7", "LS1956.1", "TC1960.10") else c("AO1953.3", "LS1956.1", "TC1960.4")
      expect_identical(out$name[seq_len(3)], labels)
      expect_identical(out$recentout[seq_len(3)], c(FALSE, FALSE, TRUE))
      if (nrow(out) > 3L) expect_true(all(out$autoout[-seq_len(3)]))

      if (frq == 4) {
        fixed_quarter <- construct(series, "rsa3", backend = "rjd3", td.option = "TradingDays",
                                   addout.type = c("AO", "LS"), addout.date = c("1953-07-01", "1956-01-01"),
                                   addout.coef = c(0.04, 0))
        fixed_quarter$run()
        quarter_out <- fixed_quarter$getOutliers()
        expect_identical(quarter_out$coef.type[1], "FIXED")
        expect_true(all(is.na(quarter_out[1, c("stde", "t", "pvalue")])))
        expect_true(all(!quarter_out$autoout[1:2]))
        quarter_expected <- summary(fixed_quarter$output$preprocessing)$xregs
        expect_equal(fixed_quarter$getRegressors()$stde, quarter_expected$`Std. Error`)
      }

      # The backend's summary supplies an independent extraction path for
      # coefficient ordering, standard errors and p-values after six TD terms.
      expected <- summary(manual$output$preprocessing)$xregs
      regressors <- manual$getRegressors()
      expect_equal(regressors$value, expected$Estimate)
      expect_equal(regressors$stde, expected$`Std. Error`)
      expect_equal(regressors$t, expected$`T-stat`)
      expect_equal(regressors$pvalue, expected$`Pr(>|t|)`)
      expect_equal(out$stde, regressors$stde[regressors$type == "outlier"])
      expect_equal(out$pvalue, regressors$pvalue[regressors$type == "outlier"])
      expect_inherits(plot(manual, showOutliers = TRUE), "dygraphs")

      # Test old Date/numeric and current ISO positions on the same fitted
      # output without requiring old rjd3 packages or altering the fit.
      view <- list(output = manual$output, ts = series, params = manual$params)
      key <- if (method == "x13") "regarima" else "tramo"
      for (convert in list(identity, as.Date, function(x) as.numeric(as.Date(x)),
                           function(x) as.POSIXct(x, tz = "UTC"))) {
        legacy <- view
        legacy$params[[key]]$regression$outliers <- lapply(
          legacy$params[[key]]$regression$outliers,
          function(record) { record$pos <- convert(record$pos); record })
        expect_silent(legacy_out <- persephone3:::getOutliers(legacy))
        expect_equal(legacy_out, out)
      }
      # An aggregate's nested params list must never be used as its own spec.
      legacy_aggregate <- structure(list(output = manual$output, ts = series,
                                         params = list(child = manual$params), paramsDirect = manual$params),
                                    class = "hierarchicalTimeSeries")
      expect_equal(persephone3:::getOutliers(legacy_aggregate), out)
      before <- manual$getOutliers()
      manual$updateParams(remout.type = "AO", remout.date = "1953-07-01")
      expect_equal(manual$getOutliers(), before) # classification belongs to the fitted spec
    }

    message("Test fixed and estimated coefficient covariance alignment")
    fixed <- construct(AirPassengers, "rsa3", backend = "rjd3", td.option = "TradingDays",
                       addout.type = c("AO", "LS"),
                       addout.date = c("1953-07-01", "1956-01-01"),
                       addout.coef = c(0.04, 0))
    fixed$run()
    out <- fixed$getOutliers()
    expect_identical(out$coef.type[1], "FIXED")
    expect_identical(out$coef.value[1], 0.04)
    expect_true(all(is.na(out[1, c("stde", "t", "pvalue")])))
    expect_true(all(!out$autoout[1:2]))
    expected <- summary(fixed$output$preprocessing)$xregs
    regressors <- fixed$getRegressors()
    expect_equal(regressors$stde, expected$`Std. Error`)
    expect_equal(regressors$pvalue, expected$`Pr(>|t|)`)
    malformed <- list(output = fixed$output, params = fixed$params, ts = fixed$ts)
    malformed$output$preprocessing$estimation$bvar <- matrix(1, 1, 1)
    expect_warning(unknown <- persephone3:::getOutliers(malformed), "bvar does not match")
    expect_true(all(is.na(unknown$stde)))

    custom <- construct(AirPassengers, "rsa0", backend = "rjd3", addout.type = "AO",
                        addout.date = "1953-07-01", addout.name = "industrial action")
    custom$run()
    expect_identical(custom$getOutliers()$name, "AO1953.7")
    expect_false(custom$getOutliers()$autoout)
    custom_regressors <- custom$getRegressors()
    expect_identical(custom_regressors$name[custom_regressors$type == "outlier"], "AO1953.7")

    hierarchy <- perHts(a = AirPassengers, b = AirPassengers / 2, method = method,
                        backend = "rjd3", iterate = FALSE, addout.type = "AO",
                        addout.date = "1953-07-01")
    hierarchy$run()
    aggregate_out <- hierarchy$getOutliers()
    expect_false(aggregate_out$autoout[aggregate_out$name == "AO1953.7"])

    # A manual effect outside the estimation span is fixed, not a recent
    # detection, even when its date lies in the last observations of the input.
    restricted <- construct(AirPassengers, "rsa3", backend = "rjd3",
                            estimate.type = "Between", estimate.d0 = "1952-01-01",
                            estimate.d1 = "1958-01-01", addout.type = c("AO", "LS"),
                            addout.date = c("1948-01-01", "1960-12-01"))
    restricted$run()
    out <- restricted$getOutliers()
    expect_true(all(!out$recentout[out$name %in% c("AO1948.1", "LS1960.12")]))
    points <- persephone3:::gettsout(restricted)
    expect_false(as.Date("1948-01-01") %in% as.Date(unlist(points[[2]]), origin = "1970-01-01"))
    expect_inherits(plot(restricted, showOutliers = TRUE), "dygraphs")
  }
}
