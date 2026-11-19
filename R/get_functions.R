#' Extract outlier information from a Persephone object
#'
#' This function retrieves outlier details (AO, LS, TC) from a fitted Persephone
#' model, including coefficients, standard errors, t-statistics, and p-values.
#' It processes the raw output from X-13ARIMA-SEATS or TRAMO/SEATS into a clean
#' data frame with formatted dates and names.
#'
#' @param x A Persephone object (e.g., `x13Single` or `tramoseatsSingle`) with
#'   completed model run
#' @return A data frame with columns:
#'   \item{name}{Formatted outlier name (e.g., "AO1953.7" for additive outlier)}
#'   \item{type}{Outlier type: AO (Additive Outlier), LS (Level Shift), or TC (Transient Change)}
#'   \item{coef.value}{Estimated coefficient value}
#'   \item{stde}{Standard error of the coefficient (`NA` if `coef.type` is
#'     "FIXED")}
#'   \item{t}{T-statistic, `coef.value / stde`}
#'   \item{crit.value}{Critical value of the outlier test applied by
#'     JDemetra+: the value from the `outlier` specification if positive
#'     (x13: per outlier type, falling back to `defva`), otherwise the
#'     threshold computed internally (see Details)}
#'   \item{pvalue}{Two-sided p-value of `t` under a t distribution with
#'     `neffectiveobs - nparams` degrees of freedom}
#'   \item{date}{Date of the outlier as Date object}
#'   \item{coef.type}{Type of coefficient (usually "ESTIMATED")}
#'   \item{autoout}{Logical, TRUE for outliers detected automatically by
#'     X-13ARIMA-SEATS or TRAMO/SEATS, FALSE for outliers that were specified
#'     manually with the `outliers` argument}
#'   \item{recentout}{Logical, TRUE if the outlier falls into one of the last
#'     observations of the series: the last 5 observations for monthly series,
#'     the last 3 for quarterly series, the last 1 for all other frequencies
#'     (e.g., annual). Outliers before the beginning of the series or before
#'     the estimation span are never flagged.}
#'
#' @details
#' X-13ARIMA-SEATS and TRAMO/SEATS do not directly report standard errors,
#' t-statistics, or p-values for outlier effects. These quantities are computed
#' here from the estimated coefficients and the covariance matrix (`bvar`),
#' following the same methodology as \pkg{rjd3toolkit}. Results can therefore
#' be verified against `summary(x$output$preprocessing)$xregs`.
#'
#' Because outliers are selected only when their absolute t-statistic exceeds
#' the critical value defined in the `outlier` specification, the reported
#' p-values are subject to selection effects. They should be viewed as
#' descriptive statistics for the fitted model rather than as formal evidence
#' of adjustment quality.
#'
#' The output of a run does not distinguish manually specified from
#' automatically detected outliers. `autoout` therefore compares the
#' `type`/`date` pairs of the run output with the manually specified outliers
#' read from the model specification (`regarima$regression$outliers` for x13,
#' `tramo$regression$outliers` for tramoseats). `recentout` is evaluated
#' independently of `autoout`, so a manually specified outlier close to the
#' end of the series is flagged as well.
#'
#' If the `outlier` specification defines no critical value, JDemetra+
#' computes one internally from the number of observations `n` of the
#' estimation domain: x13 uses `max(advancedComputer(0.05)(n), 2.8)`, an
#' extreme-value approximation for the 95% quantile of the maximum of `n`
#' standard normal variables; tramoseats uses `max(simpleComputer()(n), 2)`,
#' i.e. 3.3 up to 50 observations, rising by 0.0025 per observation up to
#' 4.3. Both formulas are reproduced in [calcOutlierCrit()] and verified
#' against the jdplus implementation. Note that during automatic model
#' identification the engines may lower this value once (by about 14% for
#' x13 and 12% for tramoseats); that reduction is not reported in the run
#' output and is therefore not reflected in `crit.value`.
#'
#' @note This is an internal function used by other get* functions and diagnostics.
#' @keywords internal
#'
#' @examples
#' # obj <- perX13(AirPassengers, "rsa3", outliers = "AO1960.5")
#' # obj$run()
#' # getOutliers(obj)                  # all outliers
#' # subset(getOutliers(obj), autoout) # without the manually specified one
getOutliers <- function(x) {
  if (is.null(x$output)) {
    stop("No results from run available.\n")
  }
  stopifnot(inherits(x$output, c("JD3_X13_RSLTS", "JD3_TRAMOSEATS_RSLTS")))

  variables <- x$output$preprocessing$description$variables
  vartype <- sapply(variables, function(v) v$type)
  otlind <- which(vartype %in% c("AO", "TC", "LS"))
  outliers <- variables[otlind]

  if (length(outliers) > 0) {
    outliers <- as.data.frame(do.call(
      rbind,
      lapply(outliers, function(x) unlist(x))
    ))
    # outliers$origname <- outliers$name
    outliers$coef.value <- as.numeric(outliers$coef.value)
    outliers$date <- substr(outliers$name, 5, 14)
    newname <- lapply(strsplit(substr(outliers$date, 1, 7), "-"), as.numeric)
    outliers$date <- as.Date(outliers$date, origin = "1970-01-01")

    if (frequency(x$ts) == 12) {
      outliers$name <- lapply(newname, function(nn) paste0(nn, collapse = "."))
    } else {
      newname <- lapply(newname, function(nn) {
        c(nn[1], lubridate::quarter(nn[2]))
      })
      outliers$name <- lapply(newname, function(nn2) {
        paste0(nn2, collapse = ".")
      })
    }
    outliers$name <- paste0(outliers$type, outliers$name)

    # bvar only covers the ESTIMATED coefficients, in the order of all
    # regression variables. FIXED ones (e.g. manually specified outliers
    # outside the estimation range) get no standard error. Same mapping as
    # in getRegressors(). One regression variable can carry several
    # coefficients (trading days: one per weekday), so everything below is
    # indexed per coefficient, not per variable.
    coef_types <- lapply(
      variables,
      function(v) {
        n <- NROW(v$coef)
        if (n == 0) {
          character(0)
        } else if (is.null(v$coef$type)) {
          rep(NA_character_, n)
        } else {
          as.character(v$coef$type)
        }
      }
    )
    coef_n <- vapply(coef_types, length, integer(1L))
    coef_type <- unlist(coef_types, use.names = FALSE)
    stde_all <- rep(NA_real_, length(coef_type))
    bvar <- x$output$preprocessing$estimation$bvar
    if (length(bvar) > 0) {
      sel <- coef_type == "ESTIMATED"
      if (nrow(bvar) == sum(sel)) {
        stde_all[sel] <- sqrt(diag(bvar))
      } else {
        warning(
          "bvar does not match the number of estimated regression ",
          "coefficients. Standard errors are unreliable."
        )
      }
    }
    # outliers always carry exactly one coefficient, so the index of an
    # outlier in the flattened vector is the cumulated number of coefficients
    # up to its variable
    stde <- stde_all[cumsum(coef_n)[otlind]]
    t <- outliers$coef.value / stde
    ndf <- x$output$preprocessing$estimation$likelihood$neffectiveobs -
      x$output$preprocessing$estimation$likelihood$nparams
    pval <- 2 * pt(abs(t), ndf, lower.tail = FALSE)
    outliers$stde <- stde
    outliers$t <- t
    outliers$pvalue <- pval

    # The output of the run does not distinguish manually specified from
    # automatically detected outliers; only the specification knows the manual
    # ones (regarima$regression$outliers for x13, tramo$regression$outliers
    # for tramoseats). Matching works on the type/date pair.
    spec <- unclass(x$params)
    spec <- if (!is.null(spec$regarima)) spec$regarima else spec$tramo
    spec_outliers <- spec$regression$outliers
    manual <- if (length(spec_outliers) == 0) {
      NULL
    } else {
      data.frame(
        type = vapply(spec_outliers, function(o) as.character(o$code), ""),
        date = as.Date(
          vapply(spec_outliers, function(o) as.numeric(o$pos), numeric(1)),
          origin = "1970-01-01"
        )
      )
    }
    outliers$autoout <- if (is.null(manual)) {
      TRUE
    } else {
      !(paste(outliers$type, outliers$date) %in% paste(manual$type, manual$date))
    }

    # recentout: outliers falling into the last 5 (monthly) or 3 (quarterly)
    # observations of the series, regardless of how they entered the model
    frq <- frequency(x$ts)
    k <- if (frq == 12) 5 else if (frq == 4) 3 else 1
    per <- if (frq == 12) {
      lubridate::year(outliers$date) +
        (lubridate::month(outliers$date) - 1) / 12
    } else {
      lubridate::year(outliers$date) +
        (lubridate::quarter(outliers$date) - 1) / 4
    }
    lag_periods <- round((as.numeric(tail(time(x$ts), 1)) - per) * frq)
    outliers$recentout <- lag_periods >= 0 & lag_periods <= k - 1

    # crit.value: the threshold JDemetra+ used to decide whether an effect
    # enters the model. A positive value in the outlier specification wins; if
    # there is none, the engine computed the threshold from the number of
    # observations of the estimation domain (see calcOutlierCrit).
    is_x13 <- inherits(x$output, "JD3_X13_RSLTS")
    otl_spec <- spec$outlier
    auto_cv <- if (is_x13) {
      calcOutlierCrit(NROW(x$output$preprocessing$estimation$y), "x13")
    } else {
      calcOutlierCrit(sum(!is.na(x$output$preprocessing$estimation$y)),
                      "tramoseats")
    }
    # a missing or empty spec entry gives numeric(0) via as.numeric(), which
    # vapply cannot coerce to length 1 -- treat it as 0 and fall back to the
    # threshold computed by calcOutlierCrit.
    as_crit <- function(v) {
      v <- suppressWarnings(as.numeric(v))
      if (length(v) != 1L || is.na(v)) 0 else v
    }
    spec_cv <- vapply(outliers$type, function(tp) {
      if (!is_x13) {
        return(as_crit(otl_spec$va))
      }
      type_o <- Filter(function(o) identical(as.character(o$type), tp),
                       otl_spec$outliers)
      va <- if (length(type_o)) as_crit(type_o[[1]]$va) else 0
      if (va > 0) va else as_crit(otl_spec$defva)
    }, numeric(1))
    outliers$crit.value <- ifelse(spec_cv > 0, spec_cv, auto_cv)

    rownames(outliers) <- NULL
    outliers <- outliers[, c(
      "name",
      "type",
      "coef.value",
      "stde",
      "t",
      "crit.value",
      "pvalue",
      "date",
      "coef.type",
      "autoout",
      "recentout"
    )]
    class(outliers) <- c("persephoneOutliers", "data.frame")
  } else {
    outliers <- NA
  }
  outliers
}

#' Critical value computed internally by JDemetra+ for outlier detection
#'
#' Transcribes the thresholds the engines apply when the `outlier`
#' specification leaves the critical value unset: x13 uses
#' `max(advancedComputer(0.05)(n), 2.8)`, an extreme-value approximation for
#' the 95% quantile of the maximum of `n` standard normal variables;
#' tramoseats uses `max(simpleComputer()(n), 2)`, i.e. 3.3 up to 50
#' observations, rising by 0.0025 per observation up to 4.3. Verified
#' against the jdplus implementation (`X13Utility.calcCv`,
#' `TramoUtility.calcCv`) via `rJava`.
#'
#' @param n Number of observations of the estimation domain
#' @param method Either `"x13"` or `"tramoseats"`
#' @return The critical value.
#' @keywords internal
calcOutlierCrit <- function(n, method = c("x13", "tramoseats")) {
  method <- match.arg(method)
  if (method == "tramoseats") {
    return(max(min(3.3 + 0.0025 * max(n - 50, 0), 4.3), 2))
  }
  threshold <- 0.05
  if (n <= 1) return(max(qnorm(threshold / 2, lower.tail = FALSE), 2.8))
  calc_val <- function(nvals, eps) {
    a <- sqrt(2 * log(nvals))
    b <- a - (log(log(nvals)) + log(4 * pi)) / (2 * a)
    -log(-0.5 * log(2 - sqrt(1 + eps))) / a + b
  }
  ncal <- c(2, 100, 200)
  a_cal <- sqrt(2 * log(ncal))
  b_cal <- (log(log(ncal)) + log(4 * pi)) / (2 * a_cal)
  y <- c(qnorm((1 + sqrt(1 - threshold)) / 2),
         calc_val(100, threshold), calc_val(200, threshold))
  coefs <- solve(cbind(1, b_cal, a_cal), y)
  a <- sqrt(2 * log(n))
  b <- (log(log(n)) + log(4 * pi)) / (2 * a)
  max(coefs[1] + coefs[2] * b + coefs[3] * a, 2.8)
}

#' Extract regression coefficients from a Persephone object
#'
#' This function retrieves the coefficients of all regression variables of a
#' fitted Persephone model: outliers, trading days, Easter, leap year, mean,
#' user defined variables and any other regressor present in the model. It
#' calculates standard errors, t-statistics, and p-values for estimated
#' coefficients.
#'
#' @param x A Persephone object (e.g., `x13Single` or `tramoseatsSingle`) with
#'   completed model run
#' @return A data frame with columns:
#'   \item{name}{Variable name (e.g., "AO1953.7", "td1", "easter", "leap")}
#'   \item{type}{Kind of regressor, as reported by the model (e.g. "TD",
#'     "EASTER", "LP", "MEAN", "USER"); all outlier types (AO, LS, TC) are
#'     summarized as "outlier"}
#'   \item{value}{Coefficient estimate}
#'   \item{stde}{Standard error (NA for non-estimated types)}
#'   \item{t}{T-statistic for estimated coefficients}
#'   \item{pvalue}{Two-sided p-value for estimated coefficients}
#'
#'   The result additionally carries the joint F test of the trading day
#'   regressors (when the model has any) as the attribute `ftests`
#'   (one-row data frame from the internal `tdJointFTest()` helper).
#'
#' @details
#' One regression variable can carry several coefficients (trading days: one
#' per weekday), so the table has one row per coefficient. Standard errors come
#' from the covariance matrix (`bvar`), which only covers the estimated
#' coefficients; fixed ones (e.g. manually specified outliers outside the
#' estimation range) get `NA`. Outliers are recognized by the type of their
#' regression variable and not by their name, so that no other regressor can be
#' mistaken for one.
#'
#' @note This is an internal function used by diagnostics and reporting.
#' @keywords internal
#'
#' @examples
#' # obj <- perX13(AirPassengers, "rsa3")
#' # obj$run()
#' # getRegressors(obj)
getRegressors <- function(x) {
  if (is.null(x$output)) {
    stop("No results from run available.\n")
  }
  stopifnot(inherits(x$output, c("JD3_X13_RSLTS", "JD3_TRAMOSEATS_RSLTS")))
  q <- x$output$preprocessing
  variables <- q$description$variables

  if (length(variables) > 0) {
    regs <- do.call(
      "rbind",
      lapply(variables, function(z) {
        z$coef
      })
    )
    xregs <- cbind(
      name = unlist(rownames(regs)),
      regs,
      stde = NA,
      t = NA,
      pvalue = NA
    )
    rownames(xregs) <- NULL
    # regs$type is the type of the *coefficient* (ESTIMATED/FIXED) and is kept
    # apart because the returned table uses `type` for the kind of regressor
    coef_type <- as.character(xregs$type)
    xregs$type <- NULL
    # same mapping as in getOutliers(): bvar covers the ESTIMATED coefficients
    # only, in the order of all regression variables and of the coefficients
    # within a variable
    stde <- rep(NA_real_, nrow(xregs))
    sel <- coef_type == "ESTIMATED"
    bvar <- q$estimation$bvar
    if (length(bvar) > 0) {
      if (nrow(bvar) == sum(sel)) {
        stde[sel] <- sqrt(diag(bvar))
      } else {
        warning(
          "bvar does not match the number of estimated regression ",
          "coefficients. Standard errors are unreliable."
        )
      }
    }
    t <- xregs$value[sel] / stde[sel]
    ndf <- q$estimation$likelihood$neffectiveobs -
      q$estimation$likelihood$nparams
    pval <- 2 * pt(abs(t), ndf, lower.tail = FALSE)
    xregs$stde[sel] <- stde[sel]
    xregs$t[sel] <- t
    xregs$pvalue[sel] <- pval

    # outliers are recognized by the type of their regression variable, not by
    # their name; all other regressors keep the name they are reported with
    vartype <- unlist(lapply(variables, function(z) as.character(z$type)))
    coef_n <- vapply(variables, function(z) NROW(z$coef), integer(1L))
    # the type of a variable repeats for each of its coefficients
    # (trading days: mon ... sat)
    regtype <- rep(vartype, times = coef_n)
    regtype[regtype %in% c("AO", "LS", "TC")] <- "outlier"
    otlvar <- which(vartype %in% c("AO", "LS", "TC"))
    # outliers always carry exactly one coefficient, so their row in the
    # flattened table is the cumulated number of coefficients up to them
    otlind <- cumsum(coef_n)[otlvar]
    otltype <- vartype[otlvar]
    date <- substr(xregs$name[otlind], 5, 14)
    newname <- lapply(strsplit(substr(date, 1, 7), "-"), as.numeric)
    if (frequency(x$ts) == 12) {
      xregs$name[otlind] <- vapply(newname, function(nn) {
        paste0(nn, collapse = ".")
      }, "")
    } else {
      newname <- lapply(newname, function(nn) {
        c(nn[1], ceiling(as.numeric(nn[2]) / 3))
      })
      xregs$name[otlind] <- vapply(newname, function(nn2) {
        paste0(nn2, collapse = ".")
      }, "")
    }
    xregs$name[otlind] <- paste0(otltype, xregs$name[otlind])

    xregs$type <- regtype
    xregs <- xregs[, c("name", "type", "value", "stde", "t", "pvalue")]
    class(xregs) <- c("persephoneRegressors", "data.frame")
    attr(xregs, "ftests") <- tryCatch(tdJointFTest(x), error = function(e) NULL)
    xregs
  } else {
    NULL
  }
}

#' Format a p-value as a plain-decimal string with a lower bound
#'
#' Shows plain decimals (`0.005`) instead of scientific notation and replaces
#' values below the display threshold by `"< 0.001"`. Only used for printing;
#' the data frame keeps the exact numeric value.
#' @param p numeric vector of p-values
#' @param digits number of decimal places (also sets the `"< ..."` bound)
#' @return character vector
#' @keywords internal
fmt.pvalue <- function(p, digits = 3) {
  s <- formatC(p, format = "f", digits = digits)
  small <- !is.na(p) & p < 10^(-digits)
  s[small] <- paste0("< ", formatC(10^(-digits), format = "f", digits = digits))
  trimws(s)
}

#' Significance stars as in printCoefmat()/summary()
#' @param p numeric vector of p-values
#' @return character vector of `"***"`, `"**"`, `"*"`, `"."` or `""`
#' @keywords internal
fmt.sig <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "***",
    ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", "")))))
}

#' Joint F test of the trading day regressors
#'
#' Tests H0: all trading day coefficients of the regression are zero. Uses the
#' test reported by JDemetra+ (`regression.td-ftest`), which also backs the
#' `TD_Ftest` column of [diagnosticsTable()], so that both views report the
#' same numbers. If the node is missing or incomplete (e.g. the trading day
#' pre-test `td.test = "Remove"` dropped the regressors during the run), the
#' test is computed as a Wald test, `F = (b' V^-1 b) / q` against `F(q, df)`,
#' from the estimated coefficient vector `b` and the covariance matrix `bvar`.
#'
#' @param x A Persephone object with completed model run
#' @return A one-row data frame with the columns `group`, `value`, `df1`,
#'   `df2` and `pvalue`, or `NULL` when the model contains no trading day
#'   regressors or the test cannot be computed.
#' @keywords internal
tdJointFTest <- function(x) {
  pp <- x$output$preprocessing
  variables <- pp$description$variables
  n_est <- vapply(variables, function(v) {
    if (is.null(v$coef$type)) {
      0L
    } else {
      sum(as.character(v$coef$type) == "ESTIMATED")
    }
  }, integer(1))
  is_td <- vapply(variables, function(v) as.character(v$type) == "TD", TRUE)
  q <- sum(n_est[is_td])
  if (q == 0L) {
    return(NULL)
  }
  df <- as.integer(pp$estimation$likelihood$neffectiveobs -
                     pp$estimation$likelihood$nparams)

  # the test reported by JDemetra+ in preference: identical numbers to
  # diagnosticsTable(); $ (not [[ ) so that a missing node yields NULL
  # instead of an error and the fallback below can take over
  node <- x$output$user_defined$`regression.td-ftest`
  fstat <- suppressWarnings(as.numeric(node$value[1]))
  pval <- suppressWarnings(as.numeric(node$pvalue[1]))
  if (length(fstat) != 1L || length(pval) != 1L || is.na(fstat) || is.na(pval)) {
    # fallback: Wald test of all trading day coefficients from the estimation
    b <- pp$estimation$b
    bvar <- pp$estimation$bvar
    if (length(b) == 0 || length(bvar) == 0 || nrow(bvar) != sum(n_est)) {
      return(NULL)
    }
    first <- cumsum(c(0L, head(n_est, -1))) + 1L
    idx <- unlist(lapply(which(is_td & n_est > 0), function(i) {
      first[i] + seq_len(n_est[i]) - 1L
    }))
    V <- bvar[idx, idx, drop = FALSE]
    Vinv <- tryCatch(solve(V), error = function(e) {
      # nearly singular covariance matrix: use the pseudo inverse
      ev <- eigen(V, symmetric = TRUE)
      tol <- max(dim(V)) * max(ev$values) * .Machine$double.eps
      ev$vectors %*% diag(ifelse(ev$values > tol, 1 / ev$values, 0),
                         nrow = length(ev$values)) %*% t(ev$vectors)
    })
    fstat <- drop(crossprod(b[idx], Vinv %*% b[idx])) / q
    pval <- stats::pf(fstat, q, df, lower.tail = FALSE)
  }
  data.frame(
    group = "Trading days", value = fstat, df1 = q, df2 = df,
    pvalue = pval, stringsAsFactors = FALSE
  )
}

#' Print the joint F tests computed by [tdJointFTest()]
#'
#' Formats the one-row data frame returned by [tdJointFTest()] as one line
#' per test, `group: F(df1, df2) = value P ...`, with the p-value in the same
#' format as the coefficient tables. Used by the print method of
#' [getRegressors()] and by [summary.persephone()].
#'
#' @param ft one-row data frame from [tdJointFTest()]
#' @param p_digits number of decimal places for the p-value
#' @return `ft`, invisibly
#' @keywords internal
printFTests <- function(ft, p_digits = 3) {
  for (i in seq_len(nrow(ft))) {
    p_str <- fmt.pvalue(ft$pvalue[i], p_digits)
    p_str <- if (startsWith(p_str, "<")) {
      paste0("P ", p_str)
    } else {
      paste0("P = ", p_str)
    }
    cat(sprintf("  %-15s F(%d, %d) = %.3f  %s\n",
                paste0(ft$group[i], ":"), ft$df1[i], ft$df2[i],
                ft$value[i], p_str))
  }
  invisible(ft)
}

#' Print an aligned table of pre-formatted display strings
#'
#' Prints a data frame of already formatted strings with one common width per
#' column across all rows: the columns named in `right_cols` are right
#' aligned, the others left aligned; headers are left aligned. Unlike
#' [print.data.frame()], which pads and trims the columns itself, this keeps
#' the alignment of the numeric columns (the decimal points line up). Tables
#' wider than `width` are split into blocks of columns that are printed one
#' below the other, as [print.data.frame()] does.
#'
#' @param df data frame of display strings
#' @param right_cols character vector of column names to right align
#' @param width maximal line width; defaults to `getOption("width")`
#' @return `df`, invisibly
#' @keywords internal
printAlignedTable <- function(df, right_cols = character(0),
                              width = getOption("width")) {
  if (length(df) == 0) {
    return(invisible(df))
  }
  widths <- vapply(seq_along(df), function(j) {
    max(c(nchar(names(df)[j]),
          nchar(trimws(as.character(df[[j]])))))
  }, integer(1))
  cells <- lapply(seq_along(df), function(j) {
    nm <- names(df)[j]
    s <- trimws(as.character(df[[j]]))
    pad <- strrep(" ", widths[[j]] - nchar(s))
    body <- if (nm %in% right_cols) paste0(pad, s) else paste0(s, pad)
    c(paste0(nm, strrep(" ", widths[[j]] - nchar(nm))), body)
  })
  # split the columns into blocks that fit into width (greedily, from the
  # left), like print.data.frame() does for wide tables
  labels <- format(seq_len(max(nrow(df), 1L)), justify = "right")
  rn <- max(nchar(labels))
  budget <- max(as.integer(width) - rn - 2L, 1L)
  block_end <- integer(0)
  used <- widths[[1L]]
  if (length(widths) > 1L) {
    for (j in seq_along(widths)[-1L]) {
      if (used + 2L + widths[[j]] > budget) {
        block_end <- c(block_end, j - 1L)
        used <- widths[[j]]
      } else {
        used <- used + 2L + widths[[j]]
      }
    }
  }
  block_end <- c(block_end, length(widths))
  mat <- do.call(cbind, cells)
  lines <- unlist(lapply(seq_along(block_end), function(b) {
    cols <- if (b == 1L) {
      seq_len(block_end[[b]])
    } else {
      (block_end[[b - 1L]] + 1L):block_end[[b]]
    }
    lab <- c(strrep(" ", rn), labels)
    paste0(lab, "  ",
           apply(mat[, cols, drop = FALSE], 1, paste, collapse = "  "))
  }))
  cat(sub(" +$", "", lines), sep = "\n")
  invisible(df)
}

#' Print method for the outlier table of a Persephone object
#'
#' Prints the data frame returned by [getOutliers()] as an aligned table:
#' text columns left aligned, numbers right aligned with fixed decimals so
#' that the decimal points line up. The standard print method of
#' [print.data.frame()] would left align the numbers and show them in full
#' precision.
#'
#' @param x A data frame of class `persephoneOutliers`, as returned by
#'   [getOutliers()]
#' @param ... further arguments passed to the aligned table printer,
#'   e.g. `width` (defaults to `getOption("width")`)
#' @param p_digits number of decimal places for the p-value (also sets the
#'   `"< 0.001"` lower bound); defaults to 3
#' @return `x`, invisibly
#'
#' @export
print.persephoneOutliers <- function(x, p_digits = 3, ...) {
  # readable fixed-decimal numbers; crit.value and t get 2 decimals, the
  # (much smaller) coefficients 4; p-value as plain decimal with a "< 0.001"
  # floor. No significance stars here on purpose -- outlier significance is
  # the abs(t) > crit.value rule (see nSigRecentout), not a 5% t-test.
  df <- x
  if (is.numeric(df$coef.value)) df$coef.value <- formatC(df$coef.value, format = "f", digits = 4)
  if (is.numeric(df$stde))       df$stde       <- formatC(df$stde,       format = "f", digits = 4)
  if (is.numeric(df$t))          df$t          <- formatC(df$t,          format = "f", digits = 2)
  if (is.numeric(df$crit.value)) df$crit.value <- formatC(df$crit.value, format = "f", digits = 2)
  if (is.numeric(df$pvalue))     df$pvalue     <- fmt.pvalue(df$pvalue, p_digits)
  printAlignedTable(
    df,
    right_cols = c("coef.value", "stde", "t", "crit.value", "pvalue"),
    ...
  )
  invisible(x)
}

#' Print method for the regression table of a Persephone object
#'
#' Prints the data frame returned by [getRegressors()] as an aligned table:
#' text columns left aligned, numbers right aligned with fixed decimals so
#' that the decimal points line up, plus a column of significance stars and,
#' when the model has trading day regressors, their joint F test below the
#' table. The standard print method of [print.data.frame()] would left align
#' the numbers and show them in full precision.
#'
#' @param x A data frame of class `persephoneRegressors`, as returned by
#'   [getRegressors()]
#' @param ... further arguments passed to the aligned table printer,
#'   e.g. `width` (defaults to `getOption("width")`)
#' @param p_digits number of decimal places for the p-value (also sets the
#'   `"< 0.001"` lower bound); defaults to 3
#' @return `x`, invisibly
#'
#' @export
print.persephoneRegressors <- function(x, p_digits = 3, ...) {
  df <- x
  # readable numbers: fixed decimals per column (value/stde keep 4 decimals
  # so small calendar effects stay visible); p-value in plain decimals with a
  # "< 0.001" floor; add printCoefmat-style significance stars
  if (is.numeric(df$value)) df$value <- formatC(df$value, format = "f", digits = 4)
  if (is.numeric(df$stde))  df$stde  <- formatC(df$stde,  format = "f", digits = 4)
  if (is.numeric(df$t))     df$t     <- formatC(df$t,     format = "f", digits = 2)
  if (is.numeric(x$pvalue)) {
    # insert the significance stars right after pvalue, using the original
    # numeric p-values, then reformat the displayed p-value
    p <- match("pvalue", names(df))
    head_df <- df[, seq_len(p), drop = FALSE]
    tail_df <- if (p < ncol(df)) df[, -seq_len(p), drop = FALSE] else NULL
    df <- cbind(head_df, sig = fmt.sig(x$pvalue))
    if (!is.null(tail_df)) df <- cbind(df, tail_df)
    df$pvalue <- fmt.pvalue(x$pvalue, p_digits)
  }
  printAlignedTable(df, right_cols = c("value", "stde", "t", "pvalue"), ...)
  # joint F tests attached by getRegressors() (trading days only)
  ft <- attr(x, "ftests")
  if (!is.null(ft) && nrow(ft) > 0) {
    cat("\nJoint F tests\n")
    printFTests(ft, p_digits)
  }
  invisible(x)
}


#' Get formatted regression coefficient table for printing
#'
#' This function provides a human-readable, formatted printout of the
#' coefficients of all regression variables of a Persephone model using R's
#' standard coefficient matrix formatting. It is primarily used for console
#' output and reporting.
#'
#' @param x A Persephone object (e.g., `x13Single` or `tramoseatsSingle`)
#' @return Character vector of formatted coefficient table lines (invisible)
#'
#' @note This is an internal function for console output.
#' @keywords internal
#'
#' @examples
#' # obj <- perX13(AirPassengers, "rsa3")
#' # obj$run()
#' # getRegressors_formatted(obj)
getRegressors_formatted <- function(x) {
  xregs <- getRegressors(x)
  if (is.null(xregs)) {
    return(character(0))
  }
  xregs.print <- xregs[, c("value", "stde", "t", "pvalue")]
  rownames(xregs.print) <- xregs$name
  colnames(xregs.print) <- c("Estimate", "Std.Err", "T-stat", "Pr(>|t|)")
  # print the table once and return the formatted lines invisibly
  formatted_output <- capture.output(printCoefmat(xregs.print))
  cat(formatted_output, sep = "\n")
  cat("\n")
  invisible(formatted_output)

  # return(list(
  #   table = xregs.print,
  #   formatted = formatted_output
  # ))
}

########################################
###         from rdj3toolkit         ###
########################################
# Note: .regarima_coef_table.default is a legacy function from rdj3toolkit
# that was replaced by getRegressors(). Kept here for reference.

#' Extract outliers and create time series for plotting
#'
#' This function processes outlier information from a Persephone object and
#' creates time series objects that can be used for plotting outliers on graphs.
#' It is primarily used internally by plot functions to display outlier events
#' (AO, LS, TC) on time series visualizations.
#'
#' @param x A Persephone object (e.g., `x13Single` or `tramoseatsSingle`)
#' @return A list with two elements:
#'   \item{`[[1]]`}{A `ts` object with columns for each outlier type (otlAO, otlLS, otlTC).
#'                Values at outlier dates contain the original series values,
#'                all other dates are NA.}
#'   \item{`[[2]]`}{A named list of Date objects, one per outlier type (AO, LS, TC),
#'                containing the dates where outliers occurred. Used for dygraph
#'                event annotations.}
#'
#' @note This is an internal function; use it only if you know what you're doing.
#' @keywords internal
#'
#' @examples
#' # obj <- perX13(AirPassengers, "rsa3")
#' # obj$run()
#' # otl <- gettsout(obj)
#' # otl[[1]]  # time series with outlier values
#' # otl[[2]]  # dates for event annotations
gettsout <- function(x) {
  outliers <- getOutliers(x)
  if (frequency(x$ts) == 12) {
    outliers$mq <- lubridate::month(outliers$date)
    outliers$y <- lubridate::year(outliers$date)
  } else {
    outliers$mq <- lubridate::quarter(outliers$date)
    outliers$y <- lubridate::year(outliers$date)
  }
  #Date format für dyevent

  dateout <- list()
  for (i in 1:nrow(outliers)) {
    dateout[[length(dateout) + 1]] <- outliers[i, ]$date
    names(dateout)[length(dateout)] <- outliers[i, ]$type
  }

  #outliersAO <-outliers[outliers$name %in% "AO1953.7",c("y","mq")]

  outliersAO <- outliers[outliers$type %in% "AO", c("y", "mq")]
  outliersLS <- outliers[outliers$type %in% "LS", c("y", "mq")]
  outliersTC <- outliers[outliers$type %in% "TC", c("y", "mq")]

  tsout <- list()

  if (nrow(outliersAO) > 0) {
    otlAO <- ts(
      start = start(x$ts),
      end = end(x$ts),
      frequency = frequency(x$ts)
    )
    for (i in 1:nrow(outliersAO)) {
      wi <- as.numeric(outliersAO[i, ])
      window(otlAO, start = wi, end = wi) <- window(x$ts, start = wi, end = wi)
    }
    tsout[[length(tsout) + 1]] <- otlAO
    names(tsout)[length(tsout)] <- "otlAO"
  }
  if (nrow(outliersLS) > 0) {
    otlLS <- ts(
      start = start(x$ts),
      end = end(x$ts),
      frequency = frequency(x$ts)
    )
    for (i in 1:nrow(outliersLS)) {
      wi <- as.numeric(outliersLS[i, ])
      window(otlLS, start = wi, end = wi) <- window(x$ts, start = wi, end = wi)
    }
    tsout[[length(tsout) + 1]] <- otlLS
    names(tsout)[length(tsout)] <- "otlLS"
  }
  if (nrow(outliersTC) > 0) {
    otlTC <- ts(
      start = start(x$ts),
      end = end(x$ts),
      frequency = frequency(x$ts)
    )
    for (i in 1:nrow(outliersTC)) {
      wi <- as.numeric(outliersTC[i, ])
      window(otlTC, start = wi, end = wi) <- window(x$ts, start = wi, end = wi)
    }
    tsout[[length(tsout) + 1]] <- otlTC
    names(tsout)[length(tsout)] <- "otlTC"
  }

  tsout <- do.call(cbind, tsout)

  return(list(tsout, dateout))
}

