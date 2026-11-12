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

    rownames(outliers) <- NULL
    outliers <- outliers[, c(
      "name",
      "type",
      "coef.value",
      "stde",
      "t",
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
    xregs
  } else {
    NULL
  }
}

#' Print method for the outlier table of a Persephone object
#'
#' Prints the data frame returned by [getOutliers()] with left aligned
#' character columns; the standard print method of [print.data.frame()]
#' right aligns them.
#'
#' @param x A data frame of class `persephoneOutliers`, as returned by
#'   [getOutliers()]
#' @param ... further arguments passed to [print.data.frame()]
#' @return `x`, invisibly
#'
#' @export
print.persephoneOutliers <- function(x, ...) {
  # drop only our own class, not "data.frame": row.names() does not work on a
  # plain list, which unclass(x) would produce
  df <- x
  class(df) <- "data.frame"
  print(df, right = FALSE, ...)
  invisible(x)
}

#' Print method for the regression table of a Persephone object
#'
#' Prints the data frame returned by [getRegressors()] with left aligned
#' character columns; the standard print method of [print.data.frame()]
#' right aligns them.
#'
#' @param x A data frame of class `persephoneRegressors`, as returned by
#'   [getRegressors()]
#' @param ... further arguments passed to [print.data.frame()]
#' @return `x`, invisibly
#'
#' @export
print.persephoneRegressors <- function(x, ...) {
  # drop only our own class, not "data.frame": row.names() does not work on a
  # plain list, which unclass(x) would produce
  df <- x
  class(df) <- "data.frame"
  print(df, right = FALSE, ...)
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

#' Run multiple diagnostic extraction functions on a Persephone object
#'
#' This convenience function allows running several diagnostic extraction
#' functions at once and printing their results. It is primarily used for
#' interactive exploration and debugging.
#'
#' @param x A Persephone object (e.g., `x13Single` or `tramoseatsSingle`)
#' @param ... Additional functions to run (passed as character strings)
#' @param funcs Character vector of function names to run (default: `c("getOutliers", "getRegressors")`)
#' @return Invisible list of results from each function
#'
#' @note This is an internal function for interactive diagnostics.
#' @keywords internal
#'
#' @examples
#' # obj <- perX13(AirPassengers, "rsa3")
#' # obj$run()
#' # getDiagnostics(obj)
#' # getDiagnostics(obj, funcs = c("getOutliers", "getRegressors", "gettsout"))
getDiagnostics <- function(x, ..., funcs = c("getOutliers", "getRegressors")) {
  # Combine default functions with additional ones
  all_funcs <- c(funcs, c(...))

  results <- list()

  for (f in all_funcs) {
    cat("\n=== ", toupper(f), " ===\n")
    results[[f]] <- get(f)(x)
    print(results[[f]])
  }

  invisible(results)
}
