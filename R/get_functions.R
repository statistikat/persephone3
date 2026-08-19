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
#'   \item{stde}{Standard error of the coefficient}
#'   \item{t}{T-statistic}
#'   \item{pvalue}{Two-sided p-value}
#'   \item{date}{Date of the outlier as Date object}
#'   \item{coef.type}{Type of coefficient (usually "ESTIMATED")}
#'
#' @note This is an internal function used by other get* functions and diagnostics.
#'
#' @examples
#' # obj <- perX13(AirPassengers, "rsa3")
#' # obj$run()
#' # getOutliers(obj)
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

    stde <- sqrt(diag(x$output$preprocessing$estimation$bvar))[otlind]
    t <- outliers$coef.value / stde
    ndf <- x$output$preprocessing$estimation$likelihood$neffectiveobs -
      x$output$preprocessing$estimation$likelihood$nparams
    pval <- 2 * pt(abs(t), ndf, lower.tail = FALSE)
    outliers$stde <- stde
    outliers$t <- t
    outliers$pvalue <- pval

    outliers <- outliers[, c(
      "name",
      "type",
      "coef.value",
      "stde",
      "t",
      "pvalue",
      "date",
      "coef.type"
    )]
  } else {
    outliers <- NA
  }
  outliers
}

#' Extract ARIMA regression coefficients from a Persephone object
#'
#' This function retrieves all regression and ARIMA coefficients from a fitted
#' Persephone model, including outliers, trading day effects, Easter effects,
#' and other regressors. It calculates standard errors, t-statistics, and
#' p-values for estimated coefficients.
#'
#' @param x A Persephone object (e.g., `x13Single` or `tramoseatsSingle`) with
#'   completed model run
#' @return A data frame with columns:
#'   \item{name}{Variable name (e.g., "AO1953.7", "td", "easter")}
#'   \item{value}{Coefficient estimate}
#'   \item{stde}{Standard error (NA for non-estimated types)}
#'   \item{t}{T-statistic for estimated coefficients}
#'   \item{pvalue}{Two-sided p-value for estimated coefficients}
#'
#' @note This is an internal function used by diagnostics and reporting.
#'
#' @examples
#' # obj <- perX13(AirPassengers, "rsa3")
#' # obj$run()
#' # getArimaCoef(obj)
getArimaCoef <- function(x) {
  if (is.null(x$output)) {
    stop("No results from run available.\n")
  }
  stopifnot(inherits(x$output, c("JD3_X13_RSLTS", "JD3_TRAMOSEATS_RSLTS")))
  q <- x$output$preprocessing

  if (length(q$description$variables) > 0) {
    regs <- do.call(
      "rbind",
      lapply(q$description$variables, function(z) {
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
    stde <- sqrt(diag(q$estimation$bvar))
    sel <- xregs$type == "ESTIMATED"
    t <- xregs$value[sel] / stde
    ndf <- q$estimation$likelihood$neffectiveobs -
      q$estimation$likelihood$nparams
    pval <- 2 * pt(abs(t), ndf, lower.tail = FALSE)
    xregs$stde[sel] <- stde
    xregs$t[sel] <- t
    xregs$pvalue[sel] <- pval

    otlind <- which(grepl(c("AO|LS|TC"), xregs$name))
    otltype <- substr(xregs[otlind, ]$name, 1, 2)
    date <- substr(xregs[otlind, ]$name, 5, 14)
    newname <- lapply(strsplit(substr(date, 1, 7), "-"), as.numeric)
    if (frequency(x$ts) == 12) {
      xregs[otlind, ]$name <- lapply(newname, function(nn) {
        paste0(nn, collapse = ".")
      })
    } else {
      newname <- lapply(newname, function(nn) {
        c(nn[1], ceiling(as.numeric(nn[2]) / 3))
      })
      xregs[otlind, ]$name <- lapply(newname, function(nn2) {
        paste0(nn2, collapse = ".")
      })
    }
    xregs[otlind, ]$name <- paste0(otltype, xregs[otlind, ]$name)

    # colnames(xregs) <- c(
    #   "Estimate", "Type", "Std. Error",
    #   "T-stat", "Pr(>|t|)"
    # )
    xregs <- xregs[, c("name", "value", "stde", "t", "pvalue")]
    xregs
  } else {
    NULL
  }
}


#' Get formatted ARIMA coefficient table for printing
#'
#' This function provides a human-readable, formatted printout of ARIMA
#' coefficients using R's standard coefficient matrix formatting. It is
#' primarily used for console output and reporting.
#'
#' @param x A Persephone object (e.g., `x13Single` or `tramoseatsSingle`)
#' @return Character vector of formatted coefficient table lines (invisible)
#'
#' @note This is an internal function for console output.
#'
#' @examples
#' # obj <- perX13(AirPassengers, "rsa3")
#' # obj$run()
#' # getArimaCoef_formatted(obj)
getArimaCoef_formatted <- function(x) {
  xregs <- getArimaCoef(x)
  if (is.null(xregs)) {
    return(character(0))
  }
  xregs.print <- xregs[, c("value", "stde", "t", "pvalue")]
  rownames(xregs.print) <- xregs$name
  colnames(xregs.print) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")

  printCoefmat(xregs.print)
  # Capture formatted output as character vector
  (formatted_output <- capture.output(printCoefmat(xregs.print)))

  # return(list(
  #   table = xregs.print,
  #   formatted = formatted_output
  # ))
}

########################################
###         from rdj3toolkit         ###
########################################
# Note: .regarima_coef_table.default is a legacy function from rdj3toolkit
# that was replaced by getArimaCoef(). Kept here for reference.

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
#' @export
#' @note This is an internal function; use it only if you know what you're doing.
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
#' @param funcs Character vector of function names to run (default: `c("getOutliers", "getArimaCoef")`)
#' @return Invisible list of results from each function
#'
#' @note This is an internal function for interactive diagnostics.
#'
#' @examples
#' # obj <- perX13(AirPassengers, "rsa3")
#' # obj$run()
#' # getDiagnostics(obj)
#' # getDiagnostics(obj, funcs = c("getOutliers", "getArimaCoef", "gettsout"))
getDiagnostics <- function(x, ..., funcs = c("getOutliers", "getArimaCoef")) {
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
