# Normalize dates explicitly: current rjd3 specs use ISO strings, while older
# serialized specs may hold Date values or numeric days since 1970-01-01.
outlier_date <- function(value) {
  if (length(value) != 1L || is.na(value)) {
    stop("An outlier date must be a single non-missing value.", call. = FALSE)
  }
  if (inherits(value, "Date")) return(as.Date(value))
  if (inherits(value, "POSIXt")) return(as.Date(value, tz = "UTC"))
  if (is.numeric(value) && is.finite(value) && value == floor(value)) {
    return(as.Date(value, origin = "1970-01-01"))
  }
  if (is.character(value) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) {
    result <- as.Date(value, format = "%Y-%m-%d")
    if (!is.na(result) && identical(format(result, "%Y-%m-%d"), value)) return(result)
  }
  stop("Unsupported outlier date; use an ISO date, Date, POSIXt, or numeric epoch day.",
       call. = FALSE)
}

outlier_period_index <- function(date, frequency) {
  if (!frequency %in% c(1, 2, 3, 4, 6, 12)) {
    stop("Outlier dates require a frequency that divides 12.", call. = FALSE)
  }
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  year * frequency + floor((month - 1) * frequency / 12)
}

outlier_period_dates <- function(index, frequency) {
  year <- floor(index / frequency)
  month <- (index %% frequency) * (12 / frequency) + 1
  as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))
}

outlier_label <- function(type, date, frequency) {
  index <- outlier_period_index(date, frequency)
  year <- floor(index / frequency)
  period <- index %% frequency + 1
  paste0(type, year, if (frequency == 1) "" else paste0(".", period))
}

fitted_regression_spec <- function(x) {
  # Relate a fitted result to the specification used for that fit, even after
  # updateParams() changes the current settings. Legacy objects lack $result.
  spec <- x$result$specification$parameters
  if (is.null(spec)) {
    spec <- if (inherits(x, "hierarchicalTimeSeries")) x$paramsDirect else x$params
  }
  if (!is.null(spec$regarima)) spec$regarima else spec$tramo
}

manual_outlier_table <- function(spec) {
  records <- spec$regression$outliers
  if (!length(records)) {
    return(data.frame(type = character(), name = character(), date = as.Date(character())))
  }
  data.frame(type = vapply(records, function(value) as.character(value$code), ""),
             name = vapply(records, function(value) {
               if (is.null(value$name)) "" else as.character(value$name)
             }, ""),
             date = as.Date(vapply(records, function(value) as.numeric(outlier_date(value$pos)),
                                   numeric(1)), origin = "1970-01-01"),
             stringsAsFactors = FALSE)
}

outlier_variable_dates <- function(variables, manual) {
  value <- vapply(variables, function(variable) {
    for (field in c("pos", "date")) {
      if (!is.null(variable[[field]])) return(as.numeric(outlier_date(variable[[field]])))
    }
    # A manually supplied display name need not contain a date. Resolve it
    # against the fitted specification before parsing standard backend labels.
    index <- which(manual$type == variable$type & manual$name == variable$name)
    dates <- unique(manual$date[index])
    if (length(dates) == 1L) return(as.numeric(dates))
    label <- regmatches(variable$name, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", variable$name))
    if (length(label) == 1L) return(as.numeric(outlier_date(label)))
    stop(sprintf("Cannot determine the date of outlier '%s'.", variable$name), call. = FALSE)
  }, numeric(1))
  as.Date(value, origin = "1970-01-01")
}

empty_outlier_table <- function() {
  structure(data.frame(name = character(), type = character(), coef.value = double(),
                       stde = double(), t = double(), pvalue = double(),
                       date = as.Date(character()), coef.type = character(),
                       autoout = logical(), recentout = logical(), stringsAsFactors = FALSE),
            class = c("persephoneOutliers", "data.frame"))
}

regression_coefficient_table <- function(preprocessing) {
  variables <- preprocessing$description$variables
  rows <- lapply(seq_along(variables), function(index) {
    variable <- variables[[index]]
    coefficients <- variable$coef
    # Current backends return a data frame; older/plain result fixtures may
    # represent the same coefficient columns as a list.
    n <- length(coefficients$value)
    if (!n) return(NULL)
    coefficient_type <- as.character(coefficients$type)
    if (!length(coefficient_type)) coefficient_type <- rep(NA_character_, n)
    if (!length(coefficient_type) %in% c(1L, n)) {
      stop("Regression coefficient values and types have inconsistent lengths.", call. = FALSE)
    }
    labels <- rownames(coefficients)
    if (is.null(labels)) labels <- rep(variable$name, n)
    data.frame(variable = index, name = labels, type = variable$type,
               value = as.numeric(coefficients$value),
               coef.type = rep_len(coefficient_type, n),
               stringsAsFactors = FALSE)
  })
  rows <- do.call(rbind, rows)
  if (is.null(rows)) return(NULL)
  rownames(rows) <- NULL
  estimated <- rows$coef.type %in% "ESTIMATED"
  rows$stde <- rep(NA_real_, nrow(rows))
  covariance <- preprocessing$estimation$bvar
  if (length(covariance)) {
    n <- sum(estimated)
    if (!is.matrix(covariance) || !identical(dim(covariance), c(n, n))) {
      warning("bvar does not match the number of estimated regression coefficients; standard errors are unavailable.", call. = FALSE)
    } else {
      variance <- diag(covariance)
      if (any(!is.na(variance) & variance < 0)) {
        warning("Negative regression coefficient variance; affected standard errors are unavailable.", call. = FALSE)
        variance[!is.na(variance) & variance < 0] <- NA_real_
      }
      rows$stde[estimated] <- sqrt(variance)
    }
  }
  rows$t <- rep(NA_real_, nrow(rows))
  rows$t[estimated] <- rows$value[estimated] / rows$stde[estimated]
  rows$pvalue <- rep(NA_real_, nrow(rows))
  likelihood <- preprocessing$estimation$likelihood
  ndf <- likelihood$neffectiveobs - likelihood$nparams
  if (length(ndf) == 1L && is.finite(ndf) && ndf > 0) {
    rows$pvalue[estimated] <- 2 * stats::pt(abs(rows$t[estimated]), ndf, lower.tail = FALSE)
  }
  rows
}

outlier_estimation_periods <- function(series, spec) {
  frq <- stats::frequency(series)
  indices <- round(as.numeric(stats::time(series)) * frq)
  observed <- which(!is.na(series))
  if (!length(observed)) return(numeric())
  indices <- indices[seq.int(min(observed), max(observed))]
  select <- function(indices, span) {
    if (is.null(span) || is.null(span$type) || span$type == "ALL") return(indices)
    dates <- outlier_period_dates(indices, frq)
    count <- function(n) if (n < 0) -n * frq else n
    switch(span$type,
           FROM = indices[dates >= outlier_date(span$d0)],
           TO = indices[dates < outlier_date(span$d1)],
           BETWEEN = indices[dates >= outlier_date(span$d0) & dates < outlier_date(span$d1)],
           FIRST = head(indices, count(span$n0)),
           LAST = tail(indices, count(span$n1)),
           EXCLUDING = {
             first <- count(span$n0) + 1L
             last <- length(indices) - count(span$n1)
             if (last < first) numeric() else indices[seq.int(first, last)]
           },
           stop("Unsupported estimation span type.", call. = FALSE))
  }
  select(select(indices, spec$basic$span), spec$estimate$span)
}

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
#' A fitted model without outliers returns a typed zero-row data frame. A model
#' that has not been run raises an error. Manual dates accept ISO strings, Date
#' objects, POSIXt values (UTC), and legacy numeric days since 1970-01-01. Dates
#' are matched and returned at the model's period boundary.
#'
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
#' read from the specification used for the fit (`regarima$regression$outliers` for x13,
#' `tramo$regression$outliers` for tramoseats). `recentout` is evaluated
#' independently of `autoout`, so a manually specified outlier close to the
#' end of the series is flagged as well when it is inside the estimation span.
#' For legacy objects without a fitted specification snapshot, the current
#' direct specification is used (including `paramsDirect` for aggregates).
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
  if (is.null(x$output)) stop("No results from run available.\n")
  stopifnot(inherits(x$output, c("JD3_X13_RSLTS", "JD3_TRAMOSEATS_RSLTS")))
  variables <- x$output$preprocessing$description$variables
  selected <- which(vapply(variables, function(variable) variable$type %in% c("AO", "LS", "TC"), logical(1)))
  if (!length(selected)) return(empty_outlier_table())

  records <- regression_coefficient_table(x$output$preprocessing)
  selected_rows <- which(records$variable %in% selected)
  if (length(selected_rows) != length(selected)) {
    stop("Each outlier must have exactly one regression coefficient.", call. = FALSE)
  }
  records <- records[selected_rows, , drop = FALSE]
  spec <- fitted_regression_spec(x)
  manual <- manual_outlier_table(spec)
  frq <- stats::frequency(x$ts)
  dates <- outlier_variable_dates(variables[selected], manual)
  periods <- outlier_period_index(dates, frq)
  # Match model periods: a date inside a quarter refers to that quarter, not
  # necessarily to its first calendar day.
  manual_keys <- paste(manual$type, outlier_period_index(manual$date, frq))
  keys <- paste(records$type, periods)
  k <- if (frq == 12) 5L else if (frq == 4) 3L else 1L
  last <- round(tail(as.numeric(stats::time(x$ts)), 1L) * frq)
  recent <- periods > last - k & periods <= last &
    periods %in% outlier_estimation_periods(x$ts, spec)

  structure(data.frame(name = outlier_label(records$type, dates, frq),
                       type = records$type, coef.value = records$value,
                       stde = records$stde, t = records$t, pvalue = records$pvalue,
                       date = outlier_period_dates(periods, frq), coef.type = records$coef.type,
                       autoout = !keys %in% manual_keys, recentout = recent,
                       stringsAsFactors = FALSE, row.names = NULL),
            class = c("persephoneOutliers", "data.frame"))
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
  if (is.null(x$output)) stop("No results from run available.\n")
  stopifnot(inherits(x$output, c("JD3_X13_RSLTS", "JD3_TRAMOSEATS_RSLTS")))
  q <- x$output$preprocessing
  records <- regression_coefficient_table(q)
  if (is.null(records)) return(NULL)
  selected <- which(records$type %in% c("AO", "LS", "TC"))
  if (length(selected)) {
    manual <- manual_outlier_table(fitted_regression_spec(x))
    dates <- outlier_variable_dates(q$description$variables[records$variable[selected]], manual)
    records$name[selected] <- outlier_label(records$type[selected], dates, stats::frequency(x$ts))
    records$type[selected] <- "outlier"
  }
  structure(records[, c("name", "type", "value", "stde", "t", "pvalue"), drop = FALSE],
            class = c("persephoneRegressors", "data.frame"))
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
  frq <- stats::frequency(x$ts)
  periods <- round(as.numeric(stats::time(x$ts)) * frq)
  index <- match(outlier_period_index(outliers$date, frq), periods)
  # Fixed outliers outside the observed series have no point to annotate.
  outliers <- outliers[!is.na(index), , drop = FALSE]
  index <- index[!is.na(index)]
  if (!nrow(outliers)) return(list(NULL, list()))
  dates <- as.list(outliers$date)
  names(dates) <- outliers$type
  types <- intersect(c("AO", "LS", "TC"), outliers$type)
  points <- lapply(types, function(type) {
    y <- rep(NA_real_, length(x$ts))
    positions <- index[outliers$type == type]
    y[positions] <- x$ts[positions]
    stats::ts(y, start = stats::start(x$ts), frequency = frq)
  })
  names(points) <- paste0("otl", types)
  list(do.call(cbind, points), dates)
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
