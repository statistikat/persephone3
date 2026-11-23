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

# Native automatic variables carry positions and estimated coefficients.
# Restricted variables carry supplied columns and fixed coefficients, without
# outlier subtypes, dates or coefficient inference.
native_regression_records <- function(result) {
  if (inherits(result, "persephone_native_tramo_fit")) {
    mean <- data.frame(name = "const", type = "MEAN", value = result$model$mean_coefficient,
                       subtype = NA_character_, position = NA_integer_)
    if (identical(result$model$mean_correction, FALSE)) mean <- mean[FALSE, , drop = FALSE]
    v <- result$model$outliers
    if (is.null(v) || !nrow(v)) return(mean)
  } else if (inherits(result, "persephone_native_automatic_fit")) {
    mean <- NULL
    v <- result$model$variables
  } else v <- NULL
  if (!is.null(v)) {
    rows <- data.frame(name = v$name, type = rep("outlier", length(v$name)),
                       value = v$coefficient, subtype = v$type, position = v$position)
    return(rbind(mean, rows))
  }
  variables <- result$specification$variables
  rows <- lapply(seq_along(variables), function(i) {
    v <- variables[[i]]
    n <- length(v$coefficient)
    labels <- names(v$columns)
    fallback <- paste(v$class, i, seq_len(n), sep = ".")
    if (is.null(labels)) labels <- fallback
    missing <- is.na(labels) | !nzchar(labels)
    labels[missing] <- fallback[missing]
    data.frame(name = labels, type = v$class, value = v$coefficient,
               subtype = NA_character_, position = NA_integer_)
  })
  do.call(rbind, rows)
}

native_outlier_table <- function(x) {
  rows <- native_regression_records(x$result)
  if (is.null(rows) || !any(rows$type == "outlier")) return(empty_outlier_table())
  rows <- rows[rows$type == "outlier", , drop = FALSE]
  dates <- as.Date(rep(NA_character_, nrow(rows)))
  dated <- !is.na(rows$position)
  if (any(dated)) {
    ordinals <- round(stats::time(x$ts)[rows$position[dated]] * stats::frequency(x$ts))
    dates[dated] <- outlier_period_dates(as.numeric(ordinals), stats::frequency(x$ts))
  }
  automatic <- inherits(x$result, c("persephone_native_automatic_fit", "persephone_native_tramo_fit"))
  structure(data.frame(name = rows$name, type = rows$subtype, coef.value = rows$value,
    stde = NA_real_, t = NA_real_, pvalue = NA_real_, date = dates,
    coef.type = if (automatic) "ESTIMATED" else "FIXED", crit.value = NA_real_,
    autoout = automatic, recentout = rows$position > length(x$ts) - 5L),
    class = c("persephoneOutliers", "data.frame"))
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
  if (is.null(x$output)) stop("No results from run available.\n")
  if (identical(x$result$backend, "native")) return(native_outlier_table(x))
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
  spec_cv <- vapply(records$type, function(tp) {
    if (!is_x13) {
      return(as_crit(otl_spec$va))
    }
    type_o <- Filter(function(o) identical(as.character(o$type), tp),
                     otl_spec$outliers)
    va <- if (length(type_o)) as_crit(type_o[[1]]$va) else 0
    if (va > 0) va else as_crit(otl_spec$defva)
  }, numeric(1))

  structure(data.frame(name = outlier_label(records$type, dates, frq),
                       type = records$type, coef.value = records$value,
                       stde = records$stde, t = records$t, pvalue = records$pvalue,
                       date = outlier_period_dates(periods, frq), coef.type = records$coef.type,
                       crit.value = ifelse(spec_cv > 0, spec_cv, auto_cv),
                       autoout = !keys %in% manual_keys, recentout = recent,
                       stringsAsFactors = FALSE, row.names = NULL),
            class = c("persephoneOutliers", "data.frame"))
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
  if (is.null(x$output)) stop("No results from run available.\n")
  if (identical(x$result$backend, "native")) {
    rows <- native_regression_records(x$result)
    if (is.null(rows) || !nrow(rows)) return(NULL)
    out <- rows[c("name", "type", "value")]
    out$stde <- out$t <- out$pvalue <- NA_real_
    return(structure(out, class = c("persephoneRegressors", "data.frame"),
                     unavailable = c("stde", "t", "pvalue")))
  }
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
  out <- structure(records[, c("name", "type", "value", "stde", "t", "pvalue"), drop = FALSE],
            class = c("persephoneRegressors", "data.frame"))
  attr(out, "ftests") <- tryCatch(tdJointFTest(x), error = function(e) NULL)
  out
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

