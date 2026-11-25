# helpers for summary.persephone: coefficient tables and Wald F tests
# computed from the regression output of a run

# regression coefficients of the variables of the given types, one row per
# coefficient, with coef, stderr, t value and P(>|t|); the standard errors
# come from the diagonal of the covariance matrix bvar of the estimated
# coefficients (the same mapping as in getOutliers()), the p-values use
# neffectiveobs - nparams degrees of freedom. `also` is an optional predicate
# over the variables, used to keep additional variables (e.g. the user-defined
# leap year regressors)
sumCoefTable <- function(x, types, labels = NULL, also = NULL) {
  pp <- x$output$preprocessing
  variables <- pp$description$variables
  keep <- which(vapply(variables, function(v) {
    as.character(v$type) %in% types || (!is.null(also) && isTRUE(also(v)))
  }, TRUE))
  if (length(keep) == 0) {
    return(NULL)
  }
  # number of estimated coefficients per variable, position of the first
  # estimated coefficient in b (some variables have several coefficients, a
  # TD variable one per trading day group; FIXED coefficients are absent
  # from b)
  n_est <- vapply(variables, function(v) {
    if (is.null(v$coef$type)) {
      0L
    } else {
      sum(as.character(v$coef$type) == "ESTIMATED")
    }
  }, integer(1))
  b <- pp$estimation$b
  bvar <- pp$estimation$bvar
  pos <- vector("list", length(variables))
  if (length(b) > 0 && nrow(bvar) == sum(n_est)) {
    se_all <- sqrt(diag(bvar))
    first <- cumsum(c(0L, head(n_est, -1))) + 1L
    for (i in which(n_est > 0)) {
      pos[[i]] <- first[i] + seq_len(n_est[i]) - 1L
    }
  }
  df <- pp$estimation$likelihood$neffectiveobs -
    pp$estimation$likelihood$nparams

  rows <- do.call(rbind, lapply(keep, function(i) {
    v <- variables[[i]]
    k <- length(v$coef$value)
    nm <- if (is.null(labels)) v$name else labels(x, v)
    if (is.null(nm) || length(nm) != k) {
      nm <- if (k == 1) v$name else paste0(v$name, ".", seq_len(k))
    }
    if (length(pos[[i]]) == k) {
      coef <- b[pos[[i]]]
      stde <- sqrt(diag(bvar)[pos[[i]]])
    } else {
      # no bvar (or a mismatch): fall back to the coefficient values of the
      # description, without standard errors
      coef <- as.numeric(v$coef$value)
      stde <- rep(NA_real_, k)
    }
    data.frame(
      name = nm, coef = coef, stderr = stde,
      t = coef / stde,
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(rows) || nrow(rows) == 0) {
    return(NULL)
  }
  rows$p <- 2 * stats::pt(abs(rows$t), df, lower.tail = FALSE)
  rownames(rows) <- rows$name
  rows[, c("coef", "stderr", "t", "p"), drop = FALSE]
}

# Wald test of H0: all coefficients of the variables of the given types are
# zero: F = (b' V^-1 b)/q with q the number of coefficients, against F(q, df).
# `also` is an optional predicate over the variables, as in sumCoefTable().
sumWaldF <- function(x, types, also = NULL) {
  pp <- x$output$preprocessing
  variables <- pp$description$variables
  n_est <- vapply(variables, function(v) {
    if (is.null(v$coef$type)) {
      0L
    } else {
      sum(as.character(v$coef$type) == "ESTIMATED")
    }
  }, integer(1))
  sel <- (vapply(variables, function(v) as.character(v$type) %in% types, TRUE) |
    vapply(variables, function(v) !is.null(also) && isTRUE(also(v)), TRUE)) &
    n_est > 0
  b <- pp$estimation$b
  bvar <- pp$estimation$bvar
  if (!any(sel) || length(b) == 0 || nrow(bvar) != sum(n_est)) {
    return(NULL)
  }
  first <- cumsum(c(0L, head(n_est, -1))) + 1L
  idx <- unlist(lapply(which(sel), function(i) first[i] + seq_len(n_est[i]) - 1L))
  V <- bvar[idx, idx, drop = FALSE]
  bb <- b[idx]
  Vinv <- tryCatch(solve(V), error = function(e) {
    # nearly singular covariance matrix: use the pseudo inverse
    ev <- eigen(V, symmetric = TRUE)
    tol <- max(dim(V)) * max(ev$values) * .Machine$double.eps
    ev$vectors %*% diag(ifelse(ev$values > tol, 1 / ev$values, 0),
                       nrow = length(ev$values)) %*% t(ev$vectors)
  })
  q <- length(idx)
  df <- pp$estimation$likelihood$neffectiveobs -
    pp$estimation$likelihood$nparams
  fstat <- drop(crossprod(bb, Vinv %*% bb)) / q
  list(value = fstat, df1 = q, df2 = df,
       pvalue = stats::pf(fstat, q, df, lower.tail = FALSE))
}

# the day names used for the trading day regressors, in the order of the
# trading day groups (Monday to Saturday, with Sunday as the reference day of
# the contrasts or as the seventh regressor)
sumTdDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
               "Saturday", "Sunday")

# labels of the trading day coefficients: the names of the user-defined
# regressor columns (e.g. "Monday") when the spec uses uservariables,
# otherwise the day names of the built-in trading day variables; the generic
# names of the calendar groups ("group_0" to "group_6") are replaced by the
# day names of the default grouping (see perTd())
sumTdLabels <- function(x, n) {
  users <- NULL
  if (inherits(x, "x13Single")) {
    users <- tryCatch(unclass(x$params)$regarima$regression$td$users,
                      error = function(e) NULL)
  } else if (inherits(x, "tramoseatsSingle")) {
    users <- tryCatch(unclass(x$params)$tramo$regression$users,
                      error = function(e) NULL)
  }
  if (length(users) > 0 && !anyNA(users)) {
    nm <- sub("^[^.]*\\.", "", as.character(users))
    if (all(grepl("^group_[0-6]$", nm[seq_len(min(n, length(nm)))]))) {
      # the generic group names of rjd3toolkit::calendar_td() with the
      # default grouping: the regressors are the days Monday to Saturday
      # (the holiday/Sunday group is the reference of the contrasts)
      nm[seq_len(min(n, length(nm)))] <- sumTdDays[seq_len(min(n, length(nm)))]
    }
    if (length(nm) >= n) {
      return(nm[seq_len(n)])
    }
  }
  if (n <= 7) sumTdDays[seq_len(n)] else paste0("td", seq_len(n))
}

# is v a user-defined leap year regressor (e.g. the "lpYear"/"leapYear" column
# of genTd() or a dedicated "leapYear" variable)? A regressor that is registered
# as a user-defined variable of the regression model (the `user` argument of
# perX13()/perTramo()) comes with the type "UNSPECIFIED", a leap year variable
# of the specification itself with the type "LP" or "USER".
isUserLeapVar <- function(v) {
  as.character(v$type) %in% c("USER", "UNSPECIFIED") &&
    grepl("leap|lpyear|lp\\.year|^lpy", v$name, ignore.case = TRUE)
}

# is v a user-defined regressor of the regression model that is shown in its own
# table (the leap year regressors belong to the "Mean / Leap year" table)?
isUserVar <- function(v) {
  as.character(v$type) %in% c("USER", "UNSPECIFIED") && !isUserLeapVar(v)
}

# one row per coefficient of a variable: for the TD variable, the label
# function provides one label per coefficient
sumVarLabels <- function(x, v) {
  k <- length(v$coef$value)
  if (as.character(v$type) == "TD") {
    return(sumTdLabels(x, k))
  }
  if (as.character(v$type) %in% c("AO", "LS", "TC")) {
    return(sumOutlierName(x, v))
  }
  if (isUserLeapVar(v)) {
    return("leap year")
  }
  switch(
    as.character(v$type),
    MEAN = "const",
    LP = "leap year",
    EASTER = "easter",
    v$name
  )
}

# does the model account for leap years? True if a dedicated leap year
# regressor is part of the regression (the LP variable or a user-defined
# regressor named like a leap year) or if the series is log transformed with
# a leap year or length-of-month adjustment (transform.adjust = "leapyear" or
# "lom"), which includes the leap day effect implicitly. An automatic
# adjustment ("Auto") cannot be resolved from the specification and does not
# count.
hasLeapYear <- function(x) {
  pp <- x$output$preprocessing
  variables <- pp$description$variables
  if (any(vapply(variables, function(v) {
    as.character(v$type) == "LP" || isUserLeapVar(v)
  }, TRUE))) {
    return(TRUE)
  }
  if (isTRUE(pp$description$log)) {
    adj <- NULL
    if (inherits(x, "x13Single")) {
      adj <- tryCatch(unclass(x$params)$regarima$transform$adjust,
                      error = function(e) NULL)
    } else if (inherits(x, "tramoseatsSingle")) {
      adj <- tryCatch(unclass(x$params)$tramo$transform$adjust,
                      error = function(e) NULL)
    }
    if (is.character(adj) && length(adj) > 0 &&
        tolower(adj[1]) %in% c("leapyear", "lom")) {
      return(TRUE)
    }
  }
  FALSE
}

# the derived Sunday regressor of the trading day regression: when the six
# regressors are the days Monday to Saturday, the effect of the reference day
# Sunday is derived from the six effects and its standard error follows from
# the full covariance of the six coefficients. Which multiple of the sum of
# the six coefficients it is depends on the parameterisation of the
# regressors: with contrasts against Sunday, x_i = n_i - n_sun, as they are
# generated by the built-in trading day regressors of X-13, by the demetra
# calendar (rjd3toolkit::calendar_td() with the default grouping, reference
# group group_0) and by genTd(), the Sunday effect (as the deviation from the
# mean day effect, which is what the six displayed coefficients are) is
# -sum(b). With regressors that are centered with respect to the mean day,
# x_i = n_i - D/7, the displayed
# coefficients are the deviations from Sunday and the Sunday effect is
# -(1/7) sum(b). When the design matrix of the estimation is available the
# cases are distinguished by the sums of the regressor rows, which reach
# values beyond 1 for the contrasts and stay below 1 for the centered
# regressors;
# without the design matrix (X-13 with the built-in
# trading day module) the contrast parameterisation is assumed. NULL when the
# regression does not have the six day regressors.
sumTdSunday <- function(x) {
  pp <- x$output$preprocessing
  variables <- pp$description$variables
  n_est <- vapply(variables, function(v) {
    if (is.null(v$coef$type)) {
      0L
    } else {
      sum(as.character(v$coef$type) == "ESTIMATED")
    }
  }, integer(1))
  sel <- vapply(variables, function(v) as.character(v$type) == "TD", TRUE) &
    n_est > 0
  if (sum(n_est[sel]) != 6L) {
    return(NULL)
  }
  b <- pp$estimation$b
  bvar <- pp$estimation$bvar
  if (length(b) == 0 || nrow(bvar) != sum(n_est)) {
    return(NULL)
  }
  first <- cumsum(c(0L, head(n_est, -1))) + 1L
  idx <- unlist(lapply(which(sel), function(i) {
    first[i] + seq_len(n_est[i]) - 1L
  }))
  # contrast or centered regressors: the row sums of the design matrix
  # columns of the trading day regressors reach values beyond 1 for the
  # contrasts against Sunday and stay below 1 for the centered regressors
  scale <- -1
  X <- pp$estimation$X
  if (!is.null(X) && ncol(X) == length(b) &&
      all(abs(rowSums(unclass(X)[, idx, drop = FALSE])) < 0.75)) {
    scale <- -1 / 7
  }
  ones <- rep(1, length(idx))
  coef <- scale * sum(b[idx])
  stde <- abs(scale) *
    sqrt(drop(crossprod(ones, bvar[idx, idx, drop = FALSE] %*% ones)))
  df <- pp$estimation$likelihood$neffectiveobs -
    pp$estimation$likelihood$nparams
  out <- data.frame(
    coef = coef, stderr = stde, t = coef / stde,
    p = 2 * stats::pt(abs(coef / stde), df, lower.tail = FALSE),
    row.names = "Sunday (derived)",
    stringsAsFactors = FALSE
  )
  out[, c("coef", "stderr", "t", "p"), drop = FALSE]
}

# "LS1985.03" / "AO2008.01" from the variable name "LS (1985-03-01)"
sumOutlierName <- function(x, v) {
  m <- regmatches(v$name, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", v$name))
  if (length(m) == 0) {
    return(v$name)
  }
  d <- as.Date(m)
  if (stats::frequency(x$ts) == 12) {
    per <- format(d, "%m")
  } else {
    per <- as.character(lubridate::quarter(d))
  }
  paste0(as.character(v$type), format(d, "%Y"), ".", per)
}

# logical for the rows of the summary() Outliers table (one per outlier
# variable, in the order of the description): TRUE for the outliers that the
# estimation detected automatically, FALSE for the ones the user set in the
# specification; the same matching as in getOutliers(), see
# spec_outlier_keys()
sumOutlierAuto <- function(x) {
  variables <- x$output$preprocessing$description$variables
  otl <- Filter(function(v) {
    as.character(v$type) %in% c("AO", "LS", "TC")
  }, variables)
  keys <- spec_outlier_keys(x$params)
  vapply(otl, function(v) {
    d <- regmatches(v$name, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", v$name))
    length(d) != 1 || !(paste(as.character(v$type), d) %in% keys)
  }, TRUE)
}

# x11 filter descriptions: "FILTER_S3X3" -> "3x3", others kept as in the
# output ("FILTER_C1303B" -> "C1303B")
sumFilterName <- function(s) {
  if (is.null(s) || length(s) == 0) {
    return(NA_character_)
  }
  s <- sub("^FILTER_", "", as.character(s[1]))
  m <- regmatches(s, regexec("^S([0-9]+)X([0-9]+)$", s))[[1]]
  if (length(m) == 3) paste0(m[2], "x", m[3]) else s
}

# estimates of the regression ARIMA parameters (phi, theta, bphi, btheta)
# with standard errors from the covariance matrix of the ML estimation;
# the names follow the JD+ convention (ar.ar.1, ar.ma.1, ar.ar.b1, ar.ma.b1)
sumArimaTable <- function(x) {
  pp <- x$output$preprocessing
  ar <- pp$description$arima
  pars <- pp$estimation$parameters
  vals <- as.numeric(pars$val)
  if (length(vals) == 0) {
    return(NULL)
  }
  # the fields are 2 x n matrices of value and type (NULL if the block is
  # not in the model); FIXED coefficients have no standard error and are
  # absent from parameters$val, which is in the block order phi, theta,
  # bphi, btheta
  fields <- c("phi", "theta", "bphi", "btheta")
  prefixes <- c("ar.ar.", "ar.ma.", "ar.ar.b", "ar.ma.b")
  # number of coefficients of each block
  counts <- vapply(fields, function(f) {
    m <- ar[[f]]
    if (is.null(m)) {
      0L
    } else if (ncol(m) > 1 || !is.null(dim(m))) {
      ncol(m)
    } else {
      1L
    }
  }, 0L)
  value <- type <- character(0)
  for (f in fields) {
    m <- ar[[f]]
    if (is.null(m)) {
      next
    }
    if (is.matrix(m)) {
      vl <- as.character(m["value", ])
      tp <- as.character(m["type", ])
    } else {
      # single coefficient: the field may collapse to a value/type pair
      vl <- as.character(m[[1]])
      tp <- as.character(m[[2]])
    }
    value <- c(value, vl)
    type <- c(type, tp)
  }
  nm <- unlist(lapply(seq_along(fields), function(i) {
    if (counts[i] == 0) character(0) else paste0(prefixes[i], seq_len(counts[i]))
  }))
  if (length(nm) != length(value)) {
    return(NULL)
  }
  n_est <- sum(type == "ESTIMATED")
  cov <- as.matrix(pars$cov)
  stde <- rep(NA_real_, length(value))
  if (n_est == length(vals) && nrow(cov) == length(vals)) {
    stde[type == "ESTIMATED"] <- sqrt(diag(cov))
  }
  coef <- suppressWarnings(as.numeric(value))
  df <- pp$estimation$likelihood$neffectiveobs -
    pp$estimation$likelihood$nparams
  tstat <- coef / stde
  data.frame(
    coef = coef, stderr = stde, t = tstat,
    p = 2 * stats::pt(abs(tstat), df, lower.tail = FALSE),
    row.names = nm,
    stringsAsFactors = FALSE
  )
}

# regression ARIMA specification "(p d q)(P D Q)" from the final parameters
sumArimaSpec <- function(x) {
  ud <- x$output$user_defined
  if (is.null(ud$arima.p)) {
    return(NA_character_)
  }
  sprintf("(%d %d %d)(%d %d %d)",
          ud$arima.p, ud$arima.d, ud$arima.q,
          ud$arima.bp, ud$arima.bd, ud$arima.bq)
}

# print a labelled table of coefficients with the columns
# coef, stderr, t value, P(>|t|); an "autoout" column of the table (the TRUE/
# FALSE of the Outliers table, see sumOutlierAuto()) is printed as a last
# column
sumPrintCoef <- function(tbl, title) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(invisible())
  }
  cat("--", title, "\n")
  out <- data.frame(
    coef = sumNum(tbl$coef),
    stderr = sumNum(tbl$stderr),
    `t value` = sumNum(tbl$t),
    `P(>|t|)` = sumPval(tbl$p),
    check.names = FALSE
  )
  if (!is.null(tbl$autoout)) {
    out <- cbind(out, autoout = tbl$autoout)
  }
  rownames(out) <- rownames(tbl)
  print(out, right = TRUE, quote = FALSE)
  cat("\n")
}

sumNum <- function(v) format(signif(v, 6), scientific = FALSE)

# the p-values of the coefficient tables: everything below 0.0001 shares the
# "< 0.0001" display, which keeps the tables readable (format.pval() would
# print such values in scientific notation or as "< 1e-04")
sumPval <- function(p) {
  small <- which(!is.na(p) & p < 0.0001)
  out <- format.pval(p, digits = 3, eps = 0)
  out[small] <- "< 0.0001"
  out
}

#' Summary of a Seasonal Adjustment
#'
#' Prints a summary of a run of a single series: the frequency, span and
#' length of the series, the model definition of the regression ARIMA model
#' (ARIMA specification, transformation, leap year and trading day effects,
#' Easter effect, number of outliers), the estimates of the ARIMA
#' parameters, the regression coefficients with their standard errors, t
#' values and p-values in one table per group (outliers, trading day
#' regressors, mean and leap year), the joint F test of the trading day
#' regressors directly below their table, the decomposition filters and, at
#' the end, the M-statistics
#' (Q and Q-M2) and the overall quality indicator.
#'
#' @details
#'
#' The `autoout` column of the Outliers table tells how each outlier entered
#' the model, with the same meaning as in [getOutlierTable()]: `TRUE` for the
#' ones the estimation detected by itself, `FALSE` for the ones the user set
#' in the specification (the `outliers` argument or [fixOutlier()]).
#'
#' In the trading day table the regressors are labelled with their names (the
#' column names of the user-defined regressors, see [perTd()] and its
#' `td_labels` argument; the generic group names of the calendar regressors
#' are shown as the days Monday to Saturday). When the trading day regression
#' consists of the six day regressors, the effect of the reference day Sunday
#' is derived from the six effects and shown as an additional row "Sunday
#' (derived)": minus their sum for regressors given as contrasts against
#' Sunday, as generated by the trading day modules of JDemetra+ and by
#' [genTd()], minus one seventh of their sum for regressors centered with
#' respect to the mean day (the two cases are distinguished automatically
#' when the design matrix of the estimation is available).
#'
#' The leap year effect counts as included ("Leap year: Yes") if a dedicated
#' leap year regressor is part of the regression or if the series is log
#' transformed with a leap year or length-of-month adjustment
#' (`transform.adjust = "leapyear"` or `"lom"`). A user-defined regressor
#' given to the `user` argument of [perX13()] or [perTramo()] and named like
#' a leap year is such a regressor; the further user-defined regressors are
#' shown in a table "User-defined regressors" and tested together by a joint
#' F test.
#'
#' @param x a `persephone` object of a single series (created with
#'   [perX13()] or [perTramo()]) for which [run][persephone] has been called.
#' @param ... currently ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' summary(obj)
#'
#' @export
summary.persephone <- function(x, ...) {
  if (!inherits(x, "persephoneSingle")) {
    stop("summary() is only available for single series objects ",
         "(perX13() and perTramo()).", call. = FALSE)
  }
  if (is.null(x$output)) {
    stop("No results from run available.\n", call. = FALSE)
  }
  ts <- x$ts
  ud <- x$output$user_defined
  pp <- x$output$preprocessing

  # ---- header ----
  cat("Frequency:", stats::frequency(ts), "  ",
      "Observations:", length(ts), "  ",
      "Span:", paste(sumPeriod(ts, stats::start(ts)), "-",
                     sumPeriod(ts, stats::end(ts))), "\n\n")

  # ---- model definition ----
  var_types <- vapply(pp$description$variables, function(v)
    as.character(v$type), "")
  n_coef <- vapply(pp$description$variables, function(v)
    length(v$coef$value), 0L)
  # number of trading day regressors = number of TD coefficients (0 to 6/7)
  n_td <- sum(n_coef[var_types == "TD"])
  n_otl <- sum(var_types %in% c("AO", "LS", "TC"))
  # user-defined regressors of the regression model (the `user` argument of
  # perX13()/perTramo()); a leap year regressor among them is counted as leap
  # year, not here
  is_user <- vapply(pp$description$variables, isUserVar, TRUE)
  n_user <- sum(n_coef[is_user])
  cat("-- Model definition ", strrep("-", 42), "\n", sep = "")
  cat("ARIMA model:     ", sumArimaSpec(x), "\n")
  cat("Transformation:  ", if (isTRUE(pp$description$log)) "Log" else "None", "\n")
  cat("Leap year:       ", if (hasLeapYear(x)) "Yes" else "None", "\n")
  cat("Trading day:     ",
      paste0(n_td, if (n_td == 1) " regressor" else " regressors"), "\n")
  cat("Easter effect:   ", if ("EASTER" %in% var_types) "Yes" else "None", "\n")
  cat("Outliers:        ", n_otl, "\n")
  if (n_user > 0) {
    cat("User regressors: ", n_user, "\n")
  }
  cat("\n")

  # ---- coefficient tables ----
  labelFun <- function(x, v) sumVarLabels(x, v)
  sumPrintCoef(
    sumArimaTable(x),
    paste("ARIMA parameters", sumArimaSpec(x))
  )
  otlTab <- sumCoefTable(x, c("AO", "LS", "TC"), labelFun)
  if (!is.null(otlTab)) {
    # as in getOutliers(): TRUE for the automatically detected outliers,
    # FALSE for the ones the user set in the specification
    otlTab$autoout <- sumOutlierAuto(x)
  }
  sumPrintCoef(otlTab, "Outliers")
  tdTab <- sumCoefTable(x, "TD", labelFun)
  if (!is.null(tdTab) && nrow(tdTab) == 6 &&
      !any(grepl("^sun", tolower(rownames(tdTab))))) {
    # six day regressors (Monday to Saturday): the effect of the reference
    # day Sunday is derived from them and added as a further row
    sunday <- sumTdSunday(x)
    if (!is.null(sunday)) {
      tdTab <- rbind(tdTab, sunday)
    }
  }
  sumPrintCoef(tdTab, "Trading day regressors")
  if (n_td > 0) {
    # the joint F test of the trading day regressors follows directly below
    # their coefficient table
    f_td <- sumWaldF(x, "TD")
    if (!is.null(f_td)) {
      cat("Trading days:            ",
          sprintf("F(%d, %d) = %.3f", f_td$df1, f_td$df2, f_td$value),
          "  P =", sumPval(f_td$pvalue), "\n\n")
    }
  }
  sumPrintCoef(
    sumCoefTable(x, c("MEAN", "LP"), labelFun, also = isUserLeapVar),
    "Mean / Leap year"
  )
  sumPrintCoef(
    sumCoefTable(x, character(0), labelFun, also = isUserVar),
    "User-defined regressors"
  )

  # ---- joint F test of the user-defined regressors ----
  if (n_user > 0) {
    f_user <- sumWaldF(x, character(0), also = isUserVar)
    if (!is.null(f_user)) {
      cat("-- Joint F tests ", strrep("-", 44), "\n", sep = "")
      cat("User regressors:         ",
          sprintf("F(%d, %d) = %.3f", f_user$df1, f_user$df2, f_user$value),
          "  P =", sumPval(f_user$pvalue), "\n")
      cat("\n")
    }
  }

  # ---- SA decomposition ----
  cat("-- SA decomposition ", strrep("-", 41), "\n", sep = "")
  mode <- ud$mode
  cat("Seasonal adjustment: ", tolower(as.character(mode)), "\n")
  if (inherits(x$output$decomposition, "JD3X11")) {
    dec <- x$output$decomposition
    cat("Final MA filter:     ", sumFilterName(dec$final_seasonal), "\n")
    hend <- dec$final_henderson
    if (!is.null(hend) && !is.na(hend)) {
      cat("Final trend-cycle:   ",
          paste0(hend, "-term Henderson filter"), "\n")
    }
  } else {
    cat("Decomposition:       SEATS (no X-11 filters)\n")
  }

  # ---- quality ----
  q1 <- ud[["m-statistics.q"]]
  q2 <- ud[["m-statistics.q-m2"]]
  totalQ <- ud$quality.summary
  if ((!is.null(q1) && !is.na(q1)) || (!is.null(totalQ) && !is.na(totalQ))) {
    cat("\n-- Quality ", strrep("-", 54), "\n", sep = "")
    if (!is.null(q1) && !is.na(q1)) {
      cat("M-statistics:        ",
          paste0("Q = ", sumNum(q1),
                 if (!is.null(q2) && !is.na(q2))
                   paste0("   Q-M2 = ", sumNum(q2))), "\n")
    }
    if (!is.null(totalQ) && !is.na(totalQ)) {
      cat("Total quality:       ", as.character(totalQ), "\n")
    }
  }
  invisible(x)
}

# "1949.1" from c(1949, 1), "2019.2" from c(2019, 2)
sumPeriod <- function(ts, t) {
  paste(t[1], paste(t[-1], collapse = "."), sep = ".")
}
