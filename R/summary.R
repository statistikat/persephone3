# helpers for summary.persephone: coefficient tables and Wald F tests
# computed from the regression output of a run

# regression coefficients of the variables of the given types, one row per
# coefficient, with coef, stderr, t value and P(>|t|); the standard errors
# come from the diagonal of the covariance matrix bvar of the estimated
# coefficients (the same mapping as in getOutliers()), the p-values use
# neffectiveobs - nparams degrees of freedom
sumCoefTable <- function(x, types, labels = NULL) {
  pp <- x$output$preprocessing
  variables <- pp$description$variables
  keep <- which(vapply(variables, function(v) as.character(v$type) %in% types, TRUE))
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
# zero: F = (b' V^-1 b)/q with q the number of coefficients, against F(q, df)
sumWaldF <- function(x, types) {
  pp <- x$output$preprocessing
  variables <- pp$description$variables
  n_est <- vapply(variables, function(v) {
    if (is.null(v$coef$type)) {
      0L
    } else {
      sum(as.character(v$coef$type) == "ESTIMATED")
    }
  }, integer(1))
  sel <- vapply(variables, function(v) as.character(v$type) %in% types, TRUE) &
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

# labels of the trading day coefficients: the names of the user-defined
# regressor columns (e.g. "Monday", "group_1") when the spec uses
# uservariables, otherwise the day abbreviations of the built-in
# trading day variables
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
    if (length(nm) >= n) {
      return(nm[seq_len(n)])
    }
  }
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
            "Saturday", "Sunday")
  if (n <= 7) days[seq_len(n)] else paste0("td", seq_len(n))
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
  switch(
    as.character(v$type),
    MEAN = "const",
    LP = "leap year",
    EASTER = "easter",
    v$name
  )
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
# coef, stderr, t value, P(>|t|)
sumPrintCoef <- function(tbl, title) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(invisible())
  }
  cat("--", title, "\n")
  out <- data.frame(
    coef = sumNum(tbl$coef),
    stderr = sumNum(tbl$stderr),
    `t value` = sumNum(tbl$t),
    `P(>|t|)` = format.pval(tbl$p, digits = 3, eps = 1e-10),
    check.names = FALSE
  )
  rownames(out) <- rownames(tbl)
  print(out, right = TRUE, quote = FALSE)
  cat("\n")
}

sumNum <- function(v) format(signif(v, 6), scientific = FALSE)

#' Summary of a Seasonal Adjustment
#'
#' Prints a summary of a run of a single series: the frequency, span and
#' length of the series, the model definition of the regression ARIMA model
#' (ARIMA specification, transformation, leap year and trading day effects,
#' Easter effect, number of outliers), the regression coefficients with
#' their standard errors, t values and p-values in one table per group
#' (outliers, trading day regressors, mean and leap year), the joint F
#' tests of the trading day regressors and of the trading day regressors
#' and outliers together, and the decomposition filters.
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
  cat("-- Model definition ", strrep("-", 42), "\n", sep = "")
  cat("ARIMA model:     ", sumArimaSpec(x), "\n")
  cat("Transformation:  ", if (isTRUE(pp$description$log)) "Log" else "None", "\n")
  cat("Leap year:       ", if ("LP" %in% var_types) "Yes" else "None", "\n")
  cat("Trading day:     ",
      paste0(n_td, if (n_td == 1) " regressor" else " regressors"), "\n")
  cat("Easter effect:   ", if ("EASTER" %in% var_types) "Yes" else "None", "\n")
  cat("Outliers:        ", n_otl, "\n\n")

  # ---- coefficient tables ----
  labelFun <- function(x, v) sumVarLabels(x, v)
  sumPrintCoef(
    sumCoefTable(x, c("AO", "LS", "TC"), labelFun),
    "Outliers"
  )
  sumPrintCoef(sumCoefTable(x, "TD", labelFun), "Trading day regressors")
  sumPrintCoef(
    sumCoefTable(x, c("MEAN", "LP"), labelFun),
    "Mean / Leap year"
  )

  # ---- joint F tests ----
  if (n_td > 0) {
    cat("-- Joint F tests ", strrep("-", 44), "\n", sep = "")
    f_td <- sumWaldF(x, "TD")
    if (!is.null(f_td)) {
      cat("Trading days:            ",
          sprintf("F(%d, %d) = %.3f", f_td$df1, f_td$df2, f_td$value),
          "  P =", format.pval(f_td$pvalue, digits = 3, eps = 1e-10), "\n")
    }
    if (n_otl > 0) {
      f_both <- sumWaldF(x, c("TD", "AO", "LS", "TC"))
      if (!is.null(f_both)) {
        cat("Trading days + outliers: ",
            sprintf("F(%d, %d) = %.3f", f_both$df1, f_both$df2, f_both$value),
            "  P =", format.pval(f_both$pvalue, digits = 3, eps = 1e-10), "\n")
      }
    }
    cat("\n")
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
  invisible(x)
}

# "1949.1" from c(1949, 1), "2019.2" from c(2019, 2)
sumPeriod <- function(ts, t) {
  paste(t[1], paste(t[-1], collapse = "."), sep = ".")
}
