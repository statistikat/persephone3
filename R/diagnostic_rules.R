# Typed metric extraction and presentation rules shared by tables and dashboards.
# Numeric p-values, categorical outcomes and overall backend quality are distinct.
diagnostic_at <- function(object, path) {
  for (name in path) {
    if (!is.list(object) || !name %in% names(object)) return(NULL)
    object <- object[[name]]
  }
  object
}

diagnostic_number <- function(value) {
  if (!is.numeric(value) || is.complex(value) || length(value) != 1L || !is.finite(value)) {
    return(NA_real_)
  }
  unname(as.numeric(value))
}

diagnostic_category <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }
  unname(value)
}

diagnostic_probability <- function(value) {
  if (is.list(value)) value <- value$pvalue
  p <- diagnostic_number(value)
  if (is.na(p) || p < 0 || p > 1) NA_real_ else p
}

diagnostic_classify <- function(value, rule) {
  rule <- match.arg(rule, c("normality", "residual", "regression_td", "outliers", "q"))
  value <- diagnostic_number(value)
  if (is.na(value) || value < 0 || (rule %in% c("normality", "residual", "regression_td") && value > 1)) return("Undefined")
  switch(rule,
    normality = if (value < 0.001) "Severe" else if (value < 0.01) "Bad" else if (value < 0.1) "Uncertain" else "Good",
    residual = if (value < 0.01) "Severe" else if (value < 0.05) "Bad" else if (value < 0.1) "Uncertain" else "Good",
    regression_td = if (value < 0.05) "Good" else if (value < 0.1) "Uncertain" else "Bad",
    outliers = if (value < 0.03) "Good" else if (value < 0.05) "Uncertain" else "Severe",
    q = if (value < 1) "Good" else if (value < 2) "Bad" else "Severe",
    stop("Unknown diagnostic classification rule.", call. = FALSE))
}

diagnostic_metrics <- function(x) {
  output <- x$output
  pick <- function(paths) {
    for (path in paths) {
      value <- diagnostic_at(output, path)
      if (!is.null(value)) return(list(value = value, source = paste(path, collapse = "/")))
    }
    list(value = NULL, source = NA_character_)
  }
  test <- function(paths, rule) {
    selected <- pick(paths)
    p <- diagnostic_probability(selected$value)
    list(kind = "test", statistic = diagnostic_number(if (is.list(selected$value)) selected$value$value else NULL),
         pvalue = p, status = diagnostic_classify(p, rule),
         availability = if (is.na(p)) "missing" else "available", source = selected$source)
  }
  category <- function(paths) {
    selected <- pick(paths)
    value <- diagnostic_category(selected$value)
    list(kind = "category", value = value, status = "",
         availability = if (is.na(value)) "missing" else "available", source = selected$source)
  }
  number <- function(paths) {
    selected <- pick(paths)
    value <- diagnostic_number(selected$value)
    list(kind = "statistic", value = value, status = diagnostic_classify(value, "q"),
         availability = if (is.na(value)) "missing" else "available", source = selected$source)
  }
  metric <- function(key, fallback = NULL, rule = "residual") {
    paths <- list(c("user_defined", key))
    if (!is.null(fallback)) paths <- c(paths, list(fallback))
    test(paths, rule)
  }
  orders <- vapply(c("p", "d", "q", "bp", "bd", "bq"), function(order) {
    if (identical(x$result$backend, "native")) {
      model <- x$result$model
      return(diagnostic_number(if (is.null(model$orders)) model[[order]] else model$orders[[order]]))
    }
    diagnostic_number(diagnostic_at(output, c("user_defined", paste0("arima.", order))))
  }, numeric(1))
  arima <- if (anyNA(orders) || any(orders < 0 | orders != floor(orders))) NA_character_ else
    paste0("(", paste(orders[1:3], collapse = " "), ")(", paste(orders[4:6], collapse = " "), ")")
  log <- diagnostic_at(output, c("preprocessing", "description", "log"))
  if (identical(x$result$backend, "native")) log <- x$result$model$log %||% x$result$provenance$log
  if (!is.logical(log) || length(log) != 1L) log <- NA
  residual_test <- function(key) {
    paths <- list(c("user_defined", paste0("residuals.", key)), c("preprocessing", "diagnostics", key))
    if (identical(x$result$backend, "native")) paths <- c(list(c("residuals", key)), paths)
    test(paths, "normality")
  }
  list(
    normality = residual_test("doornikhansen"),
    independence = residual_test("lb"),
    seasonality = category(list(c("user_defined", "diagnostics.seas-si-combined"))),
    residual_qs = metric("diagnostics.seas-sa-qs", c("diagnostics", "seas.qstest.sa")),
    residual_f = metric("diagnostics.seas-sa-f", c("diagnostics", "seas.ftest.sa")),
    td_regression = metric("regression.td-ftest", rule = "regression_td"),
    td_sa_all = metric("diagnostics.td-sa-all", c("diagnostics", "td.ftest.sa")),
    td_sa_last = metric("diagnostics.td-sa-last"),
    td_i_all = metric("diagnostics.td-i-all", c("diagnostics", "td.ftest.i")),
    td_i_last = metric("diagnostics.td-i-last"),
    td_sa_f = metric("diagnostics.td-sa-all", c("diagnostics", "td.ftest.sa")),
    q = number(list(c("mstats", "q"), c("user_defined", "m-statistics.q"))),
    q2 = number(list(c("mstats", "qm2"), c("user_defined", "m-statistics.q-m2"))),
    quality = category(list(c("user_defined", "quality.summary"))),
    arima = arima, log = log)
}

diagnostic_format <- function(value, digits = 3L) {
  if (length(value) != 1L || is.na(value)) return("Undefined")
  if (is.logical(value)) return(if (value) "Yes" else "No")
  if (is.numeric(value)) {
    if (!is.finite(value)) return("Undefined")
    return(formatC(value, digits = digits, format = "f"))
  }
  as.character(value)
}

diagnostic_format_p <- function(value, digits = 4L) {
  diagnostic_format(diagnostic_probability(value), digits)
}

diagnostic_dashboard_values <- function(x) {
  metrics <- diagnostic_metrics(x)
  list(q1 = metrics$q$value, q2 = metrics$q2$value,
       log_txt = diagnostic_format(metrics$log), arima_txt = diagnostic_format(metrics$arima),
       seasonality = metrics$seasonality$value, quality = metrics$quality$value,
       p_norm = metrics$normality$pvalue, p_indep = metrics$independence$pvalue,
       p_td = metrics$td_regression$pvalue, p_seas_qs = metrics$residual_qs$pvalue,
       p_seas_f = metrics$residual_f$pvalue, p_residual_td = metrics$td_sa_f$pvalue,
       st_norm = metrics$normality$status, st_indep = metrics$independence$status,
       st_seas = metrics$seasonality$status, st_td = metrics$td_regression$status,
       st_qs = metrics$residual_qs$status, st_sf = metrics$residual_f$status,
       st_residual_td = metrics$td_sa_f$status,
       st_q1 = metrics$q$status, st_q2 = metrics$q2$status)
}

# An absent preprocessing description is unknown, not evidence of zero outliers.
diagnostic_outliers <- function(x) {
  if (identical(x$result$backend, "native")) return(native_outlier_table(x))
  if (is.null(diagnostic_at(x$output, c("preprocessing", "description")))) return(NULL)
  getOutliers(x)
}
