#' Generates figures for a EUROSTAT quality report
#'
#' Outliers are ranked by absolute estimated t-statistic. Fixed coefficients
#' without an estimated standard error follow ranked entries in stable input
#' order. At most three labels are returned. Overall quality is the backend's
#' own result. The stage-2 Henderson filter is unavailable in the current output
#' contract and is not reconstructed from an unverified rule.
#' @keywords internal
#' @param x an object of class [persephone].
generate_Qr_List <- function(x) {
  if (is.null(x$output)) stop("No results from run available.\n")
  if (!x$method %in% c("x13", "tramoseats")) stop("Unsupported adjustment method.", call. = FALSE)
  metrics <- diagnostic_metrics(x)
  outliers <- diagnostic_outliers(x)
  outliers3 <- rep(NA_character_, 3L)
  nout <- if (is.null(outliers)) NA_integer_ else nrow(outliers)
  if (!is.na(nout) && nout > 0L) {
    ranked <- order(-abs(outliers$t), na.last = TRUE)
    selected <- head(ranked, 3L)
    outliers3[seq_along(selected)] <- outliers$name[selected]
  }

  final_henderson <- seasonal_filter <- NA_character_
  q_stat <- NA_real_
  if (x$method == "x13") {
    if (identical(x$result$backend, "native")) {
      stage <- x$result$selection$d
      henderson <- diagnostic_number(stage$final_henderson_length)
      codes <- stage$seas_filter
      filter <- if (length(codes) && all(codes %in% c(0, 1, 3, 5, 9, 15))) {
        ifelse(codes == 0, "FILTER_STABLE", paste0("FILTER_S3X", codes))
      }
    } else {
      henderson <- diagnostic_number(diagnostic_at(x$output, c("decomposition", "final_henderson")))
      filter <- diagnostic_at(x$output, c("decomposition", "final_seasonal"))
    }
    if (!is.na(henderson) && henderson > 0 && henderson == floor(henderson))
      final_henderson <- paste0("H", henderson)
    if (is.character(filter) && length(filter) && all(!is.na(filter))) {
      seasonal_filter <- paste(unique(filter), collapse = ", ")
    }
    q_stat <- round(metrics$q$value, 2)
  }
  significant <- function(metric) {
    if (is.na(metric$pvalue)) NA_character_ else if (metric$pvalue < 0.05) "Yes" else "No"
  }
  user <- x$output$user_defined
  leap_year <- if ("regression.lp" %in% names(user)) {
    value <- user$regression.lp
    if (is.null(value)) FALSE else if (!length(value) || all(is.na(value))) NA else TRUE
  } else NA
  # regression.leaster is the Easter duration; zero means no Easter term.
  moving_holiday <- if ("regression.easter" %in% names(user) && !is.null(user$regression.easter)) {
    if (!length(user$regression.easter) || all(is.na(user$regression.easter))) NA else TRUE
  } else {
    duration <- diagnostic_number(user$regression.leaster)
    if (!is.na(duration) && duration >= 0) duration > 0 else if (
      "regression.easter" %in% names(user) && is.null(user$regression.easter)
    ) FALSE else NA
  }
  ntd <- diagnostic_number(user$regression.ntd)
  if (!is.na(ntd) && (ntd < 0 || ntd != floor(ntd))) ntd <- NA_real_
  if (inherits(x$result, "persephone_native_tramo_fit")) {
    calendar <- x$result$model$calendar
    ntd <- sum(startsWith(names(calendar$coefficients) %||% character(), "td"))
    leap_year <- isTRUE(calendar$leap_year)
    moving_holiday <- isTRUE(calendar$easter)
  } else if (inherits(x$result, "persephone_native_restricted_fit") ||
      (identical(x$result$backend, "native") && x$method == "x13" &&
       identical(x$result$specification$template, "rsa3"))) {
    # Automatic X-13 RSA3 uses no calendar regressors. The fixed
    # X-13 route can carry user-supplied effects without a calendar subtype.
    variables <- x$result$specification$variables
    classes <- vapply(variables, "[[", character(1), "class")
    custom_calendar <- any(vapply(variables, function(v) {
      identical(v$class, "user") && identical(v$effect, "calendar_effect")
    }, logical(1)))
    if (!custom_calendar && !"days_related" %in% classes) {
      leap_year <- FALSE
      ntd <- 0
    }
    if ("moving_holiday" %in% classes) moving_holiday <- TRUE
    else if (!custom_calendar) moving_holiday <- FALSE
  }

  original <- x$ts
  adjusted <- x$adjustedDirect
  max_adj <- NA_real_
  if (length(original) && length(original) == length(adjusted) &&
      stats::is.ts(original) && stats::is.ts(adjusted) &&
      identical(stats::frequency(original), stats::frequency(adjusted)) &&
      identical(stats::start(original), stats::start(adjusted)) &&
      identical(stats::end(original), stats::end(adjusted))) {
    original_values <- as.numeric(original)
    adjusted_values <- as.numeric(adjusted)
    valid <- is.finite(original_values) & is.finite(adjusted_values)
    # Relative adjustment is undefined at a zero original; never print Inf%.
    if (all(valid) && !any(original_values == 0)) {
      max_adj <- 100 * max(abs((original_values - adjusted_values) / original_values))
    }
  }
  list(
    Method = if (x$method == "x13") "x13" else "TS",
    Period = stats::frequency(original), Nobs = length(original),
    Start = paste(rev(stats::start(original)), collapse = "-"),
    End = paste(rev(stats::end(original)), collapse = "-"),
    `Log-Transformation` = metrics$log,
    `ARIMA Model` = metrics$arima,
    LeapYear = leap_year, MovingHoliday = moving_holiday, NbTD = ntd,
    Noutliers = nout, Outlier1 = outliers3[1L], Outlier2 = outliers3[2L], Outlier3 = outliers3[3L],
    CombinedTest_SI = metrics$seasonality$value,
    `Residual Seasonality` = significant(metrics$residual_f),
    `Residual TD Effect` = significant(metrics$td_sa_f),
    `Q-Stat` = q_stat,
    `Final Henderson Filter` = final_henderson,
    `Stage 2 Henderson Filter` = NA_character_,
    `Seasonal Filter` = seasonal_filter,
    Quality = metrics$quality$value,
    `Max-Adj` = if (is.na(max_adj)) NA_character_ else paste0(round(max_adj), "%"),
    `Ljung-Box p-value` = metrics$independence$pvalue,
    `Doornik-Hansen p-value` = metrics$normality$pvalue
  )
}
