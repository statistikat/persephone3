# Numeric values, categories, missing data and presentation share one contract.
local({
  classify <- persephone3:::diagnostic_classify
  p <- c(0, 0.000999, 0.001, 0.009999, 0.01, 0.049999, 0.05, 0.099999, 0.1, 1)
  expect_identical(vapply(p, classify, "", rule = "normality"),
                   c("Severe", "Severe", "Bad", "Bad", "Uncertain", "Uncertain",
                     "Uncertain", "Uncertain", "Good", "Good"))
  expect_identical(vapply(p, classify, "", rule = "residual"),
                   c("Severe", "Severe", "Severe", "Severe", "Bad", "Bad",
                     "Uncertain", "Uncertain", "Good", "Good"))
  expect_identical(vapply(p, classify, "", rule = "regression_td"),
                   c(rep("Good", 6), "Uncertain", "Uncertain", "Bad", "Bad"))
  for (value in list(NULL, NA_real_, NaN, Inf, -0.1, 1.1, "0.01", c(.01, .02))) {
    for (rule in c("normality", "residual", "regression_td")) {
      expect_identical(classify(value, rule), "Undefined")
    }
  }
  expect_identical(vapply(c(0, .02999, .03, .04999, .05), classify, "", rule = "outliers"),
                   c("Good", "Good", "Uncertain", "Uncertain", "Severe"))
  expect_identical(vapply(c(0, .99999, 1, 1.99999, 2), classify, "", rule = "q"),
                   c("Good", "Good", "Bad", "Bad", "Severe"))
})

diagnostic_fixture <- function(method = "x13", frequency = 12, kind = "fixed") {
  path <- system.file("tinytest", "fixtures", "backend",
                      paste0(method, "-", frequency, "-", kind, ".rds"), package = "persephone3")
  fixture <- readRDS(path)
  list(output = fixture$raw_result, method = fixture$method, ts = fixture$input,
       params = fixture$specification, adjustedDirect = fixture$raw_result$user_defined$sa)
}

# Stored outputs were generated directly by the pinned rjd3 reference engines;
# these comparisons also run when Java and the optional wrappers are absent.
for (method in c("x13", "tramoseats")) {
  for (frequency in c(4, 12)) {
    x <- diagnostic_fixture(method, frequency, "automatic")
    raw <- x$output
    metrics <- persephone3:::diagnostic_metrics(x)
    table <- persephone3:::generate_diagnostics_table(x)
    report <- persephone3:::generate_Qr_List(x)
    expect_identical(metrics$normality$pvalue, raw$user_defined$residuals.doornikhansen$pvalue)
    expect_identical(metrics$normality$statistic, raw$user_defined$residuals.doornikhansen$value)
    expect_identical(metrics$independence$pvalue, raw$user_defined$residuals.lb$pvalue)
    expect_identical(metrics$residual_f$pvalue, raw$diagnostics$seas.ftest.sa$pvalue)
    expect_identical(metrics$td_sa_f$pvalue, raw$diagnostics$td.ftest.sa$pvalue)
    expect_identical(metrics$seasonality$kind, "category")
    expect_identical(metrics$seasonality$value, raw$user_defined[["diagnostics.seas-si-combined"]])
    expect_identical(table$seasonality, raw$user_defined[["diagnostics.seas-si-combined"]])
    expect_identical(report$CombinedTest_SI, table$seasonality)
    expect_identical(report$Quality, raw$user_defined$quality.summary)
    expect_identical(table$TotalQ, raw$user_defined$quality.summary)
    expect_identical(report$`Residual Seasonality`,
                     if (raw$diagnostics$seas.ftest.sa$pvalue < .05) "Yes" else "No")
    expect_identical(report$`Residual TD Effect`,
                     if (raw$diagnostics$td.ftest.sa$pvalue < .05) "Yes" else "No")
    expect_identical(report$`Stage 2 Henderson Filter`, NA_character_)
    if (method == "x13") {
      expect_identical(table$Q, round(raw$mstats$q, 2))
      expect_identical(report$`Final Henderson Filter`, paste0("H", raw$decomposition$final_henderson))
    } else {
      expect_identical(report$`Final Henderson Filter`, NA_character_)
      expect_identical(report$`Q-Stat`, NA_real_)
    }
  }
}

local({
  x <- diagnostic_fixture()
  # Equal p-values intentionally have opposite interpretations: significant
  # regression TD supports included regressors; significant residual TD is bad.
  keys <- c("residuals.doornikhansen", "residuals.lb", "diagnostics.seas-sa-qs",
            "diagnostics.seas-sa-f", "regression.td-ftest", "diagnostics.td-sa-all",
            "diagnostics.td-sa-last", "diagnostics.td-i-all", "diagnostics.td-i-last")
  for (p in c(.001, .01, .05, .1)) {
    for (key in keys) x$output$user_defined[[key]] <- list(value = 12.3, pvalue = p)
    table <- persephone3:::generate_diagnostics_table(x)
    values <- persephone3:::diagnostic_dashboard_values(x)
    report <- persephone3:::generate_Qr_List(x)
    normal <- if (p == .001) "Bad" else if (p < .1) "Uncertain" else "Good"
    residual <- if (p < .01) "Severe" else if (p < .05) "Bad" else if (p < .1) "Uncertain" else "Good"
    regression <- if (p < .05) "Good" else if (p < .1) "Uncertain" else "Bad"
    expect_identical(table$normality, normal)
    expect_identical(table$independence, normal)
    expect_identical(unname(unlist(table[c("QS_seas", "F_seas", "combSA_all", "combSA_last",
                                           "combI_all", "combI_last", "fTestSA_td")])), rep(residual, 7))
    expect_identical(table$TD_Ftest, p)
    expect_identical(table$TD_status, regression)
    expect_identical(values$p_td, p)
    expect_identical(values$st_td, regression)
    expect_identical(values$p_residual_td, p)
    expect_identical(values$st_residual_td, residual)
    expect_identical(report$`Residual TD Effect`, if (p < .05) "Yes" else "No")
  }
  x$output$user_defined[["diagnostics.seas-si-combined"]] <- "Absent"
  x$output$user_defined$quality.summary <- "Severe"
  x$output$mstats$q <- 0
  metrics <- persephone3:::diagnostic_metrics(x)
  expect_identical(metrics$seasonality$value, "Absent")
  expect_identical(metrics$seasonality$status, "")
  expect_identical(metrics$q$status, "Good")
  expect_identical(metrics$quality$value, "Severe")
  expect_identical(persephone3:::generate_Qr_List(x)$Quality, "Severe")
  # An explicitly missing primary metric is not replaced by a different test.
  x$output$user_defined$residuals.lb <- list(value = 2, pvalue = NA_real_)
  expect_identical(persephone3:::diagnostic_metrics(x)$independence$status, "Undefined")
  x$output$user_defined$residuals.lb <- NULL
  expect_identical(persephone3:::diagnostic_metrics(x)$independence$pvalue,
                   x$output$preprocessing$diagnostics$lb$pvalue)
  x$output$user_defined[["regression.td-ftest"]] <- "Present"
  expect_identical(persephone3:::diagnostic_metrics(x)$td_regression$pvalue, NA_real_)
  x$output$user_defined[["diagnostics.seas-si-combined"]] <- .01
  expect_identical(persephone3:::diagnostic_metrics(x)$seasonality$value, NA_character_)
})

# Report counts, ranking, absent calendar effects and missing/zero values.
local({
  x <- diagnostic_fixture()
  report <- persephone3:::generate_Qr_List(x)
  expect_identical(report$Noutliers, 0L)
  expect_identical(unname(unlist(report[c("Outlier1", "Outlier2", "Outlier3")])), rep(NA_character_, 3))
  expect_identical(report$LeapYear, FALSE)
  expect_identical(report$MovingHoliday, FALSE)
  expect_identical(report$NbTD, 0)
  x$output$user_defined$regression.lp <- 0
  x$output$user_defined$regression.easter <- 0
  expect_identical(persephone3:::generate_Qr_List(x)$LeapYear, TRUE)
  expect_identical(persephone3:::generate_Qr_List(x)$MovingHoliday, TRUE)

  values <- c(100, 2, 3, 4)
  x$output$preprocessing$description$variables <- lapply(seq_along(values), function(i) {
    list(name = paste0("Outlier ", i), type = "AO", pos = paste0(2002 + i, "-01-01"),
         coef = data.frame(value = values[i], type = "ESTIMATED"))
  })
  x$output$preprocessing$estimation$bvar <- diag(c(10000, .01, .09, 1))
  report <- persephone3:::generate_Qr_List(x)
  expect_identical(report$Noutliers, 4L)
  expect_identical(unname(unlist(report[c("Outlier1", "Outlier2", "Outlier3")])),
                   c("AO2004.1", "AO2005.1", "AO2006.1"))
  x$output$preprocessing$description$variables[[1]]$coef$type <- "FIXED"
  x$output$preprocessing$estimation$bvar <- diag(c(.01, .09, 1))
  report <- persephone3:::generate_Qr_List(x)
  expect_identical(unname(unlist(report[c("Outlier1", "Outlier2", "Outlier3")])),
                   c("AO2004.1", "AO2005.1", "AO2006.1"))

  x$ts <- stats::ts(c(0, rep(100, 179)), start = c(2001, 1), frequency = 12)
  x$adjustedDirect <- x$ts
  expect_identical(persephone3:::generate_Qr_List(x)$`Max-Adj`, NA_character_)
  x$ts[] <- 100
  x$adjustedDirect[] <- 90
  expect_identical(persephone3:::generate_Qr_List(x)$`Max-Adj`, "10%")
  x$adjustedDirect[1] <- NA_real_
  expect_identical(persephone3:::generate_Qr_List(x)$`Max-Adj`, NA_character_)
  x$output <- list(unavailable = TRUE)
  report <- persephone3:::generate_Qr_List(x)
  expect_identical(report$Noutliers, NA_integer_)
  expect_identical(report$Quality, NA_character_)
  expect_identical(report$CombinedTest_SI, NA_character_)
  expect_identical(report$LeapYear, NA)
  expect_identical(report$MovingHoliday, NA)
  expect_identical(report$NbTD, NA_real_)
  expect_identical(report$`Residual Seasonality`, NA_character_)
  expect_identical(persephone3:::generate_diagnostics_table(x)$nOutliers_Q, "Undefined")
})

# Inspect actual card children, not occurrences of labels in CSS/JavaScript.
diagnostic_cards <- function(ui, label) {
  found <- list()
  has_class <- function(node, class) {
    inherits(node, "shiny.tag") && class %in% strsplit(node$attribs$class %||% "", " ", fixed = TRUE)[[1]]
  }
  `%||%` <- function(x, y) if (is.null(x)) y else x
  text <- function(node) paste(unlist(node$children), collapse = "")
  walk <- function(node) {
    if (inherits(node, "shiny.tag")) {
      labels <- Filter(function(child) has_class(child, "box-label"), node$children)
      if (length(labels) && identical(text(labels[[1]]), label)) {
        values <- Filter(function(child) has_class(child, "box-value"), node$children)
        statuses <- Filter(function(child) has_class(child, "box-status"), node$children)
        found[[length(found) + 1L]] <<- c(value = text(values[[1]]),
                                         status = if (length(statuses)) text(statuses[[1]]) else "")
      }
      lapply(node$children, walk)
    } else if (is.list(node) && !inherits(node, "htmlwidget")) lapply(node, walk)
    invisible(NULL)
  }
  walk(ui)
  found
}

if (all(vapply(c("rjd3x13", "rjd3tramoseats"), requireNamespace, logical(1), quietly = TRUE))) {
  # Live fits ensure the public surfaces still accept both supported engines.
  fitted <- list(perX13(AirPassengers, "rsa0", backend = "rjd3"),
                 perTramo(AirPassengers, "rsa0", backend = "rjd3"))
  for (x in fitted) {
    x$run()
    expect_identical(persephone3:::diagnostic_metrics(x)$normality$pvalue,
                     x$output$user_defined$residuals.doornikhansen$pvalue)
    raw <- x$output
    raw$user_defined[["regression.td-ftest"]] <- list(value = 8, pvalue = .01)
    raw$user_defined[["diagnostics.td-sa-all"]] <- list(value = 9, pvalue = .01)
    raw$user_defined[["diagnostics.seas-si-combined"]] <- "Present"
    raw$user_defined$quality.summary <- "Severe"
    raw$mstats$q <- 0
    x$.__enclos_env__$private$output_internal <- raw
    ui <- dashboard_single(x)
    expect_identical(diagnostic_cards(ui, "TD regression F-test"), list(c(value = "0.0100", status = "Good")))
    expect_identical(diagnostic_cards(ui, "Residual TD F-test"), list(c(value = "0.0100", status = "Bad")))
    expect_identical(diagnostic_cards(ui, "Combined seasonality"), list(c(value = "Present", status = "")))
    expect_identical(diagnostic_cards(ui, "Quality summary"), list(c(value = "Severe", status = "")))
    expect_identical(diagnosticsTable(x)$TD_status, "Good")
    expect_identical(diagnosticsTable(x)$fTestSA_td, "Bad")
  }
  raw <- fitted[[2]]$output
  raw$user_defined[["regression.td-ftest"]]$pvalue <- .1
  raw$user_defined[["diagnostics.td-sa-all"]]$pvalue <- .1
  raw$user_defined[["diagnostics.seas-si-combined"]] <- "Absent"
  fitted[[2]]$.__enclos_env__$private$output_internal <- raw
  ui <- dashboard_compare(fitted[[1]], fitted[[2]])
  expect_identical(diagnostic_cards(ui, "TD regression F-test"),
                   list(c(value = "0.0100", status = "Good"), c(value = "0.1000", status = "Bad")))
  expect_identical(diagnostic_cards(ui, "Residual TD F-test"),
                   list(c(value = "0.0100", status = "Bad"), c(value = "0.1000", status = "Good")))
  expect_identical(diagnostic_cards(ui, "Combined seasonality"),
                   list(c(value = "Present", status = ""), c(value = "Absent", status = "")))
}
