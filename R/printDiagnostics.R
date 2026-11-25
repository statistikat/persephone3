# join a component prefix ("" for the root of a hierarchical object) with the
# name of a sub-series, as asTable_nested_list() builds the component ids
aggPath <- function(prefix, name) {
  if (nzchar(prefix)) paste0(prefix, "/", name) else name
}

# tests on residual seasonality and on the trading day effect of a seasonally
# adjusted series: p-values of the Qs test and of the F test of the trading
# day regression (rjd3toolkit). Used for every series of the quality overview
# of a hierarchical object; NA if the series is not available or a test fails.
adjustedTests <- function(x) {
  seas <- NA_real_
  td <- NA_real_
  if (stats::is.ts(x) && !all(is.na(x))) {
    seas <- tryCatch(
      suppressWarnings(rjd3toolkit::seasonality_qs(x)$pvalue),
      error = function(e) NA_real_
    )
    td <- tryCatch(
      suppressWarnings(rjd3toolkit::td_f(x, model = "WN")$pvalue),
      error = function(e) NA_real_
    )
  }
  c(seasTest = round(seas, digits = 3), tdTest = round(td, digits = 3))
}

# one row of the quality overview for the indirectly adjusted aggregate
# `component` with the adjusted series `series`. The indicators of a model fit
# are not available for the indirect adjustment (an indirect aggregate has no
# model of its own, so its outlier counts are NA as well), but the series is
# tested on residual seasonality and trading day effects.
indirectAggDiagnostics <- function(component, series) {
  tests <- adjustedTests(series)
  data.frame(
    component = component,
    run = TRUE,
    method = NA_character_,
    type = "aggregate",
    agg = "indirect",
    seasonality = NA_real_,
    logTransform = NA,
    arimaMdl = NA_character_,
    nOutliers = NA_real_,
    nAutoout = NA,
    nRecentout = NA,
    Q = NA_real_,
    Q2 = NA_real_,
    TotalQ = NA_real_,
    seasTest = unname(tests[["seasTest"]]),
    tdTest = unname(tests[["tdTest"]]),
    stringsAsFactors = FALSE
  )
}

# the adjusted series that is directly computed for every row of the quality
# overview of a hierarchical object, named by the component id of the row: in
# `groups` mode the adjusted series of every series of the structure, in the
# classic mode the directly adjusted aggregate and, recursively, the adjusted
# series of its components.
directSeriesMap <- function(node, prefix = "") {
  if (!inherits(node, "hierarchicalTimeSeries")) {
    out <- list(node[["adjusted"]])
    names(out) <- prefix
    return(out)
  }
  if (!is.null(node$groups)) {
    ids <- node$getSeries()
    out <- lapply(ids, function(id) node$getComponent(id)[["adjusted"]])
    names(out) <- vapply(ids, aggPath, "", prefix = prefix)
    return(out)
  }
  out <- list(node$adjustedDirect)
  names(out) <- prefix
  for (nm in names(node$components)) {
    out <- c(out, directSeriesMap(node$components[[nm]], aggPath(prefix, nm)))
  }
  out
}

# the indirectly adjusted series of every aggregate row of the quality
# overview of a hierarchical object, named by the component id of the row;
# NULL for an aggregate whose sub-series are not run yet
indirectSeriesMap <- function(node, prefix = "") {
  if (!inherits(node, "hierarchicalTimeSeries")) {
    return(NULL)
  }
  if (!is.null(node$groups)) {
    aggs <- node$getSeries(aggregates = TRUE)
    out <- lapply(aggs, function(agg) {
      tryCatch(node$getAdjustedIndirect(agg), error = function(e) NULL)
    })
    names(out) <- vapply(aggs, aggPath, "", prefix = prefix)
    return(out)
  }
  out <- list(tryCatch(node$adjustedIndirect, error = function(e) NULL))
  names(out) <- prefix
  for (nm in names(node$components)) {
    out <- c(out, indirectSeriesMap(node$components[[nm]], aggPath(prefix, nm)))
  }
  out
}

# the component ids of the rows of the quality overview that belong to an
# aggregate: the aggregates of the `groups` mode; in the classic mode the
# aggregate of the object itself and, recursively, the aggregates of its
# hierarchical components
directAggIds <- function(node, prefix = "") {
  if (!inherits(node, "hierarchicalTimeSeries")) {
    return(character(0))
  }
  if (!is.null(node$groups)) {
    return(vapply(names(node$groups), aggPath, "", prefix = prefix))
  }
  out <- prefix
  for (nm in names(node$components)) {
    out <- c(out, directAggIds(node$components[[nm]], aggPath(prefix, nm)))
  }
  out
}

printDiagnostics <- function(x) {
  #browser()
  type <- ifelse(grepl("Single",class(x)[1]),  "single" , "aggregate")

  if (is.null(x$output) | length(x$output)==0)
    return(data.frame(
      run = !is.null(x$output),
      #class = class(x)[1],
      method = x$method,
      type = type,
      seasonality = NA,
      logTransform = NA,
      arimaMdl = NA,
      nOutliers = NA,
      nAutoout = NA,
      nRecentout = NA,
      Q = NA,
      Q2 = NA,
      TotalQ = NA
    ))

  userdef <-  x$output$user_defined
  preproc <-  x$output$preprocessing


  bpbdbq <- paste0("(", userdef$arima.p, " ",
                   userdef$arima.d, " ",
                   userdef$arima.q, ")",
                   "(", userdef$arima.bp, " ",
                   userdef$arima.bd, " ",
                   userdef$arima.bq, ")")
  qStat  <- x$output$mstats$q
  qStat2 <- x$output$mstats$qm2
  out <- getOutliers(x)
  data.frame(
    run = !is.null(x$output),
    #class = class(x)[1],
    method = x$method,
    type = type,
    # seasonality: placeholder (test for stable seasonality)
    seasonality = userdef$`diagnostics.seas-si-combined`,
    logTransform = preproc$description$log,
    arimaMdl = bpbdbq,
    nOutliers = ifelse(is.null(nrow(out)), NA, nrow(out)),
    nAutoout = ifelse(is.null(nrow(out)), NA, sum(out$autoout)),
    nRecentout = ifelse(is.null(nrow(out)), NA, sum(out$recentout)),
    Q = ifelse(is.null(qStat), NA, round(qStat, digits = 2)),
    Q2 = ifelse(is.null(qStat2), NA, round(qStat2, digits = 2)),
    TotalQ = userdef$quality.summary
  )

}
