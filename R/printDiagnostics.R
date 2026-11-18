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
# are not available for the indirect adjustment, but the series is tested on
# residual seasonality and trading day effects. The number of outliers is the
# number of the unique outliers (`outlierNames`, e.g. "AO1985.03") of the
# sub-series the aggregate is built from.
indirectAggDiagnostics <- function(component, series,
                                   outlierNames = character(0)) {
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
    nOutliers = length(unique(outlierNames)),
    Q = NA_real_,
    Q2 = NA_real_,
    TotalQ = NA_real_,
    seasTest = unname(tests[["seasTest"]]),
    tdTest = unname(tests[["tdTest"]]),
    stringsAsFactors = FALSE
  )
}

# the outlier names (e.g. "AO1985.03") of a single series, character(0) if it
# is not run yet or has no outliers
seriesOutlierNames <- function(node) {
  tryCatch({
    out <- getOutliers(node)
    if (is.null(out)) character(0) else as.character(out$name)
  }, error = function(e) character(0))
}

# the unique outlier names of all leaf series that build the indirect
# aggregate of `node`: the leaf components of the object in the classic mode,
# the leaf series under the root aggregates in `groups` mode (the aggregates
# of a `groups` structure are not run in "indirect" mode and do not contribute
# their own outliers)
leafOutlierNames <- function(node) {
  if (!inherits(node, "hierarchicalTimeSeries")) {
    return(seriesOutlierNames(node))
  }
  if (!is.null(node$groups)) {
    aggs <- names(node$groups)
    collect <- function(nm) {
      if (nm %in% aggs) {
        unique(unlist(lapply(node$groups[[nm]], collect)))
      } else {
        seriesOutlierNames(node$getComponent(nm))
      }
    }
    roots <- cl_groups_roots(node$groups)
    return(unique(unlist(lapply(roots, collect))))
  }
  unique(unlist(lapply(node$components, leafOutlierNames)))
}

# the unique outlier names of the sub-series of every aggregate row of the
# quality overview of a hierarchical object, named by the component id of the
# row (in the same way as indirectSeriesMap())
indirectOutlierMap <- function(node, prefix = "") {
  if (!inherits(node, "hierarchicalTimeSeries")) {
    return(NULL)
  }
  if (!is.null(node$groups)) {
    aggs <- node$getSeries(aggregates = TRUE)
    groupNames <- names(node$groups)
    collect <- function(nm) {
      if (nm %in% groupNames) {
        unique(unlist(lapply(node$groups[[nm]], collect)))
      } else {
        seriesOutlierNames(node$getComponent(nm))
      }
    }
    out <- lapply(
      aggs,
      function(agg) unique(unlist(lapply(node$groups[[agg]], collect)))
    )
    names(out) <- vapply(aggs, aggPath, "", prefix = prefix)
    return(out)
  }
  out <- list(leafOutlierNames(node))
  names(out) <- prefix
  for (nm in names(node$components)) {
    out <- c(out, indirectOutlierMap(
      node$components[[nm]], aggPath(prefix, nm)
    ))
  }
  out
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
    Q = ifelse(is.null(qStat), NA, round(qStat, digits = 2)),
    Q2 = ifelse(is.null(qStat2), NA, round(qStat2, digits = 2)),
    TotalQ = userdef$quality.summary
  )

}
