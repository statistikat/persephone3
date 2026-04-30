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
