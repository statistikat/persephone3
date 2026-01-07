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
      qStat = NA
    ))

  userdef <-  x$output$user_defined

  bpbdbq <- paste0("(", userdef$arima.p, " ",
                   userdef$arima.d, " ",
                   userdef$arima.q, ")",
                   "(", userdef$arima.bp, " ",
                   userdef$arima.bd, " ",
                   userdef$arima.bq, ")")

  qStat  <- x$output$mstats$q
  out <- getOutliers(x)
  data.frame(
    run = !is.null(x$output),
    #class = class(x)[1],
    method = x$method,
    type = type,
    # seasonality: placeholder (test for stable seasonality)
    seasonality = userdef$`diagnostics.seas-si-combined`,
    logTransform = x$output$preprocessing$description$log,
    arimaMdl = bpbdbq,
    nOutliers = ifelse(is.null(nrow(out)), NA, nrow(out)),
    qStat = ifelse(is.null(qStat), NA, round(qStat, digits = 2))
  )

}
