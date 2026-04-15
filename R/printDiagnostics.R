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
      normality = NA,
      independence = NA,
      QS_seas = NA,
      F_seas = NA,
      combSA_last = NA,
      combSA_all = NA,
      combI_all = NA,
      combI_last = NA,
      fTestSA_td = NA,
      TD_Ftest = NA,
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
  classify_norm_indep <- function(p) {
        if (is.null(p) || length(p) == 0 || is.na(p)) return("Undefined")
        p <- as.numeric(p[1])
        if      (p < 0.001) "Severe"
        else if (p < 0.01)  "Bad"
        else if (p < 0.1)   "Uncertain"
        else                "Good"
      }
  classify_seas_td <- function(p) {
        if (is.null(p) || length(p) == 0 || is.na(p)) return("Undefined")
        p <- as.numeric(p[1])
        if      (p < 0.01) "Severe"
        else if (p < 0.05) "Bad"
        else if (p < 0.1)  "Uncertain"
        else               "Good"
      }
  classify_outl <- function(p) {
        if (is.null(p) || length(p) == 0 || is.na(p)) return("Undefined")
        p <- as.numeric(p[1])
        if      (p < 0.03) "Good"
        else if (p < 0.05) "Uncertain"
        else               "Severe"
      }  
  classify_p <- function(p) {
        if (is.null(p) || length(p) == 0 || is.na(p)) return("Undefined")
        p <- as.numeric(p[1])
        if      (p < 0.05) "Good"
        else               "Bad"
  }
  qStat  <- x$output$mstats$q
  qStat2 <- x$output$mstats$qm2
  #out <- getOutliers(x)
  out_p <- userdef$regression.nout/length(x$ts)
  fTest <- userdef$`regression.td-ftest`$pvalue[1]
  data.frame(
    run = !is.null(x$output),
    #class = class(x)[1],
    method = x$method,
    type = type,
    # seasonality: placeholder (test for stable seasonality)
    seasonality = userdef$`diagnostics.seas-si-combined`,
    normality = classify_norm_indep(userdef$residuals.doornikhansen$pvalue),
    independence = classify_norm_indep(userdef$residuals.lb$pvalue),
    QS_seas = classify_seas_td(userdef$`diagnostics.seas-sa-qs`$pvalue),
    F_seas = classify_seas_td(userdef$`diagnostics.seas-sa-f`$pvalue),
    combSA_all = classify_p(userdef$`diagnostics.td-sa-all`$pvalue),
    combSA_last = classify_p(userdef$`diagnostics.td-sa-last`$pvalue),
    combI_all = classify_p(userdef$`diagnostics.td-i-all`$pvalue),
    combI_last = classify_p(userdef$`diagnostics.td-i-last`$pvalue),
    fTestSA_td = classify_norm_indep(x$output$diagnostics$td.ftest.sa$pvalue),
    TD_Ftest = ifelse(is.null(fTest), NA, round(fTest, 2)),
    logTransform = preproc$description$log,
    arimaMdl = bpbdbq,
    #nOutliers = ifelse(is.null(nrow(out)), NA, nrow(out)),
    nOutliers = classify_outl(out_p),
    Q = ifelse(is.null(qStat), NA, round(qStat, digits = 2)),
    Q2 = ifelse(is.null(qStat2), NA, round(qStat2, digits = 2)),
    TotalQ = userdef$quality.summary
  )

}