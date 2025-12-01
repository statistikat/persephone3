#' @description fix the arima model
#' @param x an object of class [persephone].
#' @param verbose if TRUE the changed parameters will be reported
fixModel = function(x, verbose = FALSE) {
  if(is.null(x$output)){
    warning("not run yet.")
    return(invisible(NULL))
  }
  p <- x$output$regarima$specification$arima$specification$arima.p
  d <- x$output$regarima$specification$arima$specification$arima.d
  q <- x$output$regarima$specification$arima$specification$arima.q
  bp <- x$output$regarima$specification$arima$specification$arima.bp
  bd <- x$output$regarima$specification$arima$specification$arima.bd
  bq <- x$output$regarima$specification$arima$specification$arima.bq
  if(x$output$regarima$specification$arima$specification$enabled){
    if(verbose){
      message("The model", paste("(",p,d,q,") (",bp,bd,bq,") is now fixed."))
    }
    x$updateParams(arima.p=p,arima.d=d,arima.q=q,
                   arima.bp=bp,arima.bd=bd,arima.bq=bq,
                   automdl.enabled = FALSE
    )

  }else if(verbose){
    message("The model", paste("(",p,d,q,") (",bp,bd,bq,") was already fixed."))
  }
}

# noch an rjd3 anpassen
