#' @description fix the arima model
#' @param x an object of class [persephone].
#' @param verbose if TRUE the changed parameters will be reported
fixModel = function(x, verbose = FALSE) {
  if(is.null(x$output)){
    warning("not run yet.")
    return(invisible(NULL))
  }
  p <-x$output$user_defined$arima.p
  d <-x$output$user_defined$arima.d
  q <-x$output$user_defined$arima.q
  bp <-x$output$user_defined$arima.bp
  bd <-x$output$user_defined$arima.bd
  bq <-x$output$user_defined$arima.bq
  # if(x$output$regarima$specification$arima$specification$enabled){
    if(verbose){
      message("The model", paste("(",p,d,q,") (",bp,bd,bq,") is now fixed."))
    }
    x$updateParams(arima.p=p,arima.d=d,arima.q=q,
                   arima.bp=bp,arima.bd=bd,arima.bq=bq,
                   automdl.enabled = FALSE
    )

  # }else if(verbose){
  #   message("The model", paste("(",p,d,q,") (",bp,bd,bq,") was already fixed."))
  # }
}

# noch an rjd3 anpassen; bzw. überhaupt anders aufziehen, Logik ist jetzt etwas anders.

# x$params$regarima$automodel

# Identifier |	Log/level detection |	Outliers detection |	Calendar effects |	ARIMA
# RSA1 |	automatic |	AO/LS/TC |	NA |	Airline(+mean)
# RSA3 |	automatic |	AO/LS/TC |	NA |	automatic

# # Nach fixModel():
# obj$fixModel()
#
# # Spezifikation extrahieren:
# fixed_spec <- obj$params
#
# # Für neue Daten wiederverwenden:
# # so in etwa, evt bei template eigenes spec zulassen
# # evt in template?
# perX13(
#   ts,
#   template = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"),
#   context = NULL,
#   userdefined = NULL,
#   ...
# )
# new_obj <- perX13(new_series, spec = fixed_spec)
# new_obj$run()
#
#
#
# saveRDS(obj, "fixed_model.rds")
#
# # Später laden:
# obj <- readRDS("fixed_model.rds")
#
# # Neue Daten anhängen:
# # oder so, mit eigener Funktion, wo das obj selbst gespeichert werden kann und
# # auf die neue Zeitreihe dann die alten specs angewandt werden
# obj$updateSeries(new_series)
# obj$run()
