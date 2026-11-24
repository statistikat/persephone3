#' Fix ARIMA model orders
#'
#' @description Keeps the fitted ARIMA orders and disables automatic order
#'   selection. This does not fix free coefficient values; they can be
#'   re-estimated on the next run. Existing fixed-coefficient constraints remain.
#'
#' @details Native X-13 RSA0, RSA1 and RSA2c and TRAMO-SEATS RSA0 and RSA1
#' fix the airline orders when the model is constructed. Native RSA3 and RSAfull require automatic order selection. The restricted
#' X-13 decomposition receives its preprocessing inputs from the caller.
#'
#' @param x a persephone object
#' @param verbose if TRUE the changed parameters will be reported
#'
#' @export
fixModel <- function(x, verbose = FALSE) {
  if (is.null(x$output)) {
    warning("not run yet.")
    return(invisible(NULL))
  }
  if (identical(x$result$backend, "native")) {
    if (inherits(x$result, "persephone_native_estimated_fit") ||
        (inherits(x$result, "persephone_native_tramo_fit") && x$result$specification$template %in% c("rsa0", "rsa1"))) {
      if (verbose) message(toupper(x$result$specification$template), " already fixes the airline model orders.")
      current <- if (inherits(x, "hierarchicalTimeSeries")) x$paramsDirect else x$params
      return(invisible(current))
    }
    if (inherits(x$result, "persephone_native_restricted_fit"))
      stop("The restricted X-13 fit has no estimated ARIMA orders to fix.", call. = FALSE)
    backend_native_refusal("fixing estimated ARIMA orders", x$method, "automatic_spec_pinned",
                           paste0("the ", x$result$specification$template, " route requires automatic order selection;"))
  }
  orders <- x$output$user_defined
  if (verbose) {
    message(sprintf("Fixing ARIMA orders (%d %d %d)(%d %d %d).", orders$arima.p,
      orders$arima.d, orders$arima.q, orders$arima.bp, orders$arima.bd, orders$arima.bq))
  }
  x$updateParams(arima.p = orders$arima.p, arima.d = orders$arima.d, arima.q = orders$arima.q,
                 arima.bp = orders$arima.bp, arima.bd = orders$arima.bd, arima.bq = orders$arima.bq,
                 automdl.enabled = FALSE)
}
