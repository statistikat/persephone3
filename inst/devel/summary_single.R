#' @export
summary.persephoneSingle <- function(x, ...) {

  x$output

  if(is.null(x$output)) {
    stop("No results from run available.\n")
  }
  # tramoseats/x13 distinction
  if (inherits(x$output,"JD3_X13_RSLTS")) { # schauen was gleich bei x13


    # lapply(x$output,names)
    # lapply(x$output$preprocessing,names)
    told <- as.data.frame(x$output$regarima$residuals.stat[2])[,1:2]
    setDT(told)
    names(told) <- c("Statistic","P.value")
    test.Type <- c("Normality","","","Independence","","Linearity")
    told <- cbind(test.Type,told)
    told[, Sig := ""]
    told[P.value >= 0.1, Sig := "***"]
    told[P.value < 0.1 & P.value >=0.05, Sig := "**"]

    dold <- x$output$diagnostics
    # Stable seasonality
    ddef$combined_test

    # Residual seasonality

    dres <- dold$residuals_test[!names(dold$residuals_test)%in%"Description"]
    test.Type <- rownames(dres)
    setDT(dres)
    dres <- cbind(test.Type,dres)
    dres[, Sig := ""]
    dres[P.value >= 0.1, Sig := "***"]
    dres[P.value < 0.1 & P.value >=0.05, Sig := "**"]


  }

}

x <- obj$clone()

# rjdobj# rjd3 summary functions here:
# https://github.com/rjdverse/rjd3toolkit/blob/develop/R/display.R

#' @export
summary.JD3_SARIMA_ESTIMATION <- function(object, ...) {
  tables <- .sarima_coef_table(object, ...)
  class(tables) <- "summary.JD3_SARIMA_ESTIMATION"
  tables
}

#' @importFrom stats printCoefmat
#' @export
print.summary.JD3_SARIMA_ESTIMATION <- function(x, digits = max(3L, getOption("digits") - 3L), signif.stars = getOption("show.signif.stars"), ...) {
  orders <- x$sarima_orders

  cat(
    "SARIMA model:",
    .arima_node(orders$p, orders$d, orders$q),
    .arima_node(orders$bp, orders$bd, orders$bq)
  )
  if (!is.null(orders$period)) { # when sarima_estimate() is used
    cat(sprintf(" [%i]", orders$period))
  }

  cat("\n")
  cat("\nCoefficients\n")
  if (is.null(x$coef_table)) {
    cat("No SARIMA variables\n")
  } else if (ncol(x$coef_table) == 2) {
    print(x$coef_table, ...)
  } else {
    printCoefmat(x$coef_table[-2],
                 digits = digits, signif.stars = signif.stars,
                 na.print = "NA", ...
    )
  }
  invisible(x)
}

#' @export
summary.JD3_LIKELIHOOD <- function(object, ...) {
  res <- list(
    nobs = object$nobs,
    neffectiveobs = object$neffectiveobs,
    nparams = object$nparams,
    ll = object$ll,
    adjustedll = object$adjustedll,
    se = sqrt(object$ssq / object$neffectiveobs),
    aic = object$aic,
    aicc = object$aicc,
    bic = object$bic
  )
  class(res) <- "summary.JD3_LIKELIHOOD"
  res
}
#' @export
print.summary.JD3_LIKELIHOOD <- function(x, ...) {
  cat("Number of observations: ", x$nobs,
      ", Number of effective observations: ", x$neffectiveobs,
      ", Number of parameters: ", x$nparams, "\n",
      sep = ""
  )
  cat("Loglikelihood:", x$ll)
  if (x$ll != x$adjustedll) cat(", Adjusted loglikelihood:", x$adjustedll)
  cat("\nStandard error of the regression (ML estimate):", x$se, "\n")
  cat("AIC: ", x$aic, ", ",
      "AICc: ", x$aicc, ", ",
      "BIC: ", x$bic, "\n",
      sep = ""
  )
  invisible(x)
}

