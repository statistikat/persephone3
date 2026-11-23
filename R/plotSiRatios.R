# Extract the same dated inputs for the interactive and static SI plots.
si_plot_data <- function(x) {
  if (is.null(x$output)) stop("No results from run available.", call. = FALSE)
  native <- identical(x$result$backend, "native")
  x11 <- inherits(x$output$decomposition, "JD3X11") ||
    (native && identical(x$result$method, "x13"))
  if (x11) {
    if (native) {
      tables <- x$output$selection$d$tables
      d8 <- tables$D8
      d9 <- tables$D9
      if (identical(x$output$mode_effective, "log_additive")) {
        d8 <- exp(d8)
        d9 <- exp(d9)
      }
      seasonal <- x$output$results$d10
    } else {
      d8 <- x$output$decomposition$d8
      d9 <- x$output$decomposition$d9
      seasonal <- x$output$decomposition$d10
    }
    phase <- cycleName(seasonal)
    return(data.frame(cycleName = phase, year = floor(as.numeric(stats::time(seasonal))),
      `SI-Ratio` = as.numeric(d8), `Replaced SI-Ratio` = as.numeric(d9),
      `Seasonal Factor` = as.numeric(seasonal),
      `SF Mean` = stats::ave(as.numeric(seasonal), phase, FUN = mean), check.names = FALSE))
  }
  if (native && identical(x$result$method, "tramoseats")) {
    # RSA0 has no deterministic regressors. These are the final SEATS components.
    si <- x$result$components$s + x$result$components$i
  } else if (inherits(x$output$decomposition, "JD3_SEATS")) {
    ud <- x$output$user_defined
    si <- if (ud$mode == "Additive") ud$decomposition.s_cmp + ud$decomposition.i_cmp else
      ud$decomposition.s_cmp * ud$decomposition.i_cmp
  } else stop("The fit supplies no SI-ratio decomposition.", call. = FALSE)
  phase <- cycleName(si)
  data.frame(cycleName = phase, year = floor(as.numeric(stats::time(si))),
    `SI-Ratio` = as.numeric(si), Mean = stats::ave(as.numeric(si), phase, FUN = mean),
    check.names = FALSE)
}

#' SI-Ratio Plot
#'
#' Create SI-ratio plots using either highcharter (interactive) or ggplot2 (static).
#' This is a unified interface for [plotSiRatiosHc] and [plotSiRatiosGgplot].
#'
#' @param x an object of class [persephone].
#' @param plotEngine character. Either `"hc"` (default, interactive highcharter plots)
#'   or `"ggplot"` (static ggplot2 plots).
#' @param main if `plotEngine="ggplot"`, plot title. Ignored for highcharter plots.
#' @param interactive If `plotEngine="ggplot"`, whether to wrap in plotly for
#'   basic interactivity. Ignored for highcharter plots.
#' @param ... other parameters passed to the underlying plot function. Not currently used.
#'
#' @details
#' For `x13Single` objects, displays SI-ratios, replaced SI-ratios, seasonal
#' factors, and their mean. For `tramoseatsSingle` objects, shows SI-ratios
#' and their mean.
#'
#' @return A highchart object (if `plotEngine="hc"`) or a ggplot/plotly object
#'   (if `plotEngine="ggplot"`).
#'
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSiRatios(x, plotEngine = "hc")
#' plotSiRatios(x, plotEngine = "ggplot")
#' plotSiRatios(x, plotEngine = "ggplot", interactive = TRUE)
#'
#' @export
plotSiRatios <- function(
    x,
    plotEngine = c("hc", "ggplot"),
    main = NULL,
    interactive = FALSE,
    ...
) {
  stopifnot(inherits(x, "persephone"))
  plotEngine <- match.arg(plotEngine)

  if (plotEngine == "hc") {
    plotSiRatiosHc(x)
  } else {
    plotSiRatiosGgplot(x, main = main, interactive = interactive, ...)
  }
}
