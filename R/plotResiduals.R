#' Several interactive plots in connection with residuals for a persephone
#' object
#'
#' Produces either a dygraph (see the
#' [online documentation](https://rstudio.github.io/dygraphs/) for more detail)
#' or a `ggplot`/`plotly` object for objects of class [persephone].
#'
#' @param x an object of class [persephone].
#' @param which character (`"res"`,`"acf"`,`"acf2"`, `"pacf"`,`"sreshist"`,
#'   `"nqq"`, `"overview"`) selecting the preferred type of plot, see Details.
#'   A numeric value (`1:7`) corresponding to one of these characters is also
#'   accepted.
#'
#' @param main plot title
#' @param interactive If the return value would be a `ggplot` object, wrap it in
#'   [plotly::ggplotly] before returning.
#' @param ... other plotting parameters to affect the plot. Not currently used.
#'
#' @details
#'
#' The following options are available for the parameter `which`.
#'
#' * `res`: residuals (1)
#' * `acf`: autocorrelations of the residuals (2)
#' * `pacf`: partial autocorrelations of the residuals (3)
#' * `acf2`: autocorrelations of the squared residuals (4)
#' * `sreshist`: histogram of standardized residuals including normal curve (5)
#' * `nqq`: normal q-q plot of standardized residuals (6)
#' * `overview`: residuals over time with the acf, the pacf and the
#'   histogram of the standardized residuals below, in one figure (7)
#'
#' @return Returns an object of class `dygraphs`, `ggplot` or `plotly`
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#' # Generate a persephone object, in this case an x13Single object
#' obj <- perX13(AirPassengers, "rsa1")
#'
#' obj$run()
#' # Plot the residuals after run
#' plotResiduals(obj)
#' # Plot the autocorrelations of the residuals
#' plotResiduals(obj, which="acf")
#' # Residuals, acf, pacf and the normal density in one figure
#' plotResiduals(obj, which = "overview")
#'
## TO DO:
# maybe other theme_bw()
# maybe one standard for colors (for dygraph and ggplots -> take dygraph
# colors for ggplots?)
# maybe adjust tooltip for ggplotly (e.g. when count is shown even though
# y=density)
#'
#' @export
plotResiduals <-   function(x, which = c("res", "acf", "acf2", "pacf",
                                         "sreshist", "nqq", "overview"),
                            main = NULL, interactive = TRUE, ...) {

  y <- NULL # nolint

  if (is.null(x$output$user_defined)) {
    stop("No results from run available.\n")
  }

  if (is.numeric(which)) {
    which <- c("res", "acf", "acf2", "pacf", "sreshist", "nqq",
               "overview")[which]
  }else{
    which <- match.arg(which)
  }

  # the panels of the overview figure as ggplots (also the basis of the
  # single panel plots) -----------------------------------------------------

  # residuals over time, bar chart in the style of the dygraph "res" plot
  resPanel <- function() {
    r <- x$output$user_defined$residuals.tsres
    d <- data.frame(
      date = timesToDates(as.numeric(time(r)), frequency(r)),
      y = as.numeric(r)
    )
    ggplot(d, aes(x = date, y = y)) +
      geom_col(
        width = 0.85 * 365.25 / frequency(r),
        fill = p3_series_colors[["Original"]]
      ) +
      xlab(NULL) + ylab("Residual value") +
      theme_bw() +
      theme(panel.grid.minor = element_blank())
  }

  # acf/pacf of the residuals as ggplot, confidence bands as in forecast()
  acfPanel <- function(partial = FALSE, squared = FALSE, ylab_ = "ACF") {
    r <- x$output$user_defined$residuals.tsres
    if (squared) {
      r <- r^2
    }
    result <- if (partial) {
      pacf(r, plot = FALSE)
    } else {
      acf(r, plot = FALSE)
    }
    ci <- qnorm((1 + 0.95) / 2) / sqrt(result$n.used)
    # start from lag1, and show whole numbers for lags
    lags <- result$lag
    acfs <- result$acf
    if (!partial) {
      lags <- lags[-1]
      acfs <- acfs[-1, 1, 1]
    } else {
      acfs <- acfs[, 1, 1]
    }
    d <- data.frame(lag = lags * frequency(x$ts), acf = acfs)
    ggplot(d, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", width = 0.1) +
      geom_hline(yintercept = c(-ci, ci), colour = "blue",
                 linetype = "dashed") +
      xlab("Lag") + ylab(ylab_) +
      theme_bw() +
      theme(panel.grid.minor = element_blank())
  }

  # histogram of the standardized residuals with the normal curve
  histPanel <- function() {
    result <- x$output$user_defined$residuals.tsres /
      x$output$user_defined$residuals.ser
    result <- data.frame(x = as.numeric(result))
    ggplot(result, aes(x = x)) +
      geom_histogram(
        binwidth = 0.5, center = 0,
        aes(y = after_stat(density)), # nolint
        fill = p3_series_colors[["Original"]]
      ) +
      stat_function(fun = dnorm, color = "red", args = list(mean = 0, sd = 1)) +
      xlab("Standardized Residuals") +
      ylab("Density") +
      theme_bw() +
      theme(panel.grid.minor = element_blank())
  }

  if (which == "overview") {
    if (is.null(main)) {
      main <- "Residual Diagnostics"
    }
    p_res <- suppressWarnings(plotly::ggplotly(resPanel()))
    p_acf <- suppressWarnings(plotly::ggplotly(
      acfPanel(partial = FALSE, ylab_ = "ACF")
    ))
    p_pacf <- suppressWarnings(plotly::ggplotly(
      acfPanel(partial = TRUE, ylab_ = "PACF")
    ))
    p_hist <- suppressWarnings(plotly::ggplotly(histPanel()))
    bottom <- plotly::subplot(p_acf, p_pacf, p_hist, nrows = 1)
    p <- plotly::subplot(p_res, bottom, nrows = 2,
                         heights = c(0.55, 0.45))
    # subplot drops the individual titles: add the panel titles as
    # annotations (top panel, bottom row from left to right)
    p <- subplotTitles(p, c("Residuals", "ACF", "PACF", "Density"),
                       main = main)
    return(p)
  }

  if (which == "res") {
    if (is.null(main)) {
      main <- "Residuals"
    }

    dyBarChart <- function(dygraph) {
      dyPlotter(dygraph = dygraph,
                name = "BarChart",
                path = system.file("plotters/barchart.js",
                                   package = "dygraphs"))
    }
    p <- dygraph(x$output$user_defined$residuals.tsres, main = main) %>%
      dySeries("V1", label = "Residual value") %>%
      dyBarChart()

    # if(rangeSelector){
    #   graphObj <- graphObj %>%
    #     dyRangeSelector(height = 20)
    # }
  }

  # Histogram (and/or Frequency Polygon?) of Standardized Residuals
  if (which == "sreshist") {
    if (is.null(main)) {
      main <- "Histogram of Standardized Residuals and Normal Curve"
    }
    p <- histPanel() + ggtitle(main)
  }

  if (which == "nqq") {
    if (is.null(main)) {
      main <- "Normal Q-Q Plot"
    }

    # result <- x$output$user_defined$residuals.tsres / x$output$user_defined$residuals.ser
    # result <- data.frame(date = paste0(
    #   c(floor(time(result) + .01)), "-",
    #   stringfix(c(cycle(result)), 2, "0"), "-01"),
    #   y = c(result))
    #
    # p <- ggplot(result, aes(sample = y)) +
    #   stat_qq() +
    #   stat_qq_line(color = "red") +
    #   xlab("Theoretical Quantiles") +
    #   ylab("Standardized Residuals") +
    #   ggtitle(main) +
    #   theme_bw()

    result <- as.numeric(x$output$user_defined$residuals.tsres / x$output$user_defined$residuals.ser)
    result <- qqnorm(result, plot.it = FALSE)

    result <- data.frame(
      x = result$x,
      y = result$y
    )

    p <- ggplot(result, aes(x = x, y = y)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      xlab("Theoretical Quantiles") +
      ylab("Standardized Residuals") +
      ggtitle(main) +
      theme_bw()
    #stat_qq_line(color="gray50", lty=3) +
  }

  if (which == "acf") {
    if (is.null(main)) {
      main <- "Autocorrelations of the Residuals"
    }

    result <- acf(x$output$user_defined$residuals.tsres, plot = FALSE)

    # confidence interval as in R package forecast
    ci <- 0.95 #coverage probability for confidence interval
    ci <- qnorm((1 + ci) / 2) / sqrt(result$n.used)

    # start from lag1
    # to show whole numbers for lags
    result <- data.frame(lag = result$lag[-1] * frequency(x$ts),
                         acf = result$acf[-1, 1, 1])


    # require(forecast)
    # ggAcf(x$output$user_defined$residuals.tsres, lag.max = NULL,
    #       type = c("correlation", "covariance", "partial"),
    #       plot = TRUE, na.action = na.contiguous, demean=TRUE)
    #

    p <- ggplot(result, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", width = 0.1) +
      geom_hline(
        yintercept = c(-ci, ci), colour = "blue",
        linetype = "dashed"
      ) +
      xlab("Lag") +
      ylab("ACF") +
      ggtitle(main) +
      theme_bw()
  }

  if (which == "acf2") {
    if (is.null(main)) {
      main <- "Autocorrelations of the Squared Residuals"
    }

    result <- acf(x$output$user_defined$residuals.tsres ^ 2, plot = FALSE)

    # confidence interval as in R package forecast
    ci <- 0.95 #coverage probability for confidence interval
    ci <- qnorm((1 + ci) / 2) / sqrt(result$n.used)

    # start from lag1
    # to show whole numbers for lags
    result <- data.frame(lag = result$lag[-1] * frequency(x$ts),
                         acf = result$acf[-1, 1, 1])

    p <- ggplot(result, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", width = 0.1) +
      geom_hline(yintercept = c(-ci, ci), colour = "blue",
                 linetype = "dashed") +
      xlab("Lag") +
      ylab("ACF") +
      ggtitle(main) +
      theme_bw()
  }

  if (which == "pacf") {
    if (is.null(main)) {
      main <- "Partial Autocorrelations of the Residuals"
    }

    result <- pacf(x$output$user_defined$residuals.tsres, plot = FALSE)

    # confidence interval as in R package forecast
    ci <- 0.95 #coverage probability for confidence interval
    ci <- qnorm((1 + ci) / 2) / sqrt(result$n.used)

    #to show whole numbers for lags
    result <- data.frame(lag = result$lag * frequency(x$ts),
                         acf = result$acf[, 1, 1])


    p <- ggplot(result, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", width = 0.1) +
      geom_hline(yintercept = c(-ci, ci), colour = "blue",
                 linetype = "dashed") +
      xlab("Lag") +
      ylab("PACF") +
      ggtitle(main) +
      theme_bw()
  }

  if (interactive & which %in% c("acf", "acf2", "pacf", "nqq", "sreshist")) {

    # p <- p + theme_bw()

    p <- plotly::ggplotly(p)
  }

  p

}
