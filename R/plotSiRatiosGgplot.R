#' SI-Ratio Plot (ggplot2 version)
#'
#' Create a plot of SI-ratios (and, in case of x11, of seasonal factors) by
#' period using ggplot2, suitable for publications and reports
#' with easy export to PDF, PNG, and other formats. Supports optional plotly
#' conversion for basic interactivity.
#'
#' @details
#' Produces a `ggplot`/`plotly` object for objects of class [persephone].
#' If the `persephone` object is of class `x13Single`, the function
#' generates a plot of the SI-ratios, the replaced SI-ratios, the seasonal
#' factors and the mean of the seasonal factors. For `tramoseatsSingle` objects,
#' the function plots SI-ratios and their mean.
#'
#' @param x an object of class [persephone].
#' @param main plot title
#' @param interactive If TRUE, wrap the `ggplot` object in [plotly::ggplotly]
#'   to add basic interactivity (zoom, pan, hover tooltips). If FALSE (default),
#'   returns a static ggplot object suitable for export.
#' @param ... other plotting parameters to affect the plot. Not currently used.
#'
#' @return Returns an object of class `ggplot` or `plotly`
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#' # Generate a persephone object, in this case an x13Single object
#' obj <- perX13(AirPassengers, "rsa1")
#' obj$run()
#' # Plot the SI-ratios after run
#' plotSiRatiosGgplot(obj)
#'
#' data(UKgas, package = "datasets")
#' # Generate a persephone object, in this case a tramoseatsSingle object
#' obj2 <- perTramo(UKgas, "rsa3")
#' obj2$run()
#' plotSiRatiosGgplot(obj2)
#'
#' @importFrom magrittr %>%
#'
#' @export
plotSiRatiosGgplot <- function(x, main = NULL, interactive = FALSE, ...) {
  variable <- year <- value <- NULL # nolint

  if (is.null(x$output$user_defined)) {
    stop("No results from run available.\n")
  }

  cycleName <- function(ts) {
    if (frequency(ts) == 12) {
      res <- month.abb[cycle(ts)]
      res <- ordered(res, levels = c(month.abb))
    } else if (frequency(ts) == 4) {
      res <- c("Q1", "Q2", "Q3", "Q4")[cycle(ts)]
      res <- ordered(res, levels = c("Q1", "Q2", "Q3", "Q4"))
    } else if (frequency(ts) == 2) {
      res <- c("H1", "H2")[cycle(ts)]
      res <- ordered(res, levels = c("H1", "H2"))
    }
    return(res)
  }

  if (inherits(x$output$decomposition, "JD3X11")) {
    decomp <- x$output$decomposition
    d8 <- decomp$d8 # Final unmodified SI Ratios
    d9 <- decomp$d9 # Final replacement for SI
    d10 <- decomp$d10 # Seasonal Factors

    d10By <- by(d10, list(cycleName(d10)), mean)
    d10Mean <- data.frame(
      cycleName = names(d10By),
      d10Mean = as.vector(d10By)
    )
    dat <- merge(
      data.frame(
        year = floor(time(d10)),
        cycleName = cycleName(d10),
        d8, d9, d10,
        stringsAsFactors = FALSE
      ),
      d10Mean,
      by = "cycleName",
      sort = FALSE,
      all.x = TRUE
    )
    colnames(dat) <- c(
      "cycleName", "year", "SI-Ratio", "Replaced SI-Ratio",
      "Seasonal Factor", "SF Mean"
    )
    dat1 <- dat[, c("year", "cycleName", "SI-Ratio")]
    dat2 <- dat[, c("year", "cycleName", "Replaced SI-Ratio")]
    dat3 <- dat[, c("year", "cycleName", "Seasonal Factor")]
    dat4 <- dat[, c("year", "cycleName", "SF Mean")]
    colnames(dat1)[3] <- colnames(dat2)[3] <-
      colnames(dat3)[3] <- colnames(dat4)[3] <- "value"
    dat1$variable <- "SI-Ratio"
    dat2$variable <- "Replaced SI-Ratio"
    dat3$variable <- "Seasonal Factor"
    dat4$variable <- "SF Mean"
    dat <- rbind(dat1, dat2, dat3, dat4)

    if (is.null(main)) {
      main <- "SI Ratios and Seasonal Factors by Period"
    }

    p <- ggplot() +
      geom_point(
        data = subset(dat, variable == "SI-Ratio"),
        aes(x = year, y = value, colour = variable)
      ) +
      geom_point(
        data = subset(dat, variable == "Replaced SI-Ratio"),
        na.rm = TRUE,
        aes(x = year, y = value, colour = variable)
      ) +
      geom_line(
        data = subset(dat, variable == "Seasonal Factor"),
        aes(x = year, y = value, colour = variable)
      ) +
      geom_line(
        data = subset(dat, variable == "SF Mean"),
        aes(x = year, y = value, colour = variable)
      ) +
      scale_x_continuous(labels = NULL, breaks = NULL) +
      facet_grid(cols = vars(cycleName), switch = "x") +
      ggtitle(main) +
      ylab("") +
      xlab("") +
      scale_colour_manual(
        breaks = c("SI-Ratio", "Replaced SI-Ratio", "Seasonal Factor", "SF Mean"),
        values = c(
          "SI-Ratio" = "darkgreen",
          "Replaced SI-Ratio" = "red",
          "Seasonal Factor" = "black",
          "SF Mean" = "blue"
        ),
        guide = guide_legend(override.aes = list(
          linetype = c("blank", "blank", "solid", "solid"),
          shape = c(16, 16, NA, NA)
        ))
      )
  }

  if (inherits(x$output$decomposition, "JD3_SEATS")) {
    sln <- x$output$user_defined$decomposition.s_cmp
    iln <- x$output$user_defined$decomposition.i_cmp
    mode <- x$output$user_defined$mode

    siRatio <- if (mode == "Additive") {
      sln + iln
    } else {
      sln * iln # SI-Ratio
    }

    siRatioBy <- by(siRatio, list(cycleName(siRatio)), mean)
    siRatioMean <- data.frame(
      cycleName = names(siRatioBy),
      siRatioMean = as.vector(siRatioBy)
    )

    dat <- merge(
      data.frame(
        year = floor(time(siRatio)),
        cycleName = cycleName(siRatio),
        siRatio,
        stringsAsFactors = FALSE
      ),
      siRatioMean,
      by = "cycleName",
      sort = FALSE,
      all.x = TRUE
    )
    colnames(dat) <- c("cycleName", "year", "SI-Ratio", "Mean")

    dat1 <- dat[, c("year", "cycleName", "SI-Ratio")]
    dat2 <- dat[, c("year", "cycleName", "Mean")]
    colnames(dat1)[3] <- colnames(dat2)[3] <- "value"
    dat1$variable <- "SI-Ratio"
    dat2$variable <- "Mean"
    dat <- rbind(dat1, dat2)

    if (is.null(main)) {
      main <- "SI-Ratios by Period"
    }

    p <- ggplot() +
      geom_point(
        data = subset(dat, variable == "SI-Ratio"),
        aes(x = year, y = value, colour = variable)
      ) +
      geom_line(
        data = subset(dat, variable == "SI-Ratio"),
        aes(x = year, y = value, colour = variable)
      ) +
      geom_line(
        data = subset(dat, variable == "Mean"),
        aes(x = year, y = value, colour = variable)
      ) +
      scale_x_continuous(labels = NULL, breaks = NULL) +
      facet_grid(cols = vars(cycleName), switch = "x") +
      ggtitle(main) +
      ylab("") +
      xlab("") +
      scale_colour_manual(
        breaks = c("SI-Ratio", "Mean"),
        values = c("SI-Ratio" = "red", "Mean" = "blue"),
        guide = guide_legend(override.aes = list(
          linetype = c("solid", "solid"),
          shape = c(16, NA)
        ))
      )
  }

  siRatioTheme <- theme_bw() +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
      panel.border = element_rect(linetype = "solid", colour = "grey"),
      legend.title = element_blank(),
      legend.position = "bottom"
    )

  p <- p + siRatioTheme

  if (interactive) {
    p <- plotly::ggplotly(p)

    # Hide xaxis labels for all facets
    evalThis <- paste0(
      "p  %>% plotly::layout(",
      paste0(
        grep("xaxis", names(p[["x"]][["layout"]]),
             value = TRUE
        ),
        " = list(visible = FALSE)",
        collapse = ", "
      ), ")"
    )
    p <- eval(parse(text = evalThis))
  }

  p
}
