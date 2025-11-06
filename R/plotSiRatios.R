#' Plot of SI-ratios by period
#'
#' Generate a `hchart` showing the si ratios and seasonal factors.
#' @returns An object of class `highchart`
#' @param x An object of class `persephone`
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSiRatios(x)
#' @export
plotSiRatios <- function(x) {

  if (inherits(x$output$decomposition, "JD3X11")) {
    decomp <- x$output$decomposition
    d8 <- decomp$d8 # Final unmodified SI Ratios
    d9 <- decomp$d9 # Final replacement for SI
    d10 <- decomp$d10 # Seasonal Factors


    d10_by <- by(d10, cycleName(d10), mean)
    d10_mean <- data.frame(cycleName = names(d10_by),
                           d10Mean = as.vector(d10_by))
    dat <- merge(data.frame(year = floor(stats::time(d10)),
                            cycleName = cycleName(d10),
                            d8, d9, d10, stringsAsFactors = FALSE),
                 d10_mean, by = "cycleName", sort = FALSE, all.x = TRUE)
    colnames(dat) <- c("cycleName", "year", "SI-Ratio", "Replaced SI-Ratio",
                       "Seasonal Factor", "SF Mean")
    nyears <- length(unique(dat$year))
    dat$x <- as.integer(dat$cycleName) + (dat$year - min(dat$year))/nyears
    dat <- dat[order(dat$x), ]
    dat$myIndex <- order(dat$x)-1

    hc <- highcharter::highchart()


    hc <- hc %>% highcharter::hc_add_series(
      data = list(),
      name = "Seasonal Factor",
      color = "#A59187",
      marker = FALSE,
      custom = list(month = dat$cycleName, year = dat$year, myIndex = dat$myIndex, x = dat$x))

    for (x2 in split(dat, dat$cycleName)) {
      hc <- hc %>% highcharter::hc_add_series(
        data.frame(x = x2$x, y = x2$`Seasonal Factor`),
        linkedTo = ":previous",
        name = "Seasonal Factor",
        color = "#A59187"
      )
    }

    hc <- hc %>% highcharter::hc_add_series(
      data = list(),
      name = "SF Mean",
      color = "#5A8CBE",
      marker = FALSE)

    for (x2 in split(dat, dat$cycleName)) {
      hc <- hc %>% highcharter::hc_add_series(
        data.frame(x = x2$x, y = x2$`SF Mean`),
        name = "SF Mean",
        marker = list(enabled = FALSE),
        linkedTo = ":previous",
        color = "#5A8CBE",
        states = list(hover = list(enabled = FALSE))
      )
    }

    hc <- hc %>% highcharter::hc_add_series(
      data = list(),
      name = "SI-Ratio",
      color = "#00643C",
      lineWidth = 0,
      marker = list(symbol = "circle"))

    for (x2 in split(dat, dat$cycleName)) {
      hc <- hc %>% highcharter::hc_add_series(
        data.frame(x = x2$x, y = x2$`SI-Ratio`),
        name = "SI-Ratio",
        linkedTo = ":previous",
        color = "#00643C",
        lineWidth = 0,

        #type = "scatter",
        #findNearestPointBy = "x",
        marker = list(
          enabled = TRUE,
          symbol = "circle"
        ),
        states = list(hover = list(
          lineWidthPlus = 0
        ))
      )
    }

    hc <- hc %>% highcharter::hc_add_series(
      data = list(), name = "Replaced SI-Ratio", color = "red",
      lineWidth = 0, marker = list(symbol = "circle"))

    for (x2 in split(dat, dat$cycleName)) {
      hc <- hc %>% highcharter::hc_add_series(
        data.frame(x = x2$x, y = x2$`Replaced SI-Ratio`),
        name = "Replaced SI-Ratio",
        linkedTo = ":previous",
        color = "red",
        lineWidth = 0,

        #type = "scatter",
        #findNearestPointBy = "x",
        marker = list(
          enabled = TRUE,
          symbol = "circle"
        ),
        states = list(hover = list(
          lineWidthPlus = 0
        ))
      )
    }

  }
  if (inherits(x$output$decomposition, "JD3_SEATS")) {

    # evt auch implementieren nachdem das anscheinend in JDemetra gewuenscht
    #   ist:
    # if(!missing(first_date)){
    #   x$components <- window(x$components, start = first_date)
    # }
    # if(!missing(last_date)){
    #   x$components <- window(x$components, end = last_date)
    # }

    sln  <-  x$output$user_defined$decomposition.s_cmp
    iln <- x$output$user_defined$decomposition.i_cmp
    mode <- x$output$user_defined$mode

    siRatio <- if (mode == "Additive") {
      sln + iln
    } else {
      sln * iln # SI-Ratio
    }

    siRatioBy <- by(siRatio, list(cycleName(siRatio)), mean)
    siRatioMean <- data.frame(cycleName = names(siRatioBy),
                              siRatioMean = as.vector(siRatioBy))

    dat <- merge(data.frame(year = floor(time(siRatio)),
                            cycleName = cycleName(siRatio), siRatio,
                            stringsAsFactors = FALSE),
                 siRatioMean, by = "cycleName", sort = FALSE, all.x = TRUE)
    colnames(dat) <- c("cycleName", "year", "SI-Ratio", "Mean")
    nyears <- length(unique(dat$year))
    dat$x <- as.integer(dat$cycleName) + (dat$year - min(dat$year))/nyears
    dat <- dat[order(dat$x), ]
    dat$myIndex <- order(dat$x)-1

    hc <- highcharter::highchart()

    hc <- hc %>% highcharter::hc_add_series(
      data = list(),
      name = "Mean",
      color = "#5A8CBE",
      marker = FALSE,
      custom = list(month = dat$cycleName, year = dat$year, myIndex = dat$myIndex, x = dat$x))

    for (x2 in split(dat, dat$cycleName)) {
      hc <- hc %>% highcharter::hc_add_series(
        data.frame(x = x2$x, y = x2$`Mean`),
        name = "Mean",
        marker = list(enabled = FALSE),
        linkedTo = ":previous",
        color = "#5A8CBE",
        states = list(hover = list(enabled = FALSE))
      )
    }

    hc <- hc %>% highcharter::hc_add_series(
      data = list(), name = "SI-Ratio", color = "#00643C",
      marker = list(symbol = "circle"))

    for (x2 in split(dat, dat$cycleName)) {
      hc <- hc %>% highcharter::hc_add_series(
        data.frame(x = x2$x, y = x2$`SI-Ratio`),
        name = "SI-Ratio",
        linkedTo = ":previous",
        color = "#00643C",

        marker = list(
          enabled = TRUE,
          symbol = "circle"
        ),
        states = list(hover = list(
          lineWidthPlus = 0
        ))
      )
    }

  }

  hc <-  hc %>% highcharter::hc_xAxis(
    #breaks = list(
    #  list(breakSize = 0.3, from = 2, to = 2),
    #  list(breakSize = 0.5, from = 3, to = 3)
    #),
    breaks = lapply(1:11, function(i) {
      list(breakSize = 0.3, from = i + 1, to = i + 1)
    }),
    tickPositions = 1:12 + 0.5,
    labels = list(
      formatter = highcharter::JS('
          function() {
            return ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
              "Aug", "Sep", "Oct", "Nov", "Dec"][this.value-1.5];
          }
        ')
    )
  )

  # hc <- hc %>% highcharter::hc_tooltip(
  #   useHTML = TRUE,
  #   shared = TRUE,
  #   formatter = highcharter::JS(readLines(
  #     system.file("js/tooltip_si.js", package = "persephone3") %>%
  #       paste(collapse = "\n")
  #   )))


  hc <- hc %>% highcharter::hc_tooltip(
    useHTML = TRUE,
    shared = TRUE,
    formatter = highcharter::JS("function() {
    let custom = this.points[0].series.chart.series[0].options.custom;
    let xVal = this.x;

    // Funktion zum Finden des nächsten Index
    function findClosestIndex(arr, val) {
      let closest = arr.reduce((prev, curr) =>
        Math.abs(curr - val) < Math.abs(prev - val) ? curr : prev
      );
      return arr.indexOf(closest);
    }

    let index = findClosestIndex(custom.x, xVal);
    let month = custom.month[index];
    let year = custom.year[index];
    let myIndex = custom.myIndex[index];

    // Index bei Tooltip mit anzeigen
    // return '<i>' + month + ' ' + year + ' (Index: ' + myIndex + ')</i>';
    return '<i>' + month + ' ' + year + '</i>';
  }"))

  hc %>% highcharter::hc_xAxis(
    crosshair = list(width = 30, color = "#efefef"))

}

cycleName <- function(ts) {
  if (stats::frequency(ts) == 12) {
    res <-  month.abb[stats::cycle(ts)]
    res <- ordered(res, levels = c(month.abb))
  } else if (stats::frequency(ts) == 4) {
    res <-  c("Q1", "Q2", "Q3", "Q4")[stats::cycle(ts)]
    res <- ordered(res, levels = c("Q1", "Q2", "Q3", "Q4"))
  } else if (stats::frequency(ts) == 2) {
    res <-  c("H1", "H2")[stats::cycle(ts)]
    res <- ordered(res, levels = c("H1", "H2"))
  }
  return(res)
}
