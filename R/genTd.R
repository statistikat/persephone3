#' Generation of trading day regressors
#'
#' User defined trading day regressors are generated depending on a list of
#' country-specific holidays and associated weights. Each holiday can be given
#' a specific weight depending on the extent of the work load of that specific holiday.
#'
#' @param freq frequency of series
#' @param firstYear first year of calculation of trading day regressor
#' @param lastYear last year of calculation of trading day regressor
#' @param hd holidays, list of 1. exact date ("mm-dd") and/or 2. easter
#'           relation (e.g. "easter+39", "easter-3") and/or 3. name of one of
#'           the pre-specified holidays of [rjd3toolkit::special_day()]
#'           (case insensitive, see Details) and/or 4. holiday object of
#'           rjd3toolkit ([rjd3toolkit::special_day()],
#'           [rjd3toolkit::fixed_day()], [rjd3toolkit::easter_day()]).
#'           `hd` may also be a [rjd3toolkit::national_calendar()] itself,
#'           whose holidays are then used (see Details).
#' @param weight vector of individual weights for each holiday with length of hd
#'        (default 1 for all); it is multiplied into the weight of a holiday
#'        object
#' @param adjustEaster "exact" or "approximate" to employ theoretical distribution of easter dates
#' (default) or approximate distribution of easter dates for centering of trading days
#' @details
#'
#' The pre-specified holidays of [rjd3toolkit::special_day()] that `hd`
#' accepts by name are `NEWYEAR`, `SHROVEMONDAY`, `SHROVETUESDAY`,
#' `ASHWEDNESDAY`, `MAUNDYTHURSDAY`, `GOODFRIDAY`, `EASTER`, `EASTERMONDAY`,
#' `ASCENSION`, `PENTECOST`, `WHITMONDAY`, `CORPUSCHRISTI`, `MAYDAY`,
#' `ASSUMPTION`, `ALLSAINTSDAY`, `ARMISTICE`, `CHRISTMAS`, `HALLOWEEN`,
#' `LABORDAY` and `JULIANEASTER`. Their dates come from rjd3toolkit, so a
#' name like `"ASCENSION"` generates the same regressors as the easter
#' relation `"easter+39"` and `"CHRISTMAS"` the same as the exact date
#' `"12-25"`.
#'
#' Passing a [rjd3toolkit::national_calendar()] as `hd` makes the trading day
#' regressors and the calendar correction of the estimation (the `calendars`
#' argument of [rjd3toolkit::modelling_context()]) share one and the same
#' holiday definition; the `mean_correction` of the calendar is ignored. The
#' offsets and the weights of the holiday objects are respected, the
#' effective weight of a holiday is `weight[i]` times the weight of the
#' object, and its validity period restricts the dates on which the holiday
#' is subtracted from the trading day counts.
#'
#' @return list with three list elements. 1. matrix of trading day counts for each
#'        individual day, month and year. Holidays which are delivered through the
#'        parameter hd are assigned to the number of sundays. 2. multiple time
#'        series object with 6 non-centered trading day regressors plus 1
#'        non-centered leap year regressor (columns 1 to 6 refer to Mondays to
#'        Saturdays, column 7 to the leap year regressor "lpYear").
#'        In case of trading day adjustment the first 6 columns have to be selected,
#'        the leap year regressor is used on its own (see the `user` argument of
#'        [perX13()]).
#'        3. multiple time series object with 6 centered trading day regressors
#'        plus 1 centered leap year regressor
#'
#'
#' @examples
#' library(rjd3toolkit)
#' # the austrian holidays, see the commented block below for the calendar
#' AT <- national_calendar(list(
#'   special_day("NEWYEAR"), fixed_day(1, 6), special_day("EASTERMONDAY"),
#'   special_day("ASCENSION"), special_day("WHITMONDAY"),
#'   special_day("CORPUSCHRISTI"), special_day("MAYDAY"),
#'   special_day("ASSUMPTION"), fixed_day(10, 26),
#'   special_day("ALLSAINTSDAY"), fixed_day(12, 8),
#'   fixed_day(12, 24, weight = 0.5), special_day("CHRISTMAS"),
#'   fixed_day(12, 26), fixed_day(12, 31, weight = 0.5)
#' ))
#' # the one calendar for the trading day regressors ...
#' tdAT <- genTd(firstYear = 2000, lastYear = 2020, hd = AT)[[3]][, 1:6]
#' # ... and for the calendar correction of the estimation
#' my_context <- modelling_context(calendars = list(cal = AT))
# AT <- national_calendar(list(
#   special_day('NEWYEAR'),
#
#   fixed_day(1, 6),
#
#   special_day('EASTERMONDAY'),
#
#   special_day('ASCENSION'),
#
#   special_day('WHITMONDAY'),
#
#   special_day('CORPUSCHRISTI'),
#
#   special_day('MAYDAY'),
#
#   special_day('ASSUMPTION'),
#
#   fixed_day(10, 26),
#
#   special_day('ALLSAINTSDAY'),
#
#   fixed_day(12, 8),
#
#   fixed_day(12, 24, weight = 0.5),
#
#   special_day('CHRISTMAS'),
#
#   fixed_day(12, 26),
#
#   fixed_day(12, 31, weight = 0.5)
# ))
#
# # hdAT <- genTd(
# #   freq = 12,
# #   firstYear = 2000,
# #   hd = list(
# #     "01-01",
# #     "01-06",
# #     "05-01",
# #     "easter+1",
# #     "easter+39",
# #     "easter+50",
# #     "easter+60",
# #     "08-15",
# #     "10-26",
# #     "11-01",
# #     "12-08",
# #     "12-24",
# #     "12-25",
# #     "12-26",
# #     "12-31"
# #   ),
# #   weight = c(rep(1, 11), 0.5, rep(1, 2), 0.5)
# # )
# # hdAT <- hdAT[[3]][, 1:6]
#
# my_context <- rjd3toolkit::modelling_context(calendars = list(cal = AT))
#
# obj_x13 <- perX13(AirPassengers, template = "rsa3", context = my_context,
#                   td.option = "TradingDays",
#                   td.calendar.name = "cal",
#                   td.test = c("Add", "Joint_F"),
#                   transform.fun = "Log",
#                   transform.adjust = "LeapYear",
#                   tradingdays.option = "UserDefined",
#                   usrdef.varType = "Calendar",
#                   usrdef.varEnabled = TRUE,
#                   usrdef.var = hdAT[[3]])
# obj_x13$run()
# obj_x13$output$regarima
#'
#' @importFrom stats ts
#' @importFrom zoo as.yearmon as.Date
#' @importFrom timeDate Easter
#' @importFrom stats aggregate.ts
#' @importFrom utils data
#' @export
genTd <- function(
  freq = 12,
  firstYear = 1960,
  lastYear = 2099,
  hd,
  weight = NULL,
  adjustEaster = "exact"
) {
  y <- ts(frequency = 12, start = c(firstYear, 1), end = c(lastYear, 12))
  dNam <- weekdays(as.Date("2020-02-16") + 1:7, abbreviate = TRUE)
  stopifnot(adjustEaster %in% c("exact", "approximate"))
  eaDist_exact <- eaDist_approx <- NULL
  if (adjustEaster == "exact") {
    data("eaDist_exact", envir = as.environment(-1), package = "persephone3")
    eaDist <- eaDist_exact
  } else {
    data("eaDist_approx", envir = as.environment(-1), package = "persephone3")
    eaDist <- eaDist_approx
  }

  entries <- genTd_holidays(hd, weight, firstYear, lastYear)
  isEaster <- vapply(
    entries, function(e) identical(e$kind, "easter"), logical(1)
  )
  isFixed <- vapply(
    entries, function(e) identical(e$kind, "fixed"), logical(1)
  )
  isGeneric <- vapply(
    entries, function(e) identical(e$kind, "generic"), logical(1)
  )
  easterIdx <- which(isEaster)

  eMeans <- list()
  for (ii in seq_along(easterIdx)) {
    off <- sprintf("%+d", entries[[easterIdx[ii]]]$offset)
    start <- paste("as.Date(\"2285-03-22\")", off)
    start <- eval(parse(text = start))
    end <- paste("as.Date(\"2038-04-25\")", off)
    end <- eval(parse(text = end))
    start00 <- as.Date(paste0("2019-", substr(start, 6, 10)))
    end00 <- as.Date(paste0("2019-", substr(end, 6, 10)))
    ll <- seq(start00, end00, by = "day")
    nD <- table(substr(ll, 6, 7))
    j0 <- 1
    ww <- NULL
    for (jj in 1:length(nD)) {
      ww0 <- sum(eaDist[j0:cumsum(nD)[jj]])
      j0 <- cumsum(nD)[jj] + 1
      ww <- cbind(ww, ww0)
    }
    eMeans[[ii]] <- data.frame(
      rel = as.numeric(ww),
      mon = names(nD),
      dd = weekdays(start),
      stringsAsFactors = TRUE
    )
  }
  # the dates of every holiday within firstYear to lastYear
  dat <- lapply(entries, function(e) {
    d <- if (identical(e$kind, "easter")) {
      as.Date(Easter(year = seq.int(firstYear, lastYear))) + e$offset
    } else {
      e$dates
    }
    genTd_valid(d, e$validity)
  })
  wts <- vapply(entries, function(e) e$w, numeric(1))
  # the "generic" holidays get their own correction below and are not part
  # of the fixed-holiday mean
  if (any(isFixed)) {
    datNE <- data.frame(
      dat = do.call(c, dat[isFixed]),
      weight = rep(wts[isFixed], lengths(dat[isFixed])),
      stringsAsFactors = TRUE
    )
    datNE <- unique(datNE)
    datNE$mon <- as.numeric(substr(datNE$dat, 6, 7))
    aMeans <- stats::aggregate(datNE[, 2], list(mon = datNE$mon), sum)
  }

  # Elimination of redundant dates (e.g. 1st of May 2008 is both, Labour Day and Ascension )
  dat <- data.frame(
    dat = do.call(c, dat) %||% as.Date(character(0)),
    weight = rep(wts, lengths(dat)),
    stringsAsFactors = TRUE
  )
  dat <- unique(dat)
  colnames(dat) <- c("dat", "weight")

  dd <- matrix(nrow = length(y), ncol = 7, dimnames = c(list(NULL, dNam)))
  td <- matrix(
    nrow = length(y),
    ncol = 6,
    dimnames = c(list(NULL, c(dNam[1:6])))
  )
  dd0 <- matrix(
    rep(0, 7),
    nrow = length(y),
    ncol = 7,
    dimnames = c(list(NULL, dNam))
  )
  for (ii in seq_along(y)) {
    ti <- time(y)[ii]
    fti <- floor(ti)
    si <- round(12 * (ti - fti)) + 1
    d0 <- as.Date(as.yearmon(paste(c(fti, si), collapse = "-")), frac = 0)
    dN <- as.Date(as.yearmon(paste(c(fti, si), collapse = "-")), frac = 1)
    rT <- seq(from = d0, to = dN, by = "day")
    days <- weekdays(rT, abbreviate = TRUE)
    dd[ii, ] <- as.vector(table(factor(days, levels = dNam)))
    moDays <- data.frame(dat = rT, days, stringsAsFactors = TRUE)
    moDays0 <- merge(moDays, dat, by = "dat", all.x = TRUE)
    if (any(!is.na(moDays0$weight))) {
      moDays1 <- stats::aggregate(. ~ days, data = moDays0, FUN = "sum")
      moDays1$days <- as.numeric(factor(moDays1$days, levels = dNam))
      dd0[ii, moDays1$days] <- moDays1$weight
      dd0[ii, 7] <- -sum(dd0[ii, 1:6])
      dd0[ii, ] <- dd[ii, ] - dd0[ii, ]
    } else {
      dd0[ii, ] <- dd[ii, ]
    }
    td[ii, 1:6] <- c(
      dd0[ii, 1] - dd0[ii, 7],
      dd0[ii, 2] - dd0[ii, 7],
      dd0[ii, 3] - dd0[ii, 7],
      dd0[ii, 4] - dd0[ii, 7],
      dd0[ii, 5] - dd0[ii, 7],
      dd0[ii, 6] - dd0[ii, 7]
    )
  }

  ltermM <- matrix(0, nrow = 12, ncol = 6)
  colnames(ltermM) <- dNam[1:6]
  if (exists("aMeans")) {
    ltermM[aMeans[, 1], 1:6] <- aMeans[, 2] / (lastYear - firstYear + 1)
  }
  for (ii in seq_along(eMeans)) {
    # a holiday that always falls on a sunday does not change the trading
    # day pattern (and ltermM has no sunday column)
    em <- droplevels(
      eMeans[[ii]][
        substr(eMeans[[ii]][, 3], 1, 3) != substr(dNam[7], 1, 3), ,
        drop = FALSE
      ]
    )
    if (nrow(em) == 0) {
      next
    }
    row <- as.numeric(levels(em[, 2]))
    col <- rep(
      which(dNam %in% substr(em[, 3], 1, 3)),
      nrow(em)
    )
    val <- em[, 1]
    ltermM[row, col] <- ltermM[row, col] + 2 * val
    ltermM[row, -col] <- ltermM[row, -col] + val
  }
  # holidays that follow neither an exact date nor an easter relation (e.g.
  # the julian easter) get the same correction from the observed
  # distribution of their dates over the months and weekdays of the year
  for (e in entries[isGeneric]) {
    if (length(e$dates) == 0) {
      next
    }
    tab <- table(
      format(e$dates, "%m"), substr(weekdays(e$dates), 1, 3)
    )
    cells <- which(tab > 0, arr.ind = TRUE)
    for (cc in seq_len(nrow(cells))) {
      col <- match(colnames(tab)[cells[cc, 2]], substr(dNam, 1, 3))
      if (col == 7) {
        next
      }
      row <- as.integer(rownames(tab)[cells[cc, 1]])
      val <- tab[cells[cc, 1], cells[cc, 2]] / (lastYear - firstYear + 1) *
        e$w
      ltermM[row, col] <- ltermM[row, col] + 2 * val
      ltermM[row, -col] <- ltermM[row, -col] + val
    }
  }
  ltermMAll <- do.call(
    rbind,
    replicate((lastYear - firstYear + 1), ltermM, simplify = FALSE)
  )
  td1 <- td + ltermMAll

  lpY <- rowSums(dd)
  lpYear <- sapply(lpY, function(x) {
    ifelse(x == 28, -0.25, ifelse(x == 29, 0.75, 0))
  })
  td1 <- cbind(td1, lpYear)
  td1 <- ts(
    matrix(td1, nrow = nrow(td1), ncol = ncol(td1)),
    start = c(firstYear, 1),
    frequency = 12
  )
  colnames(td1) <- c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "lpYear"
  )
  td <- ts(
    matrix(td, nrow = nrow(td), ncol = ncol(td)),
    start = c(firstYear, 1),
    frequency = 12
  )
  colnames(td) <- c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
  )
  dd <- ts(
    matrix(dd, nrow = nrow(dd), ncol = ncol(dd)),
    start = c(firstYear, 1),
    frequency = 12
  )
  colnames(dd) <- c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
  )
  if (freq == 4) {
    td1 <- aggregate.ts(td1, nfrequency = 4)
    td <- aggregate.ts(td, nfrequency = 4)
    dd <- aggregate.ts(dd, nfrequency = 4)
  }

  return(list(dd, td, td1))
}

# --------------------- internals of the hd argument -----------------------

# normalizes the hd argument of genTd() into holiday entries: kind "fixed"
# (holidays with an explicit date vector, e.g. the "mm-dd" notation,
# fixed_day() and calendar events falling on the same date every year),
# kind "easter" (an offset in days from easter sunday, e.g. the "easter+39"
# notation, easter_day() and the easter based special days) or kind
# "generic" (all other dates); hd may be a national_calendar() itself, whose
# holidays are then used; the effective weight of an entry is weight[i]
# times the weight of the holiday object
genTd_holidays <- function(hd, weight, firstYear, lastYear) {
  if (inherits(hd, "JD3_CALENDARDEFINITION")) {
    # the mean_correction of the calendar belongs to the estimation and is
    # ignored here (it is the default of national_calendar(), hence no
    # warning); the @details of genTd() say so
    hd <- hd$days
  }
  hd <- as.list(hd)
  weight <- weight %||% rep(1, length(hd))
  if (!is.numeric(weight) || length(weight) != length(hd)) {
    stop(
      "'weight' must be a numeric vector with one element per holiday ",
      "of 'hd'",
      call. = FALSE
    )
  }
  unname(Map(
    function(h, w) genTd_entry(h, w, firstYear, lastYear), hd, weight
  ))
}

genTd_entry <- function(h, w, firstYear, lastYear) {
  if (is.character(h)) {
    return(genTd_str_entry(h, w, firstYear, lastYear))
  }
  if (!inherits(h, "JD3_HOLIDAY")) {
    stop(
      "every holiday of 'hd' must be a string or an rjd3toolkit holiday ",
      "(special_day(), fixed_day(), easter_day())",
      call. = FALSE
    )
  }
  if (inherits(h, "JD3_SPECIALDAY")) {
    name <- toupper(h$event %||% "")
    return(genTd_event(
      genTd_event_def(name), as.integer(h$offset %||% 0L),
      w * (h$weight %||% 1), name, firstYear, lastYear, h$validity
    ))
  }
  if (inherits(h, "JD3_FIXEDDAY")) {
    dates <- genTd_year_dates(
      sprintf("%02d-%02d", h$month, h$day), firstYear, lastYear
    )
    return(
      list(
        kind = "fixed", dates = genTd_valid(dates, h$validity),
        w = w * (h$weight %||% 1)
      )
    )
  }
  if (inherits(h, "JD3_EASTERDAY") && !isTRUE(h$julian)) {
    return(
      list(
        kind = "easter", offset = as.integer(h$offset),
        w = w * (h$weight %||% 1), validity = h$validity
      )
    )
  }
  genTd_resolved(h, w, firstYear, lastYear)
}

# a holiday of the hd character notation: an exact date, an easter relation
# or the name of one of the pre-specified rjd3toolkit special days
genTd_str_entry <- function(x, w, firstYear, lastYear) {
  if (grepl("^easter([+-][0-9]+)?$", x, ignore.case = TRUE)) {
    return(
      list(
        kind = "easter", offset = genTd_easter_off(x), w = w,
        validity = NULL
      )
    )
  }
  name <- toupper(x)
  def <- genTd_event_def(name)
  if (!is.na(def)) {
    return(genTd_event(def, 0L, w, name, firstYear, lastYear))
  }
  ok <- tryCatch(
    !is.na(as.Date(paste0("2004-", x), format = "%Y-%m-%d")),
    error = function(e) FALSE, warning = function(w) FALSE
  )
  if (ok) {
    return(
      list(
        kind = "fixed", dates = genTd_year_dates(x, firstYear, lastYear),
        w = w
      )
    )
  }
  # everything else is handed to rjd3toolkit, which knows the events it
  # implements; if it fails, genTd_resolved() reports the known names
  genTd_resolved(rjd3toolkit::special_day(name), w, firstYear, lastYear)
}

# the documented definition of a pre-specified special day, NA for the
# events that follow neither an exact date nor an easter relation
genTd_event_def <- function(name) {
  if (name %in% names(p3_specialdays)) {
    p3_specialdays[[name]]
  } else {
    NA_character_
  }
}

# the entry of a pre-specified special day: an exact date or an easter
# relation, both shifted by the offset of the holiday object (an exact date
# may leave its month or year that way); events without a definition are
# resolved through rjd3toolkit
genTd_event <- function(
  def, off, w, name, firstYear, lastYear, validity = NULL
) {
  if (is.na(def)) {
    return(genTd_resolved(
      rjd3toolkit::special_day(
        name, offset = off, weight = 1, validity = validity
      ),
      w, firstYear, lastYear
    ))
  }
  if (grepl("^easter", def)) {
    return(
      list(
        kind = "easter", offset = genTd_easter_off(def) + off,
        w = w, validity = validity
      )
    )
  }
  list(
    kind = "fixed", w = w,
    dates = genTd_valid(
      genTd_year_dates(def, firstYear, lastYear) + off, validity
    )
  )
}

# dates and kind of a holiday that rjd3toolkit resolves: a holiday falling
# on the same date every year becomes "fixed", one with a constant offset to
# easter sunday becomes "easter", everything else stays "generic"; the
# validity period is applied here (rjd3toolkit::holidays() ignores it)
genTd_resolved <- function(h, w, firstYear, lastYear) {
  start <- as.Date(sprintf("%04d-01-01", firstYear))
  hol <- tryCatch(
    rjd3toolkit::holidays(
      calendar = rjd3toolkit::national_calendar(list(h)),
      start = format(start),
      length = as.integer(
        as.Date(sprintf("%04d-12-31", lastYear)) - start
      ) + 1L,
      type = "All"
    ),
    error = function(e) NULL
  )
  if (is.null(hol)) {
    stop(
      "could not resolve the holiday '",
      h$event %||% substr(class(h)[1], 5, 100), "' with rjd3toolkit for ",
      firstYear, "..", lastYear, "; known special days are: ",
      paste(names(p3_specialdays), collapse = ", "),
      call. = FALSE
    )
  }
  dates <- genTd_valid(
    as.Date(rownames(hol)[which(hol[, 1] != 0)]), h$validity
  )
  if (length(dates) == 0) {
    return(list(kind = "fixed", dates = dates, w = w * (h$weight %||% 1)))
  }
  if (all(format(dates, "%m-%d") == format(dates[1], "%m-%d"))) {
    return(list(kind = "fixed", dates = dates, w = w * (h$weight %||% 1)))
  }
  ea <- as.Date(Easter(year = seq.int(firstYear, lastYear)))
  off <- unclass(dates - ea[as.integer(format(dates, "%Y")) - firstYear + 1L])
  if (length(unique(off)) == 1L) {
    return(
      list(
        kind = "easter", offset = as.integer(off[1]),
        w = w * (h$weight %||% 1), validity = h$validity
      )
    )
  }
  list(kind = "generic", dates = dates, w = w * (h$weight %||% 1))
}

# the offset of an "easter", "easter+N" or "easter-N" relation
genTd_easter_off <- function(x) {
  off <- substr(x, 7, nchar(x))
  if (off == "") 0L else as.integer(off)
}

# the dates of a holiday that falls on the same day of month every year
genTd_year_dates <- function(x, firstYear, lastYear) {
  as.Date(paste0(seq.int(firstYear, lastYear), "-", x))
}

# drops the dates outside the validity period of a holiday object
genTd_valid <- function(dates, validity) {
  if (is.null(validity)) {
    return(dates)
  }
  keep <- !is.na(dates)
  if (!is.null(validity$start)) {
    keep <- keep & dates >= as.Date(validity$start)
  }
  if (!is.null(validity$end)) {
    keep <- keep & dates <= as.Date(validity$end)
  }
  dates[keep]
}
