#' Generation of trading-day regressors
#'
#' Generate calendar counts and trading-day contrasts with weighted holidays.
#' A holiday transfers its weight from its weekday to Sunday. Holidays on Sunday
#' have no effect. Coincident holidays use the largest weight, so duplicate
#' declarations do not count the same day twice.
#'
#' @param freq Output frequency, either 12 (monthly) or 4 (quarterly).
#' @param firstYear,lastYear First and last Gregorian years, integers from 1583
#'   through 9999, with `firstYear <= lastYear`.
#' @param hd Character vector or list of scalar character holiday definitions.
#'   Use fixed dates (`"mm-dd"`), Easter-relative dates (`"easter"`,
#'   `"easter+39"`, `"easter-2"`), or the named aliases listed below. Easter
#'   offsets must be integers between -80 and 250, keeping every possible date
#'   in the same calendar year. `character()` gives a calendar without holidays.
#' @param weight Numeric nonnegative finite vector with one value per holiday.
#'   Zero ignores a holiday, one transfers a full day, and fractional values
#'   transfer part of a day. Values above one remain supported as legacy scaling
#'   weights. Coincident definitions use their maximum, not their sum.
#' @param adjustEaster Either `"exact"` or `"approximate"`, selecting the packaged
#'   Easter-date probability distribution used for centering.
#' @details
#' Supported fixed-date aliases are `NewYearsDay`, `SolemnityOfMary`, `Epiphany`,
#' `Annunciation`, `LaborDay`, `AssumptionOfMary`, `BirthOfVirginMary`,
#' `AllSaints`, `AllSouls`, `ChristmasEve`, `ChristmasDay`, `BoxingDay`, and
#' `NewYearsEve`. Easter-relative aliases are `Easter`, `EasterSunday`,
#' `EasterMonday`, `GoodFriday`, `AshWednesday`, `PalmSunday`, `Ascension`,
#' `Pentecost`, `PentecostMonday`, `TrinitySunday`, and `CorpusChristi`.
#' Other names from `timeDate::listHolidays()` are not implicitly evaluated.
#'
#' The centering correction uses a uniform weekday convention for fixed dates
#' and the selected probability distribution over March 22 through April 25 for
#' Easter-relative dates. Fixed-date occurrence counts (including February 29)
#' are averaged over the requested years. Easter probabilities are shifted on
#' each year's calendar and averaged over those years. Only the additional
#' holiday weight beyond an overlapping fixed holiday is included in the Easter
#' correction. All corrections use the supplied weights.
#'
#' The first result deliberately retains the historical implementation's raw
#' weekday counts. Holiday-adjusted contrasts, rather than adjusted counts,
#' are returned in the second and third results. No working-day regressor is
#' returned. Quarterly values are sums of the three corresponding months.
#' @return An unnamed list of three multivariate `ts` objects:
#'   1. Seven raw weekday counts, Monday through Sunday, before holidays.
#'   2. Six holiday-adjusted, uncentered contrasts: Monday through Saturday
#'      minus the adjusted Sunday count.
#'   3. The six centered contrasts followed by `lpYear`, which is 0.75 for leap
#'      February, -0.25 for other Februaries, and zero otherwise.
#' @examples
#' calendar <- genTd(firstYear = 2020, lastYear = 2021,
#'   hd = c("NewYearsDay", "easter+1", "12-24"), weight = c(1, 1, 0.5))
#' calendar[[3]][, 1:6]
#' @export
genTd <- function(freq = 12, firstYear = 1960, lastYear = 2099, hd,
                  weight = rep(1, length(hd)), adjustEaster = "exact") {
  scalar_integer <- function(x) is.numeric(x) && !is.complex(x) && length(x) == 1L &&
    !is.na(x) && is.finite(x) && x == floor(x)
  if (!scalar_integer(freq) || !freq %in% c(4, 12)) {
    stop("freq must be 4 or 12.", call. = FALSE)
  }
  if (!scalar_integer(firstYear) || !scalar_integer(lastYear) ||
      firstYear < 1583 || lastYear > 9999 || firstYear > lastYear) {
    stop("firstYear and lastYear must be ordered integer years from 1583 to 9999.", call. = FALSE)
  }
  if (!is.character(adjustEaster) || length(adjustEaster) != 1L ||
      is.na(adjustEaster) || !adjustEaster %in% c("exact", "approximate")) {
    stop("adjustEaster must be 'exact' or 'approximate'.", call. = FALSE)
  }
  if (missing(hd) || !(is.character(hd) || is.list(hd)) || !is.null(dim(hd)) ||
      !all(vapply(as.list(hd), function(x) is.character(x) && length(x) == 1L &&
                  !is.na(x) && nzchar(x), logical(1)))) {
    stop("hd must contain scalar, nonmissing character holiday definitions.", call. = FALSE)
  }
  hd <- unlist(hd, use.names = FALSE)
  if (is.null(hd)) hd <- character()
  if (!is.numeric(weight) || is.complex(weight) || !is.null(dim(weight)) || length(weight) != length(hd) ||
      any(!is.finite(weight)) || any(weight < 0)) {
    stop("weight must contain one finite nonnegative number per holiday.", call. = FALSE)
  }
  aliases <- c(NewYearsDay = "01-01", SolemnityOfMary = "01-01", Epiphany = "01-06",
    Annunciation = "03-25", LaborDay = "05-01", AssumptionOfMary = "08-15",
    BirthOfVirginMary = "09-08", AllSaints = "11-01", AllSouls = "11-02",
    ChristmasEve = "12-24", ChristmasDay = "12-25", BoxingDay = "12-26",
    NewYearsEve = "12-31", Easter = "easter", EasterSunday = "easter",
    EasterMonday = "easter+1", GoodFriday = "easter-2", AshWednesday = "easter-46",
    PalmSunday = "easter-7", Ascension = "easter+39", Pentecost = "easter+49",
    PentecostMonday = "easter+50", TrinitySunday = "easter+56", CorpusChristi = "easter+60")
  named <- hd %in% names(aliases)
  hd[named] <- unname(aliases[hd[named]])
  is_easter <- grepl("^easter([+-][0-9]+)?$", hd)
  is_fixed <- grepl("^[0-9]{2}-[0-9]{2}$", hd)
  if (any(!is_easter & !is_fixed)) {
    stop("Unsupported holiday definition: ", paste(hd[!is_easter & !is_fixed], collapse = ", "),
         ". Use mm-dd, an Easter offset, or a documented named alias.", call. = FALSE)
  }
  fixed_check <- if (any(is_fixed)) as.Date(paste0("2000-", hd[is_fixed]), format = "%Y-%m-%d") else as.Date(character())
  if (anyNA(fixed_check) || any(format(fixed_check, "%m-%d") != hd[is_fixed])) {
    stop("Fixed holidays must be valid mm-dd dates (02-29 is allowed).", call. = FALSE)
  }
  offsets <- sub("^easter", "", hd[is_easter])
  offsets[offsets == ""] <- "0"
  offsets <- suppressWarnings(as.numeric(offsets))
  if (any(!is.finite(offsets)) || any(offsets < -80 | offsets > 250)) {
    stop("Easter offsets must be integers between -80 and 250.", call. = FALSE)
  }
  # Canonicalize aliases and offsets before taking maximum duplicate weights.
  if (any(is_easter)) hd[is_easter] <- paste0("easter", offsets)
  unique_weights <- if (length(hd)) tapply(weight, hd, max) else setNames(numeric(), character())
  unique_weights <- unique_weights[unique_weights > 0]
  fixed <- unique_weights[!startsWith(names(unique_weights), "easter")]
  moving <- unique_weights[startsWith(names(unique_weights), "easter")]
  moving_offsets <- as.numeric(sub("^easter", "", names(moving)))

  years <- seq.int(firstYear, lastYear)
  nyears <- length(years)
  month <- rep(seq_len(12), times = nyears)
  year <- rep(years, each = 12L)
  first <- as.Date(sprintf("%04d-%02d-01", year, month))
  leap <- function(y) y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)
  days <- rep(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), times = nyears)
  days[month == 2 & leap(year)] <- 29
  weekday <- function(date) as.integer((as.numeric(date) + 3) %% 7 + 1)
  raw <- matrix(rep(days %/% 7, times = 7L), ncol = 7L)
  for (k in 0:5) {
    rows <- which(days %% 7 > k)
    columns <- (weekday(first[rows]) + k - 1L) %% 7L + 1L
    raw[cbind(rows, columns)] <- raw[cbind(rows, columns)] + 1
  }
  adjusted <- raw
  centering <- matrix(0, nrow = 12L, ncol = 6L)
  holiday_dates <- numeric()
  holiday_weights <- numeric()
  for (i in seq_along(fixed)) {
    dates <- as.Date(paste0(years, "-", names(fixed)[i]), format = "%Y-%m-%d")
    dates <- dates[!is.na(dates)]
    holiday_dates <- c(holiday_dates, as.numeric(dates))
    holiday_weights <- c(holiday_weights, rep(fixed[i], length(dates)))
    m <- as.integer(substr(names(fixed)[i], 1L, 2L))
    centering[m, ] <- centering[m, ] + unname(fixed[i]) * length(dates) / nyears
  }
  if (length(moving)) {
    easter <- as.Date(timeDate::Easter(year = years))
    distributions <- new.env(parent = emptyenv())
    distribution_name <- paste0("eaDist_", if (adjustEaster == "exact") "exact" else "approx")
    utils::data(list = distribution_name, package = "persephone3", envir = distributions)
    probability <- distributions[[distribution_name]]
    if (!is.numeric(probability) || length(probability) != 35L ||
        any(!is.finite(probability) | probability < 0) || abs(sum(probability) - 1) > 1e-10) {
      stop("Invalid packaged Easter probability distribution.", call. = FALSE)
    }
    earliest <- as.numeric(as.Date(paste0(years, "-03-22")))
    for (i in seq_along(moving)) {
      offset <- moving_offsets[i]
      holiday_dates <- c(holiday_dates, as.numeric(easter + offset))
      holiday_weights <- c(holiday_weights, rep(moving[i], nyears))
      wd <- (offset + 6L) %% 7L + 1L
      if (wd == 7L) next
      possible <- as.Date(rep(earliest, each = 35L) + rep(0:34 + offset, times = nyears),
                          origin = "1970-01-01")
      overlap <- unname(fixed[format(possible, "%m-%d")])
      overlap[is.na(overlap)] <- 0
      increment <- pmax(unname(moving[i]) - overlap, 0)
      masses <- rep(probability, times = nyears) * increment / nyears
      by_month <- tapply(masses, as.integer(format(possible, "%m")), sum)
      m <- as.integer(names(by_month))
      centering[m, ] <- centering[m, , drop = FALSE] + as.numeric(by_month)
      centering[m, wd] <- centering[m, wd] + as.numeric(by_month)
    }
  }
  if (length(holiday_dates)) {
    by_date <- tapply(holiday_weights, holiday_dates, max)
    dates <- as.Date(as.numeric(names(by_date)), origin = "1970-01-01")
    rows <- (as.integer(format(dates, "%Y")) - firstYear) * 12L + as.integer(format(dates, "%m"))
    wd <- weekday(dates)
    for (i in which(wd != 7L)) {
      adjusted[rows[i], wd[i]] <- adjusted[rows[i], wd[i]] - by_date[i]
      adjusted[rows[i], 7L] <- adjusted[rows[i], 7L] + by_date[i]
    }
  }
  contrasts <- adjusted[, 1:6, drop = FALSE] - adjusted[, 7L]
  centered <- contrasts + centering[month, , drop = FALSE]
  leap_regressor <- ifelse(month == 2L, ifelse(leap(year), 0.75, -0.25), 0)
  day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  make_series <- function(values, labels) {
    result <- stats::ts(values, start = c(firstYear, 1L), frequency = 12)
    colnames(result) <- labels
    if (freq == 4) result <- stats::aggregate(result, nfrequency = 4, FUN = sum)
    result
  }
  list(make_series(raw, day_names), make_series(contrasts, day_names[1:6]),
       make_series(cbind(centered, leap_regressor), c(day_names[1:6], "lpYear")))
}
