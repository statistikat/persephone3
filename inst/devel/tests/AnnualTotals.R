annual_totals <- function(original,
                                       sa,
                                       complete_years = TRUE,
                                       norm_span = c("all", "common")) {
  same_time_ts <- function(x, y) {
      inherits(x, "ts") &&
      inherits(y, "ts") &&
      identical(str(tsp(x)), str(tsp(y)))
    }
  stopifnot(same_time_ts(original, sa)[1])

  time_to_dt <- function(tsx) {
    tt <- time(tsx)
    yr <- as.numeric(floor(tt))
    # for monthly/quart/etc: cycle gives within-year period index
    per <- as.numeric(cycle(tsx))
    data.table(year = yr, period = per, value = as.numeric(tsx))
  }

  o_dt <- time_to_dt(original)
  s_dt <- time_to_dt(sa)
  
  # Determine which years are "complete"
  freq <- (frequency(original))
  years <- o_dt[, .N, by = year][N == freq, year]

  # Annual totals
  annual_sum <- function(df, yrs) {
    tapply(df$value[df$year %in% yrs], df$year[df$year %in% yrs], sum, na.rm = TRUE)
  }

  orig_annual <- annual_sum(o_dt, years)
  sa_annual   <- annual_sum(s_dt, years)

  # Ensure same ordering
  keep_years_chr <- as.character(sort(years))
  orig_annual <- orig_annual[keep_years_chr]
  sa_annual   <- sa_annual[keep_years_chr]

  diff_annual <- orig_annual - sa_annual
  max_abs_diff <- max(abs(diff_annual), na.rm = TRUE)

  # Euclidean norm of original series 
  denom_series <- as.numeric(na.omit(original))
  denom <- sqrt(sum(denom_series^2))

  Q <- max_abs_diff / denom

  # JD+ thresholds for annual totals diagnostic 
  quality <- if (is.na(Q)) {
    "Undefined"
  } else if (Q > 0.5) {
    "Error"
  } else if (Q > 0.1) {
    "Severe"
  } else if (Q > 0.05) {
    "Bad"
  } else if (Q > 0.01) {
    "Uncertain"
  } else {
    "Good"
  }
  return(list(Q, quality))

}
