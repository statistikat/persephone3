message("Trading-day calendars: shapes, counts, weighting and centering")

calendar <- function(hd = character(), weight = rep(1, length(hd)),
                     firstYear = 2020, lastYear = 2021, freq = 12,
                     adjustEaster = "exact") {
  genTd(freq = freq, firstYear = firstYear, lastYear = lastYear,
        hd = hd, weight = weight, adjustEaster = adjustEaster)
}
base <- calendar()
expect_identical(length(base), 3L)
expect_equal(vapply(base, ncol, integer(1)), c(7L, 6L, 7L))
expect_equal(vapply(base, nrow, integer(1)), rep(24L, 3L))
expect_equal(unname(base[[1]][1, ]), c(4, 4, 5, 5, 5, 4, 4)) # January 2020
expect_equal(unname(base[[1]][2, ]), c(4, 4, 4, 4, 4, 5, 4)) # February 2020
expect_equal(as.numeric(rowSums(base[[1]]))[1:3], c(31, 29, 31))
expect_equal(base[[2]], base[[3]][, 1:6])
expect_equal(as.numeric(base[[3]][c(2, 14), "lpYear"]), c(0.75, -0.25))
expect_equal(sum(abs(base[[3]][-c(2, 14), "lpYear"])), 0)
expect_identical(colnames(base[[1]]), c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# A zero-weight holiday must affect neither observed contrasts nor centering.
for (mode in c("exact", "approximate")) {
  zero <- calendar(c("easter+1", "01-01"), c(0, 0), adjustEaster = mode)
  expect_equal(zero, base)
  sunday <- calendar("easter", adjustEaster = mode)
  expect_equal(sunday, base)
  full <- calendar("easter+1", adjustEaster = mode)
  half <- calendar("easter+1", weight = 0.5, adjustEaster = mode)
  expect_equal(full[[1]], base[[1]]) # first result is raw weekday counts
  expect_equal(as.numeric(half[[2]] - base[[2]]), as.numeric((full[[2]] - base[[2]]) / 2))
  expect_equal(as.numeric(half[[3]] - base[[3]]), as.numeric((full[[3]] - base[[3]]) / 2))
  # Easter Monday 2020 is April 13: transfer one Monday to Sunday.
  expect_equal(unname(full[[2]][4, ] - base[[2]][4, ]), c(-2, -1, -1, -1, -1, -1))
  expect_equal(unname(full[[2]][3, ] - base[[2]][3, ]), rep(0, 6))

  dist <- new.env()
  utils::data(list = paste0("eaDist_", if (mode == "exact") "exact" else "approx"),
              package = "persephone3", envir = dist)
  probability <- dist[[paste0("eaDist_", if (mode == "exact") "exact" else "approx")]]
  # Easter Monday ranges March 23--April 26; nine of the 35 dates are in March.
  pMarch <- sum(probability[1:9])
  correction <- full[[3]][, 1:6] - full[[2]]
  expect_equal(unname(correction[3, ]), pMarch * c(2, 1, 1, 1, 1, 1))
  expect_equal(unname(correction[4, ]), (1 - pMarch) * c(2, 1, 1, 1, 1, 1))
  expect_equal(sum(abs(correction[-c(3, 4, 15, 16), ])), 0)

  # Good Friday ranges March 20--April 23 and always affects Friday.
  friday <- calendar("easter-2", adjustEaster = mode)
  correction <- friday[[3]][, 1:6] - friday[[2]]
  expect_equal(unname(correction[3, ]), sum(probability[1:12]) * c(1, 1, 1, 1, 2, 1))
}

# Fixed days and supported aliases have the same documented semantics.
newyear <- calendar("01-01")
expect_equal(calendar("NewYearsDay"), newyear)
expect_equal(calendar("GoodFriday"), calendar("easter-2"))
expect_equal(calendar("EasterMonday"), calendar("easter+1"))
expect_equal(calendar("Ascension"), calendar("easter+39"))
expect_equal(calendar("ChristmasDay"), calendar("12-25"))
expect_equal(unname(newyear[[2]][1, ] - base[[2]][1, ]), c(-1, -1, -2, -1, -1, -1))
expect_equal(unname(newyear[[3]][1, 1:6] - newyear[[2]][1, ]), rep(1, 6))
expect_equal(calendar(c("01-01", "NewYearsDay"), c(0.25, 0.75)), calendar("01-01", 0.75))
expect_equal(calendar(c("easter+01", "EasterMonday"), c(0.25, 0.75)), calendar("easter+1", 0.75))
expect_equal(calendar(c("01-01", "01-01"), c(2, 1)), calendar("01-01", 2))
# Preserve finite nonnegative legacy scaling values above one.
expect_equal(as.numeric(calendar("01-01", 5)[[2]] - base[[2]]), as.numeric(5 * (newyear[[2]] - base[[2]])))

# Ascension was May 1 in 2008; one day receives max(0.4, 0.8), not 1.2.
none08 <- calendar(firstYear = 2008, lastYear = 2008)
fixed08 <- calendar("05-01", 0.4, firstYear = 2008, lastYear = 2008)
easter08 <- calendar("easter+39", 0.8, firstYear = 2008, lastYear = 2008)
both08 <- calendar(c("05-01", "easter+39"), c(0.4, 0.8), firstYear = 2008, lastYear = 2008)
expect_equal(unname(both08[[2]][5, ] - none08[[2]][5, ]), 0.8 * c(-1, -1, -1, -2, -1, -1))
expect_equal(both08[[2]][5, ], easter08[[2]][5, ])
dist <- new.env()
utils::data("eaDist_exact", package = "persephone3", envir = dist)
# Only the second possible Easter date (March 23) gives the May 1 collision.
center <- function(x) x[[3]][, 1:6] - x[[2]]
removed_overlap <- center(fixed08) + center(easter08) - center(both08)
expect_equal(unname(removed_overlap[5, ]), 0.4 * dist$eaDist_exact[2] * c(1, 1, 1, 2, 1, 1))
expect_equal(sum(abs(removed_overlap[-5, ])), 0)

# February 29 exists only in 2020 here; its fixed-day centering incidence is 1/2.
leap_holiday <- calendar("02-29")
expect_equal(unname(leap_holiday[[2]][2, ] - base[[2]][2, ]), c(-1, -1, -1, -1, -1, -2))
expect_equal(unname(leap_holiday[[2]][14, ] - base[[2]][14, ]), rep(0, 6))
expect_equal(unname(center(leap_holiday)[c(2, 14), ]), matrix(0.5, 2, 6))

# Gregorian century rules: 1900 is not leap, 2000 is leap.
expect_equal(sum(calendar(firstYear = 1900, lastYear = 1900)[[1]][2, ]), 28)
expect_equal(sum(calendar(firstYear = 2000, lastYear = 2000)[[1]][2, ]), 29)
# The documented offset boundaries remain within the accepted calendar year.
edge <- calendar(c("easter-80", "easter+250"), firstYear = 2020, lastYear = 2020)
expect_true(all(is.finite(edge[[3]])))

# Quarterly contracts are sums, including leap-year and holiday effects.
monthly <- calendar(c("NewYearsDay", "easter+1", "12-24"), c(1, 0.5, 0.25))
quarterly <- calendar(c("NewYearsDay", "easter+1", "12-24"), c(1, 0.5, 0.25), freq = 4)
for (j in 1:3) {
  expect_equal(frequency(quarterly[[j]]), 4)
  expected <- t(vapply(seq_len(8), function(q) colSums(monthly[[j]][(3 * q - 2):(3 * q), , drop = FALSE]),
                        numeric(ncol(monthly[[j]]))))
  expect_equal(dim(quarterly[[j]]), dim(expected))
  expect_equal(as.numeric(quarterly[[j]]), as.numeric(expected))
}
expect_equal(unname(quarterly[[1]][1, ]), rep(13, 7))
expect_equal(as.numeric(quarterly[[3]][c(1, 5), "lpYear"]), c(0.75, -0.25))

# Numeric weekday indexing makes results independent of localized weekday names.
locale_check <- function() {
  original <- Sys.getlocale("LC_TIME")
  on.exit(suppressWarnings(Sys.setlocale("LC_TIME", original)))
  baseline <- calendar(c("01-01", "easter+1"))
  for (locale in c("C", "de_AT.UTF-8", "fr_FR.UTF-8")) {
    changed <- suppressWarnings(Sys.setlocale("LC_TIME", locale))
    if (nzchar(changed)) expect_equal(calendar(c("01-01", "easter+1")), baseline)
  }
}
locale_check()

message("Trading-day calendars reject invalid and executable inputs")
expect_error(calendar(freq = 1), "freq must be")
expect_error(calendar(freq = c(4, 12)), "freq must be")
expect_error(calendar(firstYear = 2021, lastYear = 2020), "ordered integer years")
expect_error(calendar(firstYear = 2020.5), "ordered integer years")
expect_error(calendar(lastYear = Inf), "ordered integer years")
expect_error(calendar(firstYear = 1500), "ordered integer years")
expect_error(calendar("02-30"), "valid mm-dd")
expect_error(calendar("13-01"), "valid mm-dd")
expect_error(calendar("1-01"), "Unsupported holiday")
expect_error(calendar("USMemorialDay"), "Unsupported holiday")
expect_error(calendar("easter+1.5"), "Unsupported holiday")
expect_error(calendar("easter+1;stop('executed')"), "Unsupported holiday")
expect_error(calendar("easter+10000000000000000000000000000000"), "offsets must be")
expect_error(calendar("easter-81"), "offsets must be")
expect_error(calendar("easter+251"), "offsets must be")
expect_error(calendar(list(c("01-01", "02-01"))), "scalar, nonmissing")
expect_error(calendar(NA_character_), "scalar, nonmissing")
expect_error(calendar("01-01", NA_real_), "finite nonnegative")
expect_error(calendar("01-01", -1), "finite nonnegative")
expect_error(calendar(c("01-01", "02-01"), 1), "one finite")
expect_error(calendar("01-01", 1 + 1i), "finite nonnegative")
expect_error(calendar("01-01", Inf), "finite nonnegative")
expect_error(calendar(adjustEaster = "unknown"), "adjustEaster must be")
expect_error(calendar(adjustEaster = c("exact", "approximate")), "adjustEaster must be")
