# Compare plain R reference results without loading rjd3, Rcpp or Persephone.
# Tolerances are looked up by exact path; unregistered numeric fields fail closed.
compare_reference <- function(actual, expected, tolerances) {
  if (!all(c("path", "atol", "rtol") %in% names(tolerances))) stop("Invalid tolerance manifest")
  if (anyDuplicated(tolerances$path)) stop("Duplicate tolerance paths")
  if (any(!is.finite(tolerances$atol) | !is.finite(tolerances$rtol) |
          tolerances$atol < 0 | tolerances$rtol < 0)) stop("Invalid tolerances")
  issues <- list()
  record <- function(path, kind, index = NA_integer_, time = NA_real_, expected = "", actual = "", max_abs_error = NA_real_) {
    issues[[length(issues) + 1L]] <<- data.frame(path = path, kind = kind, index = index,
      time = time, expected = expected, actual = actual, max_abs_error = max_abs_error,
      stringsAsFactors = FALSE)
  }
  visit <- function(a, e, path = "") {
    display_path <- if (nzchar(path)) path else "/"
    if (!identical(typeof(a), typeof(e))) {
      record(display_path, "type", expected = typeof(e), actual = typeof(a)); return()
    }
    if (length(a) != length(e)) {
      record(display_path, "length", expected = as.character(length(e)), actual = as.character(length(a))); return()
    }
    if (!identical(attributes(a), attributes(e))) {
      record(display_path, "attributes", expected = paste(capture.output(dput(attributes(e))), collapse = " "),
             actual = paste(capture.output(dput(attributes(a))), collapse = " "))
    }
    if (is.list(e)) {
      nm <- names(e)
      for (i in seq_along(e)) {
        key <- if (!is.null(nm) && nzchar(nm[[i]])) nm[[i]] else paste0("[", i, "]")
        visit(a[[i]], e[[i]], paste0(path, "/", key))
      }
    } else if (is.double(e) || is.integer(e)) {
      rule <- tolerances[tolerances$path == display_path, , drop = FALSE]
      if (nrow(rule) != 1L) {record(display_path, "missing_tolerance"); return()}
      nonfinite <- !is.finite(e) | !is.finite(a)
      state <- function(x) ifelse(is.nan(x), "NaN", ifelse(is.na(x), "NA", ifelse(is.infinite(x), ifelse(x > 0, "+Inf", "-Inf"), "finite")))
      bad_state <- which(state(a) != state(e))
      if (length(bad_state)) {
        i <- bad_state[[1]]
        record(display_path, "nonfinite_state", i, expected = state(e)[i], actual = state(a)[i])
      }
      finite <- which(!nonfinite)
      delta <- abs(as.numeric(a)[finite] - as.numeric(e)[finite])
      failed <- delta > rule$atol + rule$rtol * abs(as.numeric(e)[finite])
      if (any(failed)) {
        i <- finite[which(failed)[1L]]
        ti <- if (is.ts(e)) as.numeric(time(e))[(i - 1L) %% NROW(e) + 1L] else NA_real_
        record(display_path, "numeric", i, ti, format(e[i], digits = 17), format(a[i], digits = 17), max(delta))
      }
    } else if (!identical(a, e)) {
      i <- if (length(e)) which(is.na(a) != is.na(e) | (!is.na(a) & !is.na(e) & a != e))[1L] else NA_integer_
      record(display_path, "exact", i,
             expected = if (!is.na(i)) as.character(e[i]) else "",
             actual = if (!is.na(i)) as.character(a[i]) else "")
    }
  }
  visit(actual, expected)
  if (length(issues)) {
    result <- do.call(rbind, issues)
    attr(result, "first_divergence") <- result[1L, , drop = FALSE]
    result
  } else data.frame(path = character(), kind = character(), index = integer(), time = double(),
                    expected = character(), actual = character(), max_abs_error = double(), stringsAsFactors = FALSE)
}
