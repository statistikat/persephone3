# -----------------------------------------------------------------------------
# Chain linking helpers for the indirect adjustment of chain linked series
#
# A chain linked series y is the product of an unchained (unlinked) series x
# and a chain linking factor f, y = x * f. The indirect adjustment of such a
# series is not the (weighted) sum of the adjusted chain linked components but
# has to be performed in the unchained space:
#
#   1. sa_i                  the seasonally adjusted chain linked components
#   2. ul_i     = sa_i / f_i unchained seasonally adjusted components
#   3. agg_ul   = sum_i w_i * ul_i
#   4. agg      = agg_ul * f_agg
#   5. agg_fin  = agg / mean(agg in ref_year) * 100
#
# The weights w_i are the weights of the unchained series: sum_i w_i * x_i is
# the unchained aggregate. The same weights applied to the chain linked
# components do not give the aggregate (they differ by the factor ratio).
# -----------------------------------------------------------------------------

# number of weight columns of a weights object (vector, ts, mts or list).
# mts is checked first, because an mts is a list as well.
n_weight_cols <- function(w) {
  if (is.null(w)) {
    return(0L)
  }
  if (stats::is.mts(w)) {
    return(ncol(w))
  }
  if (stats::is.ts(w)) {
    return(1L)
  }
  if (is.list(w)) {
    return(length(w))
  }
  length(w)
}

# expand a factor or weight series to the time instances of target: a series
# of a lower frequency is repeated within its periods (an annual weight covers
# all months of its year), a series that does not cover the full span of
# target is extended by its first and last value. `warn_prefix` is used for
# the message of the extension warning, pass NULL to stay silent.
cl_align <- function(w, target, warn_prefix = NULL) {
  if (is.null(w)) {
    return(NULL)
  }
  if (!stats::is.ts(w)) {
    stop("'w' must be a ts object", call. = FALSE)
  }
  ftar <- stats::frequency(target)
  fw <- stats::frequency(w)
  if (fw != ftar) {
    if (ftar %% fw != 0) {
      stop(
        "cannot align a series of frequency ", fw, " to frequency ", ftar,
        call. = FALSE
      )
    }
    # a series of a lower frequency (e.g. annual weights) covers all periods
    # of its own period: the value is repeated ftar / fw times and positioned
    # at the period start, window(frequency = ) would spread the values over
    # the following periods instead
    k <- ftar / fw
    p0 <- (stats::start(w)[1] * fw + stats::start(w)[2] - 1) * k + 1
    w <- ts(
      rep(as.numeric(w), each = k),
      start = c(floor((p0 - 1) / ftar), (p0 - 1) %% ftar + 1),
      frequency = ftar
    )
    fw <- ftar
  }
  # pick the values of w at the time instances of target by index arithmetic
  # (window() cannot reach beyond the span of w), values outside the span of w
  # come out as NA and are replaced by its first and last value
  pos0 <- stats::start(target)[1] * ftar + stats::start(target)[2] - 1
  wpos0 <- stats::start(w)[1] * fw + stats::start(w)[2] - 1
  n <- NROW(target)
  idx <- seq(pos0, pos0 + n - 1) - wpos0 + 1
  v <- as.numeric(w)
  out <- rep(NA_real_, n)
  known <- idx >= 1L & idx <= length(v)
  out[known] <- v[idx[known]]
  ok <- which(is.finite(out))
  if (length(ok) == 0) {
    stop(
      "the series does not overlap the span of the series it belongs to",
      call. = FALSE
    )
  }
  if (ok[1] > 1L || ok[length(ok)] < n) {
    out[seq_len(ok[1] - 1L)] <- out[ok[1]]
    out[seq(ok[length(ok)] + 1L, n)] <- out[ok[length(ok)]]
    if (!is.null(warn_prefix)) {
      warning(
        warn_prefix,
        " does not cover the full span of the series (",
        paste(stats::start(target), collapse = "."), "-",
        paste(stats::end(target), collapse = "."),
        "); it is extended by its first and last value.",
        call. = FALSE
      )
    }
  }
  ts(out, start = stats::start(target), frequency = ftar)
}

# unchain / rechain: y = x * f  <=>  x = y / f
cl_unchain <- function(x, f) {
  if (is.null(f)) {
    return(x)
  }
  out <- x / cl_align(f, x)
  out[!is.finite(out)] <- NA
  out
}

cl_rechain <- function(x, f) {
  if (is.null(f)) {
    return(x)
  }
  out <- x * cl_align(f, x)
  out[!is.finite(out)] <- NA
  out
}

# re-reference a series to its mean in ref_year (= 100), as in JDemetra+
cl_rereference <- function(x, ref_year) {
  if (is.null(ref_year)) {
    return(x)
  }
  freq <- stats::frequency(x)
  if (stats::start(x)[1] > ref_year || stats::end(x)[1] < ref_year) {
    stop(
      "ref_year ", ref_year, " is outside the span of the series (",
      stats::start(x)[1], "-", stats::end(x)[1], ")",
      call. = FALSE
    )
  }
  base <- mean(
    stats::window(x, start = c(ref_year, 1), end = c(ref_year, freq)),
    na.rm = TRUE
  )
  if (!is.finite(base) || base == 0) {
    stop(
      "cannot re-reference: the mean of the series in ref_year ", ref_year,
      " is missing or zero",
      call. = FALSE
    )
  }
  ts(as.numeric(x) / base * 100, start = stats::start(x), frequency = freq)
}

# named list of chain linking factors (one ts per series) from an mts, a list
# or a single ts. Names not needed are ignored, needed names that are missing
# stay in the returned $missing (their factor is 1).
cl_factor_list <- function(cl_weights, names) {
  if (is.null(cl_weights)) {
    return(NULL)
  }
  if (stats::is.mts(cl_weights)) {
    if (is.null(colnames(cl_weights))) {
      stop("an mts 'cl_weights' must be named by its columns", call. = FALSE)
    }
    f <- stats::setNames(
      lapply(seq_len(ncol(cl_weights)), function(j) {
        ts(as.numeric(cl_weights[, j]),
           start = stats::start(cl_weights),
           frequency = stats::frequency(cl_weights))
      }),
      colnames(cl_weights)
    )
  } else if (stats::is.ts(cl_weights)) {
    if (length(names) != 1L) {
      stop(
        "a single ts 'cl_weights' is only possible for a single series",
        call. = FALSE
      )
    }
    f <- stats::setNames(list(cl_weights), names)
  } else if (is.list(cl_weights)) {
    if (is.null(names(cl_weights))) {
      stop("a list 'cl_weights' must be named by the series", call. = FALSE)
    }
    f <- cl_weights
  } else {
    stop(
      "'cl_weights' must be an mts object, a named list of ts objects or a ",
      "single ts object",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(f), names)
  if (length(unknown) > 0) {
    stop(
      "'cl_weights' contains series that are not provided: ",
      paste(sQuote(unknown), collapse = ", "),
      call. = FALSE
    )
  }
  out <- stats::setNames(lapply(names, function(nm) f[[nm]]), names)
  lapply(out, function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    if (!stats::is.ts(x)) {
      stop("all entries of 'cl_weights' must be ts objects", call. = FALSE)
    }
    if (any(stats::na.omit(as.numeric(x)) <= 0)) {
      stop("all chain linking factors must be positive", call. = FALSE)
    }
    x
  })
}

# numeric matrix of time varying weights for a set of series: one column per
# series, aligned to the time instances of the first series. `weights` is NULL
# (equal weights), a vector (constant weights), a ts or an mts.
cl_weight_matrix <- function(weights, n, target, warn_prefix = NULL) {
  if (is.null(weights)) {
    return(NULL)
  }
  if (stats::is.mts(weights)) {
    if (ncol(weights) != n) {
      stop(
        "the weights mts has ", ncol(weights), " columns, but ", n,
        " sub-series are aggregated",
        call. = FALSE
      )
    }
    cols <- lapply(seq_len(ncol(weights)), function(j) {
      ts(as.numeric(weights[, j]),
         start = stats::start(weights),
         frequency = stats::frequency(weights))
    })
    aligned <- lapply(cols, cl_align, target = target, warn_prefix = warn_prefix)
    return(do.call("cbind", aligned))
  }
  if (stats::is.ts(weights)) {
    if (n != 1L) {
      stop(
        "a single ts of weights is only possible for a single sub-series",
        call. = FALSE
      )
    }
    return(matrix(
      as.numeric(cl_align(weights, target, warn_prefix)),
      ncol = 1
    ))
  }
  if (length(weights) == 1L && n > 1L) {
    weights <- rep(weights, n)
  }
  if (length(weights) != n) {
    stop(
      "the weights have length ", length(weights), ", but ", n,
      " sub-series are aggregated",
      call. = FALSE
    )
  }
  if (anyNA(weights) || any(!is.finite(weights))) {
    stop("the weights must be finite numbers", call. = FALSE)
  }
  matrix(rep(as.numeric(weights), each = NROW(target)), ncol = n)
}

# weighted sum (NOT weighted mean) of a list of ts objects. The series are
# aligned to their common time instances first (cbind() fills the periods that
# are not covered by a series with NA), so the result has the span of the
# longest series and NA where one of them is missing. `weights` is NULL (equal
# weights), a vector, a ts or an mts with one column per series.
cl_weighted_sum <- function(tss, weights = NULL, warn_prefix = NULL) {
  if (length(tss) == 1L && is.null(weights)) {
    return(tss[[1]])
  }
  m <- do.call("cbind", tss)
  if (is.null(weights)) {
    return(ts(rowSums(m), start = stats::start(m),
              frequency = stats::frequency(m)))
  }
  # the weights are aligned to the common time instances of all series
  wmat <- cl_weight_matrix(weights, ncol(m), m, warn_prefix)
  ts(rowSums(m * wmat), start = stats::start(m), frequency = stats::frequency(m))
}

# the aggregates of a groups structure that are not a sub-series of another
# aggregate (the roots of the structure)
cl_groups_roots <- function(groups) {
  aggs <- names(groups)
  roots <- setdiff(aggs, unique(unlist(groups, use.names = FALSE)))
  if (length(roots) == 0) {
    roots <- aggs
  }
  roots
}

# structure of a groups argument: which series are aggregates, which are leaves
# and the leaves of every aggregate. Aggregates are always built from their
# leaf series, i.e. the children of an aggregate that are aggregates themselves
# are replaced by their own children.
cl_groups_structure <- function(groups, series) {
  if (!is.list(groups) || is.null(names(groups)) ||
      any(nchar(names(groups)) == 0)) {
    stop(
      "'groups' must be a named list, one entry per aggregate series with the ",
      "names of its sub-series",
      call. = FALSE
    )
  }
  aggs <- names(groups)
  unknown <- unique(unlist(c(groups, aggs)))
  unknown <- setdiff(unknown, series)
  if (length(unknown) > 0) {
    stop(
      "'groups' refers to series that are not provided: ",
      paste(sQuote(unknown), collapse = ", "),
      call. = FALSE
    )
  }
  if (any(vapply(groups, length, 0L) < 2L)) {
    stop("every aggregate in 'groups' must have at least two sub-series",
         call. = FALSE)
  }
  # every aggregate is built from its leaf series; cl_groups_leaves() stops on
  # a cycle and on a leaf that would be used twice in the same aggregate
  leaves <- stats::setNames(
    lapply(aggs, cl_groups_leaves, groups = groups, aggs = aggs),
    aggs
  )
  list(aggregates = aggs, leaves = leaves)
}

# the leaf series of an aggregate: its sub-series, with every sub-series that
# is an aggregate itself replaced by its own (recursive) leaves
cl_groups_leaves <- function(groups, agg, aggs = names(groups), seen = character(0)) {
  if (agg %in% seen) {
    stop(
      "'groups' contains a cycle at '", agg, "': ",
      paste(c(seen, agg), collapse = " -> "),
      call. = FALSE
    )
  }
  children <- groups[[agg]]
  out <- character(0)
  for (ch in children) {
    if (ch %in% aggs) {
      out <- c(out, cl_groups_leaves(groups, ch, aggs, c(seen, agg)))
    } else {
      out <- c(out, ch)
    }
  }
  if (any(duplicated(out))) {
    stop(
      "the sub-series of '", agg, "' contain duplicates: ",
      paste(sQuote(unique(out[duplicated(out)])), collapse = ", "),
      call. = FALSE
    )
  }
  out
}
