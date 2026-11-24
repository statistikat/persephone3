# period of a time series as "2000.1-2019.12"
spanLabel <- function(s) {
  paste0(
    paste(stats::start(s), collapse = "."), "-",
    paste(stats::end(s), collapse = ".")
  )
}

# does a regressor keep up with the series? Regressors that are too short are
# answered by x13/tramoseats with an uninformative java error, so the frequency
# and the span are checked here (`name` is the argument shown in the message,
# `hint` says how to fix it).
check_regressor_span <- function(ts, x, name, hint) {
  if (stats::frequency(x) != stats::frequency(ts) ||
      stats::start(x)[1] > stats::start(ts)[1] ||
      stats::end(x)[1] < stats::end(ts)[1]) {
    stop(
      name, " must have the same frequency as 'ts' and cover its full span ",
      "(series: ", spanLabel(ts), ", regressors: ", spanLabel(x), "); ",
      hint,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# add variables to a modelling context (the `context` argument of x13_fast()),
# keeping the calendars and variables that are already in it. NULL (no context
# yet) and an empty list of variables do nothing.
#
# A regressor is either a time series or a regressor of a modelling context
# already; only the time series are given to modelling_context(), which turns
# them into the form the java side works with. The names that come out of it
# are kept: a regressor is addressed later as "<name of the variable in the
# context>.<name of its column>", so renaming what modelling_context() named
# would break that addressing. Two variables of the same name (single column
# regressors all end up in the variable "r") are therefore joined by their
# columns instead of being put next to each other.
context_add <- function(context, variables) {
  if (length(variables) == 0) {
    return(context)
  }
  raw <- vapply(variables, stats::is.ts, TRUE)
  if (is.null(context)) {
    if (all(raw)) {
      return(rjd3toolkit::modelling_context(variables = variables))
    }
    context <- rjd3toolkit::modelling_context()
  } else if (!is.list(context) ||
             !all(c("calendars", "variables") %in% names(context))) {
    stop(
      "'context' must be a modelling context created with ",
      "rjd3toolkit::modelling_context()",
      call. = FALSE
    )
  }
  new <- variables
  if (any(raw)) {
    new <- c(
      rjd3toolkit::modelling_context(variables = variables[raw])$variables,
      variables[!raw]
    )
  }
  for (nm in names(new)) {
    if (!nm %in% names(context$variables)) {
      context$variables[[nm]] <- new[[nm]]
      next
    }
    old <- context_pieces(context$variables[[nm]], nm)
    add <- context_pieces(new[[nm]], nm)
    both <- intersect(names(add), names(old))
    # the same regressor again (as it happens when updateParams() merges the
    # trading day regressors into the context of a run that already has them)
    # is left alone; a different regressor of the same name is an error
    same <- vapply(both, function(p) identical(old[[p]], add[[p]]), TRUE)
    if (any(!same)) {
      stop(
        "the modelling context already has the regressor",
        if (sum(!same) > 1) "s " else " ",
        paste0("\"", both[!same], "\"", collapse = ", "),
        "; pass it only once or give it another name",
        call. = FALSE
      )
    }
    context$variables[[nm]] <- c(old, add[!names(add) %in% both])
  }
  context
}

# the regressors of one variable of a modelling context as a named list: the
# variable of a single regressor is that time series itself, the variable of
# several regressors is a list of them named by their columns
context_pieces <- function(v, nm) {
  if (is.list(v)) {
    return(v)
  }
  stats::setNames(list(v), colnames(v)[[1]] %||% nm)
}

# add the regressors of user_context() to a specification: one call of
# add_usrdefvar() per regressor, because the specification arguments for such a
# regressor (usrdefvar.name and the like) hold only one of them. A regressor
# that the specification already has is left alone (updateParams() works on the
# specification of the earlier call, which still holds them).
spec_add_usrdefvars <- function(spec, vars) {
  ids <- vapply(vars, function(var) paste0(var$group, ".", var$name), "")
  seen <- spec_usrdefvar_ids(spec)
  for (i in which(!ids %in% seen)) {
    var <- vars[[i]]
    spec <- rjd3toolkit::add_usrdefvar(
      spec,
      group = var$group,
      name = var$name,
      label = var$label
    )
  }
  spec
}

# the ids ("group.name") of the user-defined regressors of a specification
spec_usrdefvar_ids <- function(spec) {
  regs <- lapply(c("regarima", "tramo"), function(block) {
    tryCatch(unclass(spec)[[block]]$regression, error = function(e) NULL)
  })
  ids <- unlist(lapply(regs, function(reg) {
    vapply(reg$users %||% list(), function(u) as.character(u$id), "")
  }), use.names = FALSE)
  unique(ids)
}

# tramoseats takes either the user-defined trading day regressors of `td` or
# the further regressors of `user`, not both (the second list replaces the
# first in the specification), while x13 estimates both together
check_user_td_combination <- function(method, td, user) {
  if (!is.null(td) && !is.null(user) && method == "tramoseats") {
    warning(
      "tramoseats does not estimate the user-defined trading day regressors ",
      "of `td` together with the further regressors of `user`; it uses the ",
      "regressors of `user` alone. x13 estimates both (see perX13()).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# name of a regressor that the java side accepts: only letters, digits and "_"
# (other characters are replaced, which is done silently by rjd3, so it is done
# here instead); a name must not start with a digit. The original name is kept
# as the label of the regressor, which is what summary() shows.
safe_var_name <- function(name) {
  out <- gsub("[^A-Za-z0-9_]", "_", name)
  out <- paste0(ifelse(grepl("^[0-9]", out), "V", ""), out)
  out
}

# the `user` argument of perX13()/perTramo(): user-defined regressors for the
# regression model that are not trading days, e.g. the leap year regressor
# (column "lpYear" of genTd()[[3]]). One named regressor is made per column,
# named by the column names; the name of a list element is used when that
# regressor has no column names. The result is what belongs to such a
# regressor: the variables for the modelling context and the arguments for
# rjd3toolkit::add_usrdefvar(), which perX13()/perTramo() add to the
# specification one by one (the spec cannot hold several regressors under the
# same "usrdefvar.*" arguments).
user_context <- function(ts, user, group = "r") {
  if (!stats::is.ts(ts)) {
    stop("'ts' must be a time series", call. = FALSE)
  }
  if (stats::is.ts(user)) {
    user <- list(user)
  }
  if (!is.list(user) || length(user) == 0 ||
      !all(vapply(user, stats::is.ts, TRUE))) {
    stop(
      "'user' must be a ts/mts object with one regressor per column or a ",
      "non-empty named list of such objects",
      call. = FALSE
    )
  }
  listNames <- names(user)
  if (is.null(listNames)) {
    listNames <- rep("", length(user))
  }
  variables <- list()
  vars <- list()
  for (i in seq_along(user)) {
    x <- as.matrix(user[[i]])
    check_regressor_span(
      ts, user[[i]], "'user'",
      paste(
        "build the regressor over the span of the series",
        "(for genTd() pass matching values of firstYear and lastYear)"
      )
    )
    base <- listNames[[i]]
    cols <- colnames(x)
    if (is.null(cols)) {
      cols <- if (ncol(x) == 1 && nzchar(base)) {
        base
      } else {
        paste0(if (nzchar(base)) base else "user", seq_len(ncol(x)))
      }
    }
    name <- safe_var_name(cols)
    dup <- intersect(name, names(variables))
    if (length(dup) > 0) {
      stop(
        "'user' has the regressor",
        if (length(dup) > 1) "s " else " ",
        paste0("\"", dup, "\"", collapse = ", "),
        " twice; name the columns of the regressors differently",
        call. = FALSE
      )
    }
    for (j in seq_along(name)) {
      variables[[name[[j]]]] <- stats::ts(
        x[, j, drop = FALSE],
        start = stats::start(user[[i]]),
        frequency = stats::frequency(user[[i]])
      )
      colnames(variables[[name[[j]]]]) <- name[[j]]
      vars[[length(vars) + 1L]] <- list(
        group = group, name = name[[j]], label = cols[[j]]
      )
    }
  }
  list(variables = variables, vars = vars)
}

#' Create trading day regressors and a modelling context
#'
#' Helper that constructs user-defined trading day regressors and registers
#' them in a modelling context for [rjd3x13::x13_fast()] or
#' [rjd3tramoseats::tramoseats_fast()]. It combines the three manual steps
#' shown in the rjd3 workflow - building the regressors, wrapping them in a
#' [rjd3toolkit::modelling_context()] and setting the trading days spec block
#' with `option = "UserDefined"` - into one call.
#'
#' @param ts the (unadjusted) time series the regressors are built for.
#' @param calendar a national calendar of class `JD3_CALENDAR`, created with
#'   [rjd3toolkit::national_calendar()]. If given, the regressors are computed
#'   with [rjd3toolkit::calendar_td()] on `ts`.
#' @param td precomputed trading day regressors as a `mts` object, for example
#'   the third element of [genTd()] (centered regressors, usually columns 1 to
#'   6) or the output of [rjd3toolkit::calendar_td()]. The full output of
#'   [genTd()] (a list of 3 regressor sets) is also accepted, its centered
#'   day regressors (`[[3]][, 1:6]`) are then used. Exactly one of `calendar`
#'   and `td` must be provided. A single regressor (e.g. the grouping
#'   `c(1, 1, 1, 1, 1, 0, 0)` of [rjd3toolkit::calendar_td()] for weekdays
#'   versus weekend and holidays) is supported; note that trading day
#'   regressors that are not significant are dropped by the estimation, see
#'   [perX13()].
#' @param groups integer vector passed to [rjd3toolkit::calendar_td()], the
#'   group of each day of the week (Monday to Saturday, then the holiday).
#'   Only used if `calendar` is given.
#' @param holiday integer passed to [rjd3toolkit::calendar_td()], the day
#'   considered as holiday. Only used if `calendar` is given.
#' @param contrasts logical passed to [rjd3toolkit::calendar_td()]. If `TRUE`
#'   (default), one regressor less than the number of groups is used and the
#'   regressors are centered, as in [genTd()]. Only used if `calendar` is
#'   given. With `FALSE` (one regressor per group, `group_0` to `group_6` in
#'   the default grouping) the regressors are nearly collinear with the
#'   regression constant, which may make x13 estimate the model with a log
#'   transformation whose result is returned on the log scale (the trend and
#'   seasonally adjusted series then do not overlay the original series).
#' @param name name under which the regressors are registered in the
#'   modelling context; the individual regressors are then addressed as
#'   `name.column`.
#' @param td_labels optional character vector with one name per trading day
#'   regressor (one per column of `td`) naming the regressors, e.g.
#'   `c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")`.
#'   The labels are used for the regressors registered in the modelling
#'   context and shown in the specification and in
#'   [summary][summary.persephone()].
#'   By default the regressors keep the column names of `td` (the day names of
#'   [genTd()], the `group_*` names of [rjd3toolkit::calendar_td()]).
#' @param method the seasonal adjustment method the specification is built
#'   for, i.e. "x13" or "tramoseats". Only relevant for the default of `test`.
#' @param test significance tests for the trading day regression coefficients,
#'   passed as `td.test` to the specification. For x13, one of `"Add"`,
#'   `"Remove"` or `"None"` (residual trading day test of the regression),
#'   optionally combined with `"Joint_F"` (joint F test); the default is
#'   `c("Add", "Joint_F")`. For tramoseats, one of `"Separate_T"`,
#'   `"Joint_F"` or `"None"`, the default being `"Joint_F"`. Other values are
#'   rejected with an error.
#' @return A list with three elements:
#'   \item{td}{the trading day regressors as a `mts` object}
#'   \item{context}{a [rjd3toolkit::modelling_context()] registering `td`
#'     under `name`}
#'   \item{speclist}{a list of spec arguments (`td.option`, `td.uservariable`,
#'     `td.test`) to be passed to [perX13()] or [perTramo()] via `speclist`}
#'
#' @seealso [genTd()], [rjd3toolkit::calendar_td()]
#' @keywords internal
td_context <- function(ts,
                       calendar = NULL,
                       td = NULL,
                       groups = c(1, 2, 3, 4, 5, 6, 0),
                       holiday = 7,
                       contrasts = TRUE,
                       name = "REGS_TD",
                       td_labels = NULL,
                       method = c("x13", "tramoseats"),
                       test = NULL) {
  method <- match.arg(method)
  if (is.null(calendar) && is.null(td)) {
    stop("either 'calendar' or 'td' must be provided", call. = FALSE)
  }
  if (!is.null(calendar) && !is.null(td)) {
    stop("only one of 'calendar' and 'td' may be provided", call. = FALSE)
  }
  if (!is.null(calendar) && !inherits(calendar, "JD3_CALENDARDEFINITION")) {
    stop(
      "'calendar' must be a JD3 calendar created with rjd3toolkit::national_calendar()",
      call. = FALSE
    )
  }
  # the output of genTd(): a list of the 3 regressor sets, of which the
  # centered day regressors (the 3rd, day columns only) are used
  if (!is.null(td) && is.list(td) && !stats::is.ts(td) &&
      length(td) == 3L && all(vapply(td, stats::is.ts, TRUE))) {
    td <- td[[3]][, 1:6, drop = FALSE]
  }
  if (!is.null(td) && !stats::is.ts(td)) {
    stop(
      "'td' must be a ts/mts object of trading day regressors or the ",
      "output of genTd()",
      call. = FALSE
    )
  }
  if (!stats::is.ts(ts)) {
    stop("'ts' must be a time series", call. = FALSE)
  }

  if (!is.null(calendar)) {
    td <- rjd3toolkit::calendar_td(
      calendar = calendar,
      s = ts,
      groups = groups,
      holiday = holiday,
      contrasts = contrasts
    )
  } else {
    check_regressor_span(
      ts, td, "'td'",
      "pass matching years to genTd() or calendar_td()"
    )
  }

  if (is.null(test)) {
    test <- if (method == "x13") c("Add", "Joint_F") else "Joint_F"
  }
  check_td_test(test, method)

  # name the regressors (the labels flow into the context and the spec and
  # are shown in summary())
  if (!is.null(td_labels)) {
    if (!is.character(td_labels) || length(td_labels) != ncol(td) ||
        anyNA(td_labels) || any(!nzchar(td_labels))) {
      stop(
        "'td_labels' must be a character vector with one non-empty name per ",
        "trading day regressor (", ncol(td), ")",
        call. = FALSE
      )
    }
    colnames(td) <- td_labels
  }

  # a single regressor appears in the estimation under the name the context
  # gives its variable, which is `name` and not its column (see below); the
  # column name (a day name or td_labels) is the better label, so it becomes
  # the default name of a single regressor
  if (ncol(td) == 1L && name == "REGS_TD" && !is.null(colnames(td))) {
    name <- colnames(td)[[1L]]
  }
  context <- rjd3toolkit::modelling_context(
    variables = stats::setNames(list(td), name)
  )
  # the specification has to address the regressors under the names the
  # context really assigned them to: modelling_context() stores a multi-column
  # regressor under the variable `name`, one named regressor per column, but a
  # single regressor (a ts or a single-column mts) under the group "r", named
  # by `name` and not by its column. A td.uservariable that does not resolve
  # is not rejected by rjd3 - the regressors are silently dropped from the
  # model and the residual trading day diagnostics end in a java
  # NullPointerException (upstream issue)
  vars <- context$variables[[1L]]
  speclist <- list(
    td.option = "UserDefined",
    td.uservariable = paste0(
      names(context$variables)[[1L]], ".",
      if (is.list(vars)) names(vars) else colnames(td)
    ),
    td.test = test
  )
  list(td = td, context = context, speclist = speclist)
}

# valid values of the td.test argument of the specification, by method:
# x13 has an enum {ADD, REMOVE, NONE} for the residual trading day test, to
# which the joint F test can be added; tramoseats has {SEPARATE_T, JOINT_F,
# NONE} (given as the single test to use)
check_td_test <- function(test, method) {
  if (method == "x13") {
    base <- c("Add", "Remove", "None")
    allowed <- c(base, "Joint_F")
    ok <- all(test %in% allowed) &&
      length(test) <= 2 && length(unique(test)) == length(test) &&
      any(test %in% base)
    hint <- 'valid are "Add", "Remove" or "None", optionally combined with "Joint_F" (e.g. td.test = c("Add", "Joint_F"))'
  } else {
    allowed <- c("Separate_T", "Joint_F", "None")
    ok <- length(test) == 1 && test %in% allowed
    hint <- 'valid are "Separate_T", "Joint_F" or "None"'
  }
  if (!ok) {
    stop(
      "invalid td.test for method \"", method, "\": ",
      paste(test, collapse = ", "), "; ", hint,
      call. = FALSE
    )
  }
  invisible(test)
}

# resolve the `td` argument of perX13()/perTramo() into the list returned by
# td_context(). Accepts a JD3 calendar, precomputed regressors (ts/mts), the
# output of genTd() or an already built td_context() list.
resolve_td_input <- function(ts, td, method) {
  if (is.null(td)) {
    return(NULL)
  }
  if (inherits(td, "JD3_CALENDARDEFINITION")) {
    return(td_context(ts = ts, calendar = td, method = method))
  }
  if (stats::is.ts(td)) {
    return(td_context(ts = ts, td = td, method = method))
  }
  if (is.list(td)) {
    # td_context() list, or the output of genTd() (handled by td_context())
    if (all(c("context", "speclist") %in% names(td))) {
      return(td)
    }
    return(td_context(ts = ts, td = td, method = method))
  }
  stop(
    "'td' must be a JD3 calendar (rjd3toolkit::national_calendar()), a ts/mts ",
    "object with trading day regressors, the output of genTd(), or a list as ",
    "returned by td_context().",
    call. = FALSE
  )
}

# warn when the trading day regressors requested through `td` did not
# survive the estimation - rjd3 drops user-defined trading day regressors
# that are not significant without a word
check_td_output <- function(output, requested = TRUE) {
  if (isTRUE(requested)) {
    v <- output$preprocessing$description$variables
    kept <- length(v) > 0 && any(vapply(v, function(x) {
      identical(as.character(x$type), "TD") &&
        !is.null(x$coef$type) && any(as.character(x$coef$type) == "ESTIMATED")
    }, TRUE))
    if (!kept) {
      warning(
        "the trading day regressors of 'td' are not part of the estimated ",
        "model - rjd3 removes user-defined trading day regressors that are ",
        "not significant; the adjustment is free of trading day effects",
        call. = FALSE
      )
    }
  }
  output
}

#' Seasonal adjustment with user-defined trading day regressors
#'
#' @description
#' Wrapper around [perX13()] and [perTramo()] that performs a seasonal
#' adjustment with user-defined trading day regressors. The regressors are
#' either derived from a national calendar using
#' [rjd3toolkit::calendar_td()] or provided directly as a `mts` object, for
#' example from [genTd()]. Internally, the regressors are registered in a
#' [rjd3toolkit::modelling_context()] and the trading days spec block is set
#' to `option = "UserDefined"`, equivalent to using [rjd3toolkit::set_tradingdays()]
#' and passing `context` to [rjd3x13::x13_fast()] or
#' [rjd3tramoseats::tramoseats_fast()].
#'
#' @param ts a time series.
#' @param calendar a national calendar of class `JD3_CALENDAR`, created with
#'   [rjd3toolkit::national_calendar()]. The trading day regressors are then
#'   computed from this calendar on `ts`. Exactly one of `calendar` and `td`
#'   must be provided.
#' @param td precomputed trading day regressors as a `mts` object, for example
#'   `genTd()[[3]][, 1:6]` (the centered day regressors) or the output of
#'   [rjd3toolkit::calendar_td()], or the full output of [genTd()] (its
#'   centered day regressors `[[3]][, 1:6]` are then used). It must have the
#'   same frequency as `ts` and cover its full span; a single regressor is
#'   supported, see [td_context()].
#' @param method the seasonal adjustment method, i.e. "x13" or
#'   "tramoseats".
#' @param template the name of the predefined specification template, passed
#'   as the `name` argument to [rjd3x13::x13_spec()] or
#'   [rjd3tramoseats::tramoseats_spec()]. Defaults to `"rsa3"`.
#' @param groups integer vector passed to [rjd3toolkit::calendar_td()], the
#'   group of each day of the week (Monday to Saturday, then the holiday).
#'   Only used if `calendar` is given.
#' @param holiday integer passed to [rjd3toolkit::calendar_td()], the day
#'   considered as holiday. Only used if `calendar` is given.
#' @param contrasts logical passed to [rjd3toolkit::calendar_td()]. If `TRUE`
#'   (default), one regressor less than the number of groups is used and the
#'   regressors are centered, as in [genTd()]. Only used if `calendar` is
#'   given. See the remark on the log transformation under [td_context()].
#' @param name name under which the regressors are registered in the
#'   modelling context; the individual regressors are addressed as
#'   `name.column`.
#' @param td_labels optional character vector with one name per trading day
#'   regressor (e.g. `c("Monday", "Tuesday", "Wednesday", "Thursday",
#'   "Friday", "Saturday")`) naming the regressors; see [td_context()]. The
#'   labels are shown in [summary][summary.persephone()] instead of the
#'   default `group_*` names of [rjd3toolkit::calendar_td()].
#' @param test significance tests for the trading day regression coefficients
#'   (`td.test`). For x13, one of `"Add"`, `"Remove"` or `"None"`, optionally
#'   combined with `"Joint_F"`; the default is `c("Add", "Joint_F")`. For
#'   tramoseats, one of `"Separate_T"`, `"Joint_F"` or `"None"`, the default
#'   being `"Joint_F"`.
#' @param context an optional existing modelling context (e.g. containing
#'   further variables or calendars), to which the trading day regressors are
#'   added.
#' @param userdefined passed as the `userdefined` argument of
#'   [rjd3x13::x13_fast()] or [rjd3tramoseats::tramoseats_fast()].
#' @param speclist an optional list of additional specification arguments as
#'   in [perX13()] or [perTramo()]. Arguments for the trading day spec block
#'   (`td.*`) given here override the automatically generated ones.
#' @param ... additional arguments for customizing the model specifications,
#'   passed to [perX13()] or [perTramo()].
#'
#' @details
#' The returned object is an ordinary [`x13Single`] or [`tramoseatsSingle`]
#' object; call `$run()` to perform the seasonal adjustment and access the
#' rjd3 output via `$output`. The estimated coefficients of the trading day
#' groups are part of `$output$preprocessing$description$variables`, the
#' residual trading day F-tests are in
#' `$output$diagnostics$td.ftest.sa` and `$output$diagnostics$td.ftest.i`.
#'
#' @return An object of class [`x13Single`] or [`tramoseatsSingle`].
#'
#' @examples
#' library(rjd3toolkit)
#' data(AirPassengers, package = "datasets")
#'
#' # trading day regressors from an Austrian calendar
#' AT <- national_calendar(list(
#'   special_day("NEWYEAR"),
#'   fixed_day(1, 6),
#'   special_day("EASTERMONDAY"),
#'   special_day("ASCENSION"),
#'   special_day("WHITMONDAY"),
#'   special_day("CORPUSCHRISTI"),
#'   special_day("MAYDAY"),
#'   special_day("ASSUMPTION"),
#'   fixed_day(10, 26),
#'   special_day("ALLSAINTSDAY"),
#'   fixed_day(12, 8),
#'   fixed_day(12, 24, weight = 0.5),
#'   special_day("CHRISTMAS"),
#'   fixed_day(12, 26),
#'   fixed_day(12, 31, weight = 0.5)
#' ))
#' obj <- perTd(AirPassengers, calendar = AT, method = "x13")
#' obj$run()
#' obj$output$diagnostics$td.ftest
#'
#' # trading day regressors from genTd()
#' tdAT <- genTd(
#'   freq = 12, firstYear = 1949, lastYear = 1961,
#'   hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39",
#'             "easter+50", "easter+60", "08-15", "10-26", "11-01",
#'             "12-08", "12-24", "12-25", "12-26", "12-31"),
#'   weight = c(rep(1, 11), 0.5, rep(1, 2), 0.5)
#' )[[3]][, 1:6]
#' obj2 <- perTd(AirPassengers, td = tdAT, method = "tramoseats")
#' obj2$run()
#'
#' @seealso [perX13()], [perTramo()], [genTd()],
#'   [rjd3toolkit::calendar_td()], [rjd3toolkit::set_tradingdays()]
#' @export
perTd <- function(ts,
                  calendar = NULL,
                  td = NULL,
                  method = c("x13", "tramoseats"),
                  template = "rsa3",
                  groups = c(1, 2, 3, 4, 5, 6, 0),
                  holiday = 7,
                  contrasts = TRUE,
                  name = "REGS_TD",
                  td_labels = NULL,
                  test = NULL,
                  context = NULL,
                  userdefined = NULL,
                  speclist = NULL,
                  ...) {
  method <- match.arg(method)
  td_obj <- td_context(
    ts = ts,
    calendar = calendar,
    td = td,
    groups = groups,
    holiday = holiday,
    contrasts = contrasts,
    name = name,
    td_labels = td_labels,
    method = method,
    test = test
  )

  # the actual work (registering the context and setting the trading days
  # spec block) is done by the td argument of perX13()/perTramo()
  if (method == "x13") {
    perX13(
      ts,
      template = template,
      context = context,
      userdefined = userdefined,
      speclist = speclist,
      td = td_obj,
      ...
    )
  } else {
    perTramo(
      ts,
      template = template,
      context = context,
      userdefined = userdefined,
      speclist = speclist,
      td = td_obj,
      ...
    )
  }
}
