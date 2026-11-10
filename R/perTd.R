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
#'   6) or the output of [rjd3toolkit::calendar_td()]. Exactly one of
#'   `calendar` and `td` must be provided.
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
#' @param method the seasonal adjustment method the specification is built
#'   for, i.e. "x13" or "tramoseats". Only relevant for the default of `test`.
#' @param test significance tests for the trading day regression coefficients,
#'   passed as `td.test` to the specification. The default depends on the
#'   method: `c("Add", "Joint_F")` for x13 and `"Joint_F"` for tramoseats,
#'   since tramoseats does not support `"Add"`.
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
  if (!is.null(td) && !stats::is.ts(td)) {
    stop("'td' must be a ts/mts object of trading day regressors", call. = FALSE)
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
  } else if (frequency(td) != frequency(ts) ||
             start(td)[1] > start(ts)[1] ||
             end(td)[1] < end(ts)[1]) {
    stop(
      "'td' must have the same frequency as 'ts' and cover its full span",
      call. = FALSE
    )
  }

  if (is.null(test)) {
    test <- if (method == "x13") c("Add", "Joint_F") else "Joint_F"
  }

  context <- rjd3toolkit::modelling_context(
    variables = stats::setNames(list(td), name)
  )
  speclist <- list(
    td.option = "UserDefined",
    td.uservariable = paste0(name, ".", colnames(td)),
    td.test = test
  )
  list(td = td, context = context, speclist = speclist)
}

# resolve the `td` argument of perX13()/perTramo() into the list returned by
# td_context(). Accepts a JD3 calendar, precomputed regressors (ts/mts) or an
# already built td_context() list.
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
  if (is.list(td) && all(c("context", "speclist") %in% names(td))) {
    return(td)
  }
  stop(
    "'td' must be a JD3 calendar (rjd3toolkit::national_calendar()), a ts/mts ",
    "object with trading day regressors, or a list as returned by td_context().",
    call. = FALSE
  )
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
#'   [rjd3toolkit::calendar_td()]. It must have the same frequency as `ts`
#'   and cover its full span.
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
#' @param test significance tests for the trading day regression coefficients
#'   (`td.test`). The default depends on the method: `c("Add", "Joint_F")`
#'   for x13 and `"Joint_F"` for tramoseats, since tramoseats does not
#'   support `"Add"`.
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
                  contrasts = FALSE,
                  name = "REGS_TD",
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
