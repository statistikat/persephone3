#' ESS-inspired agentic seasonal adjustment
#'
#' `perAgent()` orchestrates repeated runs of [`perX13()`] and/or
#' [`perTramo()`], starting from the default seasonal-adjustment template and
#' iteratively updating the specification when the diagnostics suggest that an
#' improvement is possible. The implementation is intentionally conservative
#' and mirrors a subset of the ESS guidance:
#'
#' - test for seasonality before adjustment;
#' - compare X-13ARIMA-SEATS and TRAMO-SEATS when the method is set to `"auto"`;
#' - validate residual seasonality and trading-day effects after each run;
#' - prefer small, explainable specification changes over large jumps.
#'
#' The returned object keeps the full evaluation history together with the best
#' final model.
#'
#' @param ts A univariate time series.
#' @param method One of `"auto"`, `"x13"`, `"tramo"` or `"tramoseats"`.
#'   In `"auto"` mode both X-13ARIMA-SEATS and TRAMO-SEATS are run with the
#'   same starting specification and the better-scoring candidate is used as the
#'   starting point for the iterative search.
#' @param template The initial specification template. Defaults to `"rsa3"`.
#'   When `method = "auto"`, templates that are unsupported by one of the two
#'   methods are simply skipped for that method.
#' @param context Passed to [`perX13()`] or [`perTramo()`].
#' @param userdefined Passed to [`perX13()`] or [`perTramo()`].
#' @param speclist Optional named list of initial specification updates.
#' @param max_iter Maximum number of accepted improving iterations after the
#'   initial run.
#' @param alpha Significance level used in the ESS-inspired decision rules.
#' @param improvement_threshold Minimum score improvement that is required to
#'   accept a new candidate.
#' @param allow_no_adjustment If `TRUE`, the result records when the pre-tests do
#'   not identify seasonality clearly enough for publishing adjusted data.
#' @param fix_outliers If `TRUE`, automatically detected outliers from the final
#'   selected model are added back as fixed outliers, and automatic outlier
#'   detection is restricted to the trailing `fix_outliers_timespan`
#'   observations.
#' @param fix_outliers_timespan Number of observations at the end of the series
#'   that remain available for future automatic outlier detection when
#'   `fix_outliers = TRUE`.
#' @param fix_model If `TRUE`, the final selected ARIMA orders are fixed by
#'   disabling automatic model selection and storing the chosen orders
#'   explicitly in the specification.
#' @param td_holidays Holiday definitions passed to [`genTd()`] when the agent
#'   evaluates holiday-adjusted user-defined trading-day variants. By default,
#'   a Statistics Austria holiday calendar is used.
#' @param td_holiday_weights Base holiday weights used together with
#'   `td_holiday_scales` when constructing user-defined trading-day variants.
#' @param td_holiday_scales Multipliers applied to `td_holiday_weights` when the
#'   agent tries alternative holiday-adjusted trading-day variants. Setting a
#'   scale to `0` keeps the same calendar structure but effectively skips the
#'   holiday adjustment.
#' @param llm_decider Optional function that can prioritise the next candidate
#'   action. The function receives a compact state list and should return either
#'   a candidate action name or a list with at least an `action` element and,
#'   optionally, `updates`, `method`, and `rationale`.
#' @param verbose Passed to the underlying `$run()` method.
#' @param ... Additional specification updates passed to the underlying
#'   specification builder.
#'
#' @return An object of class `persephoneAgent`.
#'
#' @examples
#' res <- perAgent(AirPassengers)
#' res
#'
#' # Fix the method instead of selecting it automatically
#' res_x13 <- perAgent(AirPassengers, method = "x13")
#' res_x13$selected_method
#' @export
perAgent <- function(ts,
                     method = c("auto", "x13", "tramo", "tramoseats"),
                     template = NULL,
                     context = NULL,
                     userdefined = NULL,
                     speclist = NULL,
                     max_iter = 5,
                     alpha = 0.05,
                     improvement_threshold = 0.5,
                     allow_no_adjustment = TRUE,
                     fix_outliers = FALSE,
                     fix_outliers_timespan = 12,
                     fix_model = FALSE,
                     td_holidays = per_agent_default_td_holidays(),
                     td_holiday_weights = per_agent_default_td_holiday_weights(),
                     td_holiday_scales = c(1, 0.5, 0),
                     llm_decider = NULL,
                     verbose = FALSE,
                     ...) {
  if (!stats::is.ts(ts)) {
    stop("`ts` must be a time series object.", call. = FALSE)
  }
  if (NCOL(ts) != 1) {
    stop("`perAgent()` currently supports only univariate time series.", call. = FALSE)
  }
  if (!is.numeric(fix_outliers_timespan) ||
      length(fix_outliers_timespan) != 1L ||
      is.na(fix_outliers_timespan) ||
      fix_outliers_timespan < 1) {
    stop("`fix_outliers_timespan` must be a positive integer.", call. = FALSE)
  }
  fix_outliers_timespan <- as.integer(fix_outliers_timespan)
  td_variant_settings <- per_agent_validate_td_variant_settings(
    td_holidays = td_holidays,
    td_holiday_weights = td_holiday_weights,
    td_holiday_scales = td_holiday_scales
  )

  requested_method <- per_agent_normalize_method(match.arg(method))
  template <- template %||% "rsa3"

  base_speclist <- per_agent_merge_speclists(
    speclist %||% list(),
    list(...)
  )

  pretests <- per_agent_pretests(ts, alpha = alpha)
  methods <- if (identical(requested_method, "auto")) {
    c("x13", "tramoseats")
  } else {
    requested_method
  }

  initial_candidates <- lapply(
    methods,
    function(current_method) {
      per_agent_run_candidate(
        ts = ts,
        method = current_method,
        template = template,
        speclist = base_speclist,
        context = context,
        userdefined = userdefined,
        td_variant_settings = td_variant_settings,
        pretests = pretests,
        alpha = alpha,
        action = "initial",
        rationale = "Default template run",
        verbose = verbose
      )
    }
  )

  ok_candidates <- Filter(function(x) is.null(x$error), initial_candidates)
  if (length(ok_candidates) == 0) {
    msgs <- vapply(initial_candidates, `[[`, "", "error")
    stop(
      paste(
        "All initial agent runs failed:",
        paste(unique(msgs[nzchar(msgs)]), collapse = " | ")
      ),
      call. = FALSE
    )
  }

  current <- ok_candidates[[which.max(vapply(ok_candidates, `[[`, numeric(1), "score"))]]
  history_rows <- lapply(
    initial_candidates,
    function(candidate) {
      per_agent_history_row(
        candidate = candidate,
        iteration = 0L,
        accepted = is.null(candidate$error) &&
          identical(candidate$method, current$method) &&
          identical(per_agent_signature(candidate$method, candidate$speclist),
                    per_agent_signature(current$method, current$speclist))
      )
    }
  )

  seen_signatures <- unique(vapply(
    initial_candidates,
    function(candidate) per_agent_signature(candidate$method, candidate$speclist),
    character(1)
  ))

  llm_used <- FALSE
  stopped_reason <- NULL

  if (!pretests$has_identifiable_seasonality && isTRUE(allow_no_adjustment)) {
    stopped_reason <- "No identifiable seasonality detected in the pre-tests."
  }

  accepted_iter <- 0L
  while (is.null(stopped_reason) && accepted_iter < max_iter) {
    actions <- per_agent_candidate_actions(
      current = current,
      requested_method = requested_method,
      ts = ts,
      td_variant_settings = td_variant_settings,
      alpha = alpha
    )

    actions <- Filter(
      function(action) {
        sig <- per_agent_signature(
          action$method %||% current$method,
          per_agent_merge_speclists(current$speclist, action$updates)
        )
        !sig %in% seen_signatures
      },
      actions
    )

    if (length(actions) == 0) {
      stopped_reason <- "No further rule-based candidate was available."
      break
    }

        if (is.function(llm_decider)) {
      llm_state <- list(
        iteration = accepted_iter + 1L,
        current_method = current$method,
        current_score = current$score,
        current_summary = current$summary,
        pretests = pretests,
        history = per_agent_bind_history(history_rows),
        candidate_actions = lapply(actions, function(action) {
          list(
            name = action$name,
            method = action$method %||% current$method,
            updates = action$updates,
            rationale = action$rationale
          )
        })
      )
      actions_prioritised <- per_agent_prioritise_actions(actions, llm_decider, llm_state)
            if (!identical(actions_prioritised, actions)) {
                llm_used <- TRUE
                actions <- actions_prioritised
            }
        }

        actions <- Filter(function(action) {
            sig <- per_agent_signature(action$method %||% current$method,
                per_agent_merge_speclists(current$speclist, action$updates))
            !sig %in% seen_signatures
        }, actions)
        if (length(actions) == 0) {
            stopped_reason <- "No further rule-based candidate was available."
            break
        }

    candidate_runs <- lapply(
      actions,
      function(action) {
        candidate_method <- action$method %||% current$method
        candidate_speclist <- per_agent_merge_speclists(current$speclist, action$updates)
        per_agent_run_candidate(
          ts = ts,
          method = candidate_method,
          template = template,
          speclist = candidate_speclist,
          context = context,
          userdefined = userdefined,
          td_variant_settings = td_variant_settings,
          pretests = pretests,
          alpha = alpha,
          action = action$name,
          rationale = action$rationale,
          verbose = verbose
        )
      }
    )

    history_rows <- c(
      history_rows,
      lapply(candidate_runs, function(candidate) {
        per_agent_history_row(candidate = candidate, iteration = accepted_iter + 1L)
      })
    )

    seen_signatures <- unique(c(
      seen_signatures,
      vapply(
        candidate_runs,
        function(candidate) per_agent_signature(candidate$method, candidate$speclist),
        character(1)
      )
    ))

    ok_runs <- Filter(function(x) is.null(x$error), candidate_runs)
    if (length(ok_runs) == 0) {
      stopped_reason <- "All candidate updates failed."
      break
    }

    best_candidate <- ok_runs[[which.max(vapply(ok_runs, `[[`, numeric(1), "score"))]]
    if ((best_candidate$score - current$score) <= improvement_threshold) {
      stopped_reason <- "No candidate improved the diagnostics enough."
      break
    }

    accepted_iter <- accepted_iter + 1L
    current <- best_candidate

    current_signature <- per_agent_signature(current$method, current$speclist)
    history_rows <- lapply(
      history_rows,
      function(row) {
        if (identical(row$iteration, accepted_iter) &&
            identical(row$method, current$method) &&
            identical(row$signature, current_signature)) {
          row$accepted <- TRUE
        }
        row
      }
    )
  }

  if (is.null(stopped_reason)) {
    stopped_reason <- "Maximum number of improving iterations reached."
  }

  postprocess <- per_agent_apply_requested_fixes(
    current = current,
    start_iteration = accepted_iter + 1L,
    ts = ts,
    template = template,
    context = context,
    userdefined = userdefined,
    td_variant_settings = td_variant_settings,
    pretests = pretests,
    alpha = alpha,
    fix_outliers = fix_outliers,
    fix_outliers_timespan = fix_outliers_timespan,
    fix_model = fix_model,
    verbose = verbose
  )
  current <- postprocess$current
  history_rows <- c(history_rows, postprocess$history_rows)

  result <- list(
    call = match.call(),
    ts = ts,
    requested_method = requested_method,
    selected_method = current$method,
    template = template,
    context = context,
    userdefined = userdefined,
    base_speclist = base_speclist,
    td_variant_settings = td_variant_settings,
    pretests = pretests,
    history = per_agent_bind_history(history_rows),
    final_model = current$model,
    final_summary = current$summary,
    final_score = current$score,
    should_adjust = isTRUE(pretests$has_identifiable_seasonality) || !allow_no_adjustment,
    stopped_reason = stopped_reason,
    llm_used = llm_used,
    fixed_outliers = postprocess$fixed_outliers,
    fixed_model = postprocess$fixed_model
  )
  class(result) <- "persephoneAgent"
  result
}

#' Create an OpenAI-backed decider for [`perAgent()`]
#'
#' This helper creates a small adapter around `ellmer::chat_openai()` that can
#' be supplied to `perAgent(llm_decider = ...)`. The language model does not
#' create new candidate updates by itself; it only prioritises one of the
#' already generated candidate actions, or proposes a small custom update.
#'
#' By default, authentication is delegated to `ellmer`, which means that
#' setting `OPENAI_API_KEY` is the recommended approach.
#'
#' @param model OpenAI model name passed to `ellmer::chat_openai()`.
#' @param credentials Optional credential provider passed to
#'   `ellmer::chat_openai()`. It should be a zero-argument function that returns
#'   either an API key string or a named list of headers.
#' @param api_key Optional API key kept for backwards compatibility. When
#'   supplied, it is converted to an `ellmer` `credentials` function internally.
#' @param system_prompt Optional extra instructions appended to the built-in
#'   system prompt.
#' @param base_url Base URL passed to `ellmer::chat_openai()`.
#'
#' @return A function suitable for the `llm_decider` argument of [`perAgent()`].
#' @examples
#' decider <- perAgentOpenAI()
#' is.function(decider)
#' @export
perAgentOpenAI <- function(model = "gpt-4.1-mini",
                           credentials = NULL,
                           api_key = NULL,
                           system_prompt = NULL,
                           base_url = "https://api.openai.com/v1") {
  force(model)
  force(credentials)
  force(api_key)
  force(system_prompt)
  force(base_url)

  function(state) {
    if (!requireNamespace("ellmer", quietly = TRUE)) {
      stop("Package `ellmer` is required for `perAgentOpenAI()`.", call. = FALSE)
    }
    if (length(state$candidate_actions) == 0) {
      return(NULL)
    }

    built_in_prompt <- paste(
      "You are assisting an ESS-style seasonal-adjustment agent.",
      "Choose exactly one candidate action from the provided list.",
      "Prefer conservative changes that improve residual seasonality, residual",
      "trading-day diagnostics, and overall stability.",
      "Do not invent new parameter names unless you return a short custom update.",
      "Reply with two lines only:",
      "Action: <candidate-name>",
      "Reason: <short rationale>",
      sep = "\n"
    )
    ellmer_credentials <- credentials
    if (is.null(ellmer_credentials) && !is.null(api_key)) {
      ellmer_credentials <- function() api_key
    }

    chat <- ellmer::chat_openai(
      system_prompt = paste(c(built_in_prompt, system_prompt), collapse = "\n\n"),
      base_url = base_url,
      credentials = ellmer_credentials,
      model = model,
      echo = "none"
    )
    response <- chat$chat(per_agent_format_llm_state(state), echo = "none")
    per_agent_parse_llm_response(
      response,
      valid_actions = vapply(state$candidate_actions, `[[`, "", "name")
    )
  }
}

#' @export
print.persephoneAgent <- function(x, ...) {
  cat("A persephone agent result\n")
  cat("Requested method:", x$requested_method, "\n")
  cat("Selected method:", x$selected_method, "\n")
  cat("Template:", x$template, "\n")
  cat("Final score:", format(round(x$final_score, 2), nsmall = 2), "\n")
  cat("Should publish adjusted data:", if (isTRUE(x$should_adjust)) "yes" else "no", "\n")
  cat("Stopped because:", x$stopped_reason, "\n")
  if (isTRUE(x$llm_used)) {
    cat("LLM prioritisation: used\n")
  }
  if (isTRUE(x$fixed_outliers)) {
    cat("Automatic outliers fixed: yes\n")
  }
  if (isTRUE(x$fixed_model)) {
    cat("ARIMA model fixed: yes\n")
  }

  final <- x$final_summary
  cat(
    "Residual seasonality (F / QS p-values):",
    per_agent_format_number(final$residual_seasonal_f_pvalue), "/",
    per_agent_format_number(final$residual_seasonal_qs_pvalue), "\n"
  )
  cat(
    "Residual trading-day p-value:",
    per_agent_format_number(final$residual_td_pvalue), "\n"
  )
  if (!is.na(final$q_stat)) {
    cat("Q statistic:", per_agent_format_number(final$q_stat), "\n")
  }
  invisible(x)
}

per_agent_normalize_method <- function(method) {
  if (identical(method, "tramo")) {
    return("tramoseats")
  }
  method
}

per_agent_merge_speclists <- function(x, y) {
  x <- x %||% list()
  y <- y %||% list()
  if (length(y) == 0) {
    return(x)
  }
  x[names(y)] <- y
  x
}

per_agent_pretests <- function(ts, alpha = 0.05) {
  positive <- all(ts > 0, na.rm = TRUE)
  combined <- tryCatch(
    rjd3toolkit::seasonality_combined(ts, mul = positive),
    error = function(e) NULL
  )
  f_test <- tryCatch(rjd3toolkit::seasonality_f(ts), error = function(e) NULL)
  qs_test <- tryCatch(rjd3toolkit::seasonality_qs(ts), error = function(e) NULL)

  combined_label <- combined$seasonality %||% NA_character_
  f_p <- per_agent_pvalue(f_test)
  qs_p <- per_agent_pvalue(qs_test)
  has_seasonality <- identical(tolower(combined_label), "present") ||
    (!is.na(f_p) && f_p < alpha) ||
    (!is.na(qs_p) && qs_p < alpha)

  list(
    combined_seasonality = combined_label,
    f_pvalue = f_p,
    qs_pvalue = qs_p,
    has_identifiable_seasonality = has_seasonality
  )
}

per_agent_run_candidate <- function(ts,
                                    method,
                                    template,
                                    speclist,
                                    context,
                                    userdefined,
                                    td_variant_settings,
                                    pretests,
                                    alpha,
                                    action,
                                    rationale,
                                    verbose = FALSE) {
  builder <- switch(
    method,
    x13 = perX13,
    tramoseats = perTramo,
    stop("Unknown method `", method, "`.", call. = FALSE)
  )

  tryCatch(
    {
      inputs <- per_agent_prepare_candidate_inputs(
        ts = ts,
        speclist = speclist,
        context = context,
        userdefined = userdefined,
        td_variant_settings = td_variant_settings
      )
      obj <- builder(
        ts = ts,
        template = template,
        context = inputs$context,
        userdefined = inputs$userdefined,
        speclist = inputs$speclist
      )
      obj$run(verbose = verbose)
      adjusted_series <- obj$output$user_defined$sa %||% obj$adjusted
      if (is.null(adjusted_series) || !stats::is.ts(adjusted_series)) {
        stop("Candidate run did not produce an adjusted series.", call. = FALSE)
      }
      summary <- per_agent_collect_summary(obj, ts, pretests)
      score <- per_agent_score(summary, alpha = alpha)
      list(
        method = method,
        template = template,
        speclist = speclist,
        model = obj,
        summary = summary,
        score = score,
        action = action,
        rationale = rationale,
        error = NULL
      )
    },
    error = function(e) {
      list(
        method = method,
        template = template,
        speclist = speclist,
        model = NULL,
        summary = per_agent_empty_summary(ts, pretests),
        score = -Inf,
        action = action,
        rationale = rationale,
        error = conditionMessage(e)
      )
    }
  )
}

per_agent_collect_summary <- function(obj, ts, pretests) {
  output <- obj$output
  userdef <- output$user_defined %||% list()
  diagnostics <- output$diagnostics %||% list()
  sa <- userdef$sa %||% obj$adjusted
  original_positive <- all(ts > 0, na.rm = TRUE)

  list(
    n_obs = length(ts),
    original_positive = original_positive,
    original_has_identifiable_seasonality = isTRUE(pretests$has_identifiable_seasonality),
    original_combined_seasonality = pretests$combined_seasonality %||% NA_character_,
    original_f_pvalue = pretests$f_pvalue,
    original_qs_pvalue = pretests$qs_pvalue,
    combined_seasonality = userdef[["diagnostics.seas-si-combined"]] %||% NA_character_,
    combined_seasonality3 = userdef[["diagnostics.seas-si-combined3"]] %||% NA_character_,
    residual_seasonal_f_pvalue = per_agent_pvalue(diagnostics$seas.ftest.sa),
    residual_seasonal_qs_pvalue = per_agent_pvalue(diagnostics$seas.qstest.sa),
    residual_td_pvalue = per_agent_pvalue(diagnostics$td.ftest.sa),
    q_stat = output$mstats$q %||% NA_real_,
    n_outliers = suppressWarnings(as.numeric(userdef[["regression.nout"]] %||% NA_real_)),
    log_transform = isTRUE(output$preprocessing$description$log),
    negative_sa = isTRUE(original_positive) && any(sa < 0, na.rm = TRUE),
    arima.p = suppressWarnings(as.integer(userdef$arima.p %||% NA_integer_)),
    arima.d = suppressWarnings(as.integer(userdef$arima.d %||% NA_integer_)),
    arima.q = suppressWarnings(as.integer(userdef$arima.q %||% NA_integer_)),
    arima.bp = suppressWarnings(as.integer(userdef$arima.bp %||% NA_integer_)),
    arima.bd = suppressWarnings(as.integer(userdef$arima.bd %||% NA_integer_)),
    arima.bq = suppressWarnings(as.integer(userdef$arima.bq %||% NA_integer_))
  )
}

per_agent_empty_summary <- function(ts, pretests) {
  list(
    n_obs = length(ts),
    original_positive = all(ts > 0, na.rm = TRUE),
    original_has_identifiable_seasonality = isTRUE(pretests$has_identifiable_seasonality),
    original_combined_seasonality = pretests$combined_seasonality %||% NA_character_,
    original_f_pvalue = pretests$f_pvalue,
    original_qs_pvalue = pretests$qs_pvalue,
    combined_seasonality = NA_character_,
    combined_seasonality3 = NA_character_,
    residual_seasonal_f_pvalue = NA_real_,
    residual_seasonal_qs_pvalue = NA_real_,
    residual_td_pvalue = NA_real_,
    q_stat = NA_real_,
    n_outliers = NA_real_,
    log_transform = NA,
    negative_sa = NA,
    arima.p = NA_integer_,
    arima.d = NA_integer_,
    arima.q = NA_integer_,
    arima.bp = NA_integer_,
    arima.bd = NA_integer_,
    arima.bq = NA_integer_
  )
}

per_agent_score <- function(summary, alpha = 0.05) {
  score <- 100

  if (!isTRUE(summary$original_has_identifiable_seasonality)) {
    score <- score - 35
  }
  if (!identical(tolower(summary$combined_seasonality %||% ""), "present")) {
    score <- score - 10
  }
  if (!is.na(summary$residual_seasonal_f_pvalue) && summary$residual_seasonal_f_pvalue < alpha) {
    score <- score - 25
  }
  if (!is.na(summary$residual_seasonal_qs_pvalue) && summary$residual_seasonal_qs_pvalue < alpha) {
    score <- score - 25
  }
  if (!is.na(summary$residual_td_pvalue) && summary$residual_td_pvalue < alpha) {
    score <- score - 15
  }
  if (!is.na(summary$q_stat)) {
    if (summary$q_stat <= 1) {
      score <- score + (1 - summary$q_stat) * 5
    } else {
      score <- score - min(25, 10 + (summary$q_stat - 1) * 15)
    }
  }
  if (!is.na(summary$n_outliers) &&
      summary$n_outliers > max(3, ceiling(summary$n_obs / 20))) {
    score <- score - 10
  }
  if (isTRUE(summary$negative_sa)) {
    score <- score - 15
  }
  if (isTRUE(summary$log_transform) && isTRUE(summary$original_positive)) {
    score <- score + 5
  }

  max(score, 0)
}

per_agent_candidate_actions <- function(current,
                                        requested_method,
                                        ts,
                                        td_variant_settings,
                                        alpha = 0.05) {
  summary <- current$summary
  speclist <- current$speclist %||% list()
  freq <- stats::frequency(ts)

  needs_td <- !is.na(summary$residual_td_pvalue) && summary$residual_td_pvalue < alpha
  needs_seasonality <- (!is.na(summary$residual_seasonal_f_pvalue) &&
    summary$residual_seasonal_f_pvalue < alpha) ||
    (!is.na(summary$residual_seasonal_qs_pvalue) &&
      summary$residual_seasonal_qs_pvalue < alpha)

  actions <- list()

  if (isTRUE(summary$original_positive) && !isTRUE(summary$log_transform)) {
    actions <- c(actions, list(list(
      name = "enable_log",
      updates = list(transform.fun = "Log"),
      rationale = "ESS-inspired decomposition rule for positive series."
    )))
  }

  if (isTRUE(needs_td) &&
      freq %in% c(4, 12) &&
      !identical(speclist$td.option %||% NA_character_, "WorkingDays")) {
    actions <- c(actions, list(list(
      name = "working_days",
      updates = list(
        agenttd.scale = NA_real_,
        td.option = "WorkingDays",
        td.uservariable = NA_character_,
        td.leapyear = "LeapYear",
        td.test = "None"
      ),
      rationale = "Residual trading-day effects remain in the adjusted series."
    )))
  }

  actions <- c(
    actions,
    per_agent_td_variant_actions(
      needs_td = needs_td,
      freq = freq,
      td_variant_settings = td_variant_settings
    )
  )

  if (freq == 12 &&
      (isTRUE(needs_seasonality) || isTRUE(needs_td)) &&
      !isTRUE(speclist$easter.enabled)) {
    actions <- c(actions, list(list(
      name = "enable_easter",
      updates = list(
        easter.enabled = TRUE,
        easter.test = "Add"
      ),
      rationale = "Monthly series may benefit from an explicit Easter regressor."
    )))
  }

  if ((isTRUE(needs_seasonality) || isTRUE(needs_td)) &&
      isTRUE((speclist$setout.critical.value %||% Inf) > 3.5)) {
    actions <- c(actions, list(list(
      name = "tighter_outlier_detection",
      updates = list(
        setout.critical.value = 3.5
      ),
      rationale = "Conservative tightening of automatic outlier detection."
    )))
  }

  if (!identical(speclist$automdl.enabled %||% TRUE, FALSE) &&
      all(!is.na(c(
        summary$arima.p, summary$arima.d, summary$arima.q,
        summary$arima.bp, summary$arima.bd, summary$arima.bq
      )))) {
    actions <- c(actions, list(list(
      name = "fix_arima",
      updates = list(
        automdl.enabled = FALSE,
        arima.p = summary$arima.p,
        arima.d = summary$arima.d,
        arima.q = summary$arima.q,
        arima.bp = summary$arima.bp,
        arima.bd = summary$arima.bd,
        arima.bq = summary$arima.bq
      ),
      rationale = "Freeze the currently selected ARIMA model for stability."
    )))
  }

  if (identical(requested_method, "auto")) {
    other_method <- if (identical(current$method, "x13")) "tramoseats" else "x13"
    actions <- c(actions, list(list(
      name = paste0("switch_to_", other_method),
      method = other_method,
      updates = list(),
      rationale = "Re-evaluate the alternative seasonal-adjustment family."
    )))
  }

  actions
}

per_agent_default_td_holidays <- function() {
  list(
    "01-01", "01-06", "05-01", "easter+1", "easter+39",
    "easter+50", "easter+60", "08-15", "10-26", "11-01",
    "12-08", "12-24", "12-25", "12-26", "12-31"
  )
}

per_agent_default_td_holiday_weights <- function() {
  c(rep(1, 11), 0.5, rep(1, 2), 0.5)
}

per_agent_validate_td_variant_settings <- function(td_holidays,
                                                   td_holiday_weights,
                                                   td_holiday_scales) {
  if (is.null(td_holidays) || length(td_holidays) == 0 ||
      is.null(td_holiday_scales) || length(td_holiday_scales) == 0) {
    return(list(
      enabled = FALSE,
      holidays = list(),
      weights = numeric(0),
      scales = numeric(0)
    ))
  }

  holidays <- as.list(td_holidays)
  weights <- as.numeric(td_holiday_weights)
  scales <- unique(as.numeric(td_holiday_scales))

  if (length(weights) != length(holidays)) {
    stop(
      "`td_holiday_weights` must have the same length as `td_holidays`.",
      call. = FALSE
    )
  }
  if (any(is.na(weights)) || any(weights < 0)) {
    stop("`td_holiday_weights` must contain non-negative numbers.", call. = FALSE)
  }
  if (any(is.na(scales)) || any(scales < 0)) {
    stop("`td_holiday_scales` must contain non-negative numbers.", call. = FALSE)
  }

  list(
    enabled = TRUE,
    holidays = holidays,
    weights = weights,
    scales = scales
  )
}

per_agent_td_variant_actions <- function(needs_td,
                                         freq,
                                         td_variant_settings) {
  if (!isTRUE(needs_td) ||
      !identical(freq %in% c(4, 12), TRUE) ||
      !isTRUE(td_variant_settings$enabled)) {
    return(list())
  }

  lapply(td_variant_settings$scales, function(scale) {
    label <- per_agent_td_scale_label(scale)
    rationale <- if (isTRUE(all.equal(scale, 0))) {
      "Try the Austrian trading-day calendar without applying any holiday weights."
    } else {
      paste0(
        "Try a holiday-adjusted Austrian trading-day regressor with holiday-weight scale ",
        format(scale, trim = TRUE, scientific = FALSE),
        "."
      )
    }
    list(
      name = paste0("holiday_td_", label),
      updates = list(
        agenttd.scale = scale,
        td.option = "UserDefined",
        td.uservariable = paste0("peragent_td_", label),
        td.leapyear = "None",
        td.test = "None"
      ),
      rationale = rationale
    )
  })
}

per_agent_prepare_candidate_inputs <- function(ts,
                                               speclist,
                                               context,
                                               userdefined,
                                               td_variant_settings) {
  speclist_out <- speclist %||% list()
  context_out <- context %||% list()
  userdefined_out <- userdefined

  td_scale <- suppressWarnings(as.numeric(speclist_out$agenttd.scale %||% NA_real_))
  speclist_out <- per_agent_remove_speclist_keys(speclist_out, "agenttd.scale")

  if (!is.na(td_scale) && isTRUE(td_variant_settings$enabled)) {
    td_variant <- per_agent_build_td_variant(
      ts = ts,
      scale = td_scale,
      td_variant_settings = td_variant_settings
    )
    uservar_name <- speclist_out$td.uservariable %||% td_variant$name
    context_out[[uservar_name]] <- td_variant$series
    speclist_out$td.option <- "UserDefined"
    speclist_out$td.uservariable <- uservar_name
    speclist_out$td.test <- "None"
    speclist_out$td.leapyear <- "None"
  }

  list(
    speclist = speclist_out,
    context = context_out,
    userdefined = userdefined_out
  )
}

per_agent_build_td_variant <- function(ts, scale, td_variant_settings) {
  years <- per_agent_td_year_range(ts)
  label <- per_agent_td_scale_label(scale)
  td <- genTd(
    freq = stats::frequency(ts),
    firstYear = years$first,
    lastYear = years$last,
    hd = td_variant_settings$holidays,
    weight = td_variant_settings$weights * scale
  )
  list(
    name = paste0("peragent_td_", label),
    series = td[[3]][, 7]
  )
}

per_agent_td_year_range <- function(ts) {
  start_year <- as.integer(stats::start(ts)[1])
  end_year <- as.integer(stats::end(ts)[1])
  list(
    first = min(2000L, start_year),
    last = max(2026L, end_year + 2L)
  )
}

per_agent_td_scale_label <- function(scale) {
  if (isTRUE(all.equal(scale, 0))) {
    return("no_holidays")
  }
  raw <- format(scale, trim = TRUE, scientific = FALSE)
  raw <- gsub("\\.", "_", raw)
  raw <- gsub("[^A-Za-z0-9_]+", "_", raw)
  paste0("scale_", raw)
}

per_agent_apply_requested_fixes <- function(current,
                                            start_iteration,
                                            ts,
                                            template,
                                            context,
                                            userdefined,
                                            td_variant_settings,
                                            pretests,
                                            alpha,
                                            fix_outliers = FALSE,
                                            fix_outliers_timespan = 12L,
                                            fix_model = FALSE,
                                            verbose = FALSE) {
  history_rows <- list()
  next_iteration <- as.integer(start_iteration)
  current_candidate <- current
  fixed_outliers <- FALSE
  fixed_model <- FALSE

  if (isTRUE(fix_outliers)) {
    candidate_speclist <- per_agent_fixed_outlier_speclist(
      current_candidate = current_candidate,
      ts = ts,
      timespan = fix_outliers_timespan
    )
    candidate_signature <- per_agent_signature(current_candidate$method, candidate_speclist)
    current_signature <- per_agent_signature(current_candidate$method, current_candidate$speclist)
    if (!identical(candidate_signature, current_signature)) {
      candidate <- per_agent_run_candidate(
        ts = ts,
        method = current_candidate$method,
        template = template,
        speclist = candidate_speclist,
        context = context,
        userdefined = userdefined,
        td_variant_settings = td_variant_settings,
        pretests = pretests,
        alpha = alpha,
        action = "fix_outliers",
        rationale = "Persist the detected outliers as fixed regressors.",
        verbose = verbose
      )
      if (!is.null(candidate$error)) {
        stop(
          "Post-processing action `fix_outliers` failed: ",
          candidate$error,
          call. = FALSE
        )
      }
      current_candidate <- candidate
      history_rows[[length(history_rows) + 1L]] <- per_agent_history_row(
        candidate = candidate,
        iteration = next_iteration,
        accepted = TRUE
      )
      next_iteration <- next_iteration + 1L
      fixed_outliers <- TRUE
    }
  }

  if (isTRUE(fix_model)) {
    candidate_speclist <- per_agent_merge_speclists(
      current_candidate$speclist,
      per_agent_fixed_model_updates(current_candidate)
    )
    candidate_signature <- per_agent_signature(current_candidate$method, candidate_speclist)
    current_signature <- per_agent_signature(current_candidate$method, current_candidate$speclist)
    if (!identical(candidate_signature, current_signature)) {
      candidate <- per_agent_run_candidate(
        ts = ts,
        method = current_candidate$method,
        template = template,
        speclist = candidate_speclist,
        context = context,
        userdefined = userdefined,
        td_variant_settings = td_variant_settings,
        pretests = pretests,
        alpha = alpha,
        action = "fix_model",
        rationale = "Freeze the selected ARIMA orders for the final model.",
        verbose = verbose
      )
      if (!is.null(candidate$error)) {
        stop(
          "Post-processing action `fix_model` failed: ",
          candidate$error,
          call. = FALSE
        )
      }
      current_candidate <- candidate
      history_rows[[length(history_rows) + 1L]] <- per_agent_history_row(
        candidate = candidate,
        iteration = next_iteration,
        accepted = TRUE
      )
      fixed_model <- TRUE
    }
  }

  list(
    current = current_candidate,
    history_rows = history_rows,
    fixed_outliers = fixed_outliers,
    fixed_model = fixed_model
  )
}

per_agent_fixed_outlier_speclist <- function(current_candidate, ts, timespan = 12L) {
  fixed_outliers <- per_agent_existing_fixed_outlier_names(current_candidate$model)
  detected_outliers <- per_agent_detected_outlier_names(current_candidate$model)
  all_outliers <- unique(c(fixed_outliers, detected_outliers))

  speclist <- current_candidate$speclist %||% list()
  speclist <- per_agent_remove_speclist_keys(
    speclist,
    c(
      "outliers",
      "addout.type",
      "addout.date",
      "setout.span.type",
      "setout.d0",
      "setout.d1",
      "setout.n0",
      "setout.n1"
    )
  )
  if (length(all_outliers) > 0) {
    speclist$outliers <- all_outliers
  }
  speclist[["setout.span.type"]] <- "From"
  speclist[["setout.d0"]] <- per_agent_outlier_detection_from_date(ts, timespan)
  speclist
}

per_agent_fixed_model_updates <- function(current_candidate) {
  summary <- current_candidate$summary
  arima_values <- c(
    summary$arima.p,
    summary$arima.d,
    summary$arima.q,
    summary$arima.bp,
    summary$arima.bd,
    summary$arima.bq
  )
  if (any(is.na(arima_values))) {
    stop(
      "Unable to extract ARIMA orders from the final model for `fix_model = TRUE`.",
      call. = FALSE
    )
  }

  list(
    automdl.enabled = FALSE,
    arima.p = summary$arima.p,
    arima.d = summary$arima.d,
    arima.q = summary$arima.q,
    arima.bp = summary$arima.bp,
    arima.bd = summary$arima.bd,
    arima.bq = summary$arima.bq
  )
}

per_agent_existing_fixed_outlier_names <- function(model) {
  outliers <- model$params$regarima$regression$outliers %||% list()
  if (length(outliers) == 0) {
    return(character(0))
  }

  freq <- stats::frequency(model$ts)
  unique(vapply(
    outliers,
    function(outlier) {
      per_agent_outlier_name(
        type = outlier$code %||% outlier$type,
        date = outlier$pos %||% outlier$date,
        freq = freq
      )
    },
    character(1)
  ))
}

per_agent_detected_outlier_names <- function(model) {
  outliers <- tryCatch(getOutliers(model), error = function(e) NA)
  if (!is.data.frame(outliers) || nrow(outliers) == 0) {
    return(character(0))
  }
  unique(as.character(outliers$name))
}

per_agent_outlier_name <- function(type, date, freq) {
  date <- as.Date(date)
  if (is.na(date)) {
    return(NA_character_)
  }

  year <- format(date, "%Y")
  if (identical(freq, 12)) {
    period <- as.integer(format(date, "%m"))
  } else if (identical(freq, 4)) {
    period <- quarter_from_month(format(date, "%m"))
  } else {
    stop("Outlier naming is implemented only for monthly and quarterly series.", call. = FALSE)
  }
  paste0(type, year, ".", period)
}

per_agent_outlier_detection_from_date <- function(ts, timespan = 12L) {
  start_index <- max(1L, length(ts) - as.integer(timespan) + 1L)
  per_agent_time_to_date(stats::time(ts)[start_index], stats::frequency(ts))
}

per_agent_time_to_date <- function(time_point, freq) {
  year <- floor(time_point + 1e-08)
  period <- as.integer(round((time_point - year) * freq + 1))
  period <- max(1L, min(as.integer(freq), period))
  month <- if (identical(freq, 12)) {
    period
  } else if (identical(freq, 4)) {
    c(1L, 4L, 7L, 10L)[period]
  } else {
    stop("Date conversion is implemented only for monthly and quarterly series.", call. = FALSE)
  }
  as.Date(sprintf("%04d-%02d-01", year, month))
}

per_agent_remove_speclist_keys <- function(speclist, keys) {
  speclist <- speclist %||% list()
  keep <- setdiff(names(speclist), keys)
  speclist[keep]
}

per_agent_prioritise_actions <- function(actions, llm_decider, state) {
  decision <- tryCatch(llm_decider(state), error = function(e) NULL)
  if (is.null(decision)) {
    return(actions)
  }

  action_names <- vapply(actions, `[[`, "", "name")
  if (is.character(decision) && length(decision) == 1) {
    idx <- match(tolower(decision), tolower(action_names))
    if (!is.na(idx)) {
      return(c(actions[idx], actions[-idx]))
    }
    return(actions)
  }

  if (is.list(decision) && !is.null(decision$action)) {
    idx <- match(tolower(decision$action), tolower(action_names))
    if (!is.na(idx)) {
      if (!is.null(decision$rationale)) {
        actions[[idx]]$rationale <- decision$rationale
      }
      return(c(actions[idx], actions[-idx]))
    }

    custom_action <- list(
      name = as.character(decision$action),
      method = per_agent_normalize_method(decision$method %||% state$current_method),
      updates = decision$updates %||% list(),
      rationale = decision$rationale %||% "Prioritised by llm_decider."
    )
    return(c(list(custom_action), actions))
  }

  actions
}

per_agent_history_row <- function(candidate, iteration, accepted = FALSE) {
  summary <- candidate$summary
  list(
    iteration = as.integer(iteration),
    method = candidate$method,
    action = candidate$action,
    rationale = candidate$rationale %||% "",
    score = candidate$score,
    accepted = accepted,
    error = candidate$error %||% "",
    original_combined_seasonality = summary$original_combined_seasonality,
    original_f_pvalue = summary$original_f_pvalue,
    original_qs_pvalue = summary$original_qs_pvalue,
    combined_seasonality = summary$combined_seasonality,
    residual_seasonal_f_pvalue = summary$residual_seasonal_f_pvalue,
    residual_seasonal_qs_pvalue = summary$residual_seasonal_qs_pvalue,
    residual_td_pvalue = summary$residual_td_pvalue,
    q_stat = summary$q_stat,
    n_outliers = summary$n_outliers,
    log_transform = summary$log_transform,
    speclist = candidate$speclist,
    signature = per_agent_signature(candidate$method, candidate$speclist)
  )
}

per_agent_bind_history <- function(rows) {
  out <- data.frame(
    iteration = vapply(rows, `[[`, integer(1), "iteration"),
    method = vapply(rows, `[[`, character(1), "method"),
    action = vapply(rows, `[[`, character(1), "action"),
    rationale = vapply(rows, `[[`, character(1), "rationale"),
    score = vapply(rows, `[[`, numeric(1), "score"),
    accepted = vapply(rows, `[[`, logical(1), "accepted"),
    error = vapply(rows, `[[`, character(1), "error"),
    original_combined_seasonality = vapply(rows, `[[`, character(1), "original_combined_seasonality"),
    original_f_pvalue = vapply(rows, `[[`, numeric(1), "original_f_pvalue"),
    original_qs_pvalue = vapply(rows, `[[`, numeric(1), "original_qs_pvalue"),
    combined_seasonality = vapply(rows, `[[`, character(1), "combined_seasonality"),
    residual_seasonal_f_pvalue = vapply(rows, `[[`, numeric(1), "residual_seasonal_f_pvalue"),
    residual_seasonal_qs_pvalue = vapply(rows, `[[`, numeric(1), "residual_seasonal_qs_pvalue"),
    residual_td_pvalue = vapply(rows, `[[`, numeric(1), "residual_td_pvalue"),
    q_stat = vapply(rows, `[[`, numeric(1), "q_stat"),
    n_outliers = vapply(rows, `[[`, numeric(1), "n_outliers"),
    log_transform = vapply(rows, `[[`, logical(1), "log_transform"),
    signature = vapply(rows, `[[`, character(1), "signature"),
    stringsAsFactors = FALSE
  )
  out$speclist <- I(lapply(rows, `[[`, "speclist"))
  row.names(out) <- NULL
  out
}

per_agent_signature <- function(method, speclist) {
  speclist <- speclist %||% list()
  if (length(speclist) == 0) {
    return(paste(method, "default", sep = "::"))
  }
  parts <- vapply(
    sort(names(speclist)),
    function(name) paste0(name, "=", per_agent_encode_value(speclist[[name]])),
    character(1)
  )
  paste(method, paste(parts, collapse = "|"), sep = "::")
}

per_agent_encode_value <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (inherits(x, "Date")) {
    return(paste(format(x), collapse = ","))
  }
  if (length(x) == 0) {
    return("EMPTY")
  }
  paste(as.character(x), collapse = ",")
}

per_agent_pvalue <- function(test) {
  if (is.null(test) || is.null(test$pvalue)) {
    return(NA_real_)
  }
  as.numeric(test$pvalue)
}

per_agent_format_number <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return("NA")
  }
  format(round(x, 4), nsmall = 4)
}

per_agent_format_llm_state <- function(state) {
  summary <- state$current_summary
  lines <- c(
    "Current seasonal-adjustment state",
    sprintf("- iteration: %s", state$iteration),
    sprintf("- method: %s", state$current_method),
    sprintf("- score: %.2f", state$current_score),
    sprintf(
      "- original pre-tests: combined=%s; F p=%s; QS p=%s",
      summary$original_combined_seasonality %||% "NA",
      per_agent_format_number(summary$original_f_pvalue),
      per_agent_format_number(summary$original_qs_pvalue)
    ),
    sprintf(
      "- current diagnostics: combined=%s; residual F p=%s; residual QS p=%s; residual TD p=%s; q=%s; outliers=%s; log=%s",
      summary$combined_seasonality %||% "NA",
      per_agent_format_number(summary$residual_seasonal_f_pvalue),
      per_agent_format_number(summary$residual_seasonal_qs_pvalue),
      per_agent_format_number(summary$residual_td_pvalue),
      per_agent_format_number(summary$q_stat),
      ifelse(is.na(summary$n_outliers), "NA", as.character(summary$n_outliers)),
      ifelse(is.na(summary$log_transform), "NA", as.character(summary$log_transform))
    ),
    "Candidate actions:"
  )

  candidate_lines <- vapply(
    state$candidate_actions,
    function(action) {
      update_string <- if (length(action$updates) == 0) {
        "none"
      } else {
        paste(
          vapply(
            sort(names(action$updates)),
            function(name) paste0(name, "=", per_agent_encode_value(action$updates[[name]])),
            character(1)
          ),
          collapse = "; "
        )
      }
      sprintf(
        "- %s | method=%s | updates=%s | rationale=%s",
        action$name,
        action$method,
        update_string,
        action$rationale %||% ""
      )
    },
    character(1)
  )

  paste(c(lines, candidate_lines), collapse = "\n")
}

per_agent_parse_llm_response <- function(text, valid_actions) {
  if (!is.character(text) || length(text) != 1) {
    return(NULL)
  }
  lines <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
  action_line <- lines[grepl("^Action\\s*:", lines, ignore.case = TRUE)][1]
  if (is.na(action_line) || !nzchar(action_line)) {
    return(NULL)
  }
  action_name <- trimws(sub("^Action\\s*:\\s*", "", action_line, ignore.case = TRUE))
  idx <- match(tolower(action_name), tolower(valid_actions))
  if (is.na(idx)) {
    return(NULL)
  }
  reason_line <- lines[grepl("^Reason\\s*:", lines, ignore.case = TRUE)][1]
  reason <- if (is.na(reason_line)) {
    NULL
  } else {
    trimws(sub("^Reason\\s*:\\s*", "", reason_line, ignore.case = TRUE))
  }
  list(action = valid_actions[[idx]], rationale = reason)
}
