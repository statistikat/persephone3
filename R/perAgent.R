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
                     llm_decider = NULL,
                     verbose = FALSE,
                     ...) {
  if (!stats::is.ts(ts)) {
    stop("`ts` must be a time series object.", call. = FALSE)
  }
  if (NCOL(ts) != 1) {
    stop("`perAgent()` currently supports only univariate time series.", call. = FALSE)
  }

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

  result <- list(
    call = match.call(),
    ts = ts,
    requested_method = requested_method,
    selected_method = current$method,
    template = template,
    base_speclist = base_speclist,
    pretests = pretests,
    history = per_agent_bind_history(history_rows),
    final_model = current$model,
    final_summary = current$summary,
    final_score = current$score,
    should_adjust = isTRUE(pretests$has_identifiable_seasonality) || !allow_no_adjustment,
    stopped_reason = stopped_reason,
    llm_used = llm_used
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
      obj <- builder(
        ts = ts,
        template = template,
        context = context,
        userdefined = userdefined,
        speclist = speclist
      )
      obj$run(verbose = verbose)
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
        td.option = "WorkingDays",
        td.leapyear = "LeapYear",
        td.test = "None"
      ),
      rationale = "Residual trading-day effects remain in the adjusted series."
    )))
  }

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
