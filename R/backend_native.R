# Restricted native X-13 fit (rkalos). This file must remain safe to source
# without Java or rjd3: the only engine access is rkalos, whose restricted
# fixed-coefficient X-13 fit is gated by capabilities()$native_x13_fit_restricted
# and NEVER by capabilities()$fit (fit keeps its schema-1 meaning: unrestricted
# fitting stays unimplemented). Scope fields come from the pinned
# capabilities()$restricted_fit block; refusals name the violated field and
# echo the rkalos/core status verbatim. No Java fallback ever happens.
#
# Sigma policy (maintainer decision 2026-11-17): the X-11 sigma-rule product
# default is "legacy" (bit-exact against the pinned DefaultExtremeValuesCorrector
# / JDemetra behavior). Per the layering split the spec keeps sigma_policy =
# NULL meaning NOT STATED; the qualified default (restricted_fit$sigma_policy_default,
# verified as identical(.., "legacy") by the gate) is applied here when the
# request is built, mirroring rkalos applying it at its interface while the
# core keeps its explicitness contract. An explicitly stated "legacy"/"accurate"
# always wins; any other stated value is refused.

native_backend_entry <- "native_x13_fit(), native_x13_preadjust(), native_x13_restore()"

# Refusal carrying the violated scope field(s); wording keeps the established
# "does not yet implement"/"No Java fallback" contract of backend_unimplemented().
backend_native_refusal <- function(context, method, violated, detail) {
  stop(sprintf(paste0("The native backend does not yet implement %s for '%s': %s ",
                      "Violated scope field(s): %s. No Java fallback was used. ",
                      "Select backend = \"rjd3\" to use the reference implementation."),
               context, method, detail, paste(violated, collapse = ", ")),
       call. = FALSE)
}

backend_native_gate <- function(context = "the restricted fit", method = "x13") {
  cap <- rkalos::capabilities()
  rf <- cap$restricted_fit
  if (!identical(cap$schema_version, 2L) ||
      !identical(cap$native_x13_fit_restricted, TRUE) ||
      !is.list(rf) || !identical(rf$native_x13_fit_restricted, TRUE)) {
    backend_native_refusal(context, method, "native_x13_fit_restricted",
      paste0("this rkalos build does not qualify the restricted X-13 fit ",
             "(capabilities()$native_x13_fit_restricted is not TRUE)."))
  }
  # The Persephone native spec relies on the maintainer's legacy sigma default
  # (restricted_fit schema v2: sigma_policy_required was replaced by
  # sigma_policy_default on 2026-11-17); a build stating a different default
  # is outside the qualified contract this layer maps onto.
  if (!identical(rf$sigma_policy_default, "legacy")) {
    backend_native_refusal(context, method, "sigma_policy_default",
      paste0("this rkalos build does not state the maintainer's sigma default ",
             "(restricted_fit$sigma_policy_default is not \"legacy\"); the ",
             "Persephone native spec does not guess a sigma default. "))
  }
  rf
}

# The one template the native backend can construct: the restricted fixed
# specification. Auto templates (rsa*) estimate coefficients; the qualified
# fit is x13-only.
backend_native_check_template <- function(template, method = "x13") {
  if (length(template) == 1L && identical(template, "restricted")) {
    if (method != "x13") {
      backend_native_refusal("specification construction", method,
                             "native_x13_fit_restricted",
                             sprintf("the qualified native surface provides only the restricted X-13 fit (entry points: %s); ",
                                     native_backend_entry))
    }
    return(invisible(TRUE))
  }
  backend_native_refusal("specification construction", method,
                         "fixed_coefficients_only",
                         sprintf(paste0("template '%s' is an automatic template that estimates ",
                                        "coefficients; only template 'restricted' is qualified ",
                                        "natively.%s "),
                                 paste(template, collapse = ", "),
                                 if (method != "x13")
                                   paste0("The native entry points are ", native_backend_entry, ". ")
                                 else ""))
}

backend_native_default_spec <- function(method, template, freq) {
  backend_native_check_template(template, method)
  rf <- backend_native_gate("specification construction", method)
  if (as.numeric(freq) != rf$frequency) {
    backend_native_refusal("specification construction", method, "monthly_only",
      sprintf("frequency %d is outside the qualified native domain; ", freq))
  }
  structure(list(method = "x13", template = "restricted", frequency = rf$frequency,
                 log = FALSE, mode = "additive", variables = list(),
                 sigma_policy = NULL,   # not stated -> rf$sigma_policy_default at request time
                 seasonal = NULL, trend_length = NULL,
                 lower_sigma = 1.5, upper_sigma = 2.5),
            class = "persephone_native_spec")
}

# Native specification updates accept only the restricted fields; anything
# else keeps the established refusal.
backend_native_update_spec <- function(spec, spec_list, method) {
  allowed <- c("sigma_policy", "seasonal", "trend_length", "mode", "log",
               "variables", "lower_sigma", "upper_sigma")
  bad <- setdiff(names(spec_list), allowed)
  if (length(bad)) {
    backend_native_refusal("specification updates", method, allowed,
      sprintf("field(s) '%s' are not part of the restricted native specification; ",
              paste(bad, collapse = "', '")))
  }
  for (name in names(spec_list)) spec[[name]] <- spec_list[[name]]
  class(spec) <- "persephone_native_spec"
  spec
}

# Cheap deterministic fingerprint (two rolling prime-modulus hashes of a
# canonical request text; non-cryptographic, session- and platform-stable).
native_request_fingerprint <- function(request) {
  text <- paste(c(request$method, request$frequency, request$start_period,
                  request$log, request$mode, request$sigma,
                  request$trend_length, request$lower_sigma, request$upper_sigma,
                  request$seasonal,
                  unlist(lapply(request$variables, function(v) c(
                    v$class, v$effect, format(v$coefficient, digits = 17),
                    lengths(v$columns), sum(vapply(v$columns, sum, numeric(1))))))),
             collapse = "\r\n")
  bytes <- as.integer(charToRaw(text))
  hash <- function(mod) {
    state <- 0
    for (b in bytes) state <- (state * 131L + b) %% mod
    sprintf("%04x%04x", state %/% 65536, state %% 65536)
  }
  paste0(hash(2147483647L), "-", hash(4294967291))
}

# Full scope validation: returns the rkalos native_x13_fit argument list or
# refuses naming every violated scope field.
backend_native_request <- function(spec, ts) {
  rf <- backend_native_gate("seasonal-adjustment fitting", "x13")
  freq <- stats::frequency(ts)
  violated <- character(0)
  detail <- character(0)
  refuse <- function() {
    backend_native_refusal("seasonal-adjustment fitting", "x13", violated,
                           paste(detail, collapse = " "))
  }
  if (as.numeric(freq) != rf$frequency) {
    violated <- c(violated, "monthly_only")
    detail <- c(detail, sprintf("frequency %d is not the qualified frequency %d;",
                                freq, rf$frequency))
  }
  if (!is.list(spec$variables)) {
    violated <- c(violated, "fixed_coefficients_only")
    detail <- c(detail, "variables must be a list of fixed-coefficient specifications;")
  }
  n <- length(ts)
  var_classes <- c("outlier", "days_related", "moving_holiday", "user")
  var_effects <- c("undefined", "series", "trend", "seasonal",
                   "seasonally_adjusted", "irregular", "calendar_effect")
  for (index in seq_along(spec$variables)) {
    v <- spec$variables[[index]]
    where <- sprintf("variable %d", index)
    if (!is.list(v) || !all(c("class", "effect", "coefficient", "columns") %in% names(v)) ||
        length(v$class) != 1L || !(v$class %in% var_classes) ||
        length(v$effect) != 1L || !(v$effect %in% var_effects) ||
        !is.list(v$columns) || !length(v$columns)) {
      violated <- c(violated, "fixed_coefficients_only")
      detail <- c(detail, sprintf("%s must provide class, effect, coefficient and column vectors from the pinned vocabulary;", where))
      next
    }
    if (!is.numeric(v$coefficient) || length(v$coefficient) != length(v$columns) ||
        anyNA(v$coefficient) || any(!is.finite(v$coefficient))) {
      violated <- c(violated, "fixed_coefficients_only")
      detail <- c(detail, sprintf("%s must carry one finite coefficient per regression column (nothing is estimated);", where))
    }
    bad_columns <- !all(vapply(v$columns, function(col) is.numeric(col) && length(col) == n &&
                                 !anyNA(col), logical(1)))
    if (bad_columns) {
      violated <- c(violated, "fixed_coefficients_only")
      detail <- c(detail, sprintf("%s must provide numeric columns of series length %d;", where, n))
    }
  }
  fixed_filters <- c("stable", "s3x1", "s3x3", "s3x5", "s3x9", "s3x15")
  seasonal <- spec$seasonal
  if (is.null(seasonal) || length(seasonal) != freq || anyNA(seasonal)) {
    violated <- c(violated, "fixed_seasonal_requests_only")
    detail <- c(detail, "the seasonal requests must state one pinned filter per calendar period (no default is applied);")
  } else if (any(!seasonal %in% fixed_filters)) {
    violated <- c(violated, "fixed_seasonal_requests_only")
    detail <- c(detail, sprintf("filters '%s' are not fixed requests (qualified: %s);",
                                paste(unique(setdiff(seasonal, fixed_filters)), collapse = ", "),
                                paste(fixed_filters, collapse = ", ")))
  }
  trend <- spec$trend_length
  if (is.null(trend) || length(trend) != 1L || is.na(trend) || !is.finite(trend) ||
      trend != floor(trend) || trend < 5 || trend %% 2 == 0) {
    violated <- c(violated, "fixed_trend_length_only")
    detail <- c(detail, "the Henderson trend filter length must be a stated odd integer >= 5 (the automatic 0 is refused);")
  }
  # NULL/omitted sigma_policy receives the qualified default ("legacy",
  # verified by the gate); an explicitly stated policy must be one of the
  # two pinned values -- the core validates it verbatim downstream.
  sigma <- spec$sigma_policy
  sigma_stated <- !is.null(sigma)
  if (sigma_stated && (length(sigma) != 1L || is.na(sigma) ||
      !sigma %in% c("legacy", "accurate"))) {
    violated <- c(violated, "sigma_policy_default")
    detail <- c(detail, sprintf("a stated sigma policy must be \"legacy\" or \"accurate\" (the qualified default is \"%s\");",
                                rf$sigma_policy_default))
  }
  if (!sigma_stated) sigma <- rf$sigma_policy_default
  if (is.null(spec$mode) || length(spec$mode) != 1L || !spec$mode %in% rf$modes) {
    violated <- c(violated, "modes")
    detail <- c(detail, sprintf("the decomposition mode must be one of %s;",
                                paste(rf$modes, collapse = ", ")))
  }
  if (!is.logical(spec$log) || length(spec$log) != 1L || is.na(spec$log)) {
    violated <- c(violated, "modes")
    detail <- c(detail, "the log flag must be TRUE or FALSE;")
  }
  for (name in c("lower_sigma", "upper_sigma")) {
    value <- spec[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0.5) {
      violated <- c(violated, name)
      detail <- c(detail, sprintf("%s must be a finite number greater than 0.5;", name))
    }
  }
  if (length(violated)) refuse()
  span <- stats::tsp(ts)
  start_period <- if (is.null(span)) 1L else
    as.integer(round((span[1L] - floor(span[1L])) * freq)) + 1L
  list(variables = spec$variables, log = spec$log,
       seasonal = as.character(unname(seasonal)), trend_length = as.integer(trend),
       sigma = sigma, sigma_stated = sigma_stated, mode = spec$mode,
       lower_sigma = as.numeric(spec$lower_sigma),
       upper_sigma = as.numeric(spec$upper_sigma),
       method = "x13", frequency = as.integer(freq), start_period = start_period)
}

backend_native_fit <- function(ts, spec, context, userdefined) {
  if (!is.null(context)) {
    backend_native_refusal("seasonal-adjustment fitting", "x13", "context",
      "external-regressor contexts are an rjd3 estimation feature and are outside the fixed-coefficient domain; ")
  }
  if (!all(userdefined %in% userdefined_default)) {
    backend_native_refusal("seasonal-adjustment fitting", "x13", "userdefined",
      "user-defined output variables are an rjd3 descriptor-pool feature; ")
  }
  if (!inherits(spec, "persephone_native_spec")) {
    backend_native_refusal("seasonal-adjustment fitting", "x13", "specification",
      "the specification was not built by the native restricted constructor; ")
  }
  request <- backend_native_request(spec, ts)
  # The fit entry points exist in the rkalos namespace but are not exported
  # yet; access them through the namespace (documented seam, no ::: in the
  # installed code path is impossible for unexported entries, get0 is explicit).
  fit_fun <- get0("native_x13_fit", envir = asNamespace("rkalos"), mode = "function")
  if (is.null(fit_fun)) {
    backend_native_refusal("seasonal-adjustment fitting", "x13", "native_x13_fit_restricted",
      "the rkalos namespace does not provide native_x13_fit(); ")
  }
  fingerprint <- native_request_fingerprint(request)
  fit <- do.call(fit_fun, c(list(series = ts), request[c("variables", "log",
    "seasonal", "trend_length", "sigma", "mode", "lower_sigma", "upper_sigma",
    "frequency", "start_period")]))
  if (!identical(fit$status, "available")) {
    stop(sprintf(paste0("The native restricted fit refused the request with the verbatim core status: %s ",
                        "(request fingerprint %s). No Java fallback was used. ",
                        "Select backend = \"rjd3\" to use the reference implementation."),
                 fit$status, fingerprint), call. = FALSE)
  }
  attr(fit, "persephone_request_fingerprint") <- fingerprint
  fit
}

# Versioned envelope for the native fit. Deliberately NOT an rjd3-shaped
# result and not classed "persephone_result": it is a distinct honest type.
normalize_native_result <- function(fit, spec, ts) {
  # Called with either the versioned specification envelope or the raw native
  # spec; unwrap so scope-aware provenance sees the real spec fields.
  if (inherits(spec, "persephone_spec") &&
      inherits(spec$parameters, "persephone_native_spec")) {
    spec <- spec$parameters
  }
  if (inherits(spec, "persephone_native_spec") &&
      !identical(as.numeric(spec$frequency), as.numeric(stats::frequency(ts)))) {
    stop("Specification frequency does not match the result's observed series.", call. = FALSE)
  }
  components <- list(y = NULL, t = fit$finalTrend, sa = fit$finalSA,
                     s = NULL, i = fit$finalIrregular, cal = NULL)
  component_names <- c("y", "t", "sa", "s", "i", "cal")
  field_names <- c(component_names, paste0(component_names, "_f"), "y_ef")
  availability <- data.frame(
    field = field_names,
    state = vapply(field_names, function(name) {
      value <- components[[sub("_f$", "", name)]]
      if (grepl("_f$|y_ef", name)) {
        "not_provided"   # zero_horizon_only: the fixed path has no forecasts
      } else if (is.null(value)) {
        "not_provided"
      } else if (length(value) && all(is.na(value))) {
        "missing"
      } else "available"
    }, character(1)),
    stringsAsFactors = FALSE, row.names = NULL)
  code <- if (identical(fit$status, "available") && !is.null(fit$finalSA)) "success" else "incomplete"
  # Honest sigma provenance: the compound sigma_policy string records whether
  # the maintainer default ("legacy") was applied or the policy was stated by
  # the user; the core echoes (sigma/sigma_set) stay verbatim alongside. The
  # core always receives a policy (the interface applies the default), so for
  # a raw fit without a native spec the source falls back to the core echo.
  sigma_stated <- if (inherits(spec, "persephone_native_spec"))
    !is.null(spec$sigma_policy) else isTRUE(fit$sigma_set)
  provenance <- c(
    list(backend = "native", restricted = TRUE,
         request_fingerprint = attr(fit, "persephone_request_fingerprint", exact = TRUE),
         core_commit = tryCatch(rkalos::core_version()$commit, error = function(e) NA_character_),
         sigma_policy = paste0(fit$sigma, if (sigma_stated) " (explicit)" else " (default)")),
    fit[c("type", "schema_version", "frequency", "start_period", "log", "seasonal",
          "trend_length", "sigma_set", "sigma", "lower_sigma", "upper_sigma",
          "msr_policy", "mode_requested", "mode_effective", "scope")])
  structure(list(schema_version = 1L, method = "x13", backend = "native",
                 specification = spec, status = list(code = code, convergence = "not_reported"),
                 domain = backend_domain(ts), components = components,
                 forecasts = NULL, diagnostics = list(), availability = availability,
                 provenance = provenance, results = fit$results,
                 preadjustment = fit$preadjustment, selection = fit$selection,
                 raw = fit),
            class = "persephone_native_restricted_fit")
}

#' @export
print.persephone_native_restricted_fit <- function(x, ...) {
  cat("Restricted native X-13 fit (rkalos/Kalos core)\n")
  cat("  status:", x$status$code, "| scope: restricted fixed-coefficient X-13\n")
  cat("  mode requested:", x$provenance$mode_requested,
      "| effective:", x$provenance$mode_effective, "\n")
  cat("  sigma policy:", x$provenance$sigma_policy,
      "| Henderson:", x$provenance$trend_length,
      "| fingerprint:", x$provenance$request_fingerprint, "\n")
  if (!is.null(x$components$sa)) {
    cat("  final SA (last 3):", paste(round(utils::tail(as.numeric(x$components$sa), 3), 4),
                                      collapse = ", "), "\n")
  }
  invisible(x)
}
