# Native X-13 fits (rkalos). This file must remain safe to source without
# Java or rjd3: the only engine access is rkalos, whose restricted
# fixed-coefficient X-13 fit is gated by capabilities()$native_x13_fit_restricted
# and whose automatic-spec X-13 fit is gated by capabilities()$native_x13_fit_
# automatic -- NEVER by capabilities()$fit (fit keeps its schema-1 meaning:
# unrestricted fitting stays unimplemented). Scope fields come from the pinned
# capabilities()$restricted_fit and capabilities()$automatic_fit blocks;
# refusals name the violated field and echo the rkalos/core status verbatim.
# No Java fallback ever happens.
#
# Automatic-spec lane (template "rsa3", item 21-H/M4): the rkalos qualified
# automatic pipeline is driven from a PINNED decode table, never from user
# spec data -- the table below is the measured JDemetra+ rsa3 module state
# (pinned template-spec dump of the rjd3 3.9.0 jars, captured 2026-11-20),
# passed through rkalos' decoder conventions (ub1/ub2 as reciprocals of the
# JVM-space values; mode "additive" is the neutral request value matching the
# JVM x11.mode = UNKNOWN, whose effective mode X13Kernel.forceMode derives from
# the log decision). Of the six rsa templates only rsa3 (transform AUTO +
# automodel enabled + no calendar/Easter/user regressors) fits the qualified
# envelope; rsa4/rsa5c detect trading days and Easter -- refused by name with
# the measured template facts, mirroring the core's fail-closed matrix (21-I
# deferral status quoted verbatim). The default horizons are zero. The automatic route accepts
# explicit forecast and backcast counts and the sigma policy.
#
# The stock fixed-structure templates RSA0, RSA1 and RSA2c estimate the
# airline coefficients from the original series. Their native stock entry
# selects the X-11 filters and returns the one-year forecast.
#
# Sigma policy (maintainer decision 2026-11-17): the X-11 sigma-rule product
# default is "legacy" (bit-exact against the pinned DefaultExtremeValuesCorrector
# / JDemetra behavior). Per the layering split the spec keeps sigma_policy =
# NULL meaning NOT STATED; the qualified default (restricted_fit$sigma_policy_default,
# verified as identical(.., "legacy") by the gate) is applied here when the
# request is built, mirroring rkalos applying it at its interface while the
# core keeps its explicitness contract. An explicitly stated "legacy"/"accurate"
# always wins; any other stated value is refused.

native_backend_entry <- paste("native_x13_fit(), native_x13_preadjust(), native_x13_restore(),",
                              "native_x13_fit_automatic(), native_x13_estimated_fit()")

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

# Remaining automatic templates require selection of calendar regressors.
backend_native_auto_refusals <- list(
  rsa4 = list(violated = "automatic_fit.deferred",
              detail = paste0("the pinned JDemetra+ rsa4 specification carries the ",
                              "TD2/leap-year (test REMOVE) and Easter (test ADD) ",
                              "regressors; calendar detection is deferred (21-I): ",
                              "the core answers 'unsupported:x13 automatic fit ",
                              "defers calendar/easter/TD/user detection (21-I)'; ")),
  rsa5c = list(violated = "automatic_fit.deferred",
               detail = paste0("the pinned JDemetra+ rsa5c specification carries the ",
                               "TD7/leap-year (test REMOVE) and Easter (test ADD) ",
                               "regressors; calendar detection is deferred (21-I): ",
                               "the core answers 'unsupported:x13 automatic fit ",
                               "defers calendar/easter/TD/user detection (21-I)'; ")))

backend_native_check_template <- function(template, method = "x13") {
  if (identical(method, "tramoseats")) {
    scope <- rkalos::capabilities()$tramo_seats_fit
    if (length(template) != 1L || !template %in% c("rsa0", "rsa1", "rsa3", "rsafull") ||
        !isTRUE(scope$available) || !template %in% tolower(scope$templates))
      backend_native_refusal("specification construction", method, "tramo_seats_fit.templates",
                             "the qualified monthly TRAMO-SEATS templates are rsa0, rsa1, rsa3 and rsafull; ")
    return(invisible(TRUE))
  }
  if (length(template) == 1L && template %in% c("rsa0", "rsa1", "rsa2c")) {
    capability <- rkalos::capabilities()$estimated_x13_fit
    if (!isTRUE(capability$available) || !template %in% tolower(capability$templates))
      backend_native_refusal("specification construction", method, "estimated_x13_fit.templates",
                             "the installed rkalos package must support this stock X-13 template; ")
    return(invisible(TRUE))
  }
  if (identical(template, "restricted") || identical(template, "rsa3")) return(invisible(TRUE))
  name <- if (length(template) == 1L && is.character(template)) template else NULL
  refusal <- if (!is.null(name)) backend_native_auto_refusals[[name]] else NULL
  if (!is.null(refusal)) {
    backend_native_refusal("specification construction", method, refusal$violated,
                           sprintf("template '%s' is outside the qualified native envelopes (automatic and restricted): %s",
                                   name, refusal$detail))
  }
  backend_native_refusal("specification construction", method,
                         "fixed_coefficients_only",
                         sprintf(paste0("template '%s' is outside the qualified X-13 routes ",
                                        "'restricted', 'rsa0', 'rsa1', 'rsa2c' and 'rsa3'. %s"),
                                 paste(template, collapse = ", "),
                                 if (method != "x13")
                                   paste0("The native entry points are ", native_backend_entry, ". ")
                                 else ""))
}

# The automatic-fit gate: schema v2, the automatic entry qualified, the legacy
# sigma default and the monthly transform-Auto envelope the pinned decode
# table was measured against. A build that states anything else is outside the
# qualified contract this layer maps onto -- nothing is guessed.
backend_native_auto_gate <- function(context = "the automatic fit", method = "x13") {
  cap <- rkalos::capabilities()
  af <- cap$automatic_fit
  if (!identical(cap$schema_version, 2L) ||
      !identical(cap$native_x13_fit_automatic, TRUE) ||
      !is.list(af) || !identical(af$native_x13_fit_automatic, TRUE)) {
    backend_native_refusal(context, method, "native_x13_fit_automatic",
      paste0("this rkalos build does not qualify the automatic-spec X-13 fit ",
             "(capabilities()$native_x13_fit_automatic is not TRUE)."))
  }
  if (!identical(af$transform$auto_log_test, TRUE)) {
    backend_native_refusal(context, method, "transform.auto_log_test",
      paste0("this rkalos build does not qualify the automatic log test ",
             "(automatic_fit$transform$auto_log_test is not TRUE); the pinned ",
             "rsa3 decode table requests transform AUTO. "))
  }
  if (!identical(af$x11$sigma_policy_default, "legacy")) {
    backend_native_refusal(context, method, "sigma_policy_default",
      paste0("this rkalos build does not state the maintainer's sigma default ",
             "(automatic_fit$x11$sigma_policy_default is not \"legacy\"); the ",
             "Persephone native spec does not guess a sigma default. "))
  }
  if (!isTRUE(af$monthly_only) || !identical(af$frequency, 12L)) {
    backend_native_refusal(context, method, "monthly_only",
      "this rkalos build does not describe the automatic fit as monthly-only with frequency 12; ")
  }
  af
}

# The pinned rsa3 decode table (measured JDemetra+ module state of the pinned
# rjd3 3.9.0 template dump, expressed in the rkalos decoder conventions):
# transform AUTO with aicDiff -2; automodel grid max_d 2 / max_bd 1 / p<=2 /
# q<=2 / bp<=1 / bq<=1 / models_count 5, ub1/ub2 as RECIPROCALS of the JVM
# values 1.0309278350515465 / 1.1363636363636365 (rkalos decoder convention,
# pinned by the oracle corpus request echoes); outliers AO,LS,TC on the full
# span, maxiter 30, monthlytcrate 0.7; AMI precision 1e-7 / mean 1e-5, defva 0
# (va), predcv 0.14286 (reduce_va), ljungbox 0.95, tsig 1.0 (cascade ratio
# limit and mean-rule cutoff carried as ur_limit and mean_test_final). The
# X-11 block is the decoder automatic surface: msr in every period, automatic
# Henderson (trend_length 0 at the core), zero horizons, sigma handled by the
# caller, msr_policy "jdplus" and the neutral mode request (see header).
backend_native_auto_decode <- function(sigma, lower_sigma, upper_sigma) {
  list(
    transform = list(aicc = -2),
    automodel = list(max_d = 2L, max_bd = 1L,
                     ub1 = 1 / 1.0309278350515465, ub2 = 1 / 1.1363636363636365,
                     cancel = 0.1, precision = 1e-7, models_count = 5L,
                     max_p = 2L, max_q = 2L, max_bp = 1L, max_bq = 1L,
                     balanced = FALSE, mixed = TRUE,
                     accept_white_noise = FALSE, eps = 1e-7),
    outliers = list(used = TRUE, types = c("AO", "LS", "TC"), span = NULL,
                    max_round = 30L, max_outliers = 30L, tcrate = 0.7),
    ami = list(precision = 1e-7, mean_precision = 1e-5, va = 0,
               reduce_va = 0.14286, ljung_box_limit = 0.95, check_mu = TRUE,
               accept_airline = FALSE, ur_limit = 0.95),
    regression = list(mean_test_initial = 1.96, mean_test_final = 1, tsig = 1),
    sigma = sigma, lower_sigma = lower_sigma, upper_sigma = upper_sigma,
    msr_policy = "jdplus", mode = "additive", backcast = 0L, forecast = 0L)
}

backend_native_default_spec <- function(method, template, freq) {
  backend_native_check_template(template, method)
  if (method == "tramoseats") {
    if (freq != 12L) backend_native_refusal("specification construction", method, "monthly_only",
                                            paste0(toupper(template), " requires a monthly series; "))
    return(structure(list(method = method, template = template, frequency = 12L,
                          backcast = 0L, forecast = -2L),
                     class = c("persephone_native_tramo_spec", "persephone_native_spec")))
  }
  if (template %in% c("rsa0", "rsa1", "rsa2c")) {
    if (freq != 12L) backend_native_refusal("specification construction", method, "monthly_only",
                                            "stock estimated X-13 templates require monthly observations; ")
    return(structure(list(method = "x13", template = template, frequency = 12L, forecast = -1L, backcast = 0L),
                     class = c("persephone_native_estimated_spec", "persephone_native_spec")))
  }
  if (identical(template, "rsa3")) {
    af <- backend_native_auto_gate("specification construction", method)
    if (as.numeric(freq) != af$frequency) {
      backend_native_refusal("specification construction", method, "monthly_only",
        sprintf("frequency %d is outside the qualified native domain; ", freq))
    }
    return(structure(list(method = "x13", template = "rsa3",
                         frequency = af$frequency, sigma_policy = NULL,
                         backcast = 0L, forecast = 0L,
                         lower_sigma = 1.5, upper_sigma = 2.5),
                     class = c("persephone_native_auto_spec",
                               "persephone_native_spec")))
  }
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

# Native specification updates accept only the fields each lane qualifies:
# the restricted fields, or -- for the automatic template -- the horizon counts
# and sigma policy. The automatic module state is pinned decode data; anything
# else keeps the established refusal.
backend_native_update_spec <- function(spec, spec_list, method) {
  if (inherits(spec, "persephone_native_tramo_spec")) {
    if (length(setdiff(names(spec_list), c("forecast", "backcast"))))
      backend_native_refusal("specification updates", method, "tramo_seats_fit.templates",
                             paste0(toupper(spec$template), " accepts forecast and backcast counts; "))
    spec[names(spec_list)] <- spec_list
    return(spec)
  }
  if (inherits(spec, "persephone_native_estimated_spec")) {
    if (length(setdiff(names(spec_list), c("forecast", "backcast"))))
      backend_native_refusal("specification updates", method, "estimated_x13_fit.templates",
                             "the stock estimated template fixes its preprocessing and X-11 configuration; ")
    spec[names(spec_list)] <- spec_list
    return(spec)
  }
  if (inherits(spec, "persephone_native_auto_spec")) {
    bad <- setdiff(names(spec_list), c("sigma_policy", "backcast", "forecast"))
    if (length(bad)) {
      backend_native_refusal("specification updates", method,
                             c("automatic_spec_pinned", bad),
                             sprintf(paste0("field(s) '%s' are not part of the automatic native specification: ",
                                            "the template's module state is pinned decode data (the ",
                                            "adjustable fields are sigma_policy, backcast and forecast). "),
                                     paste(bad, collapse = "', '")))
    }
    for (name in names(spec_list)) spec[name] <- spec_list[name]
    class(spec) <- c("persephone_native_auto_spec", "persephone_native_spec")
    return(spec)
  }
  allowed <- c("sigma_policy", "seasonal", "trend_length", "mode", "log",
               "variables", "lower_sigma", "upper_sigma")
  bad <- setdiff(names(spec_list), allowed)
  if (length(bad)) {
    backend_native_refusal("specification updates", method, allowed,
      sprintf("field(s) '%s' are not part of the restricted native specification; ",
              paste(bad, collapse = "', '")))
  }
  for (name in names(spec_list)) spec[name] <- spec_list[name]
  class(spec) <- "persephone_native_spec"
  spec
}

# Check the same route and values used by run() before replacing R6 state.
backend_validate_native_spec <- function(spec, method, ts, allow_unset_filters = FALSE) {
  if (method == "tramoseats") return(validate_native_tramo_spec(spec, ts))
  if (!inherits(spec, "persephone_native_spec") || !identical(spec$method, method) ||
      !identical(method, "x13") || !is.numeric(spec$frequency) ||
      !identical(as.numeric(spec$frequency), as.numeric(stats::frequency(ts))))
    stop("Native specification class, method or frequency does not match this series.", call. = FALSE)
  if (inherits(spec, "persephone_native_estimated_spec")) {
    if (!spec$template %in% c("rsa0", "rsa1", "rsa2c") || anyDuplicated(names(spec)) ||
        !setequal(names(spec), c("method", "template", "frequency", "forecast", "backcast")))
      stop("Invalid stock estimated X-13 specification.", call. = FALSE)
    valid_count <- function(value, allowed) is.numeric(value) && is.null(dim(value)) &&
      length(value) == 1L && !is.na(value) && value %in% allowed
    if (!valid_count(spec$forecast, c(-1, 12)) || !valid_count(spec$backcast, 0))
      backend_native_refusal("specification updates", method, "estimated_x13_fit.forecast",
                             "the stock estimated X-13 template fixes twelve forecasts and zero backcasts; ")
    if (stats::frequency(ts) != 12 || any(!is.finite(ts)))
      stop("Stock estimated X-13 requires complete finite monthly observations.", call. = FALSE)
    return(invisible(spec))
  }
  automatic <- inherits(spec, "persephone_native_auto_spec")
  route <- if (automatic) "rsa3" else "restricted"
  if (!identical(spec$template, route))
    stop("Native specification template does not match its route.", call. = FALSE)
  shape <- backend_native_default_spec(method, route, spec$frequency)
  optional <- c("sigma_policy", if (automatic) c("backcast", "forecast") else c("trend_length", "seasonal"))
  if (anyDuplicated(names(spec)) || length(setdiff(names(spec), names(shape))) ||
      length(setdiff(setdiff(names(shape), optional), names(spec))))
    stop("Invalid fields in native specification.", call. = FALSE)
  if (automatic) {
    if (!identical(spec$lower_sigma, shape$lower_sigma) ||
        !identical(spec$upper_sigma, shape$upper_sigma))
      stop("Automatic sigma limits must match the pinned template.", call. = FALSE)
    backend_native_auto_request(spec, ts)
  } else {
    backend_native_request(spec, ts, allow_unset_filters)
  }
  invisible(spec)
}

# Cheap deterministic fingerprint (two rolling prime-modulus hashes of a
# canonical request text; non-cryptographic, session- and platform-stable).
native_text_fingerprint <- function(text) {
  bytes <- as.integer(charToRaw(text))
  hash <- function(mod) {
    state <- 0
    for (b in bytes) state <- (state * 131L + b) %% mod
    sprintf("%04x%04x", state %/% 65536, state %% 65536)
  }
  paste0(hash(2147483647L), "-", hash(4294967291))
}

native_request_fingerprint <- function(request) {
  text <- paste(c(request$method, request$frequency, request$start_period,
                  request$log, request$mode, request$sigma,
                  request$trend_length, request$lower_sigma, request$upper_sigma,
                  request$seasonal,
                  unlist(lapply(request$variables, function(v) c(
                    v$class, v$effect, format(v$coefficient, digits = 17),
                    lengths(v$columns), sum(vapply(v$columns, sum, numeric(1))))))),
             collapse = "\r\n")
  native_text_fingerprint(text)
}

# Canonical text of the automatic request (names + %.17g / verbatim values,
# nested lists flattened with dotted paths): the fingerprint must cover the
# full pinned decode state plus the request-visible scalars, so that any
# change to the decode table or the stated policy is fingerprint-visible.
native_auto_canonical_text <- function(template, request) {
  flatten <- function(x, prefix) {
    if (is.list(x)) {
      unlist(lapply(names(x), function(name)
        flatten(x[[name]], if (nzchar(prefix)) paste0(prefix, ".", name) else name)))
    } else if (is.null(x)) {
      paste0(prefix, "=NULL")
    } else {
      values <- if (is.numeric(x) && !is.logical(x))
        sprintf("%.17g", x) else as.character(x)
      paste0(prefix, "=", values, collapse = "\r\n")
    }
  }
  paste(c(paste0("template=", template),
          unlist(lapply(names(request), function(name)
            flatten(request[[name]], name)))),
        collapse = "\r\n")
}

# Full scope validation: returns the rkalos native_x13_fit argument list or
# refuses naming every violated scope field.
backend_native_request <- function(spec, ts, allow_unset_filters = FALSE) {
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
  if (!is.list(spec$variables) || is.data.frame(spec$variables)) {
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
        !is.character(v$class) || !is.null(dim(v$class)) || length(v$class) != 1L || !(v$class %in% var_classes) ||
        !is.character(v$effect) || !is.null(dim(v$effect)) || length(v$effect) != 1L || !(v$effect %in% var_effects) ||
        !is.list(v$columns) || !length(v$columns)) {
      violated <- c(violated, "fixed_coefficients_only")
      detail <- c(detail, sprintf("%s must provide class, effect, coefficient and column vectors from the pinned vocabulary;", where))
      next
    }
    if (!is.numeric(v$coefficient) || !is.null(dim(v$coefficient)) || length(v$coefficient) != length(v$columns) ||
        anyNA(v$coefficient) || any(!is.finite(v$coefficient))) {
      violated <- c(violated, "fixed_coefficients_only")
      detail <- c(detail, sprintf("%s must carry one finite coefficient per regression column (nothing is estimated);", where))
    }
    bad_columns <- !all(vapply(v$columns, function(col) is.numeric(col) && is.null(dim(col)) && length(col) == n &&
                                 all(is.finite(col)), logical(1)))
    if (bad_columns) {
      violated <- c(violated, "fixed_coefficients_only")
      detail <- c(detail, sprintf("%s must provide numeric columns of series length %d;", where, n))
    }
  }
  fixed_filters <- c("stable", "s3x1", "s3x3", "s3x5", "s3x9", "s3x15")
  seasonal <- spec$seasonal
  if (!(allow_unset_filters && is.null(seasonal)) &&
      (!is.character(seasonal) || !is.null(dim(seasonal)) || length(seasonal) != freq || anyNA(seasonal))) {
    violated <- c(violated, "fixed_seasonal_requests_only")
    detail <- c(detail, "the seasonal requests must state one pinned filter per calendar period (no default is applied);")
  } else if (any(!seasonal %in% fixed_filters)) {
    violated <- c(violated, "fixed_seasonal_requests_only")
    detail <- c(detail, sprintf("filters '%s' are not fixed requests (qualified: %s);",
                                paste(unique(setdiff(seasonal, fixed_filters)), collapse = ", "),
                                paste(fixed_filters, collapse = ", ")))
  }
  trend <- spec$trend_length
  if (!(allow_unset_filters && is.null(trend)) &&
      (!is.numeric(trend) || !is.null(dim(trend)) || length(trend) != 1L || is.na(trend) || !is.finite(trend) ||
      trend != floor(trend) || trend < 5 || trend > .Machine$integer.max || trend %% 2 == 0)) {
    violated <- c(violated, "fixed_trend_length_only")
    detail <- c(detail, "the Henderson trend filter length must be a stated odd integer >= 5 (the automatic 0 is refused);")
  }
  # NULL/omitted sigma_policy receives the qualified default ("legacy",
  # verified by the gate); an explicitly stated policy must be one of the
  # two pinned values -- the core validates it verbatim downstream.
  sigma <- spec$sigma_policy
  sigma_stated <- !is.null(sigma)
  if (sigma_stated && (!is.character(sigma) || !is.null(dim(sigma)) || length(sigma) != 1L || is.na(sigma) ||
      !sigma %in% c("legacy", "accurate"))) {
    violated <- c(violated, "sigma_policy_default")
    detail <- c(detail, sprintf("a stated sigma policy must be \"legacy\" or \"accurate\" (the qualified default is \"%s\");",
                                rf$sigma_policy_default))
  }
  if (!sigma_stated) sigma <- rf$sigma_policy_default
  if (!is.character(spec$mode) || !is.null(dim(spec$mode)) || length(spec$mode) != 1L || !spec$mode %in% rf$modes) {
    violated <- c(violated, "modes")
    detail <- c(detail, sprintf("the decomposition mode must be one of %s;",
                                paste(rf$modes, collapse = ", ")))
  }
  if (!is.logical(spec$log) || !is.null(dim(spec$log)) || length(spec$log) != 1L || is.na(spec$log)) {
    violated <- c(violated, "modes")
    detail <- c(detail, "the log flag must be TRUE or FALSE;")
  }
  for (name in c("lower_sigma", "upper_sigma")) {
    value <- spec[[name]]
    if (!is.numeric(value) || !is.null(dim(value)) || length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0.5) {
      violated <- c(violated, name)
      detail <- c(detail, sprintf("%s must be a finite number greater than 0.5;", name))
    }
  }
  if (!length(violated) && spec$upper_sigma <= spec$lower_sigma) {
    violated <- "upper_sigma"
    detail <- "upper_sigma must exceed lower_sigma;"
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

# Automatic-lane request build: validates the spec-visible envelope fields
# (sigma policy), derives the time-domain scalars and returns the rkalos
# native_x13_fit_automatic argument list plus the canonical request text. The
# module state itself is the pinned decode table, never spec data.
backend_native_auto_request <- function(spec, ts) {
  af <- backend_native_auto_gate("seasonal-adjustment fitting", "x13")
  freq <- stats::frequency(ts)
  violated <- character(0)
  detail <- character(0)
  if (as.numeric(freq) != af$frequency) {
    violated <- c(violated, "monthly_only")
    detail <- c(detail, sprintf("frequency %d is not the qualified frequency %d;",
                                freq, af$frequency))
  }
  sigma <- spec$sigma_policy
  sigma_stated <- !is.null(sigma)
  if (sigma_stated && (!is.character(sigma) || !is.null(dim(sigma)) || length(sigma) != 1L || is.na(sigma) ||
      !sigma %in% c("legacy", "accurate"))) {
    violated <- c(violated, "sigma_policy_default")
    detail <- c(detail, sprintf("a stated sigma policy must be \"legacy\" or \"accurate\" (the qualified default is \"%s\");",
                                af$x11$sigma_policy_default))
  }
  if (length(violated)) {
    backend_native_refusal("seasonal-adjustment fitting", "x13", violated,
                           paste(detail, collapse = " "))
  }
  span <- stats::tsp(ts)
  start_period <- if (is.null(span)) 1L else
    as.integer(round((span[1L] - floor(span[1L])) * freq)) + 1L
  first_year <- if (is.null(span)) 1L else as.integer(floor(span[1L]))
  args <- backend_native_auto_decode(sigma, as.numeric(spec$lower_sigma),
                                     as.numeric(spec$upper_sigma))
  # Older stored specifications predate horizon fields and use the original zero default.
  for (key in c("backcast", "forecast")) {
    value <- if (key %in% names(spec)) spec[[key]] else 0L
    if (!is.numeric(value) || is.object(value) || !is.null(dim(value)) || length(value) != 1L ||
        !is.finite(value) || value != trunc(value) || abs(value) > .Machine$integer.max)
      stop(key, " must be a single whole number in the R integer range.", call. = FALSE)
    args[[key]] <- as.integer(value)
  }
  counts <- as.double(c(args$backcast, args$forecast))
  months <- ifelse(counts < 0, -counts * freq, counts)
  if (length(ts) + sum(months) > .Machine$integer.max)
    stop("Extended series exceeds the supported index range.", call. = FALSE)
  if (any(months > 0) &&
      !all(c("a1a", "a1b", "d11a", "d12a", "d16a", "d18a") %in% af$x11$horizon_tables))
    backend_native_refusal("seasonal-adjustment fitting", "x13", "horizon_tables",
                           "the installed rkalos must expose qualified split horizon tables; ")
  args <- c(list(series = ts), args,
            list(frequency = as.integer(freq), start_period = start_period,
                 first_year = first_year))
  canonical_args <- args[c("transform", "automodel", "outliers", "ami",
                           "regression", "sigma", "lower_sigma", "upper_sigma",
                           "msr_policy", "mode", "backcast", "forecast",
                           "frequency", "start_period", "first_year")]
  # The fingerprint records the EFFECTIVE policy (the interface default is the
  # qualified "legacy", so an omitted policy fingerprints identically to the
  # explicit one, as on the restricted lane); the provenance distinguishes
  # default from explicit through the rkalos sigma_provenance echo.
  canonical_args$sigma <- if (sigma_stated) sigma else af$x11$sigma_policy_default
  canonical <- native_auto_canonical_text("rsa3", canonical_args)
  list(args = args, fingerprint = native_text_fingerprint(canonical),
       sigma_stated = sigma_stated)
}

backend_native_fit <- function(ts, spec, context, userdefined) {
  if (!is.null(context)) {
    backend_native_refusal("seasonal-adjustment fitting", "x13", "context",
      "external-regressor contexts are an rjd3 estimation feature and are outside the qualified native domain; ")
  }
  if (!all(userdefined %in% userdefined_default)) {
    backend_native_refusal("seasonal-adjustment fitting", "x13", "userdefined",
      "user-defined output variables are an rjd3 descriptor-pool feature; ")
  }
  if (inherits(spec, "persephone_native_estimated_spec")) {
    backend_validate_native_spec(spec, "x13", ts)
    fit <- rkalos::native_x13_estimated_fit(ts, spec$template)
    if (!identical(fit$status, "available")) stop("Stock estimated X-13 fit: ", fit$status, call. = FALSE)
    attr(fit, "persephone_request_fingerprint") <- native_text_fingerprint(
      paste(c("x13", spec$template, stats::start(ts), spec$forecast, spec$backcast), collapse = "/"))
    return(fit)
  }
  if (inherits(spec, "persephone_native_auto_spec")) {
    return(backend_native_auto_fit(ts, spec))
  }
  if (!inherits(spec, "persephone_native_spec")) {
    backend_native_refusal("seasonal-adjustment fitting", "x13", "specification",
      "the specification was not built by the native constructor; ")
  }
  request <- backend_native_request(spec, ts)
  fingerprint <- native_request_fingerprint(request)
  fit <- do.call(rkalos::native_x13_fit, c(list(series = ts), request[c("variables", "log",
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

# The automatic fit through rkalos' qualified automatic pipeline. The status
# echo is verbatim (pinned fail-closed matrix): outside the envelope the core
# refuses with an explicit unsupported:/failed: status and we stop with it --
# NEVER a fallback to the restricted fit and never a Java call.
backend_native_auto_fit <- function(ts, spec) {
  built <- backend_native_auto_request(spec, ts)
  fit <- do.call(rkalos::native_x13_fit_automatic, built$args)
  if (!identical(fit$status, "available")) {
    stop(sprintf(paste0("The native automatic fit refused the request with the verbatim core status: %s ",
                        "(request fingerprint %s). No Java fallback was used. ",
                        "Select backend = \"rjd3\" to use the reference implementation."),
                 fit$status, built$fingerprint), call. = FALSE)
  }
  attr(fit, "persephone_request_fingerprint") <- built$fingerprint
  fit
}

native_x13_components <- function(fit, spec) {
  pre <- fit$preadjustment
  cal <- fit$finals$tables$d18
  if (identical(spec$template, "restricted")) {
    # CalendarEffect selects days-related and moving-holiday variables; a
    # user variable assigned to the calendar component only contributes to D18.
    a6 <- pre$tables$a6
    a7 <- pre$tables$a7
    if (!is.null(a6) && !is.null(a7)) cal <- if (pre$log) a6 * a7 else a6 + a7
  }
  list(y = pre$tables$a1, t = fit$finalTrend, sa = fit$finalSA,
       s = fit$finals$tables$d16, i = fit$finalIrregular, cal = cal)
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
  components <- native_x13_components(fit, spec)
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
                 forecasts = NULL, diagnostics = fit$diagnostics %||% list(), availability = availability,
                 provenance = provenance, results = fit$results,
                 preadjustment = fit$preadjustment, selection = fit$selection,
                 raw = fit),
            class = "persephone_native_restricted_fit")
}

# Versioned envelope for the automatic fit: the honest sibling of the
# restricted envelope. Deliberately NOT an rjd3-shaped result and not classed
# "persephone_result". Components and forecasts retain the dated rkalos
# tables; the preprocessing decision chain is carried as provenance.
normalize_native_automatic_result <- function(fit, spec, ts) {
  stock <- inherits(fit, "native_x13_estimated_fit")
  classes <- c("persephone_native_auto_spec", "persephone_native_estimated_spec")
  if (inherits(spec, "persephone_spec") && inherits(spec$parameters, classes)) {
    spec <- spec$parameters
  }
  if (inherits(spec, classes) &&
      !identical(as.numeric(spec$frequency), as.numeric(stats::frequency(ts)))) {
    stop("Specification frequency does not match the result's observed series.", call. = FALSE)
  }
  components <- native_x13_components(fit, spec)
  component_names <- c("y", "t", "sa", "s", "i", "cal")
  forecasts <- native_automatic_forecasts(fit, ts)
  state <- function(value) {
    if (is.null(value)) "not_provided" else if (all(is.na(value))) "missing" else "available"
  }
  availability <- data.frame(
    field = c(component_names, paste0(component_names, "_f"), "y_ef"),
    state = c(vapply(components, state, character(1)),
              if (is.null(forecasts)) rep("not_provided", length(component_names))
              else forecasts$availability$points,
              if (is.null(forecasts)) "not_provided" else forecasts$availability$standard_errors[1L]),
    stringsAsFactors = FALSE, row.names = NULL)
  code <- if (identical(fit$status, "available") && !is.null(fit$finalSA)) "success" else "incomplete"
  provenance <- c(
    list(backend = "native", automatic = !stock,
         decode_template = if (stock) tolower(fit$template) else "rsa3",
         request_fingerprint = attr(fit, "persephone_request_fingerprint", exact = TRUE),
         core_commit = tryCatch(rkalos::core_version()$commit, error = function(e) NA_character_)),
    fit[c("type", "schema_version", "scope", "mode_requested", "mode_effective",
          "sigma", "sigma_set", "sigma_provenance", "lower_sigma", "upper_sigma",
          "effective_backcast", "effective_forecast")],
    list(model = fit$model))
  envelope <- structure(list(schema_version = 1L, method = "x13", backend = "native",
                 specification = spec,
                 status = list(code = code,
                               convergence = fit$model$estimation_status %||% "not_reported"),
                 domain = backend_domain(ts), components = components,
                 forecasts = forecasts, diagnostics = list(), availability = availability,
                 provenance = provenance, results = fit$results,
                 preadjustment = fit$preadjustment, selection = fit$selection,
                 chain = fit$chain, model = fit$model, raw = fit),
            class = if (stock) c("persephone_native_estimated_fit", "persephone_native_automatic_fit")
                    else "persephone_native_automatic_fit")
  if (stock) {
    envelope$likelihood <- fit$likelihood
    envelope$diagnostics$residuals <- fit$residuals
  }
  envelope
}

#' @export
print.persephone_native_automatic_fit <- function(x, ...) {
  cat(if (inherits(x, "persephone_native_estimated_fit")) "Stock estimated" else "Automatic",
      "native X-13 fit (rkalos/Kalos core), template", x$specification$template, "\n")
  cat("  status:", x$status$code, "| convergence:", x$status$convergence,
      "| scope:", x$provenance$scope, "\n")
  cat("  mode requested:", x$provenance$mode_requested,
      "| effective:", x$provenance$mode_effective, "\n")
  cat("  sigma policy:", x$provenance$sigma_provenance,
      "| log:", isTRUE(x$provenance$model$log),
      "| fingerprint:", x$provenance$request_fingerprint, "\n")
  if (!is.null(x$components$sa)) {
    cat("  final SA (last 3):", paste(round(utils::tail(as.numeric(x$components$sa), 3), 4),
                                      collapse = ", "), "\n")
  }
  invisible(x)
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
