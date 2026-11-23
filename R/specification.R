# Versioned value representation, additive to the existing raw $params API.
# Native specifications must not claim to be JD3 specification classes.
new_backend_spec <- function(method, template = NULL, frequency, parameters,
                             backend = NULL) {
  method <- backend_method(method)
  backend <- resolve_backend(backend)
  frequency <- backend_frequency(frequency)
  if (!is.null(template)) {
    if (backend == "native") {
      backend_native_check_template(template, method)
    } else {
      check_template(template, method)
    }
  }
  if (!is.list(parameters)) {
    stop("Specification parameters must be a list.", call. = FALSE)
  }
  structure(list(schema_version = 1L, method = method, template = template,
                 frequency = frequency, backend = backend, parameters = parameters),
            class = "persephone_spec")
}

backend_spec_parameters <- function(spec, method, backend, frequency = NULL) {
  if (inherits(spec, "persephone_spec")) {
    if (!identical(spec$schema_version, 1L)) {
      stop("Unsupported Persephone specification schema version.", call. = FALSE)
    }
    if (!identical(spec$method, method)) {
      stop("Specification method does not match the requested method.", call. = FALSE)
    }
    if (!identical(spec$backend, backend)) {
      stop("Specification backend does not match the requested backend; explicit conversion is required.",
           call. = FALSE)
    }
    if (!is.null(frequency) && !identical(as.numeric(spec$frequency), as.numeric(frequency)))
      stop("Specification frequency does not match the series.", call. = FALSE)
    if (backend == "native" && !is.null(spec$template) &&
        !identical(spec$template, spec$parameters$template))
      stop("Specification envelope and parameter templates disagree.", call. = FALSE)
    spec <- spec$parameters
  }
  # Retain the public legacy specification representation while preventing
  # accidental cross-method execution. Do not silently relabel a JD3 spec.
  wrong_class <- if (method == "x13") "JD3_TRAMOSEATS_SPEC" else "JD3_X13_SPEC"
  if (inherits(spec, wrong_class)) {
    stop("Specification method does not match the requested method.", call. = FALSE)
  }
  spec
}

# Build changes before touching R6 state. Template replacement, a complete
# specification and incremental setters have distinct, deterministic meanings.
specification_changes <- function(speclist, dots) {
  if (!is.null(speclist) && length(dots)) {
    stop("Use either 'speclist' or named parameter arguments, not both.", call. = FALSE)
  }
  changes <- if (is.null(speclist)) dots else speclist
  if (!is.list(changes) || (length(changes) &&
      (is.null(names(changes)) || anyNA(names(changes)) ||
       any(!nzchar(names(changes))) || anyDuplicated(names(changes))))) {
    stop("Specification changes must be a uniquely named list.", call. = FALSE)
  }
  changes
}

prepare_specification_update <- function(current, method, frequency, backend,
                                         template = NULL, spec = NULL,
                                         changes = list(), series = NULL) {
  if (!is.null(template) && !is.null(spec)) {
    stop("Supply either 'template' or a complete 'spec', not both.", call. = FALSE)
  }
  if (!is.null(spec)) {
    proposed <- backend_spec_parameters(spec, method, backend, frequency)
    expected <- if (method == "x13") "JD3_X13_SPEC" else "JD3_TRAMOSEATS_SPEC"
    if (backend == "rjd3" && !inherits(proposed, expected)) {
      stop("A complete rjd3 specification must have class ", expected, ".", call. = FALSE)
    }
  } else if (!is.null(template)) {
    proposed <- backend_default_spec(method, template, frequency, backend)
  } else {
    proposed <- current
  }
  if (length(changes)) {
    proposed <- backend_update_spec(proposed, method, frequency, changes, backend)
  }
  if (backend == "native") {
    draft <- is.null(current$seasonal) || is.null(current$trend_length)
    allow_draft <- identical(template, "restricted") ||
      (is.null(spec) && is.null(template) && draft)
    backend_validate_native_spec(proposed, method, series, allow_unset_filters = allow_draft)
  }
  proposed
}

validate_update_context <- function(context, userdefined) {
  if (!is.null(context) && !is.list(context)) {
    stop("'context' must be NULL or a list of calendars/regressors.", call. = FALSE)
  }
  if (!is.null(userdefined) && (!is.character(userdefined) || anyNA(userdefined))) {
    stop("'userdefined' must be NULL or a character vector without missing values.", call. = FALSE)
  }
}
