# Versioned value representation, additive to the existing raw $params API.
# Native specifications must not claim to be JD3 specification classes.
new_backend_spec <- function(method, template = NULL, frequency, parameters,
                             backend = NULL) {
  method <- backend_method(method)
  backend <- resolve_backend(backend)
  frequency <- backend_frequency(frequency)
  if (!is.null(template)) check_template(template, method)
  if (!is.list(parameters)) {
    stop("Specification parameters must be a list.", call. = FALSE)
  }
  structure(list(schema_version = 1L, method = method, template = template,
                 frequency = frequency, backend = backend, parameters = parameters),
            class = "persephone_spec")
}

backend_spec_parameters <- function(spec, method, backend) {
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
