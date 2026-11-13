# Verified projections from current rjd3 results. The raw output is retained for
# compatibility; these envelopes do not assert that unimplemented fields exist.
backend_domain <- function(series) {
  if (is.null(series)) return(NULL)
  if (!stats::is.ts(series)) stop("A normalized series must have a time domain.", call. = FALSE)
  list(start = stats::start(series), end = stats::end(series),
       frequency = stats::frequency(series), length = length(series),
       missing = is.na(series))
}

backend_metric <- function(value) {
  if (is.null(value)) return(list(kind = "missing", value = NULL))
  if (is.list(value) && all(c("value", "pvalue") %in% names(value))) {
    return(list(kind = "test", statistic = value$value, pvalue = value$pvalue,
                distribution = attr(value, "distribution", exact = TRUE)))
  }
  if (is.character(value)) return(list(kind = "category", value = value))
  if (is.numeric(value)) return(list(kind = "statistic", value = value))
  list(kind = "unclassified", value = value)
}

backend_versions <- function(backend, method) {
  if (backend != "rjd3") return(list())
  packages <- c("rjd3toolkit", paste0("rjd3", method), "rjd3jars")
  stats::setNames(lapply(packages, function(package) {
    if (!nzchar(system.file(package = package))) return(NA_character_)
    as.character(utils::packageVersion(package))
  }), packages)
}

normalize_backend_result <- function(output, method, backend, spec, ts) {
  method <- backend_method(method)
  backend <- resolve_backend(backend)
  if (!is.null(output) && !is.list(output)) {
    stop("Backend output must be a list or NULL.", call. = FALSE)
  }
  if (inherits(spec, "persephone_spec")) {
    backend_spec_parameters(spec, method, backend)
    if (!identical(as.numeric(spec$frequency), as.numeric(stats::frequency(ts)))) {
      stop("Specification frequency does not match the result's observed series.", call. = FALSE)
    }
    specification <- spec
  } else {
    specification <- new_backend_spec(method, frequency = stats::frequency(ts),
                                      parameters = spec, backend = backend)
  }
  user_defined <- output$user_defined
  component_names <- c("y", "t", "sa", "s", "i", "cal")
  components <- stats::setNames(lapply(component_names, function(name) user_defined[[name]]),
                                component_names)
  points <- stats::setNames(lapply(component_names, function(name) user_defined[[paste0(name, "_f")]]),
                            component_names)
  errors <- list(y = user_defined$y_ef)
  diagnostics_names <- names(user_defined)[grepl(
    "^(diagnostics\\.|residuals\\.(doornikhansen|lb)$|regression\\.td-ftest$|quality\\.summary$|m-statistics\\.)",
    names(user_defined))]
  diagnostics <- stats::setNames(lapply(diagnostics_names, function(name) {
    backend_metric(user_defined[[name]])
  }), diagnostics_names)
  field_names <- c(component_names, paste0(component_names, "_f"), "y_ef")
  availability <- data.frame(
    field = field_names,
    state = vapply(field_names, function(name) {
      value <- user_defined[[name]]
      if (is.null(value)) "not_provided" else if (length(value) && all(is.na(value))) "missing" else "available"
    }, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  code <- if (is.null(output)) "failed" else if (is.null(components$sa)) "incomplete" else "success"
  structure(list(schema_version = 1L, method = method, backend = backend,
                 specification = specification,
                 provenance = list(backend = backend, packages = backend_versions(backend, method)),
                 status = list(code = code, convergence = "not_reported"),
                 domain = backend_domain(ts), components = components,
                 forecasts = new_forecast_contract(points, errors, observed = ts,
                                                   mode = forecast_mode(output)),
                 diagnostics = diagnostics, availability = availability,
                 raw = output), class = "persephone_result")
}
