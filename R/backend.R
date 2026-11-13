# Backend dispatch deliberately contains names, not eagerly resolved functions.
# This file must remain safe to source on systems without Java or rjd3.
backend_method <- function(method) {
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !method %in% c("x13", "tramoseats")) {
    stop("'method' must be exactly 'x13' or 'tramoseats'.", call. = FALSE)
  }
  method
}

resolve_backend <- function(backend = NULL) {
  if (is.null(backend)) backend <- getOption("persephone.backend", "rjd3")
  if (!is.character(backend) || length(backend) != 1L || is.na(backend) ||
      !backend %in% c("rjd3", "native")) {
    stop("'backend' must be exactly 'rjd3' or 'native'.", call. = FALSE)
  }
  backend
}

backend_block_registry <- function() {
  c(arima = "set_arima", automdl = "set_automodel", basic = "set_basic",
    easter = "set_easter", estimate = "set_estimate", setout = "set_outlier",
    td = "set_tradingdays", transform = "set_transform", addout = "add_outlier",
    remout = "remove_outlier", addramp = "add_ramp", remramp = "remove_ramp",
    usrdefvar = "add_usrdefvar", x11 = "set_x11", benchmarking = "set_benchmarking")
}

#' Inspect seasonal-adjustment backend capabilities
#'
#' Reports the explicitly selected backend's implementation status without
#' initializing Java. Native seasonal-adjustment fitting is not implemented
#' yet; requesting it raises an error and never falls back to rjd3.
#'
#' @param backend Either `"rjd3"` or `"native"`. If `NULL`, use the deliberate
#'   `persephone.backend` option, defaulting to `"rjd3"` when unset.
#' @param method Optional method, exactly `"x13"` or `"tramoseats"`.
#' @return A list describing implementation status, methods, specification
#'   blocks and package availability. Package presence does not prove that a
#'   working Java runtime is available; `runtime_checked` is always `FALSE`.
#' @export
backend_capabilities <- function(backend = NULL, method = NULL) {
  backend <- resolve_backend(backend)
  methods <- if (is.null(method)) c("x13", "tramoseats") else backend_method(method)
  if (backend == "native") {
    return(list(backend = backend, schema_version = 1L,
                status = "unimplemented", fit = FALSE,
                primitives = c("time_domain", "difference_polynomial", "convolve", "finite_filter"),
                methods = character(), specification_blocks = character(),
                packages_available = NA, runtime_checked = FALSE,
                reason = "Native seasonal-adjustment fitting is not implemented."))
  }
  packages <- unique(c("rjd3toolkit", paste0("rjd3", methods)))
  available <- vapply(packages, function(package) {
    nzchar(system.file(package = package))
  }, logical(1))
  blocks <- names(backend_block_registry())
  if (identical(methods, "tramoseats")) blocks <- setdiff(blocks, "x11")
  list(backend = backend, schema_version = 1L, status = "reference", fit = TRUE,
       methods = methods, specification_blocks = blocks,
       packages_available = available, runtime_checked = FALSE,
       reason = if (all(available)) NULL else "Install the missing optional rjd3 packages to use this backend.")
}

backend_unimplemented <- function(method, operation) {
  stop(sprintf(paste0("The native backend does not yet implement %s for '%s'. ",
                      "No Java fallback was used. Select backend = \"rjd3\" ",
                      "to use the reference implementation."), operation, method),
       call. = FALSE)
}

backend_rjd3_function <- function(package, name) {
  # Toolkit initialization installs the protobuf descriptors needed by the
  # downstream wrappers. Initialization now happens only on an explicit request.
  for (required in unique(c("rjd3toolkit", package))) {
    if (!requireNamespace(required, quietly = TRUE)) {
      stop(sprintf(paste0("The rjd3 backend requires the optional package '%s' ",
                          "and its compatible Java runtime. Install it before ",
                          "using backend = \"rjd3\"."), required), call. = FALSE)
    }
  }
  # rjd3 resolves protobuf descriptors as bare symbols (for example x13.Spec).
  # cleanEx() can detach their search-path database while leaving all namespaces
  # loaded, so requireNamespace() alone cannot restore those lookups. Reattach
  # the existing database without resetting schemas or reinitializing Java.
  # RProtoBuf's attachDescriptorPool() cannot do this: it skips initialized pools.
  # Keep this compatibility bridge to its unexported OTABLE isolated here and
  # verify its type and the public descriptor API before trusting the lookup.
  if (!"RProtoBuf:DescriptorPool" %in% search()) {
    proto_namespace <- asNamespace("RProtoBuf")
    pool <- get0("OTABLE", envir = proto_namespace, inherits = FALSE)
    if (!inherits(pool, "UserDefinedDatabase") || typeof(pool) != "externalptr") {
      stop(paste0("The rjd3 backend could not restore RProtoBuf's descriptor pool. ",
                  "Restart R and use compatible RProtoBuf and rjd3 package versions."),
           call. = FALSE)
    }
    base::attach(pool, pos = length(search()), name = "RProtoBuf:DescriptorPool")
    descriptor_name <- switch(package, rjd3x13 = "x13.Spec",
                              rjd3tramoseats = "tramoseats.Spec", "jd3.TsData")
    restored <- tryCatch({
      descriptor <- getExportedValue("RProtoBuf", "P")(descriptor_name)
      visible <- get(descriptor_name, envir = as.environment("RProtoBuf:DescriptorPool"),
                     inherits = FALSE)
      inherits(descriptor, "Descriptor") && inherits(visible, "Descriptor")
    }, error = function(e) FALSE)
    if (!restored) {
      stop(paste0("The rjd3 backend restored RProtoBuf's pool but its required ",
                  "descriptors are unavailable. Restart R and use compatible ",
                  "RProtoBuf and rjd3 package versions."), call. = FALSE)
    }
  }
  getExportedValue(package, name)
}

backend_frequency <- function(freq) {
  if (!is.numeric(freq) || length(freq) != 1L || is.na(freq) ||
      !is.finite(freq) || freq < 1 || freq != floor(freq)) {
    stop("The series frequency must be a positive whole number.", call. = FALSE)
  }
  freq
}

backend_default_spec <- function(method, template, freq, backend = NULL) {
  method <- backend_method(method)
  backend <- resolve_backend(backend)
  backend_frequency(freq)
  check_template(template, method)
  if (backend == "native") backend_unimplemented(method, "specification construction")
  fun <- backend_rjd3_function(paste0("rjd3", method),
                              if (method == "x13") "x13_spec" else "tramoseats_spec")
  fun(name = template)
}

backend_spec_setter <- function(block, method, backend = NULL) {
  method <- backend_method(method)
  backend <- resolve_backend(backend)
  registry <- backend_block_registry()
  if (!is.character(block) || length(block) != 1L || is.na(block) ||
      !block %in% names(registry)) {
    stop(sprintf("Unknown specification block: %s.", paste(block, collapse = ", ")),
         call. = FALSE)
  }
  if (block == "x11" && method != "x13") {
    stop("Specification block 'x11' is supported only for method 'x13'.", call. = FALSE)
  }
  if (backend == "native") backend_unimplemented(method, paste0("specification block '", block, "'"))
  package <- if (block == "x11") "rjd3x13" else "rjd3toolkit"
  backend_rjd3_function(package, unname(registry[[block]]))
}

backend_update_spec <- function(spec, method, freq, spec_list, backend = NULL) {
  method <- backend_method(method)
  backend <- resolve_backend(backend)
  backend_frequency(freq)
  if (backend == "native") backend_unimplemented(method, "specification updates")
  if (inherits(spec, "persephone_spec") && !identical(as.numeric(spec$frequency), as.numeric(freq))) {
    stop("Specification frequency does not match the series frequency.", call. = FALSE)
  }
  spec <- backend_spec_parameters(spec, method, backend)
  update_spec(spec = spec, method = method, freq = freq,
              spec_list = spec_list, backend = backend)
}

backend_fit <- function(ts, spec, context = NULL, userdefined = NULL,
                        method, backend = NULL) {
  method <- backend_method(method)
  backend <- resolve_backend(backend)
  if (backend == "native") backend_unimplemented(method, "seasonal-adjustment fitting")
  if (!stats::is.ts(ts) || !is.numeric(ts) || !is.null(dim(ts)) || !length(ts)) {
    stop("'ts' must be a nonempty univariate numeric time series.", call. = FALSE)
  }
  backend_frequency(stats::frequency(ts))
  if (inherits(spec, "persephone_spec") &&
      !identical(as.numeric(spec$frequency), as.numeric(stats::frequency(ts)))) {
    stop("Specification frequency does not match the series frequency.", call. = FALSE)
  }
  if (any(is.infinite(ts))) stop("The time series contains infinite values.", call. = FALSE)
  if (!is.null(userdefined) && (!is.character(userdefined) || anyNA(userdefined))) {
    stop("'userdefined' must be NULL or a character vector without missing values.", call. = FALSE)
  }
  spec <- backend_spec_parameters(spec, method, backend)
  fun <- backend_rjd3_function(paste0("rjd3", method),
                              if (method == "x13") "x13_fast" else "tramoseats_fast")
  fun(ts = ts, spec = spec, context = context, userdefined = userdefined)
}
