# The native capability and value contracts must work without rjd3 or Java.
message("Test backend selection and serializable contracts")

local({
  old <- options(persephone.backend = NULL)
  on.exit(options(old))
  resolve <- persephone3:::resolve_backend
  expect_identical(resolve(), "rjd3")
  options(persephone.backend = "native")
  expect_identical(resolve(), "native")
  expect_identical(resolve("rjd3"), "rjd3")
  expect_error(resolve("r"), "exactly")
  expect_error(resolve(c("rjd3", "native")), "exactly")
  expect_error(resolve(NA_character_), "exactly")

  before <- loadedNamespaces()
  capability <- backend_capabilities("native", "x13")
  expect_identical(capability, rkalos::capabilities())
  expect_identical(capability$fit, FALSE)
  expect_identical(capability$status, "unimplemented")
  expect_identical(capability$runtime_checked, FALSE)
  expect_identical(loadedNamespaces(), before)
  expect_error(backend_capabilities("native", "x1"), "exactly")
  expect_error(persephone3:::backend_default_spec("x13", "rsa3", 12, "native"),
               "does not yet implement specification construction")
  expect_error(persephone3:::backend_fit(AirPassengers, list(), method = "x13", backend = "native"),
               "No Java fallback")
  expect_error(persephone3:::backend_update_spec(list(), "x13", 12, list(), "native"),
               "specification updates")

  spec <- persephone3:::new_backend_spec("x13", template = NULL,
                                        frequency = 12, parameters = list(), backend = "native")
  expect_inherits(spec, "persephone_spec")
  expect_identical(spec$schema_version, 1L)
  expect_identical(spec$template, NULL)
  expect_identical(unserialize(serialize(spec, NULL)), spec)
  expect_error(persephone3:::backend_spec_parameters(spec, "tramoseats", "native"), "method")
  expect_error(persephone3:::backend_spec_parameters(spec, "x13", "rjd3"), "backend")
  expect_error(persephone3:::new_backend_spec("x13", frequency = 0, parameters = list()), "frequency")
  expect_error(persephone3:::backend_spec_setter("ar", "x13", "native"), "Unknown specification block")
  expect_error(persephone3:::backend_spec_setter("x11", "tramoseats", "native"), "only")

  # A missing backend result must remain failed, not become a successful fit
  # because a requested specification is available.
  failed <- persephone3:::normalize_backend_result(NULL, "x13", "native", spec, AirPassengers)
  expect_identical(failed$status$code, "failed")
  expect_true(all(failed$availability$state == "not_provided"))
  expect_identical(failed$domain$frequency, 12)
  expect_identical(unserialize(serialize(failed, NULL)), failed)

  # Normalization preserves typed diagnostics without interpreting a category
  # as a numeric p-value; zero is distinct from missing.
  record <- list(user_defined = list(sa = AirPassengers,
                                     "diagnostics.seas-si-combined" = "Present",
                                     "m-statistics.q" = 0,
                                     "residuals.lb" = list(value = 1.2, pvalue = .3)))
  normalized <- persephone3:::normalize_backend_result(record, "x13", "native", spec, AirPassengers)
  expect_identical(normalized$diagnostics[["diagnostics.seas-si-combined"]],
                   list(kind = "category", value = "Present"))
  expect_identical(normalized$diagnostics[["m-statistics.q"]],
                   list(kind = "statistic", value = 0))
  expect_identical(normalized$diagnostics[["residuals.lb"]]$pvalue, .3)
  expect_identical(normalized$raw, record)
  expect_identical(normalized$components$sa, AirPassengers)
  expect_false(inherits(normalized, "JD3_X13_RSLTS"))
})

# The optional adapter is checked against direct calls rather than mock fits.
# Native-only CI runs the contracts above and skips only this reference section.
if (all(vapply(c("rjd3toolkit", "rjd3x13", "rjd3tramoseats"),
               requireNamespace, logical(1), quietly = TRUE))) {
  message("Test rjd3 adapter parity for monthly and quarterly fits")
  for (method in c("x13", "tramoseats")) {
    for (freq in c(12, 4)) {
      series <- if (freq == 12) AirPassengers else stats::aggregate(AirPassengers, nfrequency = 4)
      spec <- persephone3:::backend_default_spec(method, "rsa0", freq, "rjd3")
      requests <- c("y", "sa", "y_f", "y_ef", "residuals.lb", "diagnostics.seas-si-combined")
      adapter <- persephone3:::backend_fit(series, spec, userdefined = requests,
                                           method = method, backend = "rjd3")
      direct_fun <- getExportedValue(paste0("rjd3", method), paste0(method, "_fast"))
      reference <- direct_fun(series, spec = spec, userdefined = requests)
      expect_equal(adapter, reference)
      canonical <- persephone3:::normalize_backend_result(adapter, method, "rjd3", spec, series)
      expect_identical(canonical$components$sa, reference$user_defined$sa)
      expect_identical(canonical$forecasts$points$y, reference$user_defined$y_f)
      expect_identical(canonical$forecasts$standard_errors$y, reference$user_defined$y_ef)
      expect_identical(canonical$backend, "rjd3")
      expect_identical(canonical$specification$parameters, spec)
      expect_identical(unserialize(serialize(canonical, NULL)), canonical)
      expect_identical(canonical$status$code, "success")
    }
    updated <- persephone3:::backend_update_spec(spec, method, 4,
                                                list(transform.fun = "None"), "rjd3")
    expect_equal(updated, rjd3toolkit::set_transform(spec, fun = "None"))
  }
  xspec <- persephone3:::backend_default_spec("x13", "rsa3", 12, "rjd3")
  expect_error(persephone3:::backend_fit(AirPassengers, xspec,
                                        method = "tramoseats", backend = "rjd3"), "method")
  expect_error(persephone3:::backend_update_spec(xspec, "x13", 12,
                                                list(unknown.parameter = 1), "rjd3"), "Unknown specification block")

  message("Test R6 backend provenance, stale state and hierarchy dispatch")
  local({
    old <- options(persephone.backend = "rjd3")
    on.exit(options(old))
    object <- perX13(AirPassengers, "rsa0")
    expect_identical(object$backend, "rjd3")
    expect_inherits(object$specification, "persephone_spec")
    options(persephone.backend = "native")
    expect_error(perX13(AirPassengers), "does not yet implement")
    object$run()
    expect_identical(object$backend, "rjd3")
    expect_identical(object$result$backend, "rjd3")
    expect_false(object$is_stale)
    fitted_spec <- object$result$specification$parameters
    object$updateParams(transform.fun = "Log")
    expect_true(object$is_stale)
    expect_identical(object$result$specification$parameters, fitted_spec)
    object$run()
    expect_false(object$is_stale)
    restored <- unserialize(serialize(object, NULL))
    expect_identical(restored$backend, "rjd3")
    expect_equal(restored$result, object$result)
    restored$run()
    expect_equal(restored$output, object$output)

    hierarchy <- perHts(a = object, b = AirPassengers / 2,
                        method = "x13", template = "rsa0", backend = "rjd3")
    hierarchy$run()
    expect_identical(hierarchy$backend, "rjd3")
    expect_true(all(vapply(hierarchy$components, function(x) x$backend == "rjd3", logical(1))))
    expect_identical(hierarchy$result$backend, "rjd3")
    expect_identical(hierarchy$result$components$sa, hierarchy$adjustedDirect)
    expect_identical(hierarchy$specification$parameters, hierarchy$paramsDirect)
  })
} else {
  message("Skipping only optional rjd3 reference-adapter tests: dependencies unavailable")
}

# Normalization must not label monthly data with a quarterly specification.
local({
  spec <- persephone3:::new_backend_spec("x13", frequency = 4,
                                        parameters = list(), backend = "native")
  expect_error(persephone3:::normalize_backend_result(
    list(user_defined = list(sa = AirPassengers)), "x13", "native", spec, AirPassengers),
    "Specification frequency")
})
