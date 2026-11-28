# Verify the installed package without access to Java-related R packages.
# Usage: Rscript tools/verify-nojava.R [--library=/isolated/library] [--native]
# --library isolates this R process only; it never edits the user's libraries.
args <- commandArgs(trailingOnly = TRUE)
lib_arg <- args[startsWith(args, "--library=")]
if (length(lib_arg)) {
  stopifnot(length(lib_arg) == 1L)
  lib <- normalizePath(sub("^--library=", "", lib_arg), mustWork = TRUE)
  for (name in c(".Library", ".Library.site")) {
    locked <- bindingIsLocked(name, baseenv())
    if (locked) unlockBinding(name, baseenv())
    assign(name, if (name == ".Library") lib else character(), baseenv())
    if (locked) lockBinding(name, baseenv())
  }
  .libPaths(lib)
}
forbidden <- c("rJava", "rjd3toolkit", "rjd3x13", "rjd3tramoseats", "rjd3jars", "RProtoBuf")
stopifnot(!any(forbidden %in% loadedNamespaces()))
stopifnot(!any(vapply(forbidden, function(p) nzchar(system.file(package=p)), logical(1))))
library(persephone3)
stopifnot(!any(forbidden %in% loadedNamespaces()))
cap <- backend_capabilities("native")
stopifnot(identical(cap$fit, FALSE), identical(cap$status, "unimplemented"))
# M7 default-backend cutover (item 27, decision 397cfe8, 2026-11-25): no
# options(persephone.backend = ...) is set anymore. The constructions below
# run on ORDINARY defaults, so they double as the proof that the unset default
# resolves to native without Java; rjd3 remains explicitly selectable and the
# rjd3 refusal without the optional packages stays asserted further down.
stopifnot(identical(persephone3:::resolve_backend(), "native"),
          identical(persephone3:::resolve_backend("native"), "native"),
          identical(persephone3:::resolve_backend("rjd3"), "rjd3"))
# 775ce9d (2026-11-24): stock monthly X-13 RSA0/RSA1/RSA2c native specification
# construction became real (the estimated-fit route), and inst/tinytest/test-
# backend.R (975c771) states the contract the product follows: the rsa0 native
# spec CONSTRUCTS as a persephone_native_estimated_spec and only rsa4/rsa5c
# still refuse construction. Probe both routes the test pins -- the internal
# spec builder and the product path.
stock <- persephone3:::backend_default_spec("x13", "rsa0", 12, "native")
stopifnot(inherits(stock, "persephone_native_estimated_spec"))
stopifnot(inherits(perX13(AirPassengers, "rsa0")$specification$parameters,
                   "persephone_native_estimated_spec"))
err <- tryCatch(perX13(AirPassengers, "rsa4"), error = identity)
stopifnot(inherits(err, "error"),
          grepl("does not yet implement specification construction", conditionMessage(err)))
# M4-gap (2026-11-21), re-attached to the rsa4 refusal when rsa0 construction
# landed: the automodel-template refusal still cites BOTH qualified envelopes
# and quotes the pinned core status verbatim; the restricted-route part must
# name the fixed-coefficient violation, and that core status must be what the
# installed core actually answers right now.
msg0 <- conditionMessage(err)
stopifnot(grepl("envelopes \\(automatic and restricted\\)", msg0),
          grepl("automatic_fit.deferred", msg0, fixed = TRUE),
          grepl("No Java fallback", msg0, fixed = TRUE),
          grepl("unsupported:x13 automatic fit defers calendar/easter/TD/user detection (21-I)",
                msg0, fixed = TRUE))
fit_fun <- rkalos::native_x13_fit
st <- fit_fun(as.numeric(AirPassengers), seasonal = rep("msr", 12L), trend_length = 13L,
              frequency = 12L, start_period = 1L)
stopifnot(identical(st$status, "unsupported:x13 fit fixed seasonal requests only"))
err <- tryCatch(perTramo(AirPassengers, backend = "rjd3"), error = identity)
stopifnot(inherits(err, "error"), grepl("optional package", conditionMessage(err)))
# rkalos is a required dependency; it supplies every compiled primitive.
stopifnot(identical(cap$methods, c("x13", "tramoseats")),
          grepl("^[0-9a-f]{40}$", rkalos::core_version()$commit))
tramo <- perTramo(AirPassengers, template = "rsa0", backend = "native")
tramo$run()
stopifnot(identical(tramo$adjusted, tramo$output$finalSA),
          identical(tramo$result$backend, "native"), length(tramo$forecasts$points$y) == 24L)
# Restricted native fit (rkalos schema_version 2): the qualified fixed-
# coefficient X-13 fit must run through the product path WITHOUT Java, and
# out-of-domain requests must refuse naming the violated scope field.
# The sigma policy is omitted: the maintainer decision of 2026-11-17 makes
# "legacy" (pinned JDemetra behavior) the product default, recorded honestly
# in the provenance; a stated policy outside "legacy"/"accurate" is refused.
if (identical(rkalos::capabilities()$native_x13_fit_restricted, TRUE)) {
  stopifnot(identical(rkalos::capabilities()$restricted_fit$sigma_policy_default,
                      "legacy"))
  obj <- perX13(AirPassengers, template = "restricted",
                speclist = list(seasonal = rep("s3x15", 12L), trend_length = 13L))
  obj$run()
  saved <- obj$result
  stopifnot(!obj$is_stale, inherits(try(obj$updateParams(spec = list(bad = TRUE)),
                                       silent = TRUE), "try-error"),
            identical(obj$result, saved), !obj$is_stale)
  obj$getRegressors()
  obj$getOutliers()
  summary(obj)
  print(obj)
  stopifnot(identical(obj$result$status$code, "success"),
            identical(obj$result$backend, "native"),
            !inherits(obj$result, "persephone_result"),
            identical(obj$result$provenance$sigma_policy, "legacy (default)"),
            stats::frequency(obj$adjusted) == 12L,
            is.null(obj$forecasts))
  refused <- tryCatch({
    bad <- perX13(AirPassengers, template = "restricted",
                  speclist = list(sigma_policy = "jdemetra",
                                  seasonal = rep("s3x15", 12L), trend_length = 13L))
    bad$run()
    "no error"
  }, error = function(e) conditionMessage(e))
  stopifnot(grepl("sigma_policy_default", refused),
            grepl("No Java fallback", refused))
  cat("PASS: restricted native X-13 fit through the product path without Java\n")
}
# Automatic native fit (item M4): the qualified automatic-spec X-13 fit for
# the pinned rsa3 decode table, gated by capabilities()$native_x13_fit_
# automatic, runs through the product path WITHOUT Java; the remaining rsa
# templates refuse naming the measured template facts. Check both the template
# default horizon (one year of forecasts, the JVM rsa3 template's x11.nfcasts
# = -1; M12 alignment 2026-11-27) and an explicit forecast window.
if (identical(rkalos::capabilities()$native_x13_fit_automatic, TRUE)) {
  stopifnot(identical(rkalos::capabilities()$automatic_fit$x11$sigma_policy_default,
                      "legacy"))
  obj <- perX13(AirPassengers, "rsa3", backend = "native")
  obj$run()
  saved <- obj$result
  stopifnot(!obj$is_stale, inherits(try(obj$updateParams(spec = list(bad = TRUE)),
                                       silent = TRUE), "try-error"),
            identical(obj$result, saved), !obj$is_stale)
  obj$getRegressors()
  obj$getOutliers()
  summary(obj)
  print(obj)
  stopifnot(identical(obj$result$status$code, "success"),
            identical(obj$result$backend, "native"),
            inherits(obj$result, "persephone_native_automatic_fit"),
            identical(obj$result$provenance$decode_template, "rsa3"),
            identical(obj$result$provenance$sigma_provenance, "legacy (default)"),
            stats::frequency(obj$adjusted) == 12L,
            # Template-default horizon: twelve forecast points on the dated
            # A1a domain, plus the original-series forecast standard errors
            # (M13 P5, rkalos 0.1.0.9033 / core 69f83be): the envelope pair
            # is consumed by name and reported available; the other
            # components keep their honest unavailable arms.
            inherits(obj$forecasts, "persephone_forecasts"),
            identical(obj$forecasts$points$y, obj$output$preadjustment$tables$a1a),
            length(obj$forecasts$points$y) == 12L,
            identical(as.numeric(start(obj$forecasts$points$y)), c(1961, 1)),
            identical(obj$output$series_forecast_stdev_status, "available"),
            identical(obj$forecasts$standard_errors$y, obj$output$series_forecast_stdev),
            length(obj$forecasts$standard_errors$y) == 12L,
            identical(as.numeric(start(obj$forecasts$standard_errors$y)), c(1961, 1)),
            all(obj$forecasts$availability$standard_errors[
                  obj$forecasts$availability$component != "y"] == "unavailable"),
            # Compare the 17-digit round-trip strings directly. Parsing these
            # decimals as R literals can round to a neighbour on macOS ARM64.
            identical(sprintf("%.17g", as.numeric(obj$adjusted)[142:144]),
                      c("496.77291366958877", "487.57552858287698", "488.93033644682504")))
  refused <- tryCatch({
    perX13(AirPassengers, "rsa4", backend = "native")
    "no error"
  }, error = function(e) conditionMessage(e))
  stopifnot(grepl("automatic_fit.deferred", refused),
            grepl("No Java fallback", refused))
  obj$updateParams(forecast = 6L)
  obj$run()
  stopifnot(identical(obj$forecasts$points$y, obj$output$preadjustment$tables$a1a),
            length(obj$forecasts$points$y) == 6L,
            identical(as.numeric(start(obj$forecasts$points$y)), c(1961, 1)))
  cat("PASS: automatic native X-13 fit (rsa3 decode table) through the product path without Java\n")
}
if ("--native" %in% args) {
  y <- ts(exp(c(0, 1, 2, 3)), start=c(2000, 1), frequency=12)
  value <- rkalos::fixed_likelihood(y, d=1, transformation="log", estimation_start=c(2000, 2))
  stopifnot(isTRUE(all.equal(value$likelihood$adjusted_fixed, 2*dnorm(1, log=TRUE)-5)))
}
stopifnot(!any(forbidden %in% loadedNamespaces()))
cat("PASS: required rkalos integration and honest native capability reporting without Java\n")
