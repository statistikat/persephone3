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
options(persephone.backend = "native")
err <- tryCatch(perX13(AirPassengers, "rsa0"), error = identity)
stopifnot(inherits(err, "error"), grepl("native backend does not yet implement", conditionMessage(err)))
# M4-gap (2026-11-21): the automodel-OFF refusals cite BOTH qualified envelopes
# with measured jar facts; the restricted-route part must name the fixed-
# coefficient violations and quote the pinned core statuses verbatim -- and
# those statuses must be what the installed core actually answers right now.
msg0 <- conditionMessage(err)
stopifnot(grepl("envelopes \\(automatic and restricted\\)", msg0),
          grepl("fixed_seasonal_requests_only", msg0, fixed = TRUE),
          grepl("zero_horizon_only", msg0, fixed = TRUE),
          grepl("unsupported:x13 fit requires zero horizons", msg0, fixed = TRUE))
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
# templates refuse naming the measured template facts. Check both the
# zero-horizon default and an explicit forecast window.
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
            is.null(obj$forecasts),
            # Compare the 17-digit round-trip strings directly. Parsing these
            # decimals as R literals can round to a neighbour on macOS ARM64.
            identical(sprintf("%.17g", as.numeric(obj$adjusted)[142:144]),
                      c("499.37673144370382", "484.86271289531953", "485.24840286656132")))
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
