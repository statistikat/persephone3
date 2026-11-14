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
err <- tryCatch(perX13(AirPassengers), error = identity)
stopifnot(inherits(err, "error"), grepl("native backend does not yet implement", conditionMessage(err)))
err <- tryCatch(perTramo(AirPassengers, backend = "rjd3"), error = identity)
stopifnot(inherits(err, "error"), grepl("optional package", conditionMessage(err)))
# rkalos is a required dependency; it supplies every compiled primitive.
stopifnot(identical(cap, rkalos::capabilities()),
          grepl("^[0-9a-f]{40}$", rkalos::core_version()$commit))
if ("--native" %in% args) {
  y <- ts(exp(c(0, 1, 2, 3)), start=c(2000, 1), frequency=12)
  value <- rkalos::fixed_likelihood(y, d=1, transformation="log", estimation_start=c(2000, 2))
  stopifnot(isTRUE(all.equal(value$likelihood$adjusted_fixed, 2*dnorm(1, log=TRUE)-5)))
}
stopifnot(!any(forbidden %in% loadedNamespaces()))
cat("PASS: required rkalos integration and honest native capability reporting without Java\n")
