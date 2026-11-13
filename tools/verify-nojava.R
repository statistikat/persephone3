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
if ("--native" %in% args) {
  convolve <- getFromNamespace(".native_convolve", "persephone3")
  difference <- getFromNamespace(".native_difference_polynomial", "persephone3")
  stopifnot(identical(convolve(c(1, -1), c(1, 1)), c(1, 0, -1)))
  stopifnot(identical(difference(1, 0, 12), c(1, -1)))
  solve_spd <- getFromNamespace(".native_cholesky_solve", "persephone3")
  solve_qr <- getFromNamespace(".native_qr_solve", "persephone3")
  stopifnot(isTRUE(all.equal(solve_spd(diag(2), matrix(c(2, 3), 2))$solution,
                             matrix(c(2, 3), 2))))
  stopifnot(solve_qr(diag(2), matrix(c(2, 3), 2))$rank == 2L)
}
stopifnot(!any(forbidden %in% loadedNamespaces()))
cat("PASS: installed package loads, capabilities are honest and no Java fallback is attempted",
    if ("--native" %in% args) "; compiled primitives run" else "", "\n", sep="")
