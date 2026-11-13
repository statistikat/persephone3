# Reference tooling is deliberately independent of the package/adapter namespace.
reference_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    script <- normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
    root <- normalizePath(file.path(dirname(script), "../.."), mustWork = TRUE)
  } else root <- normalizePath(".", mustWork = TRUE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) stop("Run from the repository or via Rscript tools/reference/<script>.R")
  root
}

reference_fixture_dir <- function(root = reference_root()) {
  file.path(root, "inst/tinytest/fixtures/backend")
}

reference_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("Reference generation/verification requires the developer dependency 'digest'.")
  vapply(path, function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE), character(1), USE.NAMES = FALSE)
}

reference_requests <- function(root) {
  expressions <- parse(file.path(root, "R/persephone.R"))
  is_assignment <- vapply(expressions, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("userdefined_default"))
  }, logical(1))
  if (sum(is_assignment) != 1L) stop("Cannot identify userdefined_default in package source")
  eval(expressions[[which(is_assignment)]][[3]], envir = baseenv())
}

