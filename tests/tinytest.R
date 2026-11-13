# Run tinytest unit tests for persephone3
# This file is executed by R CMD check to run all tests in inst/tinytest/

# The full CI lane must exercise the reference backend; a missing optional
# package is a configuration failure there, not a successful skipped suite.
if (identical(Sys.getenv("PERSEPHONE_REQUIRE_ORACLE"), "true")) {
  required <- c("tinytest", "rjd3toolkit", "rjd3x13", "rjd3tramoseats")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Full validation requires: ", paste(missing, collapse = ", "))
}

# Ordinary optional-package checks may omit tinytest.
if (requireNamespace("tinytest", quietly = TRUE)) {
  tinytest::test_package("persephone3")
}
