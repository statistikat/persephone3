# Run tinytest unit tests for persephone3
# This file is executed by R CMD check to run all tests in inst/tinytest/

# Run all tests from inst/tinytest/
# Skip if tinytest package is not available
if (requireNamespace("tinytest", quietly = TRUE)) {
  tinytest::test_package("persephone3")
}
