# Rscript tools/reference/verify-extended.R [--live]
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][[1]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling, "common.R"))
source(file.path(tooling, "fixtures.R"))
source(file.path(tooling, "compare.R"))
source(file.path(tooling, "extended-fixtures.R"))
root <- reference_root()
base_dir <- reference_fixture_dir(root)
suite_dir <- file.path(base_dir, "extended-v1")
manifest <- dget(file.path(suite_dir, "manifest.R"))
stopifnot(identical(extended_parent_hashes(base_dir), manifest$parent_artifacts))
checksums <- read.csv(file.path(suite_dir, "SHA256SUMS.csv"), stringsAsFactors = FALSE)
stopifnot(identical(reference_sha256(file.path(suite_dir, checksums$file)), checksums$sha256))
index <- read.csv(file.path(suite_dir, "fixtures.csv"), stringsAsFactors = FALSE)
stopifnot(identical(index$id, extended_cases()$id))
fixtures <- setNames(lapply(file.path(suite_dir, index$file), readRDS), index$id)
stopifnot(all(vapply(fixtures, function(f) identical(f$requested, reference_requests(root)), logical(1))))
checks <- extended_assert_invariants(fixtures)
stopifnot(identical(checks, manifest$invariant_checks))

# Algebraic checks must catch corruption independently of stored checksums.
broken <- fixtures
broken[["x13-12-external-future-shift"]]$raw_result$user_defined$y_f[1] <-
  broken[["x13-12-external-future-shift"]]$raw_result$user_defined$y_f[1] + 1
stopifnot(inherits(tryCatch(extended_assert_invariants(broken), error = identity), "error"))
broken <- fixtures
broken[["x13-12-mixed-ls-tc"]]$raw_result$preprocessing$description$variables[[1]]$coef$value <- 21
stopifnot(inherits(tryCatch(extended_assert_invariants(broken), error = identity), "error"))

if ("--live" %in% commandArgs(trailingOnly = TRUE)) {
  extended_assert_oracle(base_dir)
  tolerances <- read.csv(file.path(base_dir, "field-tolerances.csv"), stringsAsFactors = FALSE)
  for (fixture in fixtures) {
    actual <- reference_fit(fixture$method, fixture$input, fixture$specification, fixture$requested, fixture$context)
    if (!identical(actual, fixture$raw_result)) {
      print(compare_reference(actual, fixture$raw_result, tolerances))
      stop("Pinned exact replay differs: ", fixture$id, ". No tolerance was added or widened.")
    }
    cat("PASS exact replay", fixture$id, "\n")
  }
} else stopifnot(!any(c("rJava", "rjd3toolkit", "rjd3x13", "rjd3tramoseats") %in% loadedNamespaces()))
cat("PASS: extended-v1 checksums,", checks, "independent invariants and corruption checks; parent corpus unchanged\n")
