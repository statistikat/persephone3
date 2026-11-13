# Fast default: integrity and comparator tests, without loading Java or rjd3.
# Optional: Rscript tools/reference/verify.R --live
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][[1]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling, "common.R"))
source(file.path(tooling, "fixtures.R"))
source(file.path(tooling, "compare.R"))
root <- reference_root()
fixture_dir <- reference_fixture_dir(root)
checksums <- read.csv(file.path(fixture_dir, "SHA256SUMS.csv"), stringsAsFactors = FALSE)
actual_checksums <- reference_sha256(file.path(fixture_dir, checksums$file))
if (!identical(actual_checksums, checksums$sha256)) stop("Reference artifact checksum mismatch: ", paste(checksums$file[actual_checksums != checksums$sha256], collapse = ", "))
index <- read.csv(file.path(fixture_dir, "fixtures.csv"), stringsAsFactors = FALSE)
tolerances <- read.csv(file.path(fixture_dir, "field-tolerances.csv"), stringsAsFactors = FALSE)
manifest <- dget(file.path(fixture_dir, "oracle-manifest.R"))
fixtures <- setNames(lapply(file.path(fixture_dir, index$file), readRDS), index$id)
stopifnot(nrow(index) == 12L, all(c(4L, 12L) %in% index$frequency),
          all(c("x13", "tramoseats") %in% index$method))
for (fixture in fixtures) {
  stopifnot(identical(fixture$requested, reference_requests(root)),
            length(fixture$raw_result$user_defined$sa) == length(fixture$input))
  issues <- compare_reference(fixture$raw_result, fixture$raw_result, tolerances)
  if (nrow(issues)) {print(issues); stop("Reference self-comparison failed: ", fixture$id)}
}

# Assert the comparator rejects representative numerical/contract corruption.
reference <- fixtures[["x13-12-fixed"]]$raw_result
candidate <- reference
candidate$user_defined$sa[20] <- candidate$user_defined$sa[20] + 0.25
issues <- compare_reference(candidate, reference, tolerances)
stopifnot(any(issues$path == "/user_defined/sa" & issues$kind == "numeric" & issues$index == 20L),
          identical(issues$time[issues$path == "/user_defined/sa"], as.numeric(time(reference$user_defined$sa)[20])))
candidate <- reference
candidate$user_defined$sa[2] <- NA_real_
stopifnot(any(compare_reference(candidate, reference, tolerances)$kind == "nonfinite_state"))
candidate <- reference
attr(candidate$user_defined$sa, "tsp") <- attr(reference$user_defined$sa, "tsp") + c(1, 1, 0)
stopifnot(any(compare_reference(candidate, reference, tolerances)$kind == "attributes"))
candidate <- reference
candidate$user_defined$quality.summary <- "Corrupted"
stopifnot(any(compare_reference(candidate, reference, tolerances)$kind == "exact"))
stopifnot(any(compare_reference(reference, reference,
  tolerances[tolerances$path != "/user_defined/sa", ])$kind == "missing_tolerance"))

if ("--live" %in% commandArgs(trailingOnly = TRUE)) {
  if (!identical(R.version.string, manifest$r_version)) stop("Reference R version differs: ", R.version.string)
  for (pkg in names(manifest$packages)) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing oracle package: ", pkg)
    if (!identical(packageDescription(pkg)$Version, manifest$packages[[pkg]]$Version)) stop("Reference package version mismatch: ", pkg)
  }
  for (jar in manifest$jars) {
    file <- file.path(find.package(jar$package), jar$package_relative_path)
    if (!file.exists(file) || !identical(reference_sha256(file), jar$sha256)) stop("Reference JAR mismatch: ", jar$package_relative_path)
  }
  current_java <- rJava::.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
  if (!identical(current_java, manifest$java$runtime_version)) stop("Reference JVM differs: ", current_java)
  for (fixture in fixtures) {
    actual <- reference_fit(fixture$method, fixture$input, fixture$specification, fixture$requested, fixture$context)
    issues <- compare_reference(actual, fixture$raw_result, tolerances)
    if (nrow(issues)) {print(issues); stop("Live oracle comparison failed: ", fixture$id)}
    cat("PASS", fixture$id, "\n")
  }
} else {
  stopifnot(!any(c("rJava", "rjd3toolkit", "rjd3x13", "rjd3tramoseats") %in% loadedNamespaces()))
}
cat("PASS: reference checksums, 12 corpus contracts, comparator corruption checks",
    if ("--live" %in% commandArgs(trailingOnly = TRUE)) "and pinned live replay" else "without loading Java/rjd3", "\n")
