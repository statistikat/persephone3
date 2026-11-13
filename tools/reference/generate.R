# Run: Rscript tools/reference/generate.R [--refresh-reference]
# First capture/verify the same inventory, then record direct backend results.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][[1]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling, "inventory.R"))
source(file.path(tooling, "fixtures.R"))

cases <- reference_cases()
index <- list(); all_fields <- list()
for (i in seq_len(nrow(cases))) {
  case <- cases[i, ]
  id <- paste(case$method, case$frequency, case$kind, sep = "-")
  cat("Capturing", id, "\n")
  series <- reference_series(case$frequency, case$kind)
  spec <- reference_spec(case$method, case$kind, series)
  warnings <- character()
  result <- withCallingHandlers(reference_fit(case$method, series, spec, requests),
    warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")})
  if (is.null(result) || is.null(result$user_defined$sa) || length(result$user_defined$sa) != length(series)) stop("Invalid reference fit: ", id)
  fixture <- list(schema_version = 1L, id = id, method = case$method, kind = case$kind,
    input = series, specification = spec, context = NULL, requested = requests,
    raw_result = result, warnings = warnings)
  file <- paste0(id, ".rds")
  saveRDS(fixture, file.path(outdir, file), compress = "xz", version = 2)
  fields <- reference_fields(result)
  fields$fixture <- id
  all_fields[[id]] <- fields
  index[[id]] <- data.frame(id = id, method = case$method, frequency = case$frequency,
    kind = case$kind, observations = length(series), file = file,
    sha256 = reference_sha256(file.path(outdir, file)), stringsAsFactors = FALSE)
}
write.csv(do.call(rbind, index), file.path(outdir, "fixtures.csv"), row.names = FALSE)
fields <- do.call(rbind, all_fields)
write.csv(fields, file.path(outdir, "field-schema.csv"), row.names = FALSE)
numeric_fields <- unique(fields[fields$type %in% c("double", "integer"), c("path", "type")])
# A path can have integer/double types in different fixtures; exact wins.
paths <- unique(numeric_fields$path)
tolerances <- do.call(rbind, lapply(paths, function(path) {
  type <- numeric_fields$type[numeric_fields$path == path]
  tolerance <- reference_tolerance(path, if ("integer" %in% type) "integer" else "double")
  data.frame(path = path, atol = tolerance[["atol"]], rtol = tolerance[["rtol"]],
    status = "provisional_requires_native_calibration_and_domain_review",
    reason = if (all(tolerance == 0)) "discrete_quantity" else if (grepl("estimation", path)) "fixed_spec_estimation" else "series_or_statistic",
    stringsAsFactors = FALSE)
}))
write.csv(tolerances, file.path(outdir, "field-tolerances.csv"), row.names = FALSE)
generation <- list(schema_version = 1L, scripts_sha256 = setNames(
  reference_sha256(file.path(root, "tools/reference", c("fixtures.R", "generate.R", "compare.R", "verify.R"))),
  c("fixtures.R", "generate.R", "compare.R", "verify.R")))
writeLines(sub("[[:blank:]]+$", "", capture.output(dput(generation, control = "all"))),
           file.path(outdir, "generation-manifest.R"))
files <- sort(setdiff(list.files(outdir, full.names = TRUE), file.path(outdir, "SHA256SUMS.csv")))
files <- files[!dir.exists(files)]
write.csv(data.frame(file = basename(files), sha256 = reference_sha256(files)), file.path(outdir, "SHA256SUMS.csv"), row.names = FALSE)
cat("Captured", nrow(cases), "fits;", length(jar_metadata), "JARs;", nrow(options), "specification parameters;", length(requests), "requested output keys.\n")
