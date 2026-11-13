# Rscript tools/reference/generate-extended.R [--regenerate]
# Regeneration is restricted to the original pinned oracle; no refresh switch.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][[1]])
tooling <- dirname(normalizePath(script))
source(file.path(tooling, "common.R"))
source(file.path(tooling, "fixtures.R"))
source(file.path(tooling, "extended-fixtures.R"))
flags <- commandArgs(trailingOnly = TRUE)
if (length(setdiff(flags, "--regenerate"))) stop("Only --regenerate is supported; the parent oracle cannot be refreshed here")
extended_generate(reference_root(), regenerate = "--regenerate" %in% flags)
