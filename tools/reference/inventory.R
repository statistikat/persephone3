# Run: Rscript tools/reference/inventory.R
# An existing oracle cannot be replaced with different JARs unless explicitly
# requested with --refresh-reference. Review all changed artifacts before commit.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][[1]])
source(file.path(dirname(normalizePath(script)), "common.R"))
root <- reference_root()
outdir <- reference_fixture_dir(root)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
required <- c("rjd3toolkit", "rjd3x13", "rjd3tramoseats", "rjd3jars", "rJava", "RProtoBuf", "digest")
for (pkg in required) if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing reference-generation dependency: ", pkg)

package_metadata <- lapply(required, function(pkg) {
  d <- packageDescription(pkg)
  fields <- c("Package", "Version", "License", "Built", "Repository", "RemoteType", "RemoteRepo", "RemoteRef", "RemoteSha")
  result <- setNames(lapply(fields, function(k) if (is.null(d[[k]])) NA_character_ else d[[k]]), fields)
  result$installed_path <- find.package(pkg)
  result$description_sha256 <- reference_sha256(file.path(find.package(pkg), "DESCRIPTION"))
  result
})
names(package_metadata) <- required
jar_metadata <- list()
for (pkg in required) {
  pkgdir <- find.package(pkg)
  jars <- list.files(pkgdir, pattern = "[.]jar$", recursive = TRUE, full.names = TRUE)
  for (jar in jars) {
    entries <- unzip(jar, list = TRUE)$Name
    read_entry <- function(entry) {
      con <- unz(jar, entry, open = "r"); on.exit(close(con))
      readLines(con, warn = FALSE)
    }
    properties <- entries[grepl("^META-INF/maven/.*/pom.properties$", entries)]
    key <- paste(pkg, substring(jar, nchar(pkgdir) + 2L), sep = "/")
    jar_metadata[[key]] <- list(package = pkg, package_relative_path = substring(jar, nchar(pkgdir) + 2L),
      installed_path = normalizePath(jar), sha256 = reference_sha256(jar),
      manifest = if ("META-INF/MANIFEST.MF" %in% entries) read_entry("META-INF/MANIFEST.MF") else character(),
      maven_properties = setNames(lapply(properties, read_entry), properties))
  }
}
manifest <- list(schema_version = 1L, generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  source_commit = system2("git", c("-C", shQuote(root), "rev-parse", "HEAD"), stdout = TRUE),
  inspected_upstream_commit = "15f1821fa71a949f842a5891e7cd68c83187f76d",
  inspected_upstream_version = "3.9.1-SNAPSHOT",
  reference_java_source_commit = NA_character_,
  generator_sha256 = setNames(reference_sha256(file.path(root, "tools/reference", c("common.R", "inventory.R"))), c("common.R", "inventory.R")),
  reference_java_source_note = "Installed Maven metadata identifies JD+ 3.9.0. No source SHA was present in the captured wrapper/Maven metadata. JAR checksums below pin the actual oracle; the inspected develop SHA is not its claimed source.",
  r_version = R.version.string, platform = R.version$platform,
  java = setNames(lapply(c("java.version", "java.vendor", "java.runtime.version", "java.vm.name", "os.arch"),
    function(k) rJava::.jcall("java/lang/System", "S", "getProperty", k)),
    c("version", "vendor", "runtime_version", "vm_name", "architecture")),
  java_classpath = rJava::.jclassPath(), packages = package_metadata, jars = jar_metadata,
  input_provenance = "Deterministic synthetic formula in tools/reference/fixtures.R; no third-party dataset or copied Java implementation.")
manifest_path <- file.path(outdir, "oracle-manifest.R")
if (file.exists(manifest_path) && !"--refresh-reference" %in% commandArgs(trailingOnly = TRUE)) {
  old <- dget(manifest_path)
  old_versions <- vapply(old$packages, `[[`, character(1), "Version")
  new_versions <- vapply(manifest$packages, `[[`, character(1), "Version")
  old_jars <- vapply(old$jars, `[[`, character(1), "sha256")
  new_jars <- vapply(manifest$jars, `[[`, character(1), "sha256")
  if (!identical(old_versions, new_versions) || !identical(old_jars, new_jars) ||
      !identical(old$r_version, manifest$r_version) || !identical(old$java, manifest$java)) {
    stop("Oracle packages/JARs/R/JVM differ from the pinned manifest. Use --refresh-reference only for an intentional reviewed baseline update.")
  }
}
manifest_lines <- capture.output(dput(manifest, control = "all"))
writeLines(sub("[[:blank:]]+$", "", manifest_lines), manifest_path)

# This is an inventory of the installed setters, not a promise that every
# parameter is appropriate for both methods or implemented natively.
blocks <- c(arima = "set_arima", automdl = "set_automodel", basic = "set_basic",
  easter = "set_easter", estimate = "set_estimate", setout = "set_outlier",
  td = "set_tradingdays", transform = "set_transform", addout = "add_outlier",
  remout = "remove_outlier", addramp = "add_ramp", remramp = "remove_ramp",
  usrdefvar = "add_usrdefvar", x11 = "set_x11", benchmarking = "set_benchmarking")
options <- do.call(rbind, lapply(names(blocks), function(block) {
  pkg <- if (block == "x11") "rjd3x13" else "rjd3toolkit"
  forms <- formals(getExportedValue(pkg, blocks[[block]]))
  params <- setdiff(names(forms), "x")
  do.call(rbind, lapply(params, function(param) data.frame(block = block, parameter = param,
    public_name = paste(block, param, sep = "."), package = pkg, setter = blocks[[block]],
    default_expression = paste(deparse(forms[[param]]), collapse = " "),
    native_status = "not_implemented", decision = "inventory_requires_method_validation", stringsAsFactors = FALSE)))
}))
write.csv(options, file.path(outdir, "specification-options.csv"), row.names = FALSE)
requests <- reference_requests(root)
write.csv(data.frame(key = requests, source = "R/persephone.R:userdefined_default", native_status = "not_implemented"),
          file.path(outdir, "requested-outputs.csv"), row.names = FALSE)
templates <- list(x13 = c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c"),
                  tramoseats = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"))
expanded <- lapply(names(templates), function(method) {
  constructor <- getExportedValue(if (method == "x13") "rjd3x13" else "rjd3tramoseats", paste0(method, "_spec"))
  setNames(lapply(templates[[method]], constructor), templates[[method]])
})
names(expanded) <- names(templates)
saveRDS(expanded, file.path(outdir, "expanded-templates.rds"), compress = "xz", version = 2)

files <- file.path(outdir, c("oracle-manifest.R", "specification-options.csv", "requested-outputs.csv", "expanded-templates.rds"))
write.csv(data.frame(file = basename(files), sha256 = reference_sha256(files)), file.path(outdir, "inventory-checksums.csv"), row.names = FALSE)
cat("Inventoried", length(jar_metadata), "JARs;", nrow(options), "specification parameters;", length(requests), "output requests.\n")
