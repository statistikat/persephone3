# Additive suite: never source inventory.R or rewrite the original corpus.
extended_cases <- function() {
  cases <- expand.grid(method = c("x13", "tramoseats"),
                       kind = c("mixed-ls-tc", "missing-internal", "external-context", "external-future-shift"),
                       stringsAsFactors = FALSE)
  cases$frequency <- ifelse(cases$kind == "missing-internal", 4L, 12L)
  cases$id <- paste(cases$method, cases$frequency, cases$kind, sep = "-")
  cases
}

# All inputs are independent deterministic formulas. No fitted output feeds an
# input, expected coefficient state, missing mask or future-context identity.
extended_data <- function(kind) {
  if (kind == "mixed-ls-tc") {
    y <- reference_series(12L)
    # 2006-01 is observation 59; 2010-01 is observation 107.
    y[59:180] <- y[59:180] + 20
    y[107:180] <- y[107:180] + 30 * 0.7^(0:73)
    return(list(input = y, variables = NULL))
  }
  if (kind == "missing-internal") {
    y <- reference_series(4L)
    y[c(20L, 21L, 44L)] <- NA_real_
    return(list(input = y, variables = NULL))
  }
  if (!kind %in% c("external-context", "external-future-shift")) stop("Unknown extended fixture kind")
  # One year of backcast coverage and two years of future coverage. The direct
  # TRAMO y_f result is two years even when other components have shorter spans.
  k <- seq.int(-11L, 204L)
  fixed <- ts(sin(2 * pi * k / 7) + 0.2 * cos(k * sqrt(3)), start = c(2000, 3), frequency = 12)
  free <- ts(cos(k / 9) + sin(k * sqrt(2)) / 4, start = c(2000, 3), frequency = 12)
  y <- reference_series(12L) +
    3 * window(fixed, start = c(2001, 3), end = c(2016, 2)) +
    4.5 * window(free, start = c(2001, 3), end = c(2016, 2))
  if (kind == "external-future-shift") {
    future <- round(12 * as.numeric(time(fixed))) > round(12 * tail(as.numeric(time(y)), 1))
    fixed[future] <- fixed[future] + 2
  }
  list(input = y, variables = list(r = list(fixed = fixed, free = free)))
}

extended_spec <- function(method, kind, series) {
  spec <- reference_spec(method, "fixed", series)
  if (kind == "mixed-ls-tc") {
    spec <- rjd3toolkit::add_outlier(spec, type = "LS", date = "2006-01-01", coef = 20)
    spec <- rjd3toolkit::add_outlier(spec, type = "TC", date = "2010-01-01")
  }
  if (kind %in% c("external-context", "external-future-shift")) {
    spec <- rjd3toolkit::add_usrdefvar(spec, group = "r", name = "fixed", coef = 3, regeffect = "Trend")
    spec <- rjd3toolkit::add_usrdefvar(spec, group = "r", name = "free", regeffect = "Trend")
  }
  spec
}

extended_parent_hashes <- function(base_dir) {
  declared <- read.csv(file.path(base_dir, "SHA256SUMS.csv"), stringsAsFactors = FALSE)
  actual <- reference_sha256(file.path(base_dir, declared$file))
  if (!identical(actual, declared$sha256)) stop("Original reference artifacts changed; repair/review the parent corpus first")
  rbind(declared, data.frame(file = "SHA256SUMS.csv", sha256 = reference_sha256(file.path(base_dir, "SHA256SUMS.csv"))))
}

extended_assert_oracle <- function(base_dir) {
  manifest <- dget(file.path(base_dir, "oracle-manifest.R"))
  if (!identical(R.version.string, manifest$r_version)) stop("Extended generation requires the pinned R version")
  for (pkg in names(manifest$packages)) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing pinned reference package: ", pkg)
    if (!identical(packageDescription(pkg)$Version, manifest$packages[[pkg]]$Version)) stop("Pinned package version differs: ", pkg)
  }
  for (jar in manifest$jars) {
    path <- file.path(find.package(jar$package), jar$package_relative_path)
    if (!file.exists(path) || !identical(reference_sha256(path), jar$sha256)) stop("Pinned JAR differs: ", jar$package_relative_path)
  }
  properties <- c("java.version", "java.vendor", "java.runtime.version", "java.vm.name", "os.arch")
  current <- setNames(lapply(properties, function(key) rJava::.jcall("java/lang/System", "S", "getProperty", key)), names(manifest$java))
  if (!identical(current, manifest$java)) stop("Extended generation requires the pinned JVM/vendor/architecture")
  invisible(manifest)
}

extended_assert_invariants <- function(fixtures) {
  checks <- 0L
  assert <- function(value, label) {
    if (!isTRUE(value)) stop("Extended invariant failed: ", label, call. = FALSE)
    checks <<- checks + 1L
  }
  # This is a roundoff budget for exact algebraic identities, not a native
  # statistical acceptance tolerance. Estimation values are never judged here.
  roundoff <- function(scale) 64 * .Machine$double.eps * max(1, abs(scale))
  for (fixture in fixtures) {
    id <- fixture$id
    raw <- fixture$raw_result
    input <- fixture$input
    assert(identical(input, extended_data(fixture$kind)$input), paste(id, "input formula/missing mask"))
    assert(identical(tsp(raw$user_defined$sa), tsp(input)), paste(id, "SA domain"))
    assert(all(is.finite(raw$user_defined$sa)), paste(id, "finite SA"))
    observed <- !is.na(input)
    assert(identical(as.numeric(raw$user_defined$y[observed]), as.numeric(input[observed])), paste(id, "observed original values"))
    assert(identical(raw$user_defined$mode, "Additive"), paste(id, "additive mode"))
    assert(max(abs((raw$user_defined$y - raw$user_defined$sa - raw$user_defined$s)[observed])) <= roundoff(input[observed]), paste(id, "observed y = sa + s"))
    variables <- raw$preprocessing$description$variables
    estimated <- sum(vapply(variables, function(variable) sum(variable$coef$type == "ESTIMATED"), integer(1)))
    covariance <- raw$preprocessing$estimation$bvar
    assert(if (estimated) identical(dim(covariance), c(estimated, estimated)) else is.null(covariance) || !length(covariance), paste(id, "estimated-only covariance dimensions"))
    variable <- function(type = NULL, name = NULL) {
      selected <- Filter(function(v) if (!is.null(type)) identical(v$type, type) else identical(v$name, name), variables)
      if (length(selected) != 1L) stop("Expected exactly one variable in ", id)
      selected[[1]]
    }
    if (fixture$kind == "mixed-ls-tc") {
      ls <- variable(type = "LS")
      tc <- variable(type = "TC")
      assert(identical(ls$coef$type, "FIXED") && identical(ls$coef$value, 20), paste(id, "fixed LS"))
      assert(identical(tc$coef$type, "ESTIMATED") && is.finite(tc$coef$value), paste(id, "free TC"))
    }
    if (fixture$kind == "missing-internal") {
      assert(identical(which(is.na(input)), c(20L, 21L, 44L)), paste(id, "missing positions"))
      # X-13 returns interpolated y; TRAMO preserves the original NA mask even
      # though both engines provide finite SA. Preserve that method distinction.
      assert(if (fixture$method == "x13") all(is.finite(raw$user_defined$y)) else
        identical(is.na(raw$user_defined$y), is.na(input)), paste(id, "method-specific original missing mask"))
    }
    if (fixture$kind %in% c("external-context", "external-future-shift")) {
      fixed <- variable(name = "r.fixed")
      free <- variable(name = "r.free")
      assert(identical(fixed$coef$type, "FIXED") && identical(fixed$coef$value, 3), paste(id, "fixed user coefficient"))
      assert(identical(free$coef$type, "ESTIMATED") && is.finite(free$coef$value), paste(id, "estimated user coefficient"))
      context <- fixture$context$variables$r
      assert(identical(context, extended_data(fixture$kind)$variables$r), paste(id, "complete context formula"))
      periods <- round(12 * time(context$fixed))
      forecast_periods <- round(12 * time(raw$user_defined$y_f))
      assert(all(forecast_periods %in% periods), paste(id, "future regressor coverage"))
    }
  }
  for (method in c("x13", "tramoseats")) {
    base <- fixtures[[paste(method, 12, "external-context", sep = "-")]]
    shifted <- fixtures[[paste(method, 12, "external-future-shift", sep = "-")]]
    assert(identical(base$input, shifted$input), paste(method, "paired observed inputs"))
    assert(identical(base$specification, shifted$specification), paste(method, "paired specifications"))
    assert(identical(base$raw_result$preprocessing, shifted$raw_result$preprocessing), paste(method, "unchanged fitted preprocessing"))
    before <- base$raw_result$user_defined$y_f
    after <- shifted$raw_result$user_defined$y_f
    assert(identical(tsp(before), tsp(after)), paste(method, "paired forecast domains"))
    assert(max(abs(after - before - 6)) <= roundoff(c(before, after)), paste(method, "fixed future effect 3 * 2 = 6"))
  }
  invisible(checks)
}

extended_generate <- function(root, regenerate = FALSE) {
  base_dir <- reference_fixture_dir(root)
  parent_before <- extended_parent_hashes(base_dir)
  destination <- file.path(base_dir, "extended-v1")
  if (dir.exists(destination) && !regenerate) stop("extended-v1 already exists. Verify it, or use --regenerate to recompute with the same pinned oracle.")
  oracle <- extended_assert_oracle(base_dir)
  staging <- tempfile(".extended-v1-", tmpdir = base_dir)
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  requests <- reference_requests(root)
  cases <- extended_cases()
  fixtures <- fields <- list()
  for (i in seq_len(nrow(cases))) {
    case <- cases[i, ]
    data <- extended_data(case$kind)
    spec <- extended_spec(case$method, case$kind, data$input)
    context <- if (is.null(data$variables)) NULL else rjd3toolkit::modelling_context(variables = data$variables)
    warnings <- character()
    raw <- withCallingHandlers(reference_fit(case$method, data$input, spec, requests, context),
      warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")})
    if (is.null(raw)) stop("Reference failed: ", case$id)
    fixture <- list(schema_version = 1L, suite = "extended-v1", id = case$id,
                    method = case$method, kind = case$kind, input = data$input,
                    specification = spec, context = context, requested = requests,
                    raw_result = raw, warnings = warnings)
    fixtures[[case$id]] <- fixture
    saveRDS(fixture, file.path(staging, paste0(case$id, ".rds")), compress = "xz", version = 2)
    field <- reference_fields(raw)
    field$fixture <- case$id
    fields[[case$id]] <- field
    cat("Captured", case$id, "\n")
  }
  checks <- extended_assert_invariants(fixtures)
  cases$file <- paste0(cases$id, ".rds")
  cases$sha256 <- reference_sha256(file.path(staging, cases$file))
  write.csv(cases, file.path(staging, "fixtures.csv"), row.names = FALSE)
  fields <- do.call(rbind, fields)
  write.csv(fields, file.path(staging, "field-schema.csv"), row.names = FALSE)
  inherited <- read.csv(file.path(base_dir, "field-tolerances.csv"), stringsAsFactors = FALSE)
  paths <- sort(unique(fields$path[fields$type %in% c("double", "integer")]))
  coverage <- data.frame(path = paths, status = ifelse(paths %in% inherited$path,
    "existing_provisional_rule_only", "unreviewed_no_native_acceptance_rule"))
  write.csv(coverage, file.path(staging, "tolerance-coverage.csv"), row.names = FALSE)
  scripts <- c("common.R", "fixtures.R", "compare.R", "extended-fixtures.R", "generate-extended.R", "verify-extended.R")
  manifest <- list(schema_version = 1L, suite = "extended-v1", generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    parent_artifacts = parent_before, oracle_manifest_sha256 = reference_sha256(file.path(base_dir, "oracle-manifest.R")),
    r_version = oracle$r_version, java = oracle$java,
    scripts_sha256 = setNames(reference_sha256(file.path(root, "tools/reference", scripts)), scripts),
    live_comparison = "Exact identical raw output under the pinned oracle; no new native numerical tolerances",
    input_provenance = "Independent deterministic formulas in extended-fixtures.R; no external dataset or Java implementation copied",
    invariant_checks = checks)
  writeLines(sub("[[:blank:]]+$", "", capture.output(dput(manifest, control = "all"))),
             file.path(staging, "manifest.R"))
  files <- sort(list.files(staging, full.names = TRUE))
  write.csv(data.frame(file = basename(files), sha256 = reference_sha256(files)), file.path(staging, "SHA256SUMS.csv"), row.names = FALSE)
  if (!identical(parent_before, extended_parent_hashes(base_dir))) stop("Parent artifacts changed during generation")
  backup <- NULL
  if (dir.exists(destination)) {
    backup <- tempfile(".extended-v1-backup-", tmpdir = base_dir)
    if (!file.rename(destination, backup)) stop("Could not stage the previous extended suite")
  }
  if (!file.rename(staging, destination)) {
    if (!is.null(backup)) file.rename(backup, destination)
    stop("Could not publish the generated extended suite")
  }
  if (!is.null(backup)) unlink(backup, recursive = TRUE)
  cat("PASS:", nrow(cases), "extended cases;", checks, "independent invariants; original corpus unchanged\n")
}
