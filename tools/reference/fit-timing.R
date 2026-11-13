# Matched API timing for the prospective ARMA matrix. No performance threshold
# is an acceptance rule, and no frozen numerical reference artifact is written.
fit_timing_options <- function(args) {
  result <- list(source = FALSE, enabled = TRUE, repeats = 3L,
    min_seconds = .05, output = NULL)
  seen <- character()
  for (arg in args) {
    key <- sub("=.*$", "", arg)
    if (key %in% seen) stop("Duplicate option: ", key)
    seen <- c(seen, key)
    if (identical(arg, "--source")) result$source <- TRUE
    else if (identical(arg, "--no-timing")) result$enabled <- FALSE
    else if (key == "--timing-repeats" && grepl("^--timing-repeats=[0-9]+$", arg)) {
      value <- as.numeric(sub("^[^=]+=", "", arg))
      if (!is.finite(value) || value < 2 || value > 20) stop("Timing repeats must be 2..20")
      result$repeats <- as.integer(value)
    } else if (key == "--timing-min-seconds" && grepl("=", arg, fixed = TRUE)) {
      value <- suppressWarnings(as.numeric(sub("^[^=]+=", "", arg)))
      if (!is.finite(value) || value < .01 || value > 1) stop("Timing batch duration must be 0.01..1 seconds")
      result$min_seconds <- value
    } else if (key == "--timing-output" && grepl("=.", arg)) result$output <- sub("^[^=]+=", "", arg)
    else stop("Usage: verify-arma-fits.R [--source] [--no-timing] [--timing-repeats=3] ",
              "[--timing-min-seconds=0.05] [--timing-output=directory]")
  }
  if (!result$enabled && any(c("--timing-repeats", "--timing-min-seconds", "--timing-output") %in% seen)) {
    stop("Timing controls cannot be combined with --no-timing")
  }
  result
}

fit_timing_once <- function(fun, iterations = 1L) {
  start <- proc.time()[["elapsed"]]
  for (i in seq_len(iterations)) value <- fun()
  elapsed <- proc.time()[["elapsed"]] - start
  if (!is.finite(elapsed) || elapsed < 0) stop("Invalid elapsed-time measurement")
  list(value = value, seconds = elapsed)
}

fit_timing_job <- function(case, engine, prepared, prepare, execute, objective, expected) {
  # Force every loop-local value before closures escape the current iteration.
  force(case); force(engine); force(prepared); force(prepare); force(execute); force(objective); force(expected)
  stopifnot(length(expected) == 1L, is.finite(expected))
  list(id = case$id, frequency = case$period, model = case$kind, engine = engine,
    fit_only = function() execute(prepared), with_setup = function() execute(prepare()),
    validate = function(value) {
      actual <- objective(value)
      if (length(actual) != 1L || !is.finite(actual) ||
          abs(actual - expected) > 2e-8 * (1 + abs(expected))) {
        stop("Timed fit failed qualification: ", case$id, " / ", engine)
      }
    })
}

fit_timing_summary <- function(raw, by = character()) {
  keys <- c("scope", by, "engine")
  rounds <- aggregate(raw$seconds_per_fit, raw[c(keys, "round")], sum)
  names(rounds)[ncol(rounds)] <- "total_seconds"
  groups <- split(seq_len(nrow(rounds)), interaction(rounds[keys], drop = TRUE, lex.order = TRUE))
  summary <- do.call(rbind, lapply(groups, function(index) {
    rows <- rounds[index, ]; times <- rows$total_seconds
    selected <- rep(TRUE, nrow(raw))
    for (key in keys) selected <- selected & raw[[key]] == rows[[key]][1L]
    cases <- length(unique(raw$id[selected]))
    cbind(rows[1L, keys, drop = FALSE], cases = cases,
      median_total_seconds = median(times), min_total_seconds = min(times), max_total_seconds = max(times),
      mean_ms_per_fit = 1000 * median(times) / cases)
  }))
  rownames(summary) <- NULL
  summary$native_over_engine <- vapply(seq_len(nrow(summary)), function(i) {
    same <- summary$engine == "native"
    for (key in c("scope", by)) same <- same & summary[[key]] == summary[[key]][i]
    stopifnot(sum(same) == 1L)
    numerator <- summary$median_total_seconds[same]; denominator <- summary$median_total_seconds[i]
    if (denominator > 0) numerator / denominator else NA_real_
  }, numeric(1))
  summary
}

fit_timing_run <- function(jobs, config, metadata) {
  stopifnot(length(jobs) > 0L)
  ids <- unique(vapply(jobs, function(job) job$id, character(1)))
  engines <- unique(vapply(jobs, function(job) job$engine, character(1)))
  stopifnot(identical(sort(engines), sort(c("native", "jd_x13", "jd_tramo"))))
  sizes <- matrix(0L, nrow = length(jobs), ncol = 2L,
                  dimnames = list(NULL, c("fit_only", "with_setup")))
  warmup <- 0
  # Every case/engine/scope receives an untimed-for-report warm-up. Calibration
  # uses its elapsed duration to batch short calls above the clock resolution.
  for (j in seq_along(jobs)) for (scope in colnames(sizes)) {
    invisible(gc())
    sample <- fit_timing_once(jobs[[j]][[scope]])
    jobs[[j]]$validate(sample$value)
    warmup <- warmup + sample$seconds
    sizes[j, scope] <- max(1L, min(1000L, ceiling(config$min_seconds / max(sample$seconds, .001))))
  }
  rows <- list(); count <- 0L; calibration_seconds <- 0
  for (round in seq_len(config$repeats)) {
    scopes <- if (round %% 2L) colnames(sizes) else rev(colnames(sizes))
    for (i in seq_along(ids)) {
      index <- which(vapply(jobs, function(job) job$id == ids[i], logical(1)))
      stopifnot(length(index) == length(engines))
      # Rotate the first engine across cases/rounds to reduce ordering effects.
      index <- index[1L + (seq_along(index) - 1L + i + round - 2L) %% length(index)]
      for (scope in scopes) for (j in index) {
        job <- jobs[[j]]; iterations <- sizes[j, scope]
        invisible(gc()) # Outside the timed region; natural within-batch GC remains included.
        sample <- fit_timing_once(job[[scope]], iterations)
        job$validate(sample$value)
        # Recalibrate if JIT/runtime changes shorten the batch. Never invent an
        # epsilon duration or report an infinite apparent speed advantage.
        while (sample$seconds < config$min_seconds && iterations < 1000L) {
          calibration_seconds <- calibration_seconds + sample$seconds
          iterations <- min(1000L, max(iterations + 1L,
            ceiling(iterations * config$min_seconds / max(sample$seconds, .001))))
          invisible(gc())
          sample <- fit_timing_once(job[[scope]], iterations)
          job$validate(sample$value)
        }
        if (sample$seconds == 0) stop("Timing remains below clock resolution: ", job$id)
        sizes[j, scope] <- iterations
        count <- count + 1L
        rows[[count]] <- data.frame(id = job$id, frequency = job$frequency, model = job$model,
          engine = job$engine, scope = scope, round = round, iterations = iterations,
          elapsed_seconds = sample$seconds, seconds_per_fit = sample$seconds / iterations,
          batch_target_met = sample$seconds >= config$min_seconds)
      }
    }
    cat("Timing round", round, "of", config$repeats, "complete\n")
  }
  raw <- do.call(rbind, rows)
  stopifnot(nrow(raw) == length(jobs) * 2L * config$repeats)
  summary <- fit_timing_summary(raw)
  by_model <- fit_timing_summary(raw, c("frequency", "model"))
  metadata$case_count <- length(ids); metadata$repeats <- config$repeats
  metadata$min_batch_seconds_target <- config$min_seconds
  metadata$warmup_call_seconds <- warmup
  metadata$calibration_retry_seconds <- calibration_seconds
  metadata$measured_call_seconds <- sum(raw$elapsed_seconds)
  metadata$clock <- "proc.time elapsed seconds; batched averages"
  metadata$gc <- "R gc before each batch outside timing; within-batch R/JVM GC included; no forced JVM GC"
  metadata$ratio <- "native / engine: values above one mean native takes longer"
  cat("\nMatched API timings: median sum of per-fit times over", length(ids), "cases (seconds)\n")
  print(summary, row.names = FALSE, digits = 5)
  cat("\nStartup/loading and warm-up are excluded from these totals:\n")
  print(metadata[c("native_load_seconds", "reference_load_seconds", "reference_pin_seconds", "warmup_call_seconds", "calibration_retry_seconds")])
  if (!is.null(config$output)) {
    output <- normalizePath(config$output, mustWork = TRUE)
    files <- c("raw.csv", "summary.csv", "by-model.csv", "metadata.R")
    if (any(file.exists(file.path(output, files)))) stop("Timing output already exists; choose an empty directory")
    write.csv(raw, file.path(output, "raw.csv"), row.names = FALSE)
    write.csv(summary, file.path(output, "summary.csv"), row.names = FALSE)
    write.csv(by_model, file.path(output, "by-model.csv"), row.names = FALSE)
    dput(metadata, file = file.path(output, "metadata.R"))
    cat("Timing details saved to", output, "\n")
  }
  invisible(list(raw = raw, summary = summary, by_model = by_model, metadata = metadata))
}
