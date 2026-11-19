get_series_by_path <- function(hts, path) {
  parts <- strsplit(path, "/")[[1]]

  obj <- hts

  if (parts[1] != "Aggregate") {
    for (p in parts) {
      obj <- obj$components[[p]]
    }
  }

  obj
}

get_all_series <- function(x, prefix = NULL) {
  out <- c()

  current_name <- if (is.null(prefix)) "Aggregate" else prefix

  # Only add "total aggregate" if the object has an output (i.e., is run)
  # For multipleTimeSeries without aggregate run, skip the root
  has_output <- !is.null(x$output)

  if (is.null(prefix) && !has_output) {
    # Skip root for objects without output (e.g., multipleTimeSeries)
    # but still process components
  } else {
    out <- c(out, current_name)
  }

  if (!is.null(x$components)) {
    for (n in names(x$components)) {
      child_name <- if (is.null(prefix)) n else paste0(prefix, "/", n)
      out <- c(out, get_all_series(x$components[[n]], child_name))
    }
  }

  out
}

get_quality <- function(hts, path) {
  obj <- get_series_by_path(hts, path)

  # Try multiple possible locations for quality
  quality <- NULL

  # Try user_defined first
  if (!is.null(obj$output) && !is.null(obj$output$user_defined)) {
    quality <- obj$output$user_defined$quality.summary
  }

  # Fallback to other locations
  if (is.null(quality) && !is.null(obj$diagnostics)) {
    quality <- obj$diagnostics$quality
  }

  # Return default if still null
  if (is.null(quality)) {
    quality <- "ok" # default quality
  }

  quality
}

# one-row diagnostics table of the node at path (NULL if the node was not
# run or the table cannot be created); generate_diagnostics_table() is an
# internal function of the package, so it is looked up in the namespace
get_diagnostics <- function(hts, path) {
  obj <- get_series_by_path(hts, path)
  if (is.null(obj$output)) {
    return(NULL)
  }
  gen <- tryCatch(
    get("generate_diagnostics_table", envir = asNamespace("persephone3")),
    error = function(e) NULL
  )
  if (is.null(gen)) {
    return(NULL)
  }
  tryCatch(gen(obj), error = function(e) NULL)
}
