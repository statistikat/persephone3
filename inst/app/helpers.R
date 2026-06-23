get_series_by_path <- function(hts, path) {

  parts <- strsplit(path, "/")[[1]]

  obj <- hts

  if (parts[1] != "total aggregate") {
    for (p in parts) {
      obj <- obj$components[[p]]
    }
  }

  obj
}

get_all_series <- function(x, prefix = NULL) {
  out <- c()

  #current_name <- if (is.null(prefix)) "aggregate" else prefix

  current_name <- if (is.null(prefix)) "total aggregate" else prefix

  out <- c(out, current_name)

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

  # HIER musst du anpassen!
  # je nachdem wie dein Objekt aufgebaut ist

  obj$output$user_defined$quality.summary   # oder obj$diagnostics$quality etc.
}
