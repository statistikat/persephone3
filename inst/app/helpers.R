# Freeze a session's routing separately from its display labels. Components are
# addressed by integer position, so names never become selectors or path syntax.
dashboard_registry <- function(x) {
  entries <- list()
  visit <- function(node, path = integer(), label = "Aggregate") {
    if (length(path) || !is.null(node$output)) {
      entries[[length(entries) + 1L]] <<- list(
        id = if (length(path)) paste0("node_", paste(path, collapse = "_")) else "node_0",
        label = as.character(label), depth = length(path), path = path
      )
    }
    children <- node$components
    for (i in seq_along(children)) {
      child_label <- names(children)[i]
      if (is.null(child_label) || is.na(child_label) || !nzchar(child_label)) {
        child_label <- paste("Series", i)
      }
      visit(children[[i]], c(path, i), child_label)
    }
  }
  visit(x)
  result <- data.frame(
    id = vapply(entries, `[[`, character(1), "id"),
    label = vapply(entries, `[[`, character(1), "label"),
    depth = vapply(entries, `[[`, integer(1), "depth"),
    stringsAsFactors = FALSE
  )
  result$path <- lapply(entries, `[[`, "path")
  result
}

dashboard_valid_id <- function(id, registry) {
  is.character(id) && length(id) == 1L && !is.na(id) && id %in% registry$id
}

dashboard_valid_selection <- function(ids, registry) {
  is.character(ids) && !anyNA(ids) && !anyDuplicated(ids) &&
    all(ids %in% registry$id)
}

dashboard_series <- function(x, id, registry) {
  if (!dashboard_valid_id(id, registry)) stop("Unknown dashboard series ID")
  path <- registry$path[[match(id, registry$id)]]
  for (i in path) {
    if (is.null(x$components) || i > length(x$components)) {
      stop("The dashboard series tree has changed; reopen the dashboard")
    }
    x <- x$components[[i]]
  }
  x
}

get_quality <- function(obj) {
  quality <- obj$output$user_defined$quality.summary
  if (is.null(quality)) quality <- obj$diagnostics$quality
  if (is.null(quality) || length(quality) != 1L || is.na(quality) ||
      !nzchar(trimws(as.character(quality)))) return("unavailable")
  tolower(trimws(as.character(quality)))
}
