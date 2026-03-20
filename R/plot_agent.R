#' Plot an agent search result
#'
#' For `persephoneAgent` objects, the plot overlays the original series, the
#' final adjusted series, and the adjusted series from earlier successful runs.
#' By default all successful earlier runs are shown in the background with lower
#' opacity.
#'
#' @name plot
#' @param x an object of class [persephoneAgent].
#' @param main plot title.
#' @param previous_alpha opacity used for the previous runs.
#' @param accepted_only if `TRUE`, only previously accepted runs are drawn in
#'   the background.
#' @param ... other plotting parameters to affect the plot. Not currently used.
#'
#' @return Returns a `ggplot` object for `persephoneAgent` results.
#'
#' @examples
#' res <- perAgent(AirPassengers, max_iter = 2)
#' plot(res)
#' @export
plot.persephoneAgent <- function(x,
                                 main = NULL,
                                 previous_alpha = 0.2,
                                 accepted_only = FALSE,
                                 ...) {
  if (!is.numeric(previous_alpha) ||
      length(previous_alpha) != 1L ||
      is.na(previous_alpha) ||
      previous_alpha < 0 ||
      previous_alpha > 1) {
    stop("`previous_alpha` must be a number between 0 and 1.", call. = FALSE)
  }

  previous_label <- if (isTRUE(accepted_only)) {
    "Accepted previous runs"
  } else {
    "Previous runs"
  }
  if (is.null(main)) {
    main <- "Original, final adjusted, and previous agent runs"
  }

  original_df <- per_agent_series_frame(
    x$ts,
    role = "Original",
    group = "original"
  )
  final_df <- per_agent_series_frame(
    x$final_model$adjusted,
    role = "Final adjusted",
    group = "final"
  )

  previous_rows <- per_agent_plot_history_rows(x, accepted_only = accepted_only)
  previous_series <- lapply(
    seq_len(nrow(previous_rows)),
    function(i) {
      row <- previous_rows[i, , drop = FALSE]
      model <- tryCatch(
        per_agent_rebuild_model(
          result = x,
          method = row$method[[1]],
          speclist = row$speclist[[1]]
        ),
        error = function(e) NULL
      )
      if (is.null(model)) {
        return(NULL)
      }
      adjusted <- model$adjusted %||% model$output$user_defined$sa
      if (is.null(adjusted) || !stats::is.ts(adjusted)) {
        return(NULL)
      }
      per_agent_series_frame(
        adjusted,
        role = previous_label,
        group = row$signature[[1]]
      )
    }
  )
  previous_df <- do.call(
    rbind,
    Filter(Negate(is.null), previous_series)
  )
  if (is.null(previous_df)) {
    previous_df <- data.frame(
      index = original_df$index[0],
      value = numeric(0),
      role = character(0),
      group = character(0),
      stringsAsFactors = FALSE
    )
  }

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = previous_df,
      ggplot2::aes(index, value, group = group, color = role),
      linewidth = 0.45,
      alpha = previous_alpha,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = original_df,
      ggplot2::aes(index, value, color = role),
      linewidth = 0.55,
      alpha = 0.85,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = final_df,
      ggplot2::aes(index, value, color = role),
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Original" = "#6c757d",
        "Previous runs" = "#9ecae1",
        "Accepted previous runs" = "#9ecae1",
        "Final adjusted" = "#005f73"
      ),
      breaks = c("Original", previous_label, "Final adjusted")
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Value",
      color = NULL,
      title = main
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}

per_agent_plot_history_rows <- function(result, accepted_only = FALSE) {
  history <- result$history
  history <- history[!nzchar(history$error), , drop = FALSE]
  if (isTRUE(accepted_only)) {
    history <- history[history$accepted, , drop = FALSE]
  }

  final_signature <- tail(history$signature[history$accepted], 1)
  history[history$signature != final_signature, , drop = FALSE]
}

per_agent_rebuild_model <- function(result, method, speclist) {
  builder <- switch(
    method,
    x13 = perX13,
    tramoseats = perTramo,
    stop("Unknown method `", method, "`.", call. = FALSE)
  )
  inputs <- per_agent_prepare_candidate_inputs(
    ts = result$ts,
    speclist = speclist,
    context = result$context %||% NULL,
    userdefined = result$userdefined %||% NULL,
    td_variant_settings = result$td_variant_settings %||%
      per_agent_validate_td_variant_settings(NULL, NULL, NULL)
  )

  obj <- builder(
    ts = result$ts,
    template = result$template,
    context = inputs$context,
    userdefined = inputs$userdefined,
    speclist = inputs$speclist
  )
  obj$run(verbose = FALSE)
  obj
}

per_agent_series_frame <- function(ts, role, group) {
  data.frame(
    index = per_agent_plot_index(ts),
    value = as.numeric(ts),
    role = role,
    group = group,
    stringsAsFactors = FALSE
  )
}

per_agent_plot_index <- function(ts) {
  freq <- stats::frequency(ts)
  if (identical(freq, 12)) {
    return(zoo::as.Date(zoo::as.yearmon(stats::time(ts))))
  }
  if (identical(freq, 4)) {
    return(zoo::as.Date(zoo::as.yearqtr(stats::time(ts))))
  }
  stats::time(ts)
}
