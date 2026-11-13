#' @importFrom highcharter hchart hc_title
#' @export
highcharter::hchart

#' @export
highcharter::hc_title

#' @importFrom highcharter %>%
#' @export
highcharter::`%>%`

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Null-Coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

rescale <- function(x, min, max) {
  stopifnot(
    is.numeric(x),
    is.numeric(min),
    is.numeric(max),
    length(min) == 1,
    length(max) == 1
  )
  rg <- range(x)
  min + (x - rg[1]) / (rg[2] - rg[1]) * (max - min)
}

# interne Template-Mengen je Methode (einzig wahre Quelle)
sa_templates <- function(method = c("x13", "tramoseats")) {
  switch(
    match.arg(method),
    x13 = c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c"),
    tramoseats = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
  )
}

# expliziter Exakt-Check (match.arg wuerde partiell matchen: "rsa5" -> "rsa5c")
check_template <- function(template, method) {
  allowed <- sa_templates(method)
  if (length(template) != 1L || !(template %in% allowed)) {
    stop(sprintf(
      "Invalid template '%s' for method '%s'. Allowed values: %s",
      template,
      method,
      paste(allowed, collapse = ", ")
    ))
  }
  template
}

# Methode ist die R6-Klasse und laesst sich an bestehenden Objekten nicht
# aendern; iterate = TRUE kann sie daher nicht umschreiben.
check_component_method <- function(components, method, iterate = FALSE) {
  comp_method <- vapply(
    components,
    function(x) {
      if (is.persephone(x)) x$method else NA_character_
    },
    character(1)
  )
  conflict <- names(comp_method)[!is.na(comp_method) & comp_method != method]
  if (length(conflict) == 0) {
    return(invisible(NULL))
  }
  methods <- paste(unique(comp_method[conflict]), collapse = "/")
  msg <- sprintf(
    paste0(
      "Components %s are already '%s' objects and cannot be switched to ",
      "method = \"%s\"; they stay '%s'."
    ),
    paste(sQuote(conflict), collapse = ", "),
    methods,
    method,
    methods
  )
  if (isTRUE(iterate)) {
    msg <- paste0(
      msg,
      paste0(
        " iterate = TRUE updates their spec parameters but not their method; ",
        "pass them as plain ts/mts objects to have them created with '",
        method,
        "'."
      )
    )
  }
  warning(msg, call. = FALSE)
  invisible(NULL)
}

# Names only: resolving backend functions while loading the namespace would
# make Java mandatory even for users selecting the native backend.
specfun_list <- as.list(c(
  arima = "set_arima", automdl = "set_automodel", basic = "set_basic",
  easter = "set_easter", estimate = "set_estimate", setout = "set_outlier",
  td = "set_tradingdays", transform = "set_transform", addout = "add_outlier",
  remout = "remove_outlier", addramp = "add_ramp", remramp = "remove_ramp",
  usrdefvar = "add_usrdefvar", x11 = "set_x11", benchmarking = "set_benchmarking"
))

# persephone3 internal spec names
p3_spec_names <- c("outliers")
