#' is.persephone
#'
#' Check if an R object is of or derives from the class `persephone` or convert
#' an object to persephone with method "tramoseats" as default.
#' @param x R object to be tested or converted.
#' @examples
#' is.persephone(2)
#' obj <- as.persephone(AirPassengers)
#' obj$run()
#' @export
is.persephone <- function(x) {
  inherits(x, "persephone")
}

#' @rdname is.persephone
#' @param method SA method used when coercing a bare ts object, i.e.
#'   "tramoseats" (default) or "x13". Ignored for existing persephone objects,
#'   whose method is fixed by their class.
#' @param template template name passed to the corresponding spec function;
#'   if NULL, the method-specific default is used.
#' @examples
#' obj <- as.persephone(AirPassengers)
#' obj_x13 <- as.persephone(AirPassengers, method = "x13", template = "rsa3")
#' @export
as.persephone <- function(x, method = c("tramoseats", "x13"), template = NULL) {
  method <- match.arg(method)
  # bestehende Objekte behalten Methode und Template (Methode = R6-Klasse)
  if (is.persephone(x)) {
    return(x$clone(deep = TRUE))
  }
  if (!stats::is.ts(x)) {
    stop("cannot coerce object of type ", class(x)[1], " to persephone")
  }
  if (!is.null(template)) {
    check_template(template, method)
  }
  if (method == "x13") {
    if (is.null(template)) perX13(x) else perX13(x, template = template)
  } else {
    if (is.null(template)) perTramo(x) else perTramo(x, template = template)
  }
}
