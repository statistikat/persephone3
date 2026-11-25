# Test the arima shortcut: orders as a vector or as the "(0 1 1)(0 1 1)"
# string of the summary() output (parse_arima_orders, update_spec)

message("Test arima shortcut - parsing")

p1 <- persephone3:::parse_arima_orders(c(0, 1, 1, 0, 1, 1))
p2 <- persephone3:::parse_arima_orders("(0 1 1)(0 1 1)")
p3 <- persephone3:::parse_arima_orders("0 1 1 0 1 1")
p4 <- persephone3:::parse_arima_orders("0,1,1,0,1,1")
expect_identical(names(p1), c("p", "d", "q", "bp", "bd", "bq"))
expect_identical(p1, p2)
expect_identical(p2, p3)
expect_identical(p3, p4)
expect_error(persephone3:::parse_arima_orders(c(0, 1, 1, 0, 1)), "six")
expect_error(persephone3:::parse_arima_orders(c(0, -1, 1, 0, 1, 1)), "six")
expect_error(persephone3:::parse_arima_orders(c(0, 1.5, 1, 0, 1, 1)), "vector")
expect_error(persephone3:::parse_arima_orders("(0 1 1)(0 1 x)"), "vector")

# the orders of the specification, derived from the polynomials of the arima
# block (x13 keeps it under regarima, tramoseats under tramo)
get_orders <- function(obj) {
  a <- obj$params$regarima$arima
  if (is.null(a)) a <- obj$params$tramo$arima
  n <- function(m) if (is.null(m)) 0 else ncol(m)
  c(n(a$phi), a$d, n(a$theta), n(a$bphi), a$bd, n(a$btheta))
}
get_automdl <- function(obj) {
  a <- obj$params$regarima$automodel
  if (is.null(a)) a <- obj$params$tramo$automodel
  a$enabled
}

message("Test arima shortcut - x13 specification")

y <- window(AirPassengers, end = c(1952, 12))
orders <- c(0, 1, 1, 0, 1, 1)

# the shortcut must set the same orders as the explicit arima.* arguments
dots_obj <- perX13(y, "rsa3", automdl.enabled = FALSE,
                   arima.p = 0, arima.d = 1, arima.q = 1,
                   arima.bp = 0, arima.bd = 1, arima.bq = 1)
short_obj <- perX13(y, "rsa3", arima = "(0 1 1)(0 1 1)")
expect_identical(get_orders(short_obj), orders)
expect_true(identical(unclass(short_obj$params), unclass(dots_obj$params)))

# the shortcut disables the automatic model selection (which would drop the
# orders silently) unless it is asked for explicitly
expect_false(isTRUE(get_automdl(short_obj)))
expect_warning(
  perX13(y, "rsa3", arima = "(0 1 1)(0 1 1)", automdl.enabled = TRUE),
  "automdl"
)
# ... and it must not be mixed with the explicit arima.* arguments
expect_error(
  perX13(y, "rsa3", arima = "(0 1 1)(0 1 1)", arima.p = 1),
  "not both"
)

# the updateParams path (dots without a speclist)
up_obj <- perX13(y, "rsa3")
up_obj$updateParams(arima = c(1, 1, 1, 0, 1, 1))
expect_identical(get_orders(up_obj), c(1, 1, 1, 0, 1, 1))

message("Test arima shortcut - tramo")

tr_obj <- perTramo(y, "rsa3", arima = "(0 1 1)(0 1 1)")
expect_identical(get_orders(tr_obj), orders)
expect_false(isTRUE(get_automdl(tr_obj)))

message("Test arima shortcut - hierarchical")

# classic mode: the shortcut is a specification argument, not a component
ht <- perHts(a = perX13(y, "rsa3"), b = perX13(y, "rsa3"), method = "x13",
             arima = "(0 1 1)(0 1 1)")
expect_identical(ht$getSeries(), c("a", "b"))
expect_identical(get_orders(ht$getComponent("a")), orders)
expect_identical(get_orders(ht$getComponent("b")), orders)

# groups mode: the aggregate must be provided as a series as well
mts <- ts(cbind(a = y, b = y * 2, tot = y * 3),
          start = stats::start(y), frequency = 12)
htg <- perHts(list = mts, method = "x13", groups = list(tot = c("a", "b")),
              weights = list(tot = c(a = 0.5, b = 0.5)),
              arima = "(1 1 1)(0 1 1)")
expect_identical(get_orders(htg$getComponent("a")), c(1, 1, 1, 0, 1, 1))

# one component by name, like the production pattern in the slides
htg$updateParams(component = "b", arima = "(0 1 1)(0 1 0)")
expect_identical(get_orders(htg$getComponent("b")), c(0, 1, 1, 0, 1, 0))
expect_identical(get_orders(htg$getComponent("a")), c(1, 1, 1, 0, 1, 1))
