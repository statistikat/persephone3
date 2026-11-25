# Replacing templates must reset settings; incremental updates must not.
# M7 default-backend cutover (item 27, decision 397cfe8): this file compares
# rjd3 specification objects, so the rjd3 backend is pinned at every
# construction.
for (constructor in list(perX13, perTramo)) {
  x <- constructor(AirPassengers, template = "rsa3", backend = "rjd3")
  x$run()
  original_result <- x$result
  x$updateParams(template = "rsa0")
  expect_identical(x$params, constructor(AirPassengers, template = "rsa0", backend = "rjd3")$params)
  expect_true(x$is_stale)
  expect_identical(x$result, original_result)
  x$run()
  expect_false(x$is_stale)
  expect_identical(x$result$specification$template, "rsa0")
  x$updateParams(transform.fun = "None")
  previous <- x$params
  x$updateParams(addout.type = "AO", addout.date = "1953-07-01")
  section <- if (x$method == "x13") "regarima" else "tramo"
  expect_identical(x$params[[section]]$transform, previous[[section]]$transform)

  previous <- list(params=x$params, context=x$context, result=x$result)
  expect_error(x$updateParams(template="bad", context=list(regressors=list())), "Invalid template")
  expect_identical(list(params=x$params, context=x$context, result=x$result), previous)
  expect_error(x$updateParams(template="rsa0", spec=x$params), "either 'template'")
  expect_error(x$updateParams(speclist=list(transform.fun="Log"), transform.fun="None"), "either 'speclist'")
  expect_error(x$updateParams(spec=list(bad=1)), "complete rjd3 specification")
  expect_error(x$updateParams(userdefined=NA_character_), "userdefined")
  expect_error(x$updateParams(context=1), "context")
  expect_identical(list(params=x$params, context=x$context, result=x$result), previous)

  replacement <- constructor(AirPassengers, template="rsa1", backend = "rjd3")$specification
  x$updateParams(spec=replacement)
  expect_identical(x$params, replacement$parameters)
  expect_identical(x$specification$template, NULL)
  replacement$frequency <- 4
  expect_error(x$updateParams(spec=replacement), "frequency")
}

nested <- perHts(a=AirPassengers, b=AirPassengers*2, method="x13", backend="rjd3")
h <- perHts(group=nested, c=AirPassengers*3, method="x13", backend="rjd3")
child <- h$components$group$components$a
before <- h$params
h$updateParams(template="rsa0", iterate=FALSE)
expect_identical(h$paramsDirect, perX13(h$ts,template="rsa0", backend="rjd3")$params)
expect_identical(h$components$group$params, before$group)
h$updateParams(template="rsa1", component="group/a")
expect_identical(child$params, perX13(child$ts,template="rsa1", backend="rjd3")$params)
expect_identical(h$components$c$params, before$c)
h$updateParams(template="rsa0", iterate=TRUE)
expect_identical(child$params, perX13(child$ts,template="rsa0", backend="rjd3")$params)
expect_identical(h$components$group$paramsDirect, perX13(h$components$group$ts,template="rsa0", backend="rjd3")$params)

# A later incompatible node must not leave any earlier node partially updated.
mixed <- suppressWarnings(perHts(group=h, last=perTramo(AirPassengers, backend="rjd3"), method="x13", backend="rjd3"))
before <- mixed$params
held_child <- mixed$components$group$components$group$components$a
held_params <- held_child$params
expect_error(mixed$updateParams(x11.henderson.filter=13), "only for method 'x13'")
expect_identical(mixed$params, before)
expect_identical(held_child$params, held_params)
clone <- mixed$clone(deep=TRUE)
clone$updateParams(component="group/group/a", template="rsa1")
expect_identical(mixed$params, before)
expect_false(identical(clone$params, before))

b <- suppressWarnings(perBatch(first=perX13(AirPassengers, backend="rjd3"), last=perTramo(AirPassengers, backend="rjd3"), backend="rjd3"))
before <- b$params
expect_error(b$updateParams(x11.henderson.filter=13), "only for method 'x13'")
expect_identical(b$params, before)
expect_error(b$updateParams(component="missing", template="rsa0"), "existing")
b$updateParams(component="first", template="rsa0")
expect_identical(b$components$first$params, perX13(AirPassengers,template="rsa0", backend="rjd3")$params)
expect_identical(b$components$last$params, before$last)

# fixModel freezes orders, not the estimated coefficients.
x <- perX13(AirPassengers, backend="rjd3")
x$run()
orders <- unlist(x$output$user_defined[c("arima.p","arima.d","arima.q","arima.bp","arima.bd","arima.bq")])
x$fixModel()
expect_false(x$params$regarima$automodel$enabled)
x$run()
expect_equal(unlist(x$output$user_defined[names(orders)]), orders)
