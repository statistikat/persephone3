# The adapter must preserve current fitted results and object identity semantics.
original_backend_option <- getOption("persephone.backend")
options(persephone.backend = "rjd3")

x <- perX13(AirPassengers, backend = "rjd3")
expect_identical(x$backend, "rjd3")
expect_identical(x$result, NULL)
expect_false(x$is_stale)
x$run()
reference <- rjd3x13::x13_fast(AirPassengers, spec = x$params,
                              userdefined = persephone3:::userdefined_default)
expect_equal(x$output, reference)
expect_identical(x$result$raw, x$output)
expect_identical(x$result$components$sa, x$adjusted)
expect_identical(x$result$specification$parameters, x$params)
expect_identical(x$specification$schema_version, 1L)
expect_identical(x$result$backend, "rjd3")
expect_false(x$is_stale)

options(persephone.backend = "native")
expect_identical(perX13(AirPassengers)$params$template, "rsa3")
# Existing objects, explicit selection and cloned children retain their engine.
x$run()
expect_identical(x$backend, "rjd3")
expect_identical(as.persephone(x, backend = "native")$backend, "rjd3")
b <- perBatch(a = x, b = AirPassengers * 2, method = "x13", backend = "rjd3")
expect_identical(vapply(b$components, function(z) z$backend, character(1)),
                 c(a = "rjd3", b = "rjd3"))
h <- perHts(a = x, b = AirPassengers * 2, method = "x13", backend = "rjd3")
h$run()
expect_identical(h$result$backend, "rjd3")
expect_identical(h$specification$parameters, h$paramsDirect)
expect_identical(h$result$raw, h$output)
expect_false(h$is_stale)

options(persephone.backend = "rjd3")
old_result <- x$result
x$updateParams(transform.fun = "None")
expect_true(x$is_stale)
expect_identical(x$result, old_result)
x$run()
expect_false(x$is_stale)
expect_identical(x$result$specification$parameters, x$params)
expect_error(x$updateParams(notablock.value = 1), "Unknown specification block")
expect_false(x$is_stale)

h$updateParams(component = "b", transform.fun = "None")
expect_true(h$is_stale)
h$run()
expect_false(h$is_stale)
restored <- unserialize(serialize(h, NULL))
expect_identical(restored$backend, h$backend)
expect_identical(restored$result, h$result)
expect_false(restored$is_stale)
restored$run()
expect_equal(restored$output, h$output)

canonical <- x$specification
x$updateParams(spec = canonical)
expect_identical(x$params, canonical$parameters)
x$run()
expect_false(x$is_stale)

options(persephone.backend = original_backend_option)
