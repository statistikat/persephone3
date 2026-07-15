# Test single series (x13Single) functionality

message("Test single series - basic functionality")

# Create single series object
objX13 <- perX13(AirPassengers, "rsa3")
objX13$run()

# Test that run() worked
expect_true(!is.null(objX13$output))
expect_inherits(objX13, "x13Single")
expect_inherits(objX13$ts, "ts")

# Test accessor methods
expect_true(
  inherits(objX13$params, "JD3_X13_SPEC") || inherits(objX13$params, "SA_spec")
)
expect_equal(objX13$method, "x13")

message("Test single series - different templates")

# Test with different templates
obj_rsa1 <- perX13(AirPassengers, "rsa1")
obj_rsa1$run()
expect_true(!is.null(obj_rsa1$output))

obj_rsa0 <- perX13(AirPassengers, "rsa0")
obj_rsa0$run()
expect_true(!is.null(obj_rsa0$output))

message("Test single series - fixModel")

# Test fixModel
objX13_test <- perX13(AirPassengers, "rsa3")
expect_warning(objX13_test$fixModel(), "not run yet")

objX13_test$run()
# fixModel may produce warnings, so we don't expect silence
objX13_test$fixModel()
objX13_test$run()

# After fixModel, check that params structure is still valid
expect_true(!is.null(objX13_test$params))

message("Test single series - print method")

# Test print method
expect_silent(print(objX13))
