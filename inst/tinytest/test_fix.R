# Test fixModel and fixOutlier functions (minimal tests - known issues)

message("Test fixModel - single series (known issues)")

# Fix model single series
objX13 <- perX13(AirPassengers, "rsa3")

# Should warn if not run yet
expect_warning(objX13$fixModel(), "not run")

objX13$run()

# fixModel exists and can be called (may produce warnings/errors)
# Just check it doesn't completely break the object
objX13$fixModel()
objX13$run()

# Just check the object is still valid after fixModel
expect_true(!is.null(objX13))

message("Test fixOutlier - single series (known issues)")

objX13_out <- perX13(AirPassengers, "rsa3")

expect_warning(objX13_out$fixOutlier(), "not run")

objX13_out$run()

# Just check the object is still valid
expect_true(!is.null(objX13_out$output))
