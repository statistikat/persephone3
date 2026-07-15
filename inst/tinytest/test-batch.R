# Test batch/multipleTimeSeries functionality

message("Test batch - basic functionality")

# Create batch object
objX13 <- perX13(AirPassengers, "rsa3")

bt <- perBatch(a = objX13, b = objX13)

# Before run, adjusted should be NULL
expect_true(is.null(bt$components$a$adjusted))
expect_true(is.null(bt$components$b$adjusted))

bt$run()

# After run, adjusted should exist
expect_true(!is.null(bt$components$a$adjusted))
expect_true(!is.null(bt$components$b$adjusted))
expect_inherits(bt$components$a$adjusted, "ts")
expect_inherits(bt$components$b$adjusted, "ts")

# Since both components are identical, adjusted should be identical too
expect_identical(bt$components$a$adjusted, bt$components$b$adjusted)

message("Test batch - different series with same time instances")

# Use two different series from the same time period
obj1 <- perX13(AirPassengers, "rsa3")
# Create a modified version of AirPassengers
AirPassengers2 <- AirPassengers * 1.1

obj2 <- perX13(AirPassengers2, "rsa3")

bt2 <- perBatch(series1 = obj1, series2 = obj2)
bt2$run()

expect_true(!is.null(bt2$components$series1$adjusted))
expect_true(!is.null(bt2$components$series2$adjusted))

message("Test batch - print method")

expect_silent(print(bt))
