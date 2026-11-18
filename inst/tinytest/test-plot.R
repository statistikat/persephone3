# Test plotting functions

message("Test plot - single series before run")

data(AirPassengers, package = "datasets")
obj <- perX13(AirPassengers, "rsa1")

# Plot before run should work (shows original series)
expect_silent(plot(obj, drawPoints = TRUE))
expect_silent(plot(obj, annualComparison = 1))

message("Test plot - single series after run")

obj$run()
expect_silent(obj$plot(drawPoints = TRUE))
expect_silent(plot(obj))

expect_silent(plot(obj, annualComparison = 1))

message("Test plot - quarterly data")

jj <- JohnsonJohnson
jj[7] <- 100
obj_q <- perX13(jj, "rsa1")
obj_q$run()
expect_silent(plot(obj_q, annualComparison = 1))

message("Test plotSeasIrrCal")

obj_seas <- perX13(AirPassengers, "rsa1")
obj_seas$run()

expect_silent(obj_seas$plotSeasIrrCal())
expect_silent(plotSeasIrrCal(obj_seas, annualComparison = 1))

message("Test plotResiduals")

obj_res <- perX13(AirPassengers, "rsa1")
obj_res$run()
expect_silent(obj_res$plotResiduals())

message("Test plotSiRatios")

obj_si <- perX13(AirPassengers, "rsa1")
obj_si$run()
expect_silent(obj_si$plotSiRatios())

message("Test plotSpectrum")

obj_spec <- perX13(AirPassengers, "rsa1")
obj_spec$run()
expect_silent(obj_spec$plotSpectrum())
