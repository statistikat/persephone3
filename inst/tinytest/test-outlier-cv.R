# Test the crit.value column of getOutliers() and the internal
# calcOutlierCrit() formula (verified against jdplus X13Utility/TramoUtility
# .calcCv via rJava)

message("Test outlier critical values")

# automatic thresholds, transcribed from jdplus (not exported, hence ::: )
expect_equal(persephone3:::calcOutlierCrit(144, "x13"), 3.889838, tolerance = 1e-6)
expect_equal(persephone3:::calcOutlierCrit(36, "x13"), 3.545801, tolerance = 1e-6)
expect_equal(persephone3:::calcOutlierCrit(600, "x13"), 4.149967, tolerance = 1e-6)
expect_equal(persephone3:::calcOutlierCrit(1, "x13"), 2.8)
expect_equal(persephone3:::calcOutlierCrit(144, "tramoseats"), 3.535, tolerance = 1e-12)
expect_equal(persephone3:::calcOutlierCrit(50, "tramoseats"), 3.3)
expect_equal(persephone3:::calcOutlierCrit(450, "tramoseats"), 4.3)

# explicit critical value is reported unchanged (x13 writes defva only)
obj_fix <- perX13(AirPassengers, "rsa3", setout.critical.value = 3.4)
obj_fix$run()
o <- obj_fix$getOutliers()
expect_false(all(is.na(o)))
expect_true("crit.value" %in% names(o))
expect_true(all(o$crit.value == 3.4))

# tramoseats reads the same value from its own outlier block
obj_t <- perTramo(AirPassengers, "rsa3", setout.critical.value = 3.5)
obj_t$run()
ot <- obj_t$getOutliers()
expect_false(all(is.na(ot)))
expect_true(all(ot$crit.value == 3.5))

# the tramo "rsa3" template detects no outlier in AirPassengers, so the
# table stays empty (NA) rather than carrying an unused threshold
obj_auto <- perTramo(AirPassengers, "rsa3")
obj_auto$run()
expect_true(all(is.na(obj_auto$getOutliers())))
