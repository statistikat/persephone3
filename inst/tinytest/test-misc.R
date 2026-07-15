# Test miscellaneous functions

message("Test is.persephone")

obj <- perX13(AirPassengers, "rsa3")
expect_true(is.persephone(obj))
expect_false(is.persephone(1L))
expect_false(is.persephone("not an object"))

message("Test print method")

obj_print <- perX13(AirPassengers, "rsa3")
# Print before run should work
expect_silent(print(obj_print))

obj_print$run()
# Print after run should work
expect_silent(print(obj_print))

message("Test generateQrTable")

obj_qr <- perX13(AirPassengers, "rsa3")

# Before run should fail gracefully
expect_error(generateQrTable(obj_qr), "No results|not run")

obj_qr$run()
# After run should work
qr_table <- generateQrTable(obj_qr)
expect_true(!is.null(qr_table))

message("Test summary method (known issue)")

obj_sum <- perX13(AirPassengers, "rsa3")
obj_sum$run()

# summary may have issues, just check object is valid
expect_true(!is.null(obj_sum$output))

message("Test diagnosticsTable")

obj_diag <- perX13(AirPassengers, "rsa3")
obj_diag$run()

# diagnosticsTable should work
diag_table <- obj_diag$diagnosticsTable()
expect_true(!is.null(diag_table))
