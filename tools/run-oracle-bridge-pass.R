# Oracle lane, bridge-domain pass (item M6-c, 2026-11-24): run the five
# bridge-parity files whose live-JVM blocks compare against JVM captures
# qualified with HotSpot's _dexp/_dlog intrinsics DISABLED. On x86-64 the JVM
# must therefore already run the capture flags when this starts -- export
#   JAVA_TOOL_OPTIONS="-XX:+UnlockDiagnosticVMOptions -XX:DisableIntrinsic=_dexp,_dlog"
# in the shell (or CI step) BEFORE R starts; R cannot change a running JVM's
# flags. The script pins PERSEPHONE_ORACLE_PASS=bridge; the capture-domain
# guard in R/helpers.R verifies the live JVM and fails loudly on a mismatch
# (the default-domain files are not rerun here -- they belong to pass A,
# PERSEPHONE_ORACLE_PASS=default, which inside R CMD check defers exactly
# these five files' blocks with a documented message). On aarch64 the two
# domains coincide: the guard is inert and this runs as an ordinary rerun.
files <- c("test-native-forecasts.R", "test-native-tramoseats-rsa1.R",
           "test-native-tramoseats-rsa3.R", "test-native-tramoseats-rsafull.R",
           "test-native-x13-estimated.R")
Sys.setenv(PERSEPHONE_ORACLE_PASS = "bridge")
library(persephone3)
library(tinytest)
fails <- 0L
total <- 0L
for (f in files) {
  res <- run_test_file(file.path("inst", "tinytest", f))
  st <- as.matrix(summary(res))["Total", c("Results", "fails", "passes")]
  total <- total + st[["Results"]]
  fails <- fails + st[["fails"]]
  cat(sprintf("FILE %s expectations=%d failing=%d\n", f, st[["Results"]], st[["fails"]]))
}
cat(sprintf("BRIDGE-PASS TOTAL files=%d expectations=%d fails=%d\n",
            length(files), total, fails))
quit(save = "no", status = if (fails > 0L) 1L else 0L)
