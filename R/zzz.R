# Global variables declaration to suppress R CMD check notes
# These are used internally by R6 classes and need to be declared

utils::globalVariables(c(
  "private",      # R6 private fields/methods accessor
  "public",       # R6 public fields/methods accessor (if used)
  "self",         # R6 self accessor (if used)
  "super"         # R6 super accessor (if used)
))
