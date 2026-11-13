# Native numerical foundation checks

The C++ core is independently authored and contains no translated JDemetra+
source. It currently supplies time indexing, differencing polynomials,
convolution and finite filters. It does **not** fit seasonal-adjustment models.

Run from the repository root with any supported C++17 compiler:

```sh
c++ -std=c++17 -Wall -Wextra -pedantic -Isrc src/numerics.cpp tools/native-tests/test_numerics.cpp -o /tmp/persephone-numerics-test
/tmp/persephone-numerics-test
```

On a toolchain supporting AddressSanitizer and UndefinedBehaviorSanitizer:

```sh
c++ -std=c++17 -fsanitize=address,undefined -fno-omit-frame-pointer -g -Isrc src/numerics.cpp tools/native-tests/test_numerics.cpp -o /tmp/persephone-numerics-sanitized
/tmp/persephone-numerics-sanitized
```

The installed-package tests in `inst/tinytest/test-native-primitives.R` exercise
the Rcpp boundary and compare filters with independent R operations. Missing
input and out-of-range active filter taps produce an explicit mask and `NA`
outputs; zero-weight taps do not propagate missingness. No endpoint extension
is performed. A nonfinite coefficient or arithmetic overflow is an error.
