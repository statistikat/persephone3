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

## Linear algebra (19b)

`src/core/linear_algebra.hpp` owns column-major memory without Rcpp types.
The implementation uses R's declared Fortran interfaces to DPOTRF/DPOTRS,
DPOCON, DGELSY and DGEMM, linked by the package's portable Makevars flags.
No LAPACK implementation is copied. Cholesky reports a 1-norm reciprocal
condition **estimate** and accepts symmetry differences only within 64 machine
epsilons of the largest matrix entry, averaging accepted pairs.

Pivoted QR uses the caller's positive rank threshold (default `1e-10`) and
returns minimum-norm solutions for rank-deficient or underdetermined systems.
Coefficients retain original column order; pivots are diagnostic. Residuals
are recomputed as `B - A %*% coefficients`. Rank is an estimated numerical
rank, not a promise of coefficient identifiability; no covariance or QR
condition estimate is fabricated. Zero matrix dimensions, missing/nonfinite
values, integer-range overflow, and non-SPD Cholesky inputs are explicit errors.

`inst/tinytest/test-native-linear-algebra.R` compares with independent analytic,
R solve and SVD cases. `test_linear_algebra.cpp` adds storage/factor corruption,
bounds and interruption/reuse checks outside Rcpp. Sanitizing these wrappers
does not instrument the prebuilt R BLAS/LAPACK libraries themselves.

For a Unix R installation, the standalone linear-algebra harness can be run
with its installed LAPACK/BLAS (override `CXX` for your C++17 toolchain):

```sh
${CXX:-c++} -std=c++17 -Wall -Wextra -Wconversion -pedantic \
  -fsanitize=address,undefined -fno-omit-frame-pointer -g -Isrc \
  $(R CMD config --cppflags) src/linear_algebra.cpp \
  tools/native-tests/test_linear_algebra.cpp \
  $(R CMD config LAPACK_LIBS) $(R CMD config BLAS_LIBS) \
  $(R CMD config --ldflags) -o /tmp/persephone-linear-algebra
/tmp/persephone-linear-algebra
```

## Transforms and regressor domains (19c)

The elementary `none`/`log` transformations preserve explicit missing masks.
Log requires positive observed values and returns each observation's log
Jacobian (`-log(y)`), so a future likelihood can select the correct estimation
span before summation. Inverse log is pointwise exp; it does not apply a
lognormal forecast-mean adjustment. Nonfinite input or exponential overflow/
underflow is rejected. Internal Rcpp transformation defaults select `none`.

`native_prepare_inputs()` validates a monthly/quarterly univariate `ts` and
a uniquely named list of regressor series or an `mts`. It aligns by integer
period, requires finite regressors throughout the entire requested observed,
backcast and forecast domain, and crops only unused extra supplied periods.
Missing responses are allowed; missing regressors, padding, recycling and
implicit extrapolation are not. Output is a serializable plain-data contract.
Estimation spans, context-object conversion, coefficient constraints and
automatic transformation selection remain later pipeline work.

The standalone transform harness uses no R headers or libraries:

```sh
c++ -std=c++17 -Wall -Wextra -Wconversion -pedantic \
  -fsanitize=address,undefined -fno-omit-frame-pointer -g -Isrc \
  src/numerics.cpp src/transforms.cpp tools/native-tests/test_transforms.cpp \
  -o /tmp/persephone-transforms
/tmp/persephone-transforms
```
