# Optional OpenMP execution

The C++ core can parallelize independent output positions in polynomial
convolution, finite filtering, identity/log transformation and point inverse
transformation. The state-space time recursion, diffuse rank updates, likelihood
reductions and BLAS/LAPACK calls remain serial. Parallel execution changes no
model, missing-value rule or per-output summation order.

## Build and runtime controls

`configure` uses `R CMD SHLIB` with `CXX_STD = CXX17` and R's
`SHLIB_OPENMP_CXXFLAGS` to compile/link a small OpenMP program. It then loads and
calls the resulting library, catching missing runtimes that a platform's dynamic
linker might otherwise permit. The generated `src/Makevars` and
`src/Makevars.win` use the verified flag for both compilation and linking; an
unsuccessful probe generates a serial configuration. The `.in` templates are
tracked; generated Makevars and probe artifacts are not. `configure.win` uses
the same test under the Rtools shell. No OpenMP runtime is bundled or installed.

The implementation follows the primary
[Writing R Extensions OpenMP guidance](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#OpenMP-support):
test the selected C++17 toolchain, condition code on `_OPENMP`, avoid R APIs in
workers, and limit each region without changing process-wide thread settings.

`PERSEPHONE_NUM_THREADS` is read at the start of an operation. The default is
**2**; accepted explicit values are integers from **1 to 64**. A value of **1**
forces serial execution. The request is capped by the OpenMP runtime's current
maximum. Standard OpenMP limits may further reduce the actual team; their
environment variables generally need to be set before the runtime initializes.
Do not change environment variables concurrently with a native operation.

```r
Sys.setenv(PERSEPHONE_NUM_THREADS = "4")
persephone3:::.native_parallel_info()
Sys.setenv(PERSEPHONE_NUM_THREADS = "1")  # deterministic serial comparison
```

The internal information object distinguishes compilation support, OpenMP
version, requested threads, runtime maximum, selected region limit and an actual
probe team size. A compiled OpenMP library can still execute serially because of
runtime limits or small workloads. Builds without OpenMP report that explicitly
and use the same serial algorithms.

## Safety and deterministic behavior

Input validation, R/Rcpp conversion and output allocation occur before workers
start. Workers use owned C++ arrays and write disjoint output positions; masks
use byte vectors rather than packed `vector<bool>`. They call neither R/Rcpp nor
BLAS/LAPACK. Each convolution output retains ascending left-coefficient order,
and each filter output retains input tap order. No parallel sum reduction is
introduced. Serial and parallel results are tested for exact equality within
the same compiled library/platform; this does not promise bitwise equality
between different compilers or architectures.

Exceptions are caught inside each contiguous worker block. After all workers
join, the first exception in the lowest-index failing block is rethrown on the
calling thread. No partial result is returned. Interrupt callbacks execute only
between chunks, outside OpenMP regions. Nested calls execute serially and
suppress callbacks within the outer region; an embedding caller must handle its
own cancellation outside that region.

Parallel regions require at least 65,536 estimated scalar operations and two
outputs. Chunks contain at most 65,536 outputs and target at most 262,144
estimated operations. Very long individual convolution/filter reductions use
the serial path with the previous fine-grained interruption checks; splitting
such a reduction would change its addition order. These thresholds avoid
parallelizing tiny SARIMA polynomials and short filters. They are an execution
policy, not a numerical tolerance or a promised speedup on every machine.

## Validation

`inst/tinytest/test-native-parallel.R` compares one-thread and four-thread
requests on substantial convolution/filter/transform workloads, including masks,
input immutability, invalid control values, worker errors and successful reuse.
Set `PERSEPHONE_REQUIRE_OPENMP=1` when the test lane must actually provide
multiple threads; ordinary serial-fallback installations do not claim that.

`tools/native-tests/test_parallel.cpp` additionally verifies real worker thread
IDs, callback thread identity, deterministic simultaneous errors, cancellation
and nested-region suppression. Build it both ways:

```sh
c++ -std=c++17 -Wall -Wextra -Wconversion -pedantic -O2 -Isrc \
  src/numerics.cpp src/transforms.cpp tools/native-tests/test_parallel.cpp \
  -o /tmp/persephone-parallel-serial
/tmp/persephone-parallel-serial
c++ -std=c++17 -Wall -Wextra -Wconversion -pedantic -O2 -fopenmp -Isrc \
  src/numerics.cpp src/transforms.cpp tools/native-tests/test_parallel.cpp \
  -o /tmp/persephone-parallel-openmp
PERSEPHONE_REQUIRE_OPENMP=1 OMP_NUM_THREADS=4 OMP_DYNAMIC=FALSE \
  /tmp/persephone-parallel-openmp
```

The second example assumes a compiler/runtime that supports `-fopenmp`; package
builds use the configured R flags instead. ASan/UBSan cover the C++ code;
prebuilt OpenMP and BLAS/LAPACK runtimes are not themselves instrumented.
