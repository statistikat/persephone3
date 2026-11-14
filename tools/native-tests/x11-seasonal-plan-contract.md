# Staged seasonal requests (22a.2b.3c.1)

```r
native_x11_seasonal_plan(12, "x11default", "b_initial")
native_x11_seasonal_plan(4, c("msr","x11default","s3x3","stable"), "d_final")
native_x11_seasonal_plan(12, "msr", "d_final")
```

This Java-free policy resolves fixed, X11Default and MSR requests for an explicit
`b_initial`, `b_final`, `c_initial`, `c_final`, `d_initial` or `d_final` stage.
It computes no numerical series and does not execute any X-11 pass. Existing
fixed seasonal processing remains a separate operation with its own explicit
short-series policy. A plan's fixed resolution does not establish that the
requested filter has enough input support or will survive a length-based fallback.

The six fixed choices (`stable`, `s3x1`, `s3x3`, `s3x5`, `s3x9`, `s3x15`)
remain unchanged in every stage. `x11default` resolves to `s3x3` initially and
`s3x5` at the final seasonal step of each iteration. An all-`msr` request uses
the same preliminary mapping, but the final seasonal step of D requires a
data-driven MSR decision. Its provisional `base_filters` stay `s3x5`, while
`resolved_filters` are NA and status is `requires_msr`. Passing those unresolved
choices to the fixed processor fails explicitly. No provisional filter is
silently presented as the final MSR decision.

Public validated context calls establish that global MSR is enabled only when
every calendar period requests it. Scalar MSR and a complete all-MSR vector
are equivalent. In mixed requests, each MSR entry has the preliminary 3×3/3×5
mapping and does not enable global MSR. `global_msr`, `requires_msr` and
`msr_mixed_request` reasons expose this distinction. The broad stage sequence
is documented in [Census X-13 manual, Table 7.56](https://www2.census.gov/software/x-13arima-seats/x13as/unix-linux/documentation/docx13as.pdf);
the mixed-request rule is separately qualified by public JD+ 3.9.0 contexts.

Input is one request or one per calendar month/quarter. Named vectors must
identify all periods exactly once using `"1"` through `"frequency"`; complete
names are reordered. No partial names or implicit recycling is accepted.
Output is serializable, with named requested, provisional and resolved filters,
resolution reasons, stage, eligibility/pending flags and status. No calendar
start, data mode or series length is needed for this bounded policy itself.

Core/header code links with only the standard C++ runtime, without R/Rcpp,
BLAS/LAPACK or OpenMP. Work is bounded by twelve requests, so no worker region
or interrupt callback is introduced. Strict ASan/UBSan checks cover all stages,
every fixed request, each mixed-MSR position, unresolved state and invalid
enums/vector sizes/frequencies. Prior numerical modules retain their owned
OpenMP and caller-only interruption contracts.

24,380 mandatory R assertions qualify 336 frozen public validated contexts and
2,016 explicit stage cases. Inputs include scalar/uniform-vector and mixed
requests, both frequencies, two calendar starts, spans around the five/twenty-year
gates and four mode labels. Frozen public mappings remain independent of the
separate processor's length fallbacks. Tests also qualify named reordering,
serialization, scalar/vector equivalence, strict API errors and composition with
the existing fixed processor. Global MSR estimation, iterative choices, B/C/D
values and complete native fits remain separate implementation work.
