# Pinned backend reference and native feature inventory

This directory implements the first scope/reference work from implementation
items 16 and 18. It does not implement a native fitting engine or establish
native numerical equivalence. It never calls Persephone's getters, so existing
wrapper defects are not recorded as the numerical target.

The scope/provenance inventory and remaining item 16 gates are described in
[SCOPE.md](SCOPE.md). The fixture harness below implements the first item 18
slice.

## Captured baseline

The committed corpus in `inst/tinytest/fixtures/backend/` contains:

- 12 fits: X-13 and TRAMO/SEATS, monthly and quarterly, fixed-order `rsa0`,
  fixed-order with a manually specified AO, and automatic `rsa3`.
- Deterministic synthetic input data generated from the documented formula in `fixtures.R`;
  no random seed, external data download, or third-party dataset is required.
- Full input/specification/request data and unmodified plain-R backend results,
  including preprocessing, components, diagnostics, forecasts and available
  intermediate outputs. Method-specific unavailable fields remain unavailable.
- Snapshots of all 13 supported template expansions, 113 parameters from the
  installed 15 specification setters, and the 57 default output requests.
- A field schema for every captured result and an exact-path tolerance table.
  Wildcard output requests can expand into multiple entries; 57 is the number
  of requests, not a claim that only 57 result fields are needed.
- Package metadata, 20 JAR checksums and captured Maven/manifest metadata,
  runtime JVM metadata, classpath and artifact checksums.

The actual oracle is the installed **rjd3toolkit/rjd3x13/rjd3tramoseats 3.9.0**,
**rjd3jars 0.0.6** with **JD+ 3.9.0** libraries, R 4.5.2, and the exact JVM and
JAR hashes in `oracle-manifest.R`. Absolute paths record where this evidence
was captured; replay resolves package-relative JAR paths so installation paths
need not match. Some rJava JRI JARs are inventoried with the package even though
they are not in the captured seasonal-adjustment JVM classpath.

The previously researched upstream
[`15f1821fa71a949f842a5891e7cd68c83187f76d`](https://github.com/jdemetra/jdplus-main/commit/15f1821fa71a949f842a5891e7cd68c83187f76d)
is **3.9.1-SNAPSHOT**, not the oracle's claimed source. The captured wrapper and
Maven metadata do not establish a Java source SHA. The manifest records this
as unresolved rather than inventing a correspondence. Artifact hashes pin
what was run; mapping those artifacts to an audited source revision remains
necessary before a source-based algorithm port.

## Commands

From the repository root:

```sh
# Integrity, schema and comparator-corruption checks; does not load Java/rjd3.
Rscript tools/reference/verify.R

# Replay all 12 fits against the pinned packages/JARs/R/JVM.
Rscript tools/reference/verify.R --live

# Regenerate with the same pinned runtime/artifacts.
Rscript tools/reference/generate.R

# Intentionally replace the reference baseline after reviewing version drift.
Rscript tools/reference/generate.R --refresh-reference
```

Generation needs the rjd3 packages, rJava, RProtoBuf and `digest` installed in
the R library selected for the command. Fast verification only needs base R
and the developer dependency `digest`; it verifies that Java/rjd3 namespaces
were not loaded. None of these scripts changes the user's R library.

Do not use `--refresh-reference` just to make a discrepancy disappear. Review
package/JAR version changes, input/spec changes, changed selected models and
numerical differences. Commit the relevant manifest, fixture, schema and
checksum changes together. Generated data are versioned artifacts, not test
outputs that can be silently overwritten during ordinary package checks.

## Comparing a candidate result

```r
source("tools/reference/compare.R")
f <- readRDS("inst/tinytest/fixtures/backend/x13-12-fixed.rds")
tolerance <- read.csv("inst/tinytest/fixtures/backend/field-tolerances.csv")
issues <- compare_reference(candidate_result, f$raw_result, tolerance)
stopifnot(nrow(issues) == 0L)
```

This comparator expects the same plain-data result shape. A future native
adapter must either expose a verified compatibility projection or compare a
separate canonical projection of **both** results. Do not fake JD3 classes
on native objects solely to satisfy this check.

The comparison checks structures/attributes exactly, including time domains,
labels, coefficient states and categories. It distinguishes NA, NaN and signed
infinity. Each numeric leaf requires an explicit manifest entry and uses
`abs(actual - expected) <= atol + rtol * abs(expected)`. Missing tolerance rules
fail closed. Numeric issues report the first failing index/time and the maximum
absolute error for that field; the report's `first_divergence` attribute gives
the first structural/numerical divergence in traversal order.

Tolerances are **provisional**, divided into discrete, estimation and
series/statistic groups and materialized per field. They are suitable for
checking the harness and pinned reruns, not an unreviewed approval standard
for a new algorithm. Calibrate each field against conditioning and numerical
conventions before native acceptance; retain exact decisions away from
explicitly reviewed threshold cases. Fine-grained central/endpoint/forecast
error summaries and selection traces must be added with the corresponding
native kernels.

## Scope decisions and ownership

| Capability | Inventory/reference coverage | Next implementation owner/work |
| --- | --- | --- |
| Template/specification handling | All supported template snapshots; 113 setter parameters/default expressions | R integrator: item 17 canonical schema and item 8 updates; per-method parameter validation is still needed |
| Fixed regression-SARIMA | Both methods/frequencies; coefficients, likelihood, residuals, covariance and forecasts in raw results | Numerical engineer: item 20 kernels and intermediate parity |
| Manual/automatic outliers and automatic model selection | AO and `rsa3` examples | Numerical engineer/reviewer: items 21/23; expand LS/TC, thresholds, spans, fixed-coefficient cases |
| X-11 and SEATS | Complete raw output for six cases per method | Numerical engineer/reviewer: items 22/24, modes/filters/admissibility/uncertainty |
| Additional direct output consumers | Schema captures preprocessing fields, X-11 tables/filter metadata, SEATS components and diagnostics | R integrator: items 6/10/12/17 compatibility projections; numerical outputs in 25 |
| External regressors/calendars | Setter formals inventoried; no external-context corpus yet | R integrator/domain reviewer: items 11/17; add context/future-regressor fixtures before native support |
| Benchmarking and advanced settings | Template/specification inventory only | Numerical engineer: item 25 and mandatory fixture expansion |
| Hierarchy, UI and serialization | Deliberately outside direct backend generation | R integrator: items 9/13/26 using engine-independent tests |
| Platform and native correctness | Captured macOS arm64 oracle; no native parity claim | Build/validation owner: items 14/19/26 and independent statistical sign-off |

A native setting remains unsupported until its numerical and product fixtures
pass. The inventory's `native_status = not_implemented` is intentional. It
must evolve with the code, not be interpreted as a subset to silently disable.
`specification-options.csv` records observed setter formals, not proof that
every parameter is accepted by both methods or every combination is valid.

## Provenance and remaining review

The new reference tooling is authored for this repository under its existing
licensing. Inputs are synthetic and the formula is included in `fixtures.R`.
No Java implementation, JAR or third-party input dataset is copied into the
package. The raw data artifacts are outputs of the captured reference engine.

Upstream JDemetra+ and the installed rjd3 wrappers declare EUPL terms. Existing
Persephone code declares MIT. This inventory does not authorize copying or
translating EUPL source under MIT. Before implementing source-derived numerical
code, record its source revision, applicable notices and distribution terms;
review Rcpp/numerical-library licensing as part of the build choice. Independent
algorithm authorship also needs recorded references/provenance. The source
mapping/licensing decision and full support-matrix review remain item 16 gates.

The corpus is intentionally small. It does not yet cover missing values,
LS/TC, fixed coefficients, full external contexts, benchmarking, unusual modes,
all templates, near-boundary selection or difficult SEATS models. Extend it
with independent invariants and domain-reviewed cases before claiming item
18's full acceptance matrix or any production native compatibility.
