# Persephone implementation changes needed

Reviewed 2026-11-23 on `rkalos_backend`, based on `3946e9b`. The development dependency points to the matching local rkalos implementation commit `50eee9f`.

## P1 — Make the existing reporting methods work for native fits

**Fixed for both current routes. Full diagnostics and reporting parity remain planned for M6.** Getters, summary, print and the diagnostic projection read the fitted native result. The automatic route reports model orders, effective mode and detected variables. The restricted route reports supplied coefficients. Unknown dates and unavailable inference remain typed missing values. Tests cover fits with and without outliers, including a fixed pulse and a detected outlier. [Getters](R/get_functions.R) · [Summary](R/summary.R) · [Reporting regressions](inst/tinytest/test-native-reporting.R) · [M6 reporting scope](https://github.com/statistikat/kalos/blob/24e4452/docs/NUMERICAL_ROADMAP.md#L607)

## P1 — Validate complete native specifications before committing an update

**Fixed.** Constructors, complete and incremental updates, and hierarchy preflight validate the native class, route, method, calendar and request values before committing state. Existing restricted drafts can be configured in stages. Tests exercise valid envelopes, rejected changes to current and stale fits, and a refusal in a later hierarchy component. [Specification updates](R/specification.R) · [Route validation](R/backend_native.R) · [State regressions](inst/tinytest/test-native-reporting.R)

## P2 — Use a method-aware capability response and a supported rkalos entry point

**Fixed locally across both packages.** Method-scoped queries advertise the two qualified X-13 routes and return both gates disabled for TRAMO-SEATS. The unrestricted fit flag retains its existing meaning. The router calls the documented rkalos exports and requires version `0.1.0.9001`. The remote development pin will resolve for other installations once rkalos commit `50eee9f` is published. [Capabilities](R/backend.R) · [Fit routing](R/backend_native.R) · [Dependency](DESCRIPTION)

## P1 — Align the application tests with the installed backend

**Fixed for current application behavior. macOS and Windows qualification remain planned for M6.** Omitting the native X-13 template selects the documented `rsa3` default. Both full-check branch filters include `rkalos_backend`. The Java-free workflow now exercises reporting and rejected updates on both routes, alongside the existing numerical gates. [Constructor](R/perX13.R) · [Full workflow](.github/workflows/check-full.yaml) · [Java-free checks](tools/verify-nojava.R) · [M6 platform qualification](IMPLEMENTATION_STATUS.md#L915)

## Verification of the current fixes

The installed application passed **30/30** native reporting and state assertions, **93/93** backend assertions, **32/32** integration assertions and **180/180** diagnostic assertions. Live reference X-13 and TRAMO-SEATS fits both completed their getter, summary and print checks. The fully isolated Java-free check passed on checksum-pinned OpenBLAS 0.3.34. On this host R rounded one expected decimal literal to a neighbouring double. Comparing 17-digit round-trip text preserves the exact gate. The pre-change and updated applications produced the same three AirPassengers values with the pinned provider.

The full parity suite passed **414/422** assertions with rjd3toolkit 3.9.1. All eight failures compare direct `rjd3x13::x13_fast()` output byte-for-byte against frozen reference files for `rsa0`, `rsa1` and `rsa2c`. On macOS ARM64, moving-average coefficients differed in eight of nine template/series combinations, with regression coefficients also differing in three. The same differences reproduced when calling rjd3x13 directly with Temurin 21.0.11. The native decision checks, numeric tolerance checks and AirPassengers checksums passed. Hosted workflows were not run. [Parity tests](inst/tinytest/test-parity.R) · [Reference captures](inst/tinytest/fixtures/native/refusals-m4gap)

## Original review checks at `bf996ff`

The native fit and state probes used current Persephone sources, a scratch rkalos build, and a scratch Persephone manifest with the unavailable dashboard-only `shinycssloaders` import omitted. Dashboard behavior, the reference backend, and hosted CI were outside this review run.
