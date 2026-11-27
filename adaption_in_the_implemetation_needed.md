# Persephone implementation changes needed

Reviewed 2026-11-23 on `rkalos_backend`, based on `3946e9b`. The development dependency points to the matching local rkalos implementation commit `50eee9f`.
Refreshed 2026-11-27 on `rkalos_backend` at `789f684`; all five review items remain fixed and were re-certified through the M8 release review. The historical verification section is retained as recorded.

## Status refresh (2026-11-27)

The M6 reporting/diagnostics scope closed, and the **M7 cutover landed**: the native
backend is the **default** (`8ec3036`, item 27) — a fresh Java-free install (469-entry
library, all six Java packages absent) runs the ordinary-defaults lifecycle end to end
(`STEPS=41 GAPS=0` in the M8 report), with the Java reference backend retained as an
opt-in parity tool. M8 release review on the pushed artifacts: native suite
**4,451/0**, parity Pass A **6,473/0** (+5 recorded deferrals), Pass B **5,781/0**,
`R CMD check` **Status: OK** with vignettes, and the published `Remotes:` install chain
proven end to end through `install_github` from GitHub. The dependency now tracks
`rkalos` 0.1.0.9031+ through the GitHub chain (core `ec17543`; core `6fee68c`
lands via the in-flight rkalos re-pin).

## Remaining items (2026-11-27)

1. **Branch integration**: `master` is an ancestor of `rkalos_backend` (146 commits
   behind); merging and publishing the branch is the maintainer's release step.
2. **Downstream re-certification** when the rkalos re-pin (D4 drift class ≤3.1e-08 on
   arming shapes; gapped/k=0 fixtures byte-identical) lands: rerun the native suite,
   Pass A/B and the lifecycle on the new backend, with any moved self-pins re-qualified
   the same way as the rkalos/pykalos re-pin. — **executed 2026-11-27: pushed-chain
   re-cert green** (fresh GitHub install: `persephone3` at `336858f` + `rkalos` main
   `4fde8b0` = `0.1.0.9032` on core `6fee68c`, vendored SLICOT compiled ON; 326-entry
   Java-free library, six reference packages absent): native suite 17 files **4,451/0**,
   Pass A 39 files **6,473/0** with the same five recorded deferrals, Pass B
   **5,781/0** under the bridge JVM (capture flags proven), lifecycle **STEPS=41
   GAPS=0** — all per-file tables zero movement against the M8 baselines (every tuple
   SAME, no self-pin moved). Spot witnesses equal the recorded values: estimated
   `RSA1` `td.ftest.sa` 2.8406525858195382 (p 0.012278267499684494; rel 6.903e-09 to
   the frozen oracle, inside the documented 9.3e-09), automatic `RSA3`
   2.919299467916046 / 0.01039020933615779, likelihood 244.69648645086414 / ssq
   0.17660260497368421 with fixture-pinned ARMA coefficients identical, and all eight
   delivered-component dumps sha256-identical to the M8-era records.
3. **Hosted platform qualification**: Java-free install and native suites on
   macOS/Windows hosted runners (Linux fully certified here).
4. **Publication**: CRAN/GitHub release actions stay maintainer-side.

## Accepted numeric divergence classes (2026-11-27, M12 rsa3 lane)

Recorded per the M12 assessment (AirPassengers rsa3 reference example, both
backends at the aligned template horizons): final-table ulp residue
<= 5.12e-12 abs (<= 1e-14 rel) with bit-exact preadjustment/residuals/model;
TD F-test diagnostics at fc12 measure 2.65e-08 (bridge domain, inside the
2e-8*(1+|ref|) band) / 1.06e-07 (default domain — where the rjd3 tables
themselves move 2.22e-07 between math domains, so no implementation can meet
the band there at fc12) — the documented maTest ~10^4 amplification of the
table ulps, inside the ratified D4 drift class (<= 3.1e-08). The fc0-qualified
bit-exact layers are unchanged; nothing was relaxed. Full tuples and the
qualification ledger: the dated M12 entry in `IMPLEMENTATION_STATUS.md`.

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
