# Persephone implementation changes needed

Status checked 2026-11-23 at `4b51d49` on `rkalos_backend`. Persephone owns the `persephone3` application API, R6 state, backend routing, result presentation, and dashboards. Native numerical changes belong in `rkalos` or `kalos`.

## P1 — Make the existing reporting methods work for native fits

**Needs work now; full reporting parity is planned for M6.** Make the reporting methods work with the values available from current native fits. M6 covers complete diagnostics and product parity. [Reporting roadmap](https://github.com/statistikat/kalos/blob/24e4452/docs/NUMERICAL_ROADMAP.md#L607)

`perX13(AirPassengers, template = "rsa3", backend = "native")$run()` succeeded in a scratch installation. `getRegressors()`, `getOutliers()`, `summary()`, and `print()` then failed at an assertion requiring a JDemetra result class. `diagnosticsTable()` returned successfully. The summary also assumes raw JDemetra structures when naming the decomposition, so removing the assertions alone would leave incorrect or missing output. [Result getters](R/get_functions.R#L249) · [Summary](R/summary.R#L77) · [Print diagnostics](R/printDiagnostics.R#L22) · [Native result model](R/backend_native.R#L660)

Dispatch extraction and presentation by backend using the normalized native result. Expose the model, mode, and outlier or regression data the route actually provides. Use typed missing values and explicit availability for unavailable statistics, such as standard errors or JDemetra quality metrics. Reuse the diagnostic projection for the summary and print paths. Test both native routes with and without outliers, plus reference X-13 and TRAMO-SEATS to preserve their current results.

## P1 — Validate complete native specifications before committing an update

**Needs work now.** Reject invalid updates before changing an existing fitted object. M6 includes end-to-end verification of specification updates across workflows. [Product qualification](https://github.com/statistikat/kalos/blob/24e4452/docs/NUMERICAL_ROADMAP.md#L632)

After a successful native fit, `updateParams(spec = list(bad = TRUE))` returned successfully and replaced the fitted specification. The next `run()` refused it. `prepare_specification_update()` checks the complete specification’s class for `rjd3` only, and the caller commits the proposed object immediately. Native incremental updates check allowed names but can store values the fit later rejects. This breaks the documented guarantee that invalid updates leave object state unchanged. [Specification preparation](R/specification.R#L61) · [Native incremental updates](R/backend_native.R#L308) · [R6 update commit](R/persephone.R#L89)

Validate the proposed native class, method, template route, frequency, and field values before committing. Accept existing valid native specification envelopes and apply the same validation to named changes. Use the same validator in construction, single-series updates, and hierarchy preflight. Test that a failed update preserves parameters, the previous result, and its stale state, including failure in a later hierarchy node.

## P2 — Use a method-aware capability response and a supported rkalos entry point

**Needs work now.** Scope the application response to the requested method and coordinate the supported fit interface with rkalos. Neither change has a recorded later milestone.

`backend_capabilities("native", "x13")` and `backend_capabilities("native", "tramoseats")` both returned the same rkalos block with automatic X-13 enabled and an empty `methods` vector. The native branch bypasses the method filter, and the actual fit router refuses native TRAMO-SEATS. Separately, both X-13 lanes resolve unexported functions with `get0()` while the hard dependency is versioned only as `rkalos (>= 0.1.0)`. [Capability routing](R/backend.R#L42) · [Fit router](R/backend.R#L176) · [Private entry point lookups](R/backend_native.R#L552) · [Dependency version and pin](DESCRIPTION#L18)

Report method scope at the Persephone boundary while preserving existing unrestricted `fit` semantics and the explicit route-specific gates. Migrate to the versioned rkalos adapter after it is available and update the minimum dependency or interface requirement along with the development pin. Test capability queries for both methods and actual fit calls with the installed package. [rkalos interface work](https://github.com/statistikat/rkalos/blob/7d72e6c/adaption_in_the_implemetation_needed.md)

## P1 — Align the application tests with the installed backend

**Partially fixed; needs work now and at M6.** The dependency and provider pins now agree with the parity tests.

**Fixed in M5.** `DESCRIPTION` now pins `rkalos@49e2b11`, which supplies automatic-fit schema 3. The CI provider-tooling refs also advanced to the matching Kalos revision. [Dependency pin](DESCRIPTION#L53) · [Parity assertions](inst/tinytest/test-parity.R#L565)

**Needs work now.** With the native backend selected, omitting the template passes the entire default vector to the validator and fails. Explicit `"rsa3"` passes. The existing refusal test reflects actual behavior. Decide the omitted-template contract and make construction, documentation, and tests agree. Enable the full workflow on `rkalos_backend` and extend the Java-free checks to reporting and stale state. [Constructor](R/perX13.R#L173) · [Integration test](inst/tinytest/test-backend-integration.R#L20) · [Full check triggers](.github/workflows/check-full.yaml#L7)

**Planned for M6.** Qualify the macOS and Windows native installation jobs, including the Windows CBLAS/LAPACKE provider. [Platform qualification record](IMPLEMENTATION_STATUS.md#L915)

## Original review checks at `bf996ff`

The native fit and state probes used current Persephone sources, a scratch rkalos build, and a scratch Persephone manifest with the unavailable dashboard-only `shinycssloaders` import omitted. Dashboard behavior, the reference backend, and hosted CI were outside this review run.
