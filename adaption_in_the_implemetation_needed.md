# Persephone implementation changes needed

Reviewed 2026-11-23 at `bf996ff` on `rkalos_backend`. Persephone owns the `persephone3` application API, R6 state, backend routing, result presentation, and dashboards. Native numerical changes belong in `rkalos` or `kalos`.

## P1 — Make the existing reporting methods work for native fits

`perX13(AirPassengers, template = "rsa3", backend = "native")$run()` succeeded in a scratch installation. `getRegressors()`, `getOutliers()`, `summary()`, and `print()` then failed at an assertion requiring a JDemetra result class. `diagnosticsTable()` returned successfully. The summary also assumes raw JDemetra structures when naming the decomposition, so removing the assertions alone would leave incorrect or missing output. [Result getters](R/get_functions.R#L249) · [Summary](R/summary.R#L77) · [Print diagnostics](R/printDiagnostics.R#L22) · [Native result model](R/backend_native.R#L660)

Dispatch extraction and presentation by backend using the normalized native result. Expose the model, mode, and outlier or regression data the route actually provides. Use typed missing values and explicit availability for unavailable statistics, such as standard errors or JDemetra quality metrics. Reuse the diagnostic projection for the summary and print paths. Test both native routes with and without outliers, plus reference X-13 and TRAMO-SEATS to preserve their current results.

## P1 — Validate complete native specifications before committing an update

After a successful native fit, `updateParams(spec = list(bad = TRUE))` returned successfully and replaced the fitted specification. The next `run()` refused it. `prepare_specification_update()` checks the complete specification’s class for `rjd3` only, and the caller commits the proposed object immediately. Native incremental updates check allowed names but can store values the fit later rejects. This breaks the documented guarantee that invalid updates leave object state unchanged. [Specification preparation](R/specification.R#L61) · [Native incremental updates](R/backend_native.R#L308) · [R6 update commit](R/persephone.R#L89)

Validate the proposed native class, method, template route, frequency, and field values before committing. Accept existing valid native specification envelopes and apply the same validation to named changes. Use the same validator in construction, single-series updates, and hierarchy preflight. Test that a failed update preserves parameters, the previous result, and its stale state, including failure in a later hierarchy node.

## P2 — Use a method-aware capability response and a supported rkalos entry point

`backend_capabilities("native", "x13")` and `backend_capabilities("native", "tramoseats")` both returned the same rkalos block with automatic X-13 enabled and an empty `methods` vector. The native branch bypasses the method filter, and the actual fit router refuses native TRAMO-SEATS. Separately, both X-13 lanes resolve unexported functions with `get0()` while the hard dependency is versioned only as `rkalos (>= 0.1.0)`. [Capability routing](R/backend.R#L42) · [Fit router](R/backend.R#L176) · [Private entry point lookups](R/backend_native.R#L552) · [Dependency version and pin](DESCRIPTION#L18)

Report method scope at the Persephone boundary while preserving existing unrestricted `fit` semantics and the explicit route-specific gates. Migrate to the versioned rkalos adapter after it is available and update the minimum dependency or interface requirement along with the development pin. Test capability queries for both methods and actual fit calls with the installed package. [rkalos interface work](../rkalos/adaption_in_the_implemetation_needed.md)

## P1 — Align the application tests with the installed backend

`DESCRIPTION` pins `rkalos@00c3a88`, whose automatic capability schema is version 2. The checked-in parity suite expects version 3 and a horizon envelope. The reference integration test also expects `perX13(AirPassengers)` to fail after switching the default backend to native, although the default `rsa3` template now constructs and fits. The full workflow’s push and pull-request target filters omit `rkalos_backend`, so a push to the current integration branch does not exercise those tests. [Dependency pin](DESCRIPTION#L53) · [Parity schema assertions](inst/tinytest/test-parity.R#L565) · [Reference integration assertion](inst/tinytest/test-backend-integration.R#L20) · [Full check triggers](.github/workflows/check-full.yaml#L7)

Pin a buildable rkalos revision with the schema the parity suite expects. Update the stale default-template assertion to cover the enabled `rsa3` route and use an explicitly unsupported template for the refusal check. Run the full workflow on the integration branch. Keep the existing Java-free fits and extend them to cover reporting and stale state. Provision or explicitly qualify the CBLAS/LAPACKE provider on Windows. [Java-free fit checks](tools/verify-nojava.R#L45) · [Native workflow](.github/workflows/native-foundations.yaml#L27)

## Review checks

The native fit and state probes used current Persephone sources, a scratch rkalos build, and a scratch Persephone manifest with the unavailable dashboard-only `shinycssloaders` import omitted. Dashboard behavior, the reference backend, and hosted CI were outside this review run.
