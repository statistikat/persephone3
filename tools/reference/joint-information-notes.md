# Joint profile-ML regression/ARMA inference

Slice 20c.2a.2 implements a joint inverse observed Hessian for free regression
and physical ARMA coefficients with innovation scale profiled out. Its Schur
construction is derived independently from the Gaussian GLS first-order
condition. The [core contract](../native-tests/regression-information-contract.md)
explains the formula, correction-response derivatives, scales, conditioning,
unavailable states and the explicitly separate conditional estimator.

`verify-joint-information.R` freezes prospective acceptance budgets before
recording results: `2e-5` relative covariance error in standardized coordinates,
`2e-5` inverse-identity defect and `2e-7 * (1 + abs(B))` for each GLS sensitivity.
The full Gaussian likelihood is differentiated in all regression/ARMA coordinates
and inverted directly; the oracle never calls the production Schur code.

The [2026-11-14 records](information/2026-11-14-regression-joint/metadata.R) pass
1,985 checks over all 64 quarterly/monthly cases, including transformations,
windows, gaps, means/drifts and fixed/free coefficients. Maximum errors are
`2.636e-6` for covariance, `4.982e-6` for the inverse identity and `8.352e-10` for
scaled GLS sensitivity. All frozen reference artifacts remain unchanged.

The same run includes 128 prepared pinned JDemetra reference fits and three
warmed timing rounds with 9,365 calls including warm-up/calibration. Median sums
of per-fit times over the 64 cases are:

| Prepared workload | Seconds |
| --- | ---: |
| Native fit + conditional GLS/profile-ML ARMA covariance | 56.0480 |
| Native fit + joint profile-ML covariance | 64.0920 |
| JDemetra X-13 fit and reported covariance | 0.205697 |
| JDemetra TRAMO fit and reported covariance | 0.190499 |

Batches target at least 50 ms, with alternating engine order and two actual
native workers. Setup, loading/JVM startup and explicit pre-batch collection
are excluded; natural in-batch collection remains included. The covariance
estimators and API output workloads differ. In particular, JDemetra's regression
covariance is conditional and is **not a numerical target for joint covariance**.
These timings document the substantial remaining performance gap, not a runtime
improvement or a comparison of identical operations. Source/runtime hashes and
all [timing rows](information/2026-11-14-regression-joint/timings.csv) are retained.
