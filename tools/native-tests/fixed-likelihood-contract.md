# Fixed-model likelihood inputs and windows (20a.3)

`persephone3:::native_fixed_likelihood()` is an internal, serializable evaluator
for fixed monthly/quarterly SARIMA parameters. It joins the validated input
contract to the exact diffuse kernel. It does not estimate ARMA or regression
coefficients and is not a native seasonal-adjustment constructor.

## Window and model semantics

`estimation_start` and `estimation_end` are inclusive `c(year, period)` pairs.
Omitting an endpoint uses that endpoint of the input. Endpoints must be whole,
valid periods inside the supplied univariate `ts`; reversed and empty windows
are rejected. Selection occurs before transformation. Values outside the
window do not condition the model, and invalid unused values do not affect the
evaluation. Missing calendar positions inside the window are preserved.

Initialization is a **fresh prior at the selected span start**. The retained
source domain, estimation domain and source-row map make that choice explicit.
This is equivalent to evaluating a directly supplied window of the data. It
does not claim equivalence to every JD+ preprocessing estimation-span selector:
the pinned selector has different leading-missing Jacobian behavior and can
reject missing values outside its estimation span.

AR, MA and seasonal coefficients remain fixed. The sign conventions and
state limits are those in [diffuse-state-space-contract.md](diffuse-state-space-contract.md).
`mean` is the level mean for a stationary model; integrated models require zero
mean. Drift, regressors, parameter optimization and automatic transformation
selection are not implemented. Unsupported arguments cannot be silently ignored.

## Likelihood and Jacobian

Both the fixed-variance and variance-profiled transformed-scale objectives are
retained without changing the kernel's initial-coordinate diffuse measure.
The result separately reports adjusted objectives and the terms added to them.
Unavailable diffuse objectives remain unavailable; the zero-scale boundary
remains explicitly infinite with its existing status.

For `transformation = "none"`, the adjustment policy is `identity` and all
terms are zero. For `"log"`, positive observed values are required in the
selected window. Let `k = d + period * seasonal_d`, and let `b` be the first
observed calendar position in that window. The policy named
`jdplus_3.9.0_calendar_window` includes precisely the observed positions
`i >= b + k`, contributing `-log(y[i])`. Missing positions have zero terms.
The selection uses calendar positions, **not** the first `k` observations or
the diffuse-pivot positions. For a stationary model, `k = 0` includes every
observation and gives the ordinary change-of-variable density.

For integrated models the diffuse measure is improper. This named adjustment
records pinned JD+ direct-window behavior; it is not asserted to define a
universal original-scale probability density. Future regression, drift,
missing-value interpolation and information-criterion work must explicitly
extend the convention rather than silently reuse an unqualified formula.

## Result and evidence

The `persephone_native_likelihood` object includes schema version, fixed model
parameters, domains and row maps, prepared inputs, raw kernel output, objective
statuses, adjustment policy and terms, and time-indexed innovation/rank traces.
It contains no external pointers or JVM state and survives serialization.

`inst/tinytest/test-native-fixed-likelihood.R` checks independent Gaussian and
random-walk identities, quarterly/monthly windows, missing positions, all
supported differencing combinations, invalid inputs and serialization. These
tests run without Java. The optional pinned-reference probe
[`verify-native-likelihood.R`](../reference/verify-native-likelihood.R) checks
fixed direct preprocessing fits and selected native windows without changing
the original reference corpus or its tolerance baselines. Its normalization,
AR sign bridge and estimation-selector findings are recorded in
[fixed-likelihood-notes.md](../reference/fixed-likelihood-notes.md).
