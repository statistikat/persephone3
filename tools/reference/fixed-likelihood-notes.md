# Fixed likelihood windows and the pinned log adjustment

Evidence from direct JD+ 3.9.0 APIs, checked on 2026-11-13. This records the
bounded compatibility convention for item 20a.3. It does not define a universal
density for improper initial conditions or qualify coefficient estimation,
regression effects, automatic transforms, outliers or seasonal adjustment.

## Reproduction

```sh
R_LIBS=/tmp/persephone3-status-lib Rscript tools/reference/verify-native-likelihood.R
R_LIBS=/tmp/persephone3-status-lib Rscript tools/reference/verify-native-likelihood.R --native --source
```

The script checks the pinned R/package/JVM versions, JAR SHA256 values and
original corpus hashes before running. It writes no fixtures, manifests or
tolerances. It calls `regarima_fast()`/`tramo_fast()` directly, without a
Persephone adapter. Source/build provenance remains as recorded in
[NATIVE_PROVENANCE.md](NATIVE_PROVENANCE.md). Only public API calls and installed
wrapper documentation were used; no Java or R numerical implementation was read,
copied or translated for this investigation.

`--native` uses an installed development package selected through `R_LIBS`;
`--source` additionally uses `pkgload` on an already compiled source tree without
recompiling. It exercises the internal `native_fixed_likelihood()` wrapper.

The main matrix contains 852 direct fixed fits: periods 4/12; all `d,D` in
`0:2`; white innovations, ARMA coefficients `(AR=0.3, MA=0.2)`, and airline
coefficients `(MA=0.2, seasonal MA=-0.4)`; complete, leading-missing, internal-gap
and edge-missing inputs; full and explicitly selected windows. Configurations
outside the native pilot's declared 32-state dimension bound are excluded by
dimension before evaluation. Identity-transform controls cover every `d,D` and
frequency combination. All returned ARIMA coefficient values and `FIXED` states
are checked; no mean/drift, other regressors or estimated ARIMA coefficients are
allowed. Innovation variance is profiled, so JD+'s parameter count is one.

The matrix passes 9,377 assertions, with maximum absolute log-adjustment error
`5.69e-14`. Another 44 API calls and 333 checks verify individual Jacobian
membership by perturbation and distinguish explicit JD+ estimation selectors
from passing a selected input series directly.

Native comparison passes all 426 fixed-window cases and 7,243 additional checks.
Maximum absolute raw/adjusted profiled-likelihood discrepancy is `3.15e-12`;
profiled-variance discrepancy is `8.89e-15`. The checks also require exact selected
values and time domain, original index maps, local/original Jacobian membership,
term-series domains, availability and the named fresh-prior operation. No native
rejection is treated as a passing comparison. These are bounded fixed-model
checks under the existing `2e-8` absolute-plus-relative comparison rule, not
qualification of every missingness pattern or an estimated model.

## Coefficient-sign bridge

All coefficient descriptions above use the native/R convention
`(1-phi*B)y = (1+theta*B)e`, and similarly for seasonal factors. JD+'s `phi` and
`bphi` arguments name the AR polynomial coefficients themselves, so the public
oracle receives their negatives. MA signs agree. Thus native AR `+0.3` is
compared to JD+ `phi=-0.3`, while native MA `+0.2` remains JD+ `theta=+0.2`.
The returned fixed coefficients are asserted after this explicit conversion.

Public `sarima_properties()` autocovariances independently guard the bridge:
`phi=-0.3` has lag-one correlation `+0.3`; `bphi=-0.25` at period 4 has lag-four
correlation `+0.25`; `theta=+0.2` has lag-one correlation `0.2/(1+0.2^2)`.
Silently passing native AR coefficients unchanged to the oracle would compare
different models even though their log-Jacobian adjustments still agree.

## Exact observation-selection rule

First select the inclusive user-requested period window from the original
series. Preserve its missing values and its mapping to original indices. Let
the selected series be `x[1:n]`, its frequency be `s`, and define

```
k = d + s*D
a = first index i in the selected window with observed x[i]
b = last index i in the selected window with observed x[i]
J = { i : a+k <= i <= b and x[i] is observed }
```

For positive observed data and an explicit log transform, the pinned convention
is

```
log adjustment             = -sum(log(x[J]))
adjusted profiled likelihood = raw profiled likelihood on log(x) + log adjustment
```

For the identity transform, the adjustment is zero. Missing positions contribute
neither a value nor an imputed Jacobian term. The same selection holds for every
tested fixed AR/MA/seasonal-MA configuration; it depends on the selected calendar
domain and integration orders, not those coefficient values.

The internal compatibility-policy name is `jdplus_3.9.0_calendar_window`.
Retain the selected local indices `J`, their original-series indices, and the
per-observation terms separately. Selecting a window precedes transformation
and model initialization. A likelihood over a selected window uses a fresh
model prior; it does not condition on excluded earlier observations.

The cutoff advances `k` calendar positions from the first observed value. It
does **not** discard the first `k` observed values, and it does **not** select
only observations receiving finite Kalman updates. For example, monthly
`d=0,D=2` with missing positions 15, 16 and 25 has 117 observations and 93 finite
likelihood contributions, but 95 Jacobian terms: observed positions 25 through
120. Observation 27 is the second observed value of its seasonal phase because
15 is missing; it identifies a diffuse direction and still belongs to `J`.

Individual-value probes multiply observations on both sides of the cutoff by
`exp(0.125)`. The adjustment changes by exactly `-0.125` only for indices in
`J`. For a monthly selected window with its first three values missing and
`d=1`, local index 4 is excluded and local index 5 is included. These checks
establish membership separately from matching an aggregate sum.

## Deterministic inputs and window indices

For `t=1:120`, the original positive input is

```
exp(2 + 0.15*sin(t/9) + 0.02*cos(t*sqrt(2)) + 0.001*t)
```

It begins in period 1 of 2000. The full window selects indices 1:120. The middle
window is 2002 period 1 through the last period of 2007, inclusive: original
indices `(2*s+1):(8*s)`. No fitted value enters this formula or the index maps.

Missing masks are expressed relative to the selected window:

| Mask | Missing local positions |
| --- | --- |
| Complete | None |
| Leading | 1, 2, 3 |
| Internal | `s+3`, `s+4`, `2*s+1` |
| Edges | 1, 2, 3, `n-1`, `n` |

Middle-window cases also have missing values immediately before and after the
window in the original input. Those excluded values must not affect a direct
window calculation. The main comparisons pass only the selected series to JD+.

## Why an explicit JD+ selector is a different contract

With `set_estimate(type="Between", d0="2002-01-01", d1="2008-01-01")`, the
direct wrappers report likelihood over 2002 period 1 through the last period of
2007. The upper date is the exclusive boundary for this tested selection.
Using `d1="2007-12-31"` instead excludes the final quarter/month: the complete
selected sample then has 23/71 observations rather than 24/72. Callers should
not infer an inclusive period endpoint from that date string.

Even with every ARIMA coefficient fixed, this estimate selector changes the
reported likelihood domain; it is not merely a parameter-estimation option.
For complete and internal-gap windows, its raw and adjusted likelihoods match
the direct-window call. For leading or edge missingness, the raw likelihood
still matches, but its adjustment anchors the calendar cutoff at the requested
window start before trimming the window's leading NAs. For tested `d=1`, it
sums observed local positions 2 onward. A direct selected-series call trims
the leading NAs first and sums from local position 5 when 1:3 are missing.
The executable checks preserve this discrepancy explicitly.

The selector also throws `Missing values outside the estimation domain` for
the deliberately missing observations immediately outside that window. Passing
the selected series directly succeeds. Consequently the native inclusive-period
window wrapper is compared to a direct JD+ call on the selected series; it must
not silently promise identical behavior to every JD+ selector option.

## Limits and next integration gates

The compatibility adjustment is separate from the raw fixed-parameter
likelihood and from the native transform primitive's all-observation Jacobian.
Changing those primitives to discard terms would erase information needed for
other estimator conventions. Both raw and adjusted values should remain visible
in the internal result, together with the named policy and exact contributing
indices.

The tests concern fixed, no-regression specifications and direct finite windows.
They do not qualify regression determinants, estimated coefficients, automatic
model/transform selection, multiplicative preadjustments, or every JD+ estimation
selector. Any extension of the adjustment policy needs its own pinned cases.
The diffuse-coordinate normalization and distinction between diffuse/marginal
likelihood objectives are recorded in [diffuse-notes.md](diffuse-notes.md).
