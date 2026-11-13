# Fixed-ARMA regression likelihood conventions (20b.1)

Evidence checked against the pinned rjd3/JD+ 3.9.0 reference on 2026-11-13.
The probes call public `regarima_fast()`, `tramo_fast()`, specification setters
and modelling contexts directly. No numerical implementation was inspected,
copied or translated. Original fixtures, manifests and tolerance rules are
unchanged. The existing artifact pin and source/distribution limits remain in
[NATIVE_PROVENANCE.md](NATIVE_PROVENANCE.md).

## Reproduction and scope

```sh
R_LIBS=/tmp/persephone3-status-lib Rscript tools/reference/verify-regression.R
# Compare the already compiled development package as well:
R_LIBS=/tmp/persephone3-status-lib Rscript tools/reference/verify-regression.R --native --source
```

The main prospective matrix contains **432 cases / 864 direct fixed fits**:
monthly and quarterly inputs; every `d,D` in `0:2`; white or fixed
ARMA(1,1) coefficients `(AR=0.3, MA=0.2)`; complete or leading/internal/trailing
gaps; identity or log transformation; and all-free, mixed fixed/free or all-fixed
regression coefficients. Inclusive full/middle windows are selected directly
before passing the series to JD+. AR polynomial signs are bridged explicitly:
JD+ `phi=-0.3` corresponds to native/R `AR=0.3`; MA signs agree.

The deterministic, distinct regressors are independent formulas:

```
x_fixed[t] = sin(t/7) + 0.1*cos(t*sqrt(2))
x_one[t]   = cos(t/13) + sin(t*sqrt(3))
x_two[t]   = sin(t*sqrt(5)) + 0.2*cos(t/17)
z[t]       = 3 + 0.2*x_fixed[t] + 0.3*x_one[t] - 0.15*x_two[t]
             + 0.04*sin(t*sqrt(2)) + 0.01*cos(t*sqrt(7))
```

Identity inputs use `z`; log inputs use `exp(z)`. Fixed values are
`(0.2,0.3,-0.15)`; mixed cases fix only the first coefficient. Extra context
outside the selected window remains available but does not condition its prior.
Separate future-context perturbations change every future regressor by 1000
and require byte-identical direct fitted descriptions and estimation results.
This qualifies fitted-window isolation, not a forecast implementation.

The main matrix passes **9,506 checks**. Maximum direct/independent discrepancies
are `6.89e-15` for coefficients, `7.40e-13` for profile likelihood, `2.28e-13`
for the sum of squares, `2.15e-14` for coefficient covariance and `5.69e-14`
for the log-Jacobian. These are bounded roundoff checks using the existing
`1e-10` direct and `2e-8` native comparison budgets; no general acceptance
tolerance is inferred from this synthetic matrix.

The compiled native implementation passes the same **432 cases / 8,640 native
checks** against the independent construction. Maximum absolute discrepancies
are `2.10e-14` for coefficients, `2.34e-12` for profile likelihood, `1.14e-13`
for the likelihood at supplied variance 1.7, `2.28e-13` for the sum of squares,
`3.56e-13` for unit-scale coefficient covariance and `1.98e-14` for residual-df
covariance; the Jacobian agrees exactly. The native probe additionally passes
**72 generated mean/drift cases / 288 checks**, **3 explicit alias cases /
9 checks**, and **4 future-context cases / 8 checks**. It checks original
fixture hashes again after all comparisons. `--source` only loads an existing
compiled development tree; it does not build or install the package.

Nonzero seasonal AR/MA coefficient products are covered by the existing fixed
SARIMA likelihood qualification, but are not separately enumerated in this
prospective regression matrix. This matrix adds regression concentration to
the previously qualified fixed covariance operator; it does not qualify
estimated ARMA parameters or their uncertainty.

## Independent density construction

Let `A` be the ordinary/seasonal differencing operator constructed by applying
base R `diff()` to the identity matrix. Let `Sigma` be the stationary covariance
of the differenced errors. For the ARMA(1,1) controls its Toeplitz entries are

```
gamma0 = (1 + theta^2 + 2*phi*theta)/(1 - phi^2)
gamma1 = phi*gamma0 + theta
gamma[h] = phi^(h-1)*gamma1, h >= 1
```

Whiten `A` using a dense Cholesky factor of `Sigma`. Integrate missing original
observations by projecting onto the orthogonal complement of the whitened
missing-coordinate columns. Their QR determinant contributes to the density.
This gives a response and design on exactly `n = n_observed - d - s*D` finite
likelihood coordinates, without using a state recursion, native intermediate,
reference-estimated coefficient or large finite approximation to a diffuse prior.
The independent oracle requires a clear singular-value gap for missing and
regression design columns.

Subtract fixed effects before estimating free effects. If `Xw` is the resulting
whitened free design and `yw` the response, ordinary QR obtains the GLS estimate
and `Q = ||yw-Xw*beta_hat||^2`. With `L` the already-qualified finite-plus-diffuse
log determinant, the concentrated maximum-likelihood objective is

```
sigma2_ML = Q/n
ll_profile = -0.5 * (n*(log(2*pi) + 1 + log(Q/n)) + L)
```

There is **no additional `log|Xw'Xw|` term**: this is concentrated ML over free
regression coefficients, not REML or a likelihood integrating those coefficients
against another flat measure. The existing diffuse initial-history integration
is separate. No regression degrees of freedom are subtracted from the ML scale.

## Covariance, counts and coefficient states

For fixed ARMA and `p` independent free regressors, both pinned preprocessors
report `nparams = p + 1`: regression coefficients plus innovation scale. Fixed
regression coefficients do not increase the count and do not appear in `b` or
`bvar`.

The exposed coefficient covariance **does use a residual-degrees-of-freedom
scale**, even though the likelihood uses ML scale:

```
covariance_unscaled = inverse(Xw'Xw)
covariance_ML       = covariance_unscaled * Q/n
JD+ bvar            = covariance_unscaled * Q/(n-p)
```

These must remain separately named in a native contract. The denominator of
the likelihood cannot be inferred from `bvar`, and a residual-df covariance is
unavailable when `n-p <= 0`. Later estimated-ARMA uncertainty and inferential
degrees of freedom require separate qualification.

The reference silently changes some requested free coefficient states. Six
explicit probes show that both engines convert zero columns, one of two
duplicate columns, and a constant column under `d=1` into `FIXED` zero values.
Only retained columns appear in `b` and `bvar`. This is recorded as reference
behavior, not a requirement for native implementation: native rank deficiency
must be explicit and must not falsely label a nonunique estimate as identified.
An objective over the estimable fitted effect may still exist when individual
coefficients are not identifiable; that distinction needs explicit statuses.

## Mean and drift normalization

An explicitly supplied deterministic column

```
x[t] = t^(d+D) / ((d+D)! * s^D)
```

has `Delta(B) x[t] = 1`. At `d=D=0` it is the ordinary level-mean column;
with integration it is the differenced-mean/drift column. Changing the time
origin adds only homogeneous terms and does not change the fitted coefficient
under the stated flat-history measure.

**144 paired comparisons / 288 direct fits / 576 checks** compare that column
against the JD+ built-in mean across both frequencies, every `d,D` in `0:2`,
white/ARMA models and complete/missing inputs. Maximum discrepancies are
`4.03e-11` for the coefficient, `9.50e-12` for likelihood and `8.15e-13` for
covariance. In the pinned setter, `mean=0, mean.type="Undefined"` activates
this estimated mean; `mean=NULL` removes it. The installed help's unqualified
statement about zero disabling the mean does not describe this explicit
`mean.type` combination.

## Log transformation and estimation windows

Regressors and fixed coefficient effects are on the **transformed response
scale**. A log model subtracts its fixed effects from `log(y)`, while the
Jacobian still uses the original positive observations in the selected window.
For the tested fixed/free regressions, the previously qualified policy remains
unchanged: with first observed local index `a` and `k=d+s*D`, include observed
indices `i >= a+k`, contributing `-log(y[i])`. Regression rank does not move
that cutoff. Fixed effects are not subtracted from the values used to calculate
the Jacobian.

This is the named direct-window convention documented in
[fixed-likelihood-notes.md](fixed-likelihood-notes.md). It does not imply
equivalence to every JD+ estimation-span selector, nor qualify automatic
transformation choice, calendar preadjustments, model selection, outlier search,
estimated ARMA coefficients or seasonal adjustment.
