# Extract outlier information from a Persephone object

This function retrieves outlier details (AO, LS, TC) from a fitted
Persephone model, including coefficients, standard errors, t-statistics,
and p-values. It processes the raw output from X-13ARIMA-SEATS or
TRAMO/SEATS into a clean data frame with formatted dates and names.

## Usage

``` r
getOutliers(x)
```

## Arguments

- x:

  A Persephone object (e.g., `x13Single` or `tramoseatsSingle`) with
  completed model run

## Value

A data frame with columns:

- name:

  Formatted outlier name (e.g., "AO1953.7" for additive outlier)

- type:

  Outlier type: AO (Additive Outlier), LS (Level Shift), or TC
  (Transient Change)

- coef.value:

  Estimated coefficient value

- stde:

  Standard error of the coefficient (`NA` if `coef.type` is "FIXED")

- t:

  T-statistic, `coef.value / stde`

- crit.value:

  Critical value of the outlier test applied by JDemetra+: the value
  from the `outlier` specification if positive (x13: per outlier type,
  falling back to `defva`), otherwise the threshold computed internally
  (see Details)

- pvalue:

  Two-sided p-value of `t` under a t distribution with
  `neffectiveobs - nparams` degrees of freedom

- date:

  Date of the outlier as Date object

- coef.type:

  Type of coefficient (usually "ESTIMATED")

- autoout:

  Logical, TRUE for outliers detected automatically by X-13ARIMA-SEATS
  or TRAMO/SEATS, FALSE for outliers that were specified manually with
  the `outliers` argument

- recentout:

  Logical, TRUE if the outlier falls into one of the last observations
  of the series: the last 5 observations for monthly series, the last 3
  for quarterly series, the last 1 for all other frequencies (e.g.,
  annual). Outliers before the beginning of the series or before the
  estimation span are never flagged.

## Details

X-13ARIMA-SEATS and TRAMO/SEATS do not directly report standard errors,
t-statistics, or p-values for outlier effects. These quantities are
computed here from the estimated coefficients and the covariance matrix
(`bvar`), following the same methodology as rjd3toolkit. Results can
therefore be verified against `summary(x$output$preprocessing)$xregs`.

Because outliers are selected only when their absolute t-statistic
exceeds the critical value defined in the `outlier` specification, the
reported p-values are subject to selection effects. They should be
viewed as descriptive statistics for the fitted model rather than as
formal evidence of adjustment quality.

The output of a run does not distinguish manually specified from
automatically detected outliers. `autoout` therefore compares the
`type`/`date` pairs of the run output with the manually specified
outliers read from the model specification
(`regarima$regression$outliers` for x13, `tramo$regression$outliers` for
tramoseats). `recentout` is evaluated independently of `autoout`, so a
manually specified outlier close to the end of the series is flagged as
well.

If the `outlier` specification defines no critical value, JDemetra+
computes one internally from the number of observations `n` of the
estimation domain: x13 uses `max(advancedComputer(0.05)(n), 2.8)`, an
extreme-value approximation for the 95% quantile of the maximum of `n`
standard normal variables; tramoseats uses
`max(simpleComputer()(n), 2)`, i.e. 3.3 up to 50 observations, rising by
0.0025 per observation up to 4.3. Both formulas are reproduced in
[`calcOutlierCrit()`](https://statistikat.github.io/persephone3/reference/calcOutlierCrit.md)
and verified against the jdplus implementation. Note that during
automatic model identification the engines may lower this value once (by
about 14% for x13 and 12% for tramoseats); that reduction is not
reported in the run output and is therefore not reflected in
`crit.value`.

## Note

This is an internal function used by other get\* functions and
diagnostics.

## Examples

``` r
# obj <- perX13(AirPassengers, "rsa3", outliers = "AO1960.5")
# obj$run()
# getOutliers(obj)                  # all outliers
# subset(getOutliers(obj), autoout) # without the manually specified one
```
