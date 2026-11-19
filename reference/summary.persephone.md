# Summary of a Seasonal Adjustment

Prints a summary of a run of a single series: the frequency, span and
length of the series, the model definition of the regression ARIMA model
(ARIMA specification, transformation, leap year and trading day effects,
Easter effect, number of outliers and the result of the combined
seasonality test), the estimates of the ARIMA parameters, the regression
coefficients with their standard errors, t values and p-values in one
table per group (outliers, trading day regressors, mean and leap year,
user-defined regressors), the joint F test of the trading day
regressors, the decomposition filters and, at the end, the summary
indicators Q and Q-M2 of the X-11 M-statistics and the overall quality
indicator.

## Usage

``` r
# S3 method for class 'persephone'
summary(x, ...)
```

## Arguments

- x:

  a `persephone` object of a single series (created with
  [`perX13()`](https://statistikat.github.io/persephone3/reference/perX13.md)
  or
  [`perTramo()`](https://statistikat.github.io/persephone3/reference/perTramo.md))
  for which `run()` has been called.

- ...:

  currently ignored.

## Value

`x`, invisibly.

## Details

The tables reuse the output of
[`getRegressors()`](https://statistikat.github.io/persephone3/reference/getRegressors.md)
and the diagnostic row of
[`diagnosticsTable()`](https://statistikat.github.io/persephone3/reference/diagnosticsTable.md),
so the summary is consistent with `obj$getRegressors()` and
`diagnosticsTable(obj)`.

In the trading day table the regressors are labelled with their names
(the column names of the user-defined regressors passed to the `user`
argument of
[`perX13()`](https://statistikat.github.io/persephone3/reference/perX13.md)
or
[`perTramo()`](https://statistikat.github.io/persephone3/reference/perTramo.md);
the generic group names of the calendar regressors are shown as the days
Monday to Saturday). When the trading day regression consists of the six
day regressors, the effect of the reference day Sunday is derived from
the six effects and shown as an additional row "Sunday (derived)": minus
their sum for regressors given as contrasts against Sunday, as generated
by the trading day modules of JDemetra+ and by
[`genTd()`](https://statistikat.github.io/persephone3/reference/genTd.md),
minus one seventh of their sum for regressors centered with respect to
the mean day (the two cases are distinguished automatically when the
design matrix of the estimation is available).

The leap year effect counts as included ("Leap year: Yes") if a
dedicated leap year regressor is part of the regression or if the series
is log transformed with a leap year or length-of-month adjustment
(`transform.adjust = "leapyear"` or `"lom"`). A user-defined regressor
given to the `user` argument of
[`perX13()`](https://statistikat.github.io/persephone3/reference/perX13.md)
or
[`perTramo()`](https://statistikat.github.io/persephone3/reference/perTramo.md)
and named like a leap year is such a regressor; the further user-defined
regressors are shown in a table "User-defined regressors".

The "Identifiable seasonality" row shows the result of JDemetra+'s
combined seasonality test (the same source as the `seasonality` column
of
[`diagnosticsTable()`](https://statistikat.github.io/persephone3/reference/diagnosticsTable.md)):
typically "Yes" when the test detects identifiable seasonality and "No"
otherwise; other reported values (such as "Uncertain" or "Undefined")
are shown unchanged, a missing result as "Not available".

Q and Q-M2 are not single M-statistics but composite quality indicators
built from the X-11 M-statistics M1 to M11: Q is their weighted mean,
Q-M2 the same composite computed without M2. The individual M-statistics
range from 0 to 3 and are accepted below 1, so Q and Q-M2 indicate a
satisfactory quality of the seasonal adjustment when they are below 1.
See the [JDemetra+
documentation](https://jdemetradocumentation.github.io/JDemetra-documentation/pages/reference-manual/sa-output-X13.html).

## Examples

``` r
if (FALSE) { # \dontrun{
data(AirPassengers, package = "datasets")
obj <- perX13(AirPassengers, "rsa3")
obj$run()
summary(obj)
} # }
```
