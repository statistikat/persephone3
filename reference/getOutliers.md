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

  Standard error of the coefficient

- t:

  T-statistic

- pvalue:

  Two-sided p-value

- date:

  Date of the outlier as Date object

- coef.type:

  Type of coefficient (usually "ESTIMATED")

## Note

This is an internal function used by other get\* functions and
diagnostics.

## Examples

``` r
# obj <- perX13(AirPassengers, "rsa3")
# obj$run()
# getOutliers(obj)
```
