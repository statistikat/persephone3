# Extract regression coefficients from a Persephone object

This function retrieves the coefficients of all regression variables of
a fitted Persephone model: outliers, trading days, Easter, leap year,
mean, user defined variables and any other regressor present in the
model. It calculates standard errors, t-statistics, and p-values for
estimated coefficients.

## Usage

``` r
getRegressors(x)
```

## Arguments

- x:

  A Persephone object (e.g., `x13Single` or `tramoseatsSingle`) with
  completed model run

## Value

A data frame with columns:

- name:

  Variable name (e.g., "AO1953.7", "td1", "easter", "leap")

- type:

  Kind of regressor, as reported by the model (e.g. "TD", "EASTER",
  "LP", "MEAN", "USER"); all outlier types (AO, LS, TC) are summarized
  as "outlier"

- value:

  Coefficient estimate

- stde:

  Standard error (NA for non-estimated types)

- t:

  T-statistic for estimated coefficients

- pvalue:

  Two-sided p-value for estimated coefficients

The result additionally carries the joint F test of the trading day
regressors (when the model has any) as the attribute `ftests` (one-row
data frame from the internal
[`tdJointFTest()`](https://statistikat.github.io/persephone3/reference/tdJointFTest.md)
helper).

## Details

One regression variable can carry several coefficients (trading days:
one per weekday), so the table has one row per coefficient. Standard
errors come from the covariance matrix (`bvar`), which only covers the
estimated coefficients; fixed ones (e.g. manually specified outliers
outside the estimation range) get `NA`. Outliers are recognized by the
type of their regression variable and not by their name, so that no
other regressor can be mistaken for one.

## Note

This is an internal function used by diagnostics and reporting.

## Examples

``` r
# obj <- perX13(AirPassengers, "rsa3")
# obj$run()
# getRegressors(obj)
```
