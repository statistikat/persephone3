# Extract ARIMA regression coefficients from a Persephone object

This function retrieves all regression and ARIMA coefficients from a
fitted Persephone model, including outliers, trading day effects, Easter
effects, and other regressors. It calculates standard errors,
t-statistics, and p-values for estimated coefficients.

## Usage

``` r
getArimaCoef(x)
```

## Arguments

- x:

  A Persephone object (e.g., `x13Single` or `tramoseatsSingle`) with
  completed model run

## Value

A data frame with columns:

- name:

  Variable name (e.g., "AO1953.7", "td", "easter")

- value:

  Coefficient estimate

- stde:

  Standard error (NA for non-estimated types)

- t:

  T-statistic for estimated coefficients

- pvalue:

  Two-sided p-value for estimated coefficients

## Note

This is an internal function used by diagnostics and reporting.

## Examples

``` r
# obj <- perX13(AirPassengers, "rsa3")
# obj$run()
# getArimaCoef(obj)
```
