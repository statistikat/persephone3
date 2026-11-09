# Get formatted ARIMA coefficient table for printing

This function provides a human-readable, formatted printout of ARIMA
coefficients using R's standard coefficient matrix formatting. It is
primarily used for console output and reporting.

## Usage

``` r
getArimaCoef_formatted(x)
```

## Arguments

- x:

  A Persephone object (e.g., `x13Single` or `tramoseatsSingle`)

## Value

Character vector of formatted coefficient table lines (invisible)

## Note

This is an internal function for console output.

## Examples

``` r
# obj <- perX13(AirPassengers, "rsa3")
# obj$run()
# getArimaCoef_formatted(obj)
```
