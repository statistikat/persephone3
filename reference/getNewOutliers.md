# Extract automatically detected outliers from a Persephone object

Returns only the outliers that were detected automatically by
X-13ARIMA-SEATS or TRAMO/SEATS, that is all outliers reported in the
model output minus the ones specified manually with the `outliers`
argument. The manual ones are read from the model specification
(`regarima$regression$outliers` for x13, `tramo$regression$outliers` for
tramoseats), because the output of the run does not distinguish manually
specified from automatically detected outliers.

## Usage

``` r
getNewOutliers(x)
```

## Arguments

- x:

  A Persephone object (e.g., `x13Single` or `tramoseatsSingle`) with
  completed model run

## Value

A data frame with the columns of
[`getOutliers()`](https://statistikat.github.io/persephone3/reference/getOutliers.md),
restricted to the automatically detected outliers. It has zero rows if
the model contains no such outlier.

## Note

This is an internal function. Unlike
[`getOutliers()`](https://statistikat.github.io/persephone3/reference/getOutliers.md)
it works on single series objects only.

## Examples

``` r
# obj <- perX13(AirPassengers, "rsa3", outliers = "AO1960.5")
# obj$run()
# getOutliers(obj)     # all outliers
# getNewOutliers(obj)  # without the manually specified one
```
