# Run multiple diagnostic extraction functions on a Persephone object

This convenience function allows running several diagnostic extraction
functions at once and printing their results. It is primarily used for
interactive exploration and debugging.

## Usage

``` r
getDiagnostics(x, ..., funcs = c("getOutliers", "getArimaCoef"))
```

## Arguments

- x:

  A Persephone object (e.g., `x13Single` or `tramoseatsSingle`)

- ...:

  Additional functions to run (passed as character strings)

- funcs:

  Character vector of function names to run (default:
  `c("getOutliers", "getArimaCoef")`)

## Value

Invisible list of results from each function

## Note

This is an internal function for interactive diagnostics.

## Examples

``` r
# obj <- perX13(AirPassengers, "rsa3")
# obj$run()
# getDiagnostics(obj)
# getDiagnostics(obj, funcs = c("getOutliers", "getArimaCoef", "gettsout"))
```
