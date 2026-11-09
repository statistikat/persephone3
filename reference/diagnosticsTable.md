# Diagnostics table for persephone objects

Creates a comprehensive table of diagnostic statistics and quality
indicators for seasonal adjustment results. For hierarchical persephone
objects, the function uses the iterate mechanism to process all
components.

## Usage

``` r
diagnosticsTable(x)
```

## Arguments

- x:

  a persephone object created with
  [`perX13`](https://statistikat.github.io/persephone3/reference/perX13.md),
  [`perTramo`](https://statistikat.github.io/persephone3/reference/perTramo.md),
  or
  [`perHts`](https://statistikat.github.io/persephone3/reference/perHts.md)

## Value

A data.frame with diagnostic statistics. For hierarchical objects,
includes a 'component' column identifying each series.

## Details

The diagnostics table provides an overview of key quality indicators:

- **Residual diagnostics:** normality and independence tests

- **Seasonality tests:** QS and F-tests for residual seasonality

- **Calendar effects:** tests for trading day and other calendar effects

- **Model information:** ARIMA specification, transformation, outliers

- **Quality measures:** M-statistics (Q, Q2) and overall quality

All p-values are classified into quality categories (Good, Uncertain,
Bad, Severe) based on commonly used thresholds in seasonal adjustment
diagnostics.

## See also

[`generateQrTable`](https://statistikat.github.io/persephone3/reference/generateQrTable.md)
for the Eurostat quality report

## Examples

``` r
if (FALSE) { # \dontrun{
# Single series
obj <- perX13(AirPassengers, template = "rsa3")
obj$run()
diagnosticsTable(obj)

# Hierarchical series
obj1 <- perX13(AirPassengers, "rsa3")
obj2 <- perTramo(AirPassengers, "rsafull")
ht <- perHts(x13_series = obj1, tramo_series = obj2)
ht$run()
diagnosticsTable(ht)

# R6 method
obj$diagnosticsTable()
} # }
```
