# Interactive Highcharts plot for single persephone objects

Creates an interactive time series plot showing the original series,
trend, seasonally adjusted series, and forecast with confidence
intervals. Outliers are marked with vertical lines.

## Usage

``` r
# S3 method for class 'persephoneSingle'
hchart(object, ...)
```

## Arguments

- object:

  A persephone object of class `persephoneSingle`

- ...:

  Additional arguments (currently unused)

## Value

A `highchart` object

## Details

The plot includes:

- Original series (blue)

- Trend (orange)

- Seasonally adjusted series (green)

- Forecast with 95\\

- Outliers marked with pink dashed lines

The plot is interactive with zoom functionality (double-click to reset
zoom).

## Examples

``` r
if (FALSE) { # \dontrun{
obj <- perX13(AirPassengers, "rsa3")
obj$run()
hchart(obj)
} # }
```
