# Extract outliers and create time series for plotting

This function processes outlier information from a Persephone object and
creates time series objects that can be used for plotting outliers on
graphs. It is primarily used internally by plot functions to display
outlier events (AO, LS, TC) on time series visualizations.

## Usage

``` r
gettsout(x)
```

## Arguments

- x:

  A Persephone object (e.g., `x13Single` or `tramoseatsSingle`)

## Value

A list with two elements:

- `[[1]]`:

  A `ts` object with columns for each outlier type (otlAO, otlLS,
  otlTC). Values at outlier dates contain the original series values,
  all other dates are NA.

- `[[2]]`:

  A named list of Date objects, one per outlier type (AO, LS, TC),
  containing the dates where outliers occurred. Used for dygraph event
  annotations.

## Note

This is an internal function; use it only if you know what you're doing.

## Examples

``` r
# obj <- perX13(AirPassengers, "rsa3")
# obj$run()
# otl <- gettsout(obj)
# otl[[1]]  # time series with outlier values
# otl[[2]]  # dates for event annotations
```
