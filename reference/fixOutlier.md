# Fix outliers in the model

Fixes detected outliers to prevent re-detection

## Usage

``` r
fixOutlier(x, timespan = 12, verbose = FALSE)
```

## Arguments

- x:

  a persephone object

- timespan:

  number of months from the end of the time series where outliers are
  not fixed

- verbose:

  if TRUE the changed parameters will be reported
