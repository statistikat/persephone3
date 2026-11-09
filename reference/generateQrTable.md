# Generate figures from the EUROSTAT quality report

This function uses the method `iterate` to create figures for a quality
report for each component of a hierarchical `persephone` object

## Usage

``` r
generateQrTable(x)
```

## Arguments

- x:

  a persephone object

## Columns

- **component** is a column added by
  [`iterate()`](https://statistikat.github.io/persephone3/reference/iterate.md)
  and informs where in the hierarchy a certain component is.

- **Method** is `TS` for
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)
  and `x13` for
  [`rjd3x13::x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)

- **Period** is `12` for yearly measurements and `4` for quarterly
  measurements

- **Start** denotes the start of the time series

- **End** denotes the end of the time series

- **Log.Transformation**

- **ARIMA.Model**

- **LeapYear**

- **MovingHoliday**

- **NbTF**

- **Noutliers** Number of outliers

- **Outlier1**

- **Outlier2**

- **Outlier3**

- **CombinedTest_SI**

- **Residual.Seasonality**

- **Residual.TD.Effect**

- **Q.Stat**

- **Final.Henderson.Filter**

- **Stage.2.Henderson.Filter**

- **Seasonal.Filter**

- **Max.Adj**

## Examples

``` r
obj_x13 <- perX13(AirPassengers, "rsa3")
ht <- perHts(a = obj_x13, b = obj_x13, method = "x13")
ht$run()
generateQrTable(ht)
#>   component Method Period Nobs  Start     End Log.Transformation
#> 1              x13     12  144 1-1949 12-1960               TRUE
#> 2         a    x13     12  144 1-1949 12-1960               TRUE
#> 3         b    x13     12  144 1-1949 12-1960               TRUE
#>      ARIMA.Model LeapYear MovingHoliday NbTD Noutliers Outlier1
#> 1 (0 1 1)(0 1 1)    FALSE         FALSE    0         0       NA
#> 2 (0 1 1)(0 1 1)    FALSE         FALSE    0         0       NA
#> 3 (0 1 1)(0 1 1)    FALSE         FALSE    0         0       NA
#>   Outlier2 Outlier3 CombinedTest_SI Residual.Seasonality
#> 1       NA       NA         Present                   No
#> 2       NA       NA         Present                   No
#> 3       NA       NA         Present                   No
#>   Residual.TD.Effect Q.Stat Final.Henderson.Filter
#> 1                Yes   0.26                     H9
#> 2                Yes   0.26                     H9
#> 3                Yes   0.26                     H9
#>   Stage.2.Henderson.Filter Seasonal.Filter Max.Adj
#> 1                      H13     FILTER_S3X3     25%
#> 2                      H13     FILTER_S3X3     25%
#> 3                      H13     FILTER_S3X3     25%
```
