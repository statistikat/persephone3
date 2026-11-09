# Generation of trading day regressors

User defined trading day regressors are generated depending on a list of
country-specific holidays and associated weights. Each holiday can be
given a specific weight depending on the extent of the work load of that
specific holiday.

## Usage

``` r
genTd(
  freq = 12,
  firstYear = 1960,
  lastYear = 2099,
  hd,
  weight = rep(1, length(hd)),
  adjustEaster = "exact"
)
```

## Arguments

- freq:

  frequency of series

- firstYear:

  first year of calculation of trading day regressor

- lastYear:

  last year of calculation of trading day regressor

- hd:

  holidays, list of 1. exact date ("mm-dd") and/or 2. name of day
  (listHolidays() from timeDate function) and/or 3. easter relation
  (e.g. "easter+39", "easter-3")

- weight:

  vector of individual weights for each holiday with length of hd

- adjustEaster:

  "exact" or "approximate" to employ theoretical distribution of easter
  dates (default) or approximate distribution of easter dates for
  centering of trading days

## Value

list with three list elements. 1. matrix of trading day counts for each
individual day, month and year. Holidays which are delivered through the
parameter hd are assigned to the number of sundays. 2. multiple time
series object with 6 non-centered trading day regressors plus 1
non-centered working day regressor (columns 1 to 6 refer to Mondays to
Saturdays, colunm 7 refers to the one-regressor case of 5 working days).
In case of trading day adjustment the first 6 columns have to be
selected, in case of wording day adjustment only column 7 has to be
selected. 3. multiple time series object with 6 centered trading day
regressors plus 1 centered working day regressor

## Examples

``` r
library(rjd3toolkit)
#> 
#> Attaching package: ‘rjd3toolkit’
#> The following objects are masked from ‘package:stats’:
#> 
#>     aggregate, mad
```
