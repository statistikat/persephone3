# Print method for the outlier table of a Persephone object

Prints the data frame returned by
[`getOutliers()`](https://statistikat.github.io/persephone3/reference/getOutliers.md)
as an aligned table: text columns left aligned, numbers right aligned
with fixed decimals so that the decimal points line up. The standard
print method of
[`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)
would left align the numbers and show them in full precision.

## Usage

``` r
# S3 method for class 'persephoneOutliers'
print(x, p_digits = 3, ...)
```

## Arguments

- x:

  A data frame of class `persephoneOutliers`, as returned by
  [`getOutliers()`](https://statistikat.github.io/persephone3/reference/getOutliers.md)

- p_digits:

  number of decimal places for the p-value (also sets the `"< 0.001"`
  lower bound); defaults to 3

- ...:

  further arguments passed to the aligned table printer, e.g. `width`
  (defaults to `getOption("width")`)

## Value

`x`, invisibly
