# Print method for the regression table of a Persephone object

Prints the data frame returned by
[`getRegressors()`](https://statistikat.github.io/persephone3/reference/getRegressors.md)
as an aligned table: text columns left aligned, numbers right aligned
with fixed decimals so that the decimal points line up, plus a column of
significance stars and, when the model has trading day regressors, their
joint F test below the table. The standard print method of
[`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)
would left align the numbers and show them in full precision.

## Usage

``` r
# S3 method for class 'persephoneRegressors'
print(x, p_digits = 3, ...)
```

## Arguments

- x:

  A data frame of class `persephoneRegressors`, as returned by
  [`getRegressors()`](https://statistikat.github.io/persephone3/reference/getRegressors.md)

- p_digits:

  number of decimal places for the p-value (also sets the `"< 0.001"`
  lower bound); defaults to 3

- ...:

  further arguments passed to the aligned table printer, e.g. `width`
  (defaults to `getOption("width")`)

## Value

`x`, invisibly
