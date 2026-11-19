# Print the joint F tests computed by [`tdJointFTest()`](https://statistikat.github.io/persephone3/reference/tdJointFTest.md)

Formats the one-row data frame returned by
[`tdJointFTest()`](https://statistikat.github.io/persephone3/reference/tdJointFTest.md)
as one line per test, `group: F(df1, df2) = value P ...`, with the
p-value in the same format as the coefficient tables. Used by the print
method of
[`getRegressors()`](https://statistikat.github.io/persephone3/reference/getRegressors.md)
and by
[`summary.persephone()`](https://statistikat.github.io/persephone3/reference/summary.persephone.md).

## Usage

``` r
printFTests(ft, p_digits = 3)
```

## Arguments

- ft:

  one-row data frame from
  [`tdJointFTest()`](https://statistikat.github.io/persephone3/reference/tdJointFTest.md)

- p_digits:

  number of decimal places for the p-value

## Value

`ft`, invisibly
