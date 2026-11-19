# Joint F test of the trading day regressors

Tests H0: all trading day coefficients of the regression are zero. Uses
the test reported by JDemetra+ (`regression.td-ftest`), which also backs
the `TD_Ftest` column of
[`diagnosticsTable()`](https://statistikat.github.io/persephone3/reference/diagnosticsTable.md),
so that both views report the same numbers. If the node is missing or
incomplete (e.g. the trading day pre-test `td.test = "Remove"` dropped
the regressors during the run), the test is computed as a Wald test,
`F = (b' V^-1 b) / q` against `F(q, df)`, from the estimated coefficient
vector `b` and the covariance matrix `bvar`.

## Usage

``` r
tdJointFTest(x)
```

## Arguments

- x:

  A Persephone object with completed model run

## Value

A one-row data frame with the columns `group`, `value`, `df1`, `df2` and
`pvalue`, or `NULL` when the model contains no trading day regressors or
the test cannot be computed.
