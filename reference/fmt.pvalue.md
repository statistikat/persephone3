# Format a p-value as a plain-decimal string with a lower bound

Shows plain decimals (`0.005`) instead of scientific notation and
replaces values below the display threshold by `"< 0.001"`. Only used
for printing; the data frame keeps the exact numeric value.

## Usage

``` r
fmt.pvalue(p, digits = 3)
```

## Arguments

- p:

  numeric vector of p-values

- digits:

  number of decimal places (also sets the `"< ..."` bound)

## Value

character vector
