# Critical value computed internally by JDemetra+ for outlier detection

Transcribes the thresholds the engines apply when the `outlier`
specification leaves the critical value unset: x13 uses
`max(advancedComputer(0.05)(n), 2.8)`, an extreme-value approximation
for the 95% quantile of the maximum of `n` standard normal variables;
tramoseats uses `max(simpleComputer()(n), 2)`, i.e. 3.3 up to 50
observations, rising by 0.0025 per observation up to 4.3. Verified
against the jdplus implementation (`X13Utility.calcCv`,
`TramoUtility.calcCv`) via `rJava`.

## Usage

``` r
calcOutlierCrit(n, method = c("x13", "tramoseats"))
```

## Arguments

- n:

  Number of observations of the estimation domain

- method:

  Either `"x13"` or `"tramoseats"`

## Value

The critical value.
