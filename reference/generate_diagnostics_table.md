# Generate diagnostics table for persephone objects (internal)

Generate diagnostics table for persephone objects (internal)

## Usage

``` r
generate_diagnostics_table(x)
```

## Arguments

- x:

  an object of class
  [`persephone`](https://statistikat.github.io/persephone3/reference/persephone.md)

## Value

A data.frame with one row containing diagnostic measures

## Details

The function classifies various diagnostic statistics into quality
categories:

- **Normality/Independence tests:** "Good" (p ≥ 0.1), "Uncertain" (0.01
  ≤ p \< 0.1), "Bad" (0.001 ≤ p \< 0.01), "Severe" (p \< 0.001)

- **Seasonality/TD tests:** "Good" (p ≥ 0.1), "Uncertain" (0.05 ≤ p \<
  0.1), "Bad" (0.01 ≤ p \< 0.05), "Severe" (p \< 0.01)

- **Outliers:** "Good" (\< 3\\

## Returned Columns

- run:

  Logical indicating if the model has been run

- method:

  Seasonal adjustment method used (x13 or tramoseats)

- type:

  Type of series ("single" or "aggregate")

- seasonality:

  Result of combined seasonality test

- normality:

  Classification of Doornik-Hansen normality test

- independence:

  Classification of Ljung-Box test for independence

- QS_seas:

  Classification of QS test for residual seasonality

- F_seas:

  Classification of F-test for residual seasonality

- combSA_all:

  Combined test for calendar effects in SA (all observations)

- combSA_last:

  Combined test for calendar effects in SA (last year)

- combI_all:

  Combined test for calendar effects in irregular (all observations)

- combI_last:

  Combined test for calendar effects in irregular (last year)

- fTestSA_td:

  F-test for trading day effects in SA series

- TD_Ftest:

  P-value of regression F-test for trading days

- logTransform:

  Logical indicating if log transformation was applied

- arimaMdl:

  ARIMA model specification as string

- nOutliers:

  Classification of outlier proportion

- Q:

  M-statistics Q value

- Q2:

  M-statistics Q-M2 value

- TotalQ:

  Overall quality summary
