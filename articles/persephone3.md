# Get started with persephone3

## Introduction

[persephone3](https://github.com/statistikat/persephone3) provides an
object-oriented wrapper around the seasonal adjustment packages in the
[rjdverse](https://github.com/rjdverse/), specifically
[rjd3x13](https://github.com/rjdverse/rjd3x13) and
[rjd3tramoseats](https://github.com/rjdverse/rjd3tramoseats). The
package performs time series adjustments using the Java library
[JDemetra+](https://jdemetra-new-documentation.netlify.app/) with a
focus on batch processing, hierarchical time series, and interactive
visualizations.

### Installation

Install [persephone3](https://github.com/statistikat/persephone3) and
its dependencies from GitHub:

``` r

# Install rjdverse dependencies first
remotes::install_github("rjdverse/rjd3toolkit@*release")
remotes::install_github("rjdverse/rjd3x13@*release")
remotes::install_github("rjdverse/rjd3tramoseats@*release")

# Install persephone3
remotes::install_github("statistikat/persephone3")
```

## Basic Workflow

### Creating a Seasonal Adjustment Object

The first step is to create a seasonal adjustment object using either
[`perX13()`](https://statistikat.github.io/persephone3/reference/perX13.md)
for X-13ARIMA-SEATS or
[`perTramo()`](https://statistikat.github.io/persephone3/reference/perTramo.md)
for TRAMO/SEATS:

``` r

library(persephone3)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
```

``` r

# X-13ARIMA-SEATS with default rsa3 template
obj_x13 <- perX13(AirPassengers)
obj_x13
#> A persephone object
#> Not yet run.

# TRAMO/SEATS with default template
obj_tramo <- perTramo(AirPassengers)
obj_tramo
#> A persephone object
#> Not yet run.
```

The `template` argument determines the default specification. Available
templates differ between X-13 and TRAMO/SEATS:

#### X-13ARIMA-SEATS Templates (`perX13()`)

| Template | Log/Level Detection | Outlier Detection | Calendar Effects | ARIMA Model |
|----|----|----|----|----|
| `rsa0` | None | None | None | Airline (+mean) |
| `rsa1` | Automatic | AO, LS, TC | None | Airline (+mean) |
| `rsa2c` | Automatic | AO, LS, TC | 2 TD vars + Easter | Airline (+mean) |
| `rsa3` | Automatic | AO, LS, TC | None | Automatic |
| `rsa4` | Automatic | AO, LS, TC | 2 TD vars + Easter | Automatic |
| `rsa5c` | Automatic | AO, LS, TC | 7 TD vars + Easter | Automatic |

**Default**: `"rsa3"`

See
[`rjd3x13::x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
for details.

#### TRAMO/SEATS Templates (`perTramo()`)

| Template | Log/Level Detection | Outlier Detection | Calendar Effects | ARIMA Model |
|----|----|----|----|----|
| `rsa0` | None | None | None | Airline (+mean) |
| `rsa1` | Automatic | AO, LS, TC | None | Airline (+mean) |
| `rsa2` | Automatic | AO, LS, TC | 2 TD vars + Easter | Airline (+mean) |
| `rsa3` | Automatic | AO, LS, TC | None | Automatic |
| `rsa4` | Automatic | AO, LS, TC | 2 TD vars + Easter | Automatic |
| `rsa5` | Automatic | AO, LS, TC | 7 TD vars + Easter | Automatic |
| `rsafull` | Automatic | AO, LS, TC | Automatic | Automatic |

**Default**: `"rsa3"` for
[`perTramo()`](https://statistikat.github.io/persephone3/reference/perTramo.md),
`"rsa5"` for `tramoseats_spec()`

See
[`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html)
for details.

**Abbreviations**: - **AO**: Additive Outlier - **LS**: Level Shift -
**TC**: Temporary Change - **TD**: Trading Day

### Running the Model

Execute the seasonal adjustment with the `run()` method:

``` r

obj_x13$run()
```

After running, you can access the results through various methods.

### Inspecting Results

#### Print and rjd3 SA-Output

``` r

# Print object information
obj_x13
#> A persephone object
#> Output:
#>  run  method type   seasonality logTransform arimaMdl       nOutliers
#>  TRUE x13    single Present     TRUE         (0 1 1)(0 1 1) NA       
#>  Q    Q2   TotalQ
#>  0.26 0.29 Good

# Access the rjd3 output object
obj_x13$output
#> Model: X-13
#> Log-transformation: yes 
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>   -0.4018   -0.5569 
#> 
#> No regression variables
#> 
#>  Seasonal filter: FILTER_S3X3;  Trend filter: H-9 terms
#>  M-Statistics: q Good (0.260); q-m2 Good (0.290)
#>  QS test on SA: Good (1.000);  F-test on SA: Good (0.966)
#> 
#> For a more detailed output, use the 'summary()' function.
```

#### Diagnostics

``` r

# Get diagnostic statistics
obj_x13$diagnosticsTable()
#>   component  run method   type seasonality normality independence
#> 1           TRUE    x13 single     Present      Good         Good
#>   QS_seas F_seas combSA_all combSA_last combI_all combI_last
#> 1    Good   Good       Good        Good      Good       Good
#>   fTestSA_td TD_Ftest logTransform       arimaMdl nOutliers    Q   Q2
#> 1  Uncertain       NA         TRUE (0 1 1)(0 1 1)      Good 0.26 0.29
#>   TotalQ
#> 1   Good
```

#### Visualization

`persephone3` provides multiple visualization options:

``` r

# dygraphs plot method
obj_x13$plot()

# Highcharter interactive plot
hchart(obj_x13)
```

### Other Available Plots

Additional visualization methods are available:

- `obj$plotSpectrum()`: Spectral plots
- `obj$plotSeasIrrCal()`: Seasonal, irregular, and calendar effects
- `obj$plotResiduals()`: Residual diagnostics (ACF, PACF, QQ, histogram)
- `obj$plotSiRatios()`: Seasonal-to-Irregular ratio plot

For detailed documentation on all plotting options, see
[`vignette("persephone-plotting")`](https://statistikat.github.io/persephone3/articles/persephone-plotting.md).

## Customizing Parameters

### Parameter Structure

`persephone3` provides a unified interface to the underlying
[rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) and
[rjd3x13](https://github.com/rjdverse/rjd3x13) specification functions.
Parameters are organized into **spec-blocks**, where each block
corresponds to a specific `{rjd3*}` function.

The parameter name format is:

    block.parameter

- **block**: The first part (before the dot) indicates the spec-block
  and maps to an `{rjd3*}` function
- **parameter**: The second part (after the dot) specifies the parameter
  within that block

When you pass parameters to `updateParams()` or during object creation,
`persephone3` internally routes them to the appropriate `{rjd3*}`
function based on the block name.

### Updating Parameters

Use `updateParams()` to modify seasonal adjustment settings. Parameters
are passed directly and mapped to the corresponding `{rjd3*}` functions:

``` r

# Disable log transformation and enable iterative estimation
obj_x13$updateParams(
  transform.fun = "None",
  arima.p = 1,
  arima.d = 2,
  arima.q = 0,
  arima.bp = 1,
  arima.bd = 1,
  arima.bq = 0
)
#> Warning in set_arima.default(x$regarima, ...): autmodel enabled: the
#> parameters will not impact the final parameters

# Re-run with new parameters
obj_x13$run()

# Check updated diagnostics
obj_x13$diagnosticsTable()
#>   component  run method   type seasonality normality independence
#> 1           TRUE    x13 single     Present      Good    Uncertain
#>   QS_seas F_seas combSA_all combSA_last combI_all combI_last
#> 1    Good   Good       Good        Good      Good       Good
#>   fTestSA_td TD_Ftest logTransform       arimaMdl nOutliers    Q   Q2
#> 1     Severe       NA        FALSE (0 1 1)(0 1 0)      Good 0.47 0.53
#>   TotalQ
#> 1 Severe
```

### Available Spec-Blocks

Each spec-block corresponds to a specific function in
[rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) or
[rjd3x13](https://github.com/rjdverse/rjd3x13). You can pass parameters
directly using the block.parameter notation, and `persephone3` will
route them to the appropriate `{rjd3*}` function:

| Block | `{rjd3*}` Function | Package | Description |
|----|----|----|----|
| `arima` | [`set_arima()`](https://rjdverse.github.io/rjd3toolkit/reference/set_arima.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | ARIMA model specification |
| `automdl` | [`set_automodel()`](https://rjdverse.github.io/rjd3toolkit/reference/set_automodel.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Automatic model selection |
| `basic` | [`set_basic()`](https://rjdverse.github.io/rjd3toolkit/reference/set_basic.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Basic settings (series type, frequency, etc.) |
| `easter` | [`set_easter()`](https://rjdverse.github.io/rjd3toolkit/reference/set_easter.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Easter effect adjustment |
| `estimate` | [`set_estimate()`](https://rjdverse.github.io/rjd3toolkit/reference/set_estimate.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Estimation settings |
| `setout` | [`set_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/set_outlier.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Outlier detection settings |
| `td` | [`set_tradingdays()`](https://rjdverse.github.io/rjd3toolkit/reference/set_tradingdays.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Trading day effects |
| `transform` | [`set_transform()`](https://rjdverse.github.io/rjd3toolkit/reference/set_transform.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Log/Box-Cox transformation |
| `addout` | [`add_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Add specific outliers |
| `remout` | [`remove_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/remove_outlier.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Remove outliers |
| `addramp` | [`add_ramp()`](https://rjdverse.github.io/rjd3toolkit/reference/add_ramp.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Add ramp adjustments |
| `remramp` | [`remove_ramp()`](https://rjdverse.github.io/rjd3toolkit/reference/remove_ramp.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Remove ramp adjustments |
| `usrdefvar` | [`add_usrdefvar()`](https://rjdverse.github.io/rjd3toolkit/reference/add_usrdefvar.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Add user-defined variables |
| `x11` | [`set_x11()`](https://rjdverse.github.io/rjd3x13/reference/set_x11.html) | [rjd3x13](https://github.com/rjdverse/rjd3x13) | X-11 specific options (X-13 only) |
| `benchmarking` | [`set_benchmarking()`](https://rjdverse.github.io/rjd3toolkit/reference/set_benchmarking.html) | [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) | Benchmarking settings |

**Note:** The parameter names you use in `persephone3` (e.g., `arima.p`,
`transform.fun`) correspond to the argument names in the underlying
`{rjd3*}` functions. For complete parameter documentation, refer to the
linked `{rjd3*}` function documentation.

### Simplified Syntax for Some parameters

For certain parameters (e.g., outliers), `persephone3` provides a
simplified syntax. Instead of using `addout.type = c("AO","LS")` and
`addout.date = c("1953-07-01","1954-06-01")`, you can write:

``` r

obj_x13$updateParams(
  outliers = c("AO1953.7", "LS1954.6")
)
obj_x13$run()
```

This combines the outlier type and date into a single, more intuitive
format.

## Accessing Specific Results

### Preprocessing Results

``` r

# Access preprocessing output
obj_x13$output$preprocessing
#> Log-transformation: no 
#> SARIMA model: (0,1,1) (0,1,0)
#> 
#> SARIMA coefficients:
#> theta(1) 
#>   -0.256 
#> 
#> Regression model:
#> AO (1960-03-01) 
#>          -43.98 
#> 
#> For a more detailed output, use the 'summary()' function.
```

### Original Series

``` r

# Original series
orig_series <- obj_x13$ts
head(orig_series)
#>      Jan Feb Mar Apr May Jun
#> 1949 112 118 132 129 121 135
```

### Seasonally Adjusted Series

``` r

# Get seasonally adjusted series
sa_series <- obj_x13$adjusted
head(sa_series)
#>           Jan      Feb      Mar      Apr      May      Jun
#> 1949 127.4149 126.3824 124.1177 128.1166 127.5550 124.1789
```

### Access Original, Seasonally Adjusted and Trend Series via `{rjd3*}` `user_defined` Slot

``` r

# Original series
head(obj_x13$output$user_defined$y)
#>      Jan Feb Mar Apr May Jun
#> 1949 112 118 132 129 121 135
# Seasonally adjusted
head(obj_x13$output$user_defined$sa)
#>           Jan      Feb      Mar      Apr      May      Jun
#> 1949 127.4149 126.3824 124.1177 128.1166 127.5550 124.1789
# Trend
head(obj_x13$output$user_defined$t)
#>           Jan      Feb      Mar      Apr      May      Jun
#> 1949 127.1220 126.6891 126.3950 125.9305 125.2265 124.6092
```

## Next Steps

- See
  [`vignette("persephone-plotting")`](https://statistikat.github.io/persephone3/articles/persephone-plotting.md)
  for detailed visualization options
- See
  [`vignette("persephone-hierarchical")`](https://statistikat.github.io/persephone3/articles/persephone-hierarchical.md)
  for hierarchical time series adjustment
- Check
  [`?perX13`](https://statistikat.github.io/persephone3/reference/perX13.md)
  and
  [`?perTramo`](https://statistikat.github.io/persephone3/reference/perTramo.md)
  for complete parameter documentation
- Explore [rjd3toolkit](https://github.com/rjdverse/rjd3toolkit) and
  [rjd3x13](https://github.com/rjdverse/rjd3x13) documentation for
  advanced spec-block options

## Common Issues

### Java not found

If you encounter Java-related errors, ensure Java 17+ is installed:

``` r

# Check Java installation
Sys.getenv("JAVA_HOME")
```

### Model convergence issues

If the model fails to converge:

1.  Try a simpler template (e.g., `"rsa0"` instead of `"rsa3"`)
2.  Disable automatic outlier detection
3.  Check for extreme values or missing data in your series

## Repository

Source code and issue tracker:
<https://github.com/statistikat/persephone3>
