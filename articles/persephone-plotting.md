# Plotting with persephone3

## Introduction

`persephone3` provides multiple ways to visualize seasonal adjustment
results. The plotting functions return interactive HTML widgets using
`dygraphs`, `plotly`, and `highcharter`.

**Note:** The appearance of plots depends on whether `run()` has been
called. Before running, only the raw time series is available. After
`run()`, additional components (seasonally adjusted, trend-cycle,
forecasts) become accessible.

``` r

library(persephone3)

# Create seasonal adjustment object
obj <- perX13(AirPassengers, "rsa1")
```

## Default Plot Method

The default
[`plot()`](https://statistikat.github.io/persephone3/reference/plot.md)
method creates an interactive time series plot using `dygraphs`. You can
zoom into specific periods and hover to see exact values.

``` r

# Plot before running - shows raw data only
plot(obj, drawPoints = TRUE)
```

After running the model, the plot includes additional components and
forecasts:

``` r

obj$run()

# Plot after running - shows SA, trend, and forecast
plot(obj, drawPoints = TRUE, showOutliers = FALSE)
```

## Seasonal and Irregular Components

Visualize the seasonal and irregular components with
[`plotSeasIrrCal()`](https://statistikat.github.io/persephone3/reference/plotSeasIrrCal.md):

``` r

plotSeasIrrCal(obj)
```

## Residual Analysis

[`plotResiduals()`](https://statistikat.github.io/persephone3/reference/plotResiduals.md)
creates interactive diagnostic plots using `plotly`. You can examine
autocorrelation (ACF) or partial autocorrelation (PACF):

``` r

# ACF of residuals
plotResiduals(obj, which = "acf")
```

``` r


# PACF of residuals
plotResiduals(obj, which = "pacf")
```

## Highcharter Alternative

For alternative styling, use
[`hchart()`](https://jkunst.com/highcharter/reference/hchart.html) which
leverages the `highcharter` package:

``` r

hchart(obj)
```

## Plotly Integration

Some methods return `plotly` objects directly, allowing further
customization:

``` r

# ACF plot with plotly
p <- plotResiduals(obj, which = "acf")
p  # The plotly object can be further customized
```

## Repository

Source code and issue tracker:
<https://github.com/statistikat/persephone3>
