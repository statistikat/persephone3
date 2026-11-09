# SI-Ratio Plot (ggplot2 version)

Create a plot of SI-ratios (and, in case of x11, of seasonal factors) by
period using ggplot2, suitable for publications and reports with easy
export to PDF, PNG, and other formats. Supports optional plotly
conversion for basic interactivity.

## Usage

``` r
plotSiRatiosGgplot(x, main = NULL, interactive = FALSE, ...)
```

## Arguments

- x:

  an object of class
  [persephone](https://statistikat.github.io/persephone3/reference/persephone.md).

- main:

  plot title

- interactive:

  If TRUE, wrap the `ggplot` object in
  [plotly::ggplotly](https://rdrr.io/pkg/plotly/man/ggplotly.html) to
  add basic interactivity (zoom, pan, hover tooltips). If FALSE
  (default), returns a static ggplot object suitable for export.

- ...:

  other plotting parameters to affect the plot. Not currently used.

## Value

Returns an object of class `ggplot` or `plotly`

## Details

Produces a `ggplot`/`plotly` object for objects of class
[persephone](https://statistikat.github.io/persephone3/reference/persephone.md).
If the `persephone` object is of class `x13Single`, the function
generates a plot of the SI-ratios, the replaced SI-ratios, the seasonal
factors and the mean of the seasonal factors. For `tramoseatsSingle`
objects, the function plots SI-ratios and their mean.

## Examples

``` r
data(AirPassengers, package = "datasets")
# Generate a persephone object, in this case an x13Single object
obj <- perX13(AirPassengers, "rsa1")
obj$run()
# Plot the SI-ratios after run
plotSiRatiosGgplot(obj)


data(UKgas, package = "datasets")
# Generate a persephone object, in this case a tramoseatsSingle object
obj2 <- perTramo(UKgas, "rsa3")
obj2$run()
plotSiRatiosGgplot(obj2)

```
