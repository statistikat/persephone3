# x13 class

An R6 class that represents a x13 model.

## Super classes

[`persephone3::persephone`](https://statistikat.github.io/persephone3/reference/persephone.md)
-\>
[`persephone3::persephoneSingle`](https://statistikat.github.io/persephone3/reference/persephoneSingle.md)
-\> `x13Single`

## Active bindings

- `method`:

  SA method used

## Methods

### Public methods

- [`x13Single$clone()`](#method-x13Single-clone)

Inherited methods

- [`persephone3::persephone$diagnosticsTable()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-diagnosticsTable)
- [`persephone3::persephone$fixModel()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-fixModel)
- [`persephone3::persephone$fixOutlier()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-fixOutlier)
- [`persephone3::persephone$generateQrTable()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-generateQrTable)
- [`persephone3::persephone$hchart()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-hchart)
- [`persephone3::persephone$iterate()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-iterate)
- [`persephone3::persephone$plot()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-plot)
- [`persephone3::persephone$plotResiduals()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-plotResiduals)
- [`persephone3::persephone$plotSeasIrrCal()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-plotSeasIrrCal)
- [`persephone3::persephone$plotSiRatios()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-plotSiRatios)
- [`persephone3::persephone$plotSpectrum()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-plotSpectrum)
- [`persephone3::persephone$print()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-print)
- [`persephone3::persephone$updateParams()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-updateParams)
- [`persephone3::persephoneSingle$initialize()`](https://statistikat.github.io/persephone3/html/persephoneSingle.html#method-persephoneSingle-initialize)
- [`persephone3::persephoneSingle$run()`](https://statistikat.github.io/persephone3/html/persephoneSingle.html#method-persephoneSingle-run)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    x13Single$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
data(AirPassengers, package = "datasets")

obj <- perX13(AirPassengers, "rsa3")
obj$run()
obj$output
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
