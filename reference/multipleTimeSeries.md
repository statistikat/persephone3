# R6 Class for a batch of time series

Combine multiple objects of persephone objects into a new persephone
object. On the resulting objects seasonal adjustment can be performed to
all elements at once

## Super class

[`persephone3::persephone`](https://statistikat.github.io/persephone3/reference/persephone.md)
-\> `multipleTimeSeries`

## Public fields

- `components`:

  the series of the multiple time series

## Active bindings

- `ts`:

  the time series (inherited from persephone)

- `adjusted`:

  results from the seasonal adjustment

- `params`:

  of all components

- `forecasts`:

  get forecasts

## Methods

### Public methods

- [`multipleTimeSeries$new()`](#method-multipleTimeSeries-new)

- [`multipleTimeSeries$run()`](#method-multipleTimeSeries-run)

- [`multipleTimeSeries$print()`](#method-multipleTimeSeries-print)

- [`multipleTimeSeries$iterate()`](#method-multipleTimeSeries-iterate)

- [`multipleTimeSeries$getComponent()`](#method-multipleTimeSeries-getComponent)

- [`multipleTimeSeries$updateParams()`](#method-multipleTimeSeries-updateParams)

- [`multipleTimeSeries$generateQrTable()`](#method-multipleTimeSeries-generateQrTable)

- [`multipleTimeSeries$clone()`](#method-multipleTimeSeries-clone)

Inherited methods

- [`persephone3::persephone$diagnosticsTable()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-diagnosticsTable)
- [`persephone3::persephone$fixModel()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-fixModel)
- [`persephone3::persephone$fixOutlier()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-fixOutlier)
- [`persephone3::persephone$hchart()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-hchart)
- [`persephone3::persephone$plot()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-plot)
- [`persephone3::persephone$plotResiduals()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-plotResiduals)
- [`persephone3::persephone$plotSeasIrrCal()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-plotSeasIrrCal)
- [`persephone3::persephone$plotSiRatios()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-plotSiRatios)
- [`persephone3::persephone$plotSpectrum()`](https://statistikat.github.io/persephone3/html/persephone.html#method-persephone-plotSpectrum)

------------------------------------------------------------------------

### Method `new()`

create a new multiple time series object

#### Usage

    multipleTimeSeries$new(
      list = NULL,
      method = c("tramoseats", "x13"),
      template = NULL,
      context = NULL,
      userdefined = NULL,
      speclist = NULL,
      spec = NULL,
      ...
    )

#### Arguments

- `list`:

  a list of persephone objects as alternative input to `...`. This
  argument can also handle mts objects

- `method`:

  specifies the method to be used for the direct adjustment of the
  aggregate series, i.e. "tramoseats" or "x13"

- `template`:

  the name of the predefined specification template, passed as the
  `name` argument to
  [`rjd3x13::x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
  or
  [`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html)

- `context`:

  a list of external regressors (calendar or other) to be used for
  estimation, passed as the `context` argument of
  [`rjd3x13::x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  or
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)

- `userdefined`:

  a character vector of user-defined variables to be included in the
  output, passed as the `userdefined` argument to
  [`rjd3x13::x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  or
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)

- `speclist`:

  a list of additional arguments for customizing the model
  specifications, passed to the rjd3 specification functions internally.
  As alternative input to `...`

- `spec`:

  a model specification object of class JD3_X13_SPEC or
  JD3_TRAMOSEATS_SPEC

- `...`:

  one or more objects which are either of class persephone or can be
  coerced to persephone objects with asPersephone. If more than one
  element is supplied, the underlying time series must have the same
  time instances. All elements supplied in ... must be named. In
  addition to the ts/mts/persephone objects, `...` accepts arguments for
  customizing the model specifications as alternative input to speclist

#### Details

A batch has no aggregate series, so `method`, `template` and any spec
arguments in `...`/`speclist` always apply to all components.

------------------------------------------------------------------------

### Method `run()`

run the model

#### Usage

    multipleTimeSeries$run(verbose = FALSE)

#### Arguments

- `verbose`:

  if `FALSE` (the default), the results of the run will be returned
  invisibly

#### Examples

    perX13(AirPassengers)$run()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print mutliple timeseries to screen

#### Usage

    multipleTimeSeries$print()

------------------------------------------------------------------------

### Method [`iterate()`](https://statistikat.github.io/persephone3/reference/iterate.md)

iterate over all components

#### Usage

    multipleTimeSeries$iterate(
      fun,
      asTable = FALSE,
      component = "",
      unnest = FALSE
    )

#### Arguments

- `fun`:

  a function that takes a persephone object as a parameter

- `asTable`:

  if true, the return value of this method will be coerced to a
  data.frame

- `component`:

  the id of the component

- `unnest`:

  if `asTable = FALSE`, converts the return value from a nested list
  into a flat list

#### Details

this function is similar to
[`lapply()`](https://rdrr.io/r/base/lapply.html) in the sense that it
can be used to apply a function to several persephone objects
simultaniusely

------------------------------------------------------------------------

### Method `getComponent()`

extract a component series

#### Usage

    multipleTimeSeries$getComponent(componentId)

#### Arguments

- `componentId`:

  the id of a component

------------------------------------------------------------------------

### Method `updateParams()`

change all or some parameters of components

#### Usage

    multipleTimeSeries$updateParams(component = NULL, ...)

#### Arguments

- `component`:

  character vector with names of the components for which the parameters
  should be changed. If NULL (default) the parameters of all components
  will be changed

- `...`:

  named arguments to be changed

#### Details

this functions provides the possibility to update parameters of one or
more persephone single objects

------------------------------------------------------------------------

### Method [`generateQrTable()`](https://statistikat.github.io/persephone3/reference/generateQrTable.md)

Generate a table for the eurostat quality report

#### Usage

    multipleTimeSeries$generateQrTable(component = "", ...)

#### Arguments

- `component`:

  (optional) a sub-component to create the report for

- `...`:

  additional arguments for the generate qr table function

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    multipleTimeSeries$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
objX13 <- perX13(AirPassengers, "rsa3")

bt <- perBatch(a = objX13, b = objX13)
bt$run()

# access the list of unadjusted series
bt$ts
# access the list of adjusted series
bt$adjusted

bt$updateParams(easter.enabled = FALSE)

bt$updateParams(component = "a", automdl.enabled = FALSE,
                arima.p = 1, arima.d = 2, arima.q = 0,
                arima.bp = 1, arima.bd = 1, arima.bq = 0)
bt$run()
bt
} # }


## ------------------------------------------------
## Method `multipleTimeSeries$run`
## ------------------------------------------------

perX13(AirPassengers)$run()
```
