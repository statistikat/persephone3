# R6 Class for hierarchical time series

Combine mutliple objects of persephone objects into a new hierachical
persephone object. On the resulting time series the user can perform
direct and indirect seasonal adjustments.

## Super classes

[`persephone3::persephone`](https://statistikat.github.io/persephone3/reference/persephone.md)
-\>
[`persephone3::multipleTimeSeries`](https://statistikat.github.io/persephone3/reference/multipleTimeSeries.md)
-\> `hierarchicalTimeSeries`

## Public fields

- `components`:

  the sub series of the hierarchical time series

- `weights`:

  the weights used for aggregating components

- `indirect`:

  whether to use direct or indirect adjustment

## Active bindings

- `ts`:

  the aggregated time series

- `params`:

  of all components and the aggregated series

- `paramsDirect`:

  params of the aggregated series

- `adjustedIndirect`:

  results from the indirect adjustment where all components are adjusted
  and then aggregated

- `adjusted`:

  results from the seasonal adjustment

- `forecasts`:

  get forecasts

- `forecastsIndirect`:

  get forecasts according to indirect adjustments

## Methods

### Public methods

- [`hierarchicalTimeSeries$new()`](#method-hierarchicalTimeSeries-new)

- [`hierarchicalTimeSeries$run()`](#method-hierarchicalTimeSeries-run)

- [`hierarchicalTimeSeries$print()`](#method-hierarchicalTimeSeries-print)

- [`hierarchicalTimeSeries$iterate()`](#method-hierarchicalTimeSeries-iterate)

- [`hierarchicalTimeSeries$getComponent()`](#method-hierarchicalTimeSeries-getComponent)

- [`hierarchicalTimeSeries$updateParams()`](#method-hierarchicalTimeSeries-updateParams)

- [`hierarchicalTimeSeries$generateQrTable()`](#method-hierarchicalTimeSeries-generateQrTable)

- [`hierarchicalTimeSeries$clone()`](#method-hierarchicalTimeSeries-clone)

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

create a new hierarchical time series

#### Usage

    hierarchicalTimeSeries$new(
      ...,
      list = NULL,
      method = c("tramoseats", "x13"),
      template = NULL,
      context = NULL,
      userdefined = NULL,
      spec = NULL,
      weights = NULL,
      iterate = TRUE,
      speclist = NULL
    )

#### Arguments

- `...`:

  one or more objects which are either of class persephone or can be
  coerced to persephone objects with asPersephone. If more than one
  element is supplied, the underlying time series must have the same
  time instances. All elements supplied in ... must be named.

  In addition to the ts/mts/persephone objects, `...` accepts arguments
  for customizing the model specifications as alternative input to
  speclist. These are passed to the rjd3 specification functions
  internally.

- `list`:

  a list of persephone objects as alternative input to `...`. This
  argument can also handle mts objects

- `method`:

  specifies the method to be used for the direct adjustment of the
  aggregate series. tramoseats or x13

- `template`:

  the name of the predefined specification template, passed as the
  `name` argument to
  [`rjd3x13::x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
  or
  [`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html).
  Must be one of: `"rsa0"`, `"rsa1"`, `"rsa2c"`, `"rsa3"`, `"rsa4"`,
  `"rsa5c"` for x13 `"rsafull"`, `"rsa0"`, `"rsa1"`, `"rsa2"`, `"rsa3"`,
  `"rsa4"`, `"rsa5"` for tramoseats. Defaults to `"rsa3"`.

- `context`:

  passed as the `context` argument of
  [`rjd3x13::x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  or
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html),
  a list of external regressors (calendar or other) to be used for
  estimation.

- `userdefined`:

  passed as the userdefined argument to x13_fast() or tramoseats_fast()

- `spec`:

  a model specification object of class JD3_X13_SPEC or
  JD3_TRAMOSEATS_SPEC returned by
  [`rjd3x13::x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
  or
  [`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html).

- `weights`:

  either a vector if the same weight is used for all time points or a
  list of ts objects or a mts object if the weight varies for different
  time points. They must have the same length as the number of
  components.

- `iterate`:

  logical, if TRUE apply the given ... spec/params also to all
  components. For plain ts/mts components, method and template are
  inherited only if iterate is TRUE.

- `speclist`:

  a list of additional arguments for customizing the model
  specifications, passed to the rjd3 specification functions internally.
  As alternative input to `...`.

------------------------------------------------------------------------

### Method `run()`

run the model

#### Usage

    hierarchicalTimeSeries$run(verbose = FALSE)

#### Arguments

- `verbose`:

  if `FALSE` (the default), the results of the run will be returned
  invisibly

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print a hierarchical timeseries to screen

#### Usage

    hierarchicalTimeSeries$print()

------------------------------------------------------------------------

### Method [`iterate()`](https://statistikat.github.io/persephone3/reference/iterate.md)

iterate over all components

#### Usage

    hierarchicalTimeSeries$iterate(
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
simultaneously

------------------------------------------------------------------------

### Method `getComponent()`

extract a component series

#### Usage

    hierarchicalTimeSeries$getComponent(componentId)

#### Arguments

- `componentId`:

  the id of a component

------------------------------------------------------------------------

### Method `updateParams()`

change parameters of components or aggregate

#### Usage

    hierarchicalTimeSeries$updateParams(
      component = "",
      iterate = TRUE,
      speclist = NULL,
      ...
    )

#### Arguments

- `component`:

  character scalar name of the component to change; if empty, operate on
  aggregate

- `iterate`:

  logical: if TRUE, apply parameter changes to all components
  recursively

- `speclist`:

  list with named arguments to be changed

- `...`:

  named arguments to be changed (spec options forwarded to updateFun)

------------------------------------------------------------------------

### Method [`generateQrTable()`](https://statistikat.github.io/persephone3/reference/generateQrTable.md)

Generate a table for the eurostat quality report

#### Usage

    hierarchicalTimeSeries$generateQrTable(component = "")

#### Arguments

- `component`:

  (optional) a sub-component to create the report for

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    hierarchicalTimeSeries$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
objX13 <- perX13(AirPassengers, "rsa3")

# change parameters for all series (iterate = TRUE)
ht <- perHts(a = objX13, b = objX13, method = "x13", template = "rsa0",
            automdl.enabled = FALSE,
            arima.p = 1, arima.d = 2, arima.q = 0,
            arima.bp = 1, arima.bd = 1, arima.bq = 0,
            iterate = TRUE)

ht$run()
ht

# change parameters only for aggregate series (iterate = FALSE)
ht2 <- perHts(a = ht, b = objX13, method = "x13", template = "rsa0",
             transform.fun = "Log", transform.adjust = NA,
             easter.enabled = TRUE,
             outliers = c("AO1953.7", "LS1954.6"),
             iterate = FALSE)
ht2$run()
ht2
} # }
```
