# persephone class

Common interface for x13 and tramoseats

## Constructor

- `new()`: Initializes an object. See the documentation for the derived
  classes for details.

## Methods

- `run(verbose = FALSE)`: Runs the model

- `updateParams(...)`: Updates the current parameters

- `plot(...)`: Interactive time series plot for a persephone object, see
  [plot.persephoneSingle](https://statistikat.github.io/persephone3/reference/plot.md)
  or
  [plot.hierarchicalTimeSeries](https://statistikat.github.io/persephone3/reference/plot.md)

- `plotSeasIrrCal(...)`: Interactive plot of the seasonal component,
  irregular component and calendar effects for a persephone object, see
  [plotSeasIrrCal](https://statistikat.github.io/persephone3/reference/plotSeasIrrCal.md)

- `plotResiduals(...)`: Several interactive plots in connection with
  residuals for a persephone object, see
  [plotResiduals](https://statistikat.github.io/persephone3/reference/plotResiduals.md)

## Read-Only fields

- `params`: A parameters object of class `SA_spec`. See
  [rjd3x13::x13_spec](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
  and
  [rjd3tramoseats::tramoseats_spec](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html).

- `ts`: The time series.

- `output`: The return value from the underlying functions `x13_fast` or
  `tramoseats_fast`. This slot will be empty (`NULL`) before `run()` is
  called for the first time.

## Parameters

- `verbose`: should the output from the underlying
  rjd3x13/rjd3tramoseats function be displayed after the run? If `FALSE`
  (the default), the output will be returned invisibly.

## derived classes

[`perX13()`](https://statistikat.github.io/persephone3/reference/perX13.md),
[`perTramo()`](https://statistikat.github.io/persephone3/reference/perTramo.md),
[`perHts()`](https://statistikat.github.io/persephone3/reference/perHts.md)

## Active bindings

- `params`:

  A parameters object of class `SA_spec`. See
  [`rjd3x13::x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
  and
  [`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html).

- `method`:

  SA method used

- `ts`:

  the (unajusted) time series

- `tsp`:

  the [`tsp()`](https://rdrr.io/r/stats/tsp.html) of the underlying time
  series

- `output`:

  The return value from the underlying functions x13_fast() or
  tramoseats_fast(). This slot will be empty (NULL) before run() is
  called for the first time.

- `context`:

  passed to
  [`rjd3x13::x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  and
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)
  when the `$run()` method is invoked

- `adjusted`:

  get the adjusted series

- `adjustedDirect`:

  see
  [perHts](https://statistikat.github.io/persephone3/reference/perHts.md)

- `forecasts`:

  get forecasts from the model

- `forecastsDirect`:

  get direct forecasts from the model

## Methods

### Public methods

- [`persephone$new()`](#method-persephone-new)

- [`persephone$run()`](#method-persephone-run)

- [`persephone$updateParams()`](#method-persephone-updateParams)

- [`persephone$plot()`](#method-persephone-plot)

- [`persephone$hchart()`](#method-persephone-hchart)

- [`persephone$plotResiduals()`](#method-persephone-plotResiduals)

- [`persephone$plotSeasIrrCal()`](#method-persephone-plotSeasIrrCal)

- [`persephone$plotSiRatios()`](#method-persephone-plotSiRatios)

- [`persephone$plotSpectrum()`](#method-persephone-plotSpectrum)

- [`persephone$print()`](#method-persephone-print)

- [`persephone$diagnosticsTable()`](#method-persephone-diagnosticsTable)

- [`persephone$iterate()`](#method-persephone-iterate)

- [`persephone$generateQrTable()`](#method-persephone-generateQrTable)

- [`persephone$fixModel()`](#method-persephone-fixModel)

- [`persephone$fixOutlier()`](#method-persephone-fixOutlier)

- [`persephone$clone()`](#method-persephone-clone)

------------------------------------------------------------------------

### Method `new()`

create a new persephone object

#### Usage

    persephone$new()

#### Details

this function should not be used directly because this class is
abstract. Only initialize derived classes.

------------------------------------------------------------------------

### Method `run()`

perform a run of the model

#### Usage

    persephone$run(verbose = FALSE)

#### Arguments

- `verbose`:

  whether to show outputs

------------------------------------------------------------------------

### Method `updateParams()`

update parameters for the adjustment

#### Usage

    persephone$updateParams(
      template = NULL,
      speclist = NULL,
      spec = NULL,
      context = NULL,
      userdefined = NULL,
      iterate = FALSE,
      component = "",
      ...
    )

#### Arguments

- `template`:

  template name passed to `x13_spec()` or `tramoseats_spec()`

- `speclist`:

  a list of additional arguments for customizing the model
  specifications

- `spec`:

  a specification object to use as starting point

- `context`:

  a list of external regressors (calendar or other) to be used for
  estimation

- `userdefined`:

  character vector of user-defined output variables

- `iterate`:

  logical, whether to iterate (for hierarchical objects)

- `component`:

  character, component id for hierarchical objects

- `...`:

  additional arguments passed to `x13_spec()` or `tramoseats_spec()`

------------------------------------------------------------------------

### Method [`plot()`](https://statistikat.github.io/persephone3/reference/plot.md)

visualize the results of an adjustment

#### Usage

    persephone$plot(...)

#### Arguments

- `...`:

  passed to
  [`plot()`](https://statistikat.github.io/persephone3/reference/plot.md)
  see plot method for persephoneSingle or hierarchicalTimeSeries objects

------------------------------------------------------------------------

### Method [`hchart()`](https://jkunst.com/highcharter/reference/hchart.html)

visualize the results of an adjustment

#### Usage

    persephone$hchart(...)

#### Arguments

- `...`:

  passed to
  [`hchart()`](https://jkunst.com/highcharter/reference/hchart.html) see
  the hchart method for persephoneSingle or hierarchicalTimeSeries
  objects

------------------------------------------------------------------------

### Method [`plotResiduals()`](https://statistikat.github.io/persephone3/reference/plotResiduals.md)

visualize residuals

#### Usage

    persephone$plotResiduals(...)

#### Arguments

- `...`:

  passed to
  [`plotResiduals()`](https://statistikat.github.io/persephone3/reference/plotResiduals.md)

------------------------------------------------------------------------

### Method [`plotSeasIrrCal()`](https://statistikat.github.io/persephone3/reference/plotSeasIrrCal.md)

visualize the seasonal component, irregular component and calendar
effects for a persephone object

#### Usage

    persephone$plotSeasIrrCal(...)

#### Arguments

- `...`:

  passed to
  [`plotSeasIrrCal()`](https://statistikat.github.io/persephone3/reference/plotSeasIrrCal.md)

------------------------------------------------------------------------

### Method [`plotSiRatios()`](https://statistikat.github.io/persephone3/reference/plotSiRatios.md)

visualize SI-ratios (and, in case of x11, of seasonal factors) by period

#### Usage

    persephone$plotSiRatios(...)

#### Arguments

- `...`:

  passed to
  [`plotSiRatios()`](https://statistikat.github.io/persephone3/reference/plotSiRatios.md)

------------------------------------------------------------------------

### Method [`plotSpectrum()`](https://statistikat.github.io/persephone3/reference/plotSpectrum.md)

visualize the periodogram or the autogregressive spectrum.

#### Usage

    persephone$plotSpectrum(...)

#### Arguments

- `...`:

  passed to
  [`plotSpectrum()`](https://statistikat.github.io/persephone3/reference/plotSpectrum.md)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

display a persephone object in the console

#### Usage

    persephone$print()

------------------------------------------------------------------------

### Method [`diagnosticsTable()`](https://statistikat.github.io/persephone3/reference/diagnosticsTable.md)

display a persephone object in the console

create a table for the eurostat quality report

#### Usage

    persephone$diagnosticsTable()

------------------------------------------------------------------------

### Method [`iterate()`](https://statistikat.github.io/persephone3/reference/iterate.md)

see
[perHts](https://statistikat.github.io/persephone3/reference/perHts.md)

#### Usage

    persephone$iterate(fun, asTable = FALSE, unnest = FALSE)

#### Arguments

- `fun`:

  see
  [perHts](https://statistikat.github.io/persephone3/reference/perHts.md)

- `asTable`:

  see
  [perHts](https://statistikat.github.io/persephone3/reference/perHts.md)

- `unnest`:

  see
  [perHts](https://statistikat.github.io/persephone3/reference/perHts.md)

------------------------------------------------------------------------

### Method [`generateQrTable()`](https://statistikat.github.io/persephone3/reference/generateQrTable.md)

create a table for the eurostat quality report

#### Usage

    persephone$generateQrTable()

------------------------------------------------------------------------

### Method [`fixModel()`](https://statistikat.github.io/persephone3/reference/fixModel.md)

fix the arima model

#### Usage

    persephone$fixModel(...)

#### Arguments

- `...`:

  additional arguments passed to
  [`fixModel`](https://statistikat.github.io/persephone3/reference/fixModel.md)

------------------------------------------------------------------------

### Method [`fixOutlier()`](https://statistikat.github.io/persephone3/reference/fixOutlier.md)

create a new single object

#### Usage

    persephone$fixOutlier(...)

#### Arguments

- `...`:

  additional arguments passed to
  [`fixOutlier`](https://statistikat.github.io/persephone3/reference/fixOutlier.md)

- `timespan`:

  number of months from the end of the time series where outliers are
  not fixed

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    persephone$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
