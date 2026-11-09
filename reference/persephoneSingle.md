# Class for single time series

Class for single time series

Class for single time series

## Details

A common class for the wrapper classes
[x13Single](https://statistikat.github.io/persephone3/reference/x13Single.md)
and
[tramoseatsSingle](https://statistikat.github.io/persephone3/reference/tramoseatsSingle.md)

## Super class

[`persephone3::persephone`](https://statistikat.github.io/persephone3/reference/persephone.md)
-\> `persephoneSingle`

## Methods

### Public methods

- [`persephoneSingle$new()`](#method-persephoneSingle-new)

- [`persephoneSingle$run()`](#method-persephoneSingle-run)

- [`persephoneSingle$clone()`](#method-persephoneSingle-clone)

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

------------------------------------------------------------------------

### Method `new()`

create a new single object

#### Usage

    persephoneSingle$new(
      ts,
      template = "rsa3",
      context = NULL,
      userdefined = NULL,
      speclist = NULL,
      ...
    )

#### Arguments

- `ts`:

  a time series object

- `template`:

  passed as the `spec` argument of
  [`rjd3x13::x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  or
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)

- `context`:

  passed as the `context` argument of
  [`rjd3x13::x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  or
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html),
  a list of external regressors (calendar or other) to be used for
  estimation

- `userdefined`:

  passed as the `userdefined` argument of
  [`rjd3x13::x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  or
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)

- `speclist`:

  a list of additional arguments for customizing the model
  specifications, passed to the rjd3 specification functions internally.
  As alternative input to `...`.

- `...`:

  passed to
  [`rjd3x13::x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
  or
  [`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html)
  if no speclist parameter is set.

------------------------------------------------------------------------

### Method `run()`

run the model

#### Usage

    persephoneSingle$run(verbose = FALSE)

#### Arguments

- `verbose`:

  wether to display outputs after the run

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    persephoneSingle$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
