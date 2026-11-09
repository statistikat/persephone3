# Define a hierarchical time series

Combine mutliple objects of persephone objects into a new persephone
object. The resulting time series can perform direct and indirect
adjustments.

## Usage

``` r
perHts(
  ...,
  list = NULL,
  method = c("tramoseats", "x13"),
  template = "rsa3",
  context = NULL,
  userdefined = NULL,
  speclist = NULL,
  spec = NULL,
  weights = NULL,
  iterate = TRUE
)
```

## Arguments

- ...:

  one or more objects which are either of class persephone or can be
  coerced to persephone objects with asPersephone. If more than one
  element is supplied, the underlying time series must have the same
  time instances. All elements supplied in ... must be named.

  In addition to the ts/mts/persephone objects, `...` accepts arguments
  for customizing the model specifications as alternative input to
  speclist. These are passed to the rjd3 specification functions
  internally.

- list:

  a list of persephone objects as alternative input to `...`. This
  argument can also handle mts objects.

- method:

  specifies the method to be used for the direct adjustment of the
  aggregate series, i.e. "tramoseats" or "x13".

- template:

  the name of the predefined specification template, passed as the
  `name` argument to
  [`rjd3x13::x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
  or
  [`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html).
  Must be one of: `"rsa0"`, `"rsa1"`, `"rsa2c"`, `"rsa3"`, `"rsa4"`,
  `"rsa5c"` for x13 `"rsafull"`, `"rsa0"`, `"rsa1"`, `"rsa2"`, `"rsa3"`,
  `"rsa4"`, `"rsa5"` for tramoseats. Defaults to `"rsa3"`.

- context:

  a list of external regressors (calendar or other) to be used for
  estimation, passed as the `context` argument of
  [`rjd3x13::x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  or
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)..

- userdefined:

  a character vector of user-defined variables to be included in the
  output, passed as the `userdefined` argument to
  [`rjd3x13::x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  or
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html).

- speclist:

  a list of additional arguments for customizing the model
  specifications, passed to the rjd3 specification functions internally.
  As alternative input to `...`.

- spec:

  a model specification object of class JD3_X13_SPEC or
  JD3_TRAMOSEATS_SPEC returned by
  [`rjd3x13::x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
  or
  [`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html).

- weights:

  either a vector if the same weight is used for all time points or a
  list of ts objects or a mts object if the weight varies for different
  time points. They must have the same length as the number of
  components.

- iterate:

  logical, if TRUE apply the given spec/params also to all components
  recursively (only takes effect when a spec/parameter is supplied).
  Plain ts/mts components inherit method and template only if iterate is
  TRUE; otherwise they are created with the method-specific defaults of
  [`perTramo()`](https://statistikat.github.io/persephone3/reference/perTramo.md)
  /
  [`perX13()`](https://statistikat.github.io/persephone3/reference/perX13.md).

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
ht2$adjustedDirect
ht2$adjustedIndirect
} # }
```
