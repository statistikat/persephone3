# Update rjd3-spec object for seasonal adjustment

Internal specification dispatching for rjd3x13/rjd3tramoseats.

## Usage

``` r
update_spec(spec = NULL, method = c("x13", "tramoseats"), freq, spec_list)
```

## Arguments

- spec:

  an rjd3-specification object.

- method:

  the seasonal adjustment method to be used, i.e. one of the characters
  "x13" oder "tramoseats".

- freq:

  the frequency of the time series, i.e. one of the integers 12 (months)
  or 4 (quarters). Used for parsing outlier dates.

- spec_list:

  a list containing the additional arguments for customizing the model
  specifications. Passed to the rjd3 specification functions internally.
  See
  [`perX13`](https://statistikat.github.io/persephone3/reference/perX13.md)
  or
  [`perTramo`](https://statistikat.github.io/persephone3/reference/perTramo.md)
  for more details.

## Value

An updated specification object (JD3\_\*\_SPEC).
