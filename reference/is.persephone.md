# is.persephone

Check if an R object is of or derives from the class `persephone` or
convert an object to persephone with method "tramoseats" as default.

## Usage

``` r
is.persephone(x)

as.persephone(x, method = c("tramoseats", "x13"), template = NULL)
```

## Arguments

- x:

  R object to be tested or converted.

- method:

  SA method used when coercing a bare ts object, i.e. "tramoseats"
  (default) or "x13". Ignored for existing persephone objects, whose
  method is fixed by their class.

- template:

  template name passed to the corresponding spec function; if NULL, the
  method-specific default is used.

## Examples

``` r
is.persephone(2)
#> [1] FALSE
obj <- as.persephone(AirPassengers)
obj$run()
obj <- as.persephone(AirPassengers)
obj_x13 <- as.persephone(AirPassengers, method = "x13", template = "rsa3")
```
