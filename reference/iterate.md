# Iterate over a hierarchical persephone object

This funtion is an alias to the method `iterate` and can be used to
iterate over all children of a hierarchical time series produced with
[`perHts()`](https://statistikat.github.io/persephone3/reference/perHts.md).

## Usage

``` r
iterate(x, fun, asTable = FALSE, component = "", unnest = FALSE)
```

## Arguments

- x:

  a hierarchical persephone object

- fun:

  a function with one argument

- asTable:

  should the return value be coerced to a `data.frame`?

- component:

  a component id from which the iteration should start

- unnest:

  should the return value be unnested, so that every component is an
  entry of the resulting list?

## Examples

``` r
obj_x13 <- perX13(AirPassengers, "rsa3")
ht <- perHts(a = obj_x13, b = obj_x13, method = "x13")
ht2 <- perHts(a = ht, b = obj_x13)
#> Warning: Components ‘a’, ‘b’ are already 'x13' objects and cannot be switched to method = "tramoseats"; they stay 'x13'. iterate = TRUE updates their spec parameters but not their method; pass them as plain ts/mts objects to have them created with 'tramoseats'.
ht2$iterate(function(x) {class(x)[1]})
#> $value
#> [1] "hierarchicalTimeSeries"
#> 
#> $a
#> $a$value
#> [1] "hierarchicalTimeSeries"
#> 
#> $a$a
#> $a$a$value
#> [1] "x13Single"
#> 
#> 
#> $a$b
#> $a$b$value
#> [1] "x13Single"
#> 
#> 
#> 
#> $b
#> $b$value
#> [1] "x13Single"
#> 
#> 
ht2$iterate(function(x) {list(class = class(x)[1])}, asTable = TRUE)
#>   component                  class
#> 1           hierarchicalTimeSeries
#> 2         a hierarchicalTimeSeries
#> 3       a/a              x13Single
#> 4       a/b              x13Single
#> 5         b              x13Single
ht2$iterate(function(x) {list(class = class(x)[1])}, unnest = TRUE)
#> [[1]]
#> [[1]]$class
#> [1] "hierarchicalTimeSeries"
#> 
#> 
#> $a
#> $a$class
#> [1] "hierarchicalTimeSeries"
#> 
#> 
#> $`a/a`
#> $`a/a`$class
#> [1] "x13Single"
#> 
#> 
#> $`a/b`
#> $`a/b`$class
#> [1] "x13Single"
#> 
#> 
#> $b
#> $b$class
#> [1] "x13Single"
#> 
#> 
```
