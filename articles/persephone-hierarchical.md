# Hierarchical time series with persephone3

## Introduction

Hierarchical time series are time series that can be divided into
sub-series with different levels of aggregation. In `persephone3`, such
objects are created similar to a nested list structure.

The following example creates a hierarchical object with two subseries
`a` and `b`:

``` r

library(persephone3)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo

hts <- perHts(
  a = perX13(AirPassengers),
  b = perTramo(AirPassengers)
)
#> Warning: Components 'a' are already 'x13' objects and cannot be
#> switched to method = "tramoseats"; they stay 'x13'. iterate = TRUE
#> updates their spec parameters but not their method; pass them as
#> plain ts/mts objects to have them created with 'tramoseats'.
hts
#>  component run   method     type     
#>            FALSE tramoseats aggregate
#>  a         FALSE x13        single   
#>  b         FALSE tramoseats single
```

### Running the Model

When the `run()` method of `hts` is invoked, the following adjustments
are performed:

1.  The aggregate series `a+b` is adjusted and the results are saved in
    `hts$output`
2.  `a` is adjusted using X-13ARIMA-SEATS
3.  `b` is adjusted using TRAMO/SEATS

The results can be viewed as follows:

``` r

display_result <- function(ts) {
  window(ts, end = c(1949, 6))
}

hts$run()

# Direct adjustment (seasonally adjusted aggregate series)
display_result(hts$adjusted)
#> Warning in (function () : The decision between direct and indirect adjustment was not recorded yet. 
#> Direct adjustment is returned.
#>           Jan      Feb      Mar      Apr      May      Jun
#> 1949 247.6452 250.2852 249.5234 256.3732 251.9721 251.5898

# Seasonally adjusted component series "a"
display_result(hts$components$a$adjusted)
#>           Jan      Feb      Mar      Apr      May      Jun
#> 1949 124.5467 124.6262 124.8914 129.0707 125.1287 126.7552

# Indirect adjustment (sum of seasonally adjusted components)
display_result(hts$adjustedIndirect)
#>           Jan      Feb      Mar      Apr      May      Jun
#> 1949 248.3693 249.7688 249.6531 257.2573 251.1148 252.5501
```

The last line displays the results of an indirect adjustment, which
amounts to `a$adjusted` + `b$adjusted` in this case.

### Multiple Levels of Hierarchies

For more complex structures with multiple hierarchy levels, `perHts`
calls can be nested:

``` r

hts_nested <- perHts(
  hts = hts,
  hts_child = perHts(
    a = perX13(AirPassengers)
  ),
  b = perTramo(AirPassengers)
)
#> Warning: Components 'a' are already 'x13' objects and cannot be
#> switched to method = "tramoseats"; they stay 'x13'. iterate = TRUE
#> updates their spec parameters but not their method; pass them as
#> plain ts/mts objects to have them created with 'tramoseats'.

hts_nested
#>  component   run   method     type      seasonality logTransform
#>              FALSE tramoseats aggregate <NA>          NA        
#>  hts          TRUE tramoseats aggregate Present     TRUE        
#>  hts/a        TRUE x13        single    Present     TRUE        
#>  hts/b        TRUE tramoseats single    Present     TRUE        
#>  hts_child   FALSE tramoseats aggregate <NA>          NA        
#>  hts_child/a FALSE x13        single    <NA>          NA        
#>  b           FALSE tramoseats single    <NA>          NA        
#>  arimaMdl       nOutliers Q    Q2   TotalQ
#>  <NA>           NA          NA   NA <NA>  
#>  (0 1 1)(0 1 1) NA          NA   NA Good  
#>  (0 1 1)(0 1 1) NA        0.26 0.29 Good  
#>  (0 1 1)(0 1 1) NA          NA   NA Good  
#>  <NA>           NA          NA   NA <NA>  
#>  <NA>           NA          NA   NA <NA>  
#>  <NA>           NA          NA   NA <NA>
```

As you can see, the structure of `hts` is available within this nested
object.

### Iterating Over the Hierarchy Tree

To iterate over all components of a `perHts` object, the
[`iterate()`](https://statistikat.github.io/persephone3/reference/iterate.md)
method can be used. It behaves similarly to
[`lapply()`](https://rdrr.io/r/base/lapply.html) and allows you to apply
custom functions to each node in the hierarchy:

``` r

# Run the model first
hts_nested$run()

# Apply custom function to get mean and standard deviation
hts_nested$iterate(asTable = TRUE, function(obj) {
  list(
    mean = mean(obj$adjusted),
    sd = sd(obj$adjusted)
  )
})
#>     component      mean       sd
#> 1             1120.3433 445.1091
#> 2         hts  560.1717 222.5546
#> 3       hts/a  280.0574 111.7210
#> 4       hts/b  280.0858 111.2773
#> 5   hts_child  280.0858 111.2773
#> 6 hts_child/a  280.0574 111.7210
#> 7           b  280.0858 111.2773
```

### Setting Options Recursively

When creating a `perHts` object, you can pass specification parameters
that will be applied to the aggregate series. With `iterate = TRUE` (the
default), these parameters are also applied to all components
recursively:

``` r

# Create hierarchical object with custom parameters applied to all nodes
hts_custom <- perHts(
  a = perX13(AirPassengers),
  b = perTramo(AirPassengers),
  template = "rsa1",
  automdl.enabled = FALSE
)

hts_custom$run()
hts_custom
#>  component run  method     type      seasonality logTransform
#>            TRUE tramoseats aggregate Present     TRUE        
#>  a         TRUE x13        single    Present     TRUE        
#>  b         TRUE tramoseats single    Present     TRUE        
#>  arimaMdl       nOutliers Q    Q2   TotalQ
#>  (0 1 1)(0 1 1) NA          NA   NA Good  
#>  (0 1 1)(0 1 1) NA        0.26 0.29 Good  
#>  (0 1 1)(0 1 1) NA          NA   NA Good
```

The `automdl.enabled = FALSE` setting was applied to both the aggregate
series and both components (`a` and `b`). You can verify this by
inspecting the specifications:

``` r

# Check aggregate spec
hts_custom$paramsDirect$automdl.enabled
#> NULL

# Check component a spec
hts_custom$components$a$paramsDirect$automdl.enabled
#> NULL

# Check component b spec
hts_custom$components$b$paramsDirect$automdl.enabled
#> NULL
```

To modify individual components after creation, access them directly:

``` r

# Re-enable automdl for component a only
hts_custom$components$a$updateParams(automdl.enabled = TRUE)
hts_custom$components$a$paramsDirect$automdl.enabled
#> NULL
```

## Next Steps

- See
  [`vignette("persephone3")`](https://statistikat.github.io/persephone3/articles/persephone3.md)
  for basic usage and template options
- See
  [`vignette("persephone-plotting")`](https://statistikat.github.io/persephone3/articles/persephone-plotting.md)
  for visualization methods for hierarchical objects
- Check
  [`?perHts`](https://statistikat.github.io/persephone3/reference/perHts.md)
  for complete documentation on hierarchical time series

## Repository

Source code and issue tracker:
<https://github.com/statistikat/persephone3>
