# Hierarchical time series - construction and weights

Hierarchical time series are time series that can be divided into
sub-series. In `persephone3`, such objects are created similar to a
nested list. The following example creates a series with European
aggregates based on export data from different European countries.

First we load the `Exports` data from `rjd3toolkit`:

``` r

library(persephone3)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo

data(Exports, package = "rjd3toolkit")
```

For all countries’ time series we now want to generate a persephone
object with Tramo as method.

``` r

# EU countries only (27 member states, excluding UK and non-EU countries)
eu_countries <- c("France", "Luxembourg", "Netherlands", "Germany", "Italy",
                  "Ireland", "Denmark", "Greece", "Portugal",
                  "Spain", "Sweden", "Finland", "Austria", "Malta", "Estonia",
                  "Latvia", "Lithuania", "Poland", "Czech.Republic", "Slovakia",
                  "Hungary", "Slovenia", "Cyprus", "Bulgaria", "Romania", "Croatia")

ts_28 <- lapply(Exports[eu_countries], perTramo)
```

We generate a additional layer based on a certain regional grouping of
the countries.

``` r

# Define regional groupings using the actual country names from the dataset
south_eastern_europe <- c("Greece", "Cyprus", "Bulgaria", "Romania")
northern_europe <- c("Denmark", "Finland", "Sweden", "Ireland")
central_europe <- c("Croatia", "Germany", "Czech.Republic", "Slovakia", "Hungary",
                    "Austria", "Slovenia", "Luxembourg", "Estonia", "Lithuania",
                    "Latvia", "Poland")
southern_europe <- c("Italy", "Spain", "Portugal", "Malta")
western_europe <- c("France", "Netherlands")
```

The list argument can be used to create a hierachical persephone object
based on a list of persephone objects.

``` r

hts_see <- perHts(list = ts_28[south_eastern_europe])
hts_ne <- perHts(list = ts_28[northern_europe])
hts_ce <- perHts(list = ts_28[central_europe])
hts_se <- perHts(list = ts_28[southern_europe])
hts_we <- perHts(list = ts_28[western_europe])
```

An additional layer is only the nothern part and southern part

``` r

hts_np <- perHts(norhtern_europe = hts_ne,
                 western_europe = hts_we,
                 central_europe = hts_ce
)
hts_sp <- perHts(southern_europe = hts_se,
                 south_eastern_europe = hts_see
)
```

Finally we aggregate them to the whole of EU.

``` r

ht_europe <- perHts(north_part = hts_np,
                 south_part = hts_sp
)
ht_europe
#>  component                                run   method     type     
#>                                           FALSE tramoseats aggregate
#>  north_part                               FALSE tramoseats aggregate
#>  north_part/norhtern_europe               FALSE tramoseats aggregate
#>  north_part/norhtern_europe/Denmark       FALSE tramoseats single   
#>  north_part/norhtern_europe/Finland       FALSE tramoseats single   
#>  north_part/norhtern_europe/Sweden        FALSE tramoseats single   
#>  north_part/norhtern_europe/Ireland       FALSE tramoseats single   
#>  north_part/western_europe                FALSE tramoseats aggregate
#>  north_part/western_europe/France         FALSE tramoseats single   
#>  north_part/western_europe/Netherlands    FALSE tramoseats single   
#>  north_part/central_europe                FALSE tramoseats aggregate
#>  north_part/central_europe/Croatia        FALSE tramoseats single   
#>  north_part/central_europe/Germany        FALSE tramoseats single   
#>  north_part/central_europe/Czech.Republic FALSE tramoseats single   
#>  north_part/central_europe/Slovakia       FALSE tramoseats single   
#>  north_part/central_europe/Hungary        FALSE tramoseats single   
#>  north_part/central_europe/Austria        FALSE tramoseats single   
#>  north_part/central_europe/Slovenia       FALSE tramoseats single   
#>  north_part/central_europe/Luxembourg     FALSE tramoseats single   
#>  north_part/central_europe/Estonia        FALSE tramoseats single   
#>  north_part/central_europe/Lithuania      FALSE tramoseats single   
#>  north_part/central_europe/Latvia         FALSE tramoseats single   
#>  north_part/central_europe/Poland         FALSE tramoseats single   
#>  south_part                               FALSE tramoseats aggregate
#>  south_part/southern_europe               FALSE tramoseats aggregate
#>  south_part/southern_europe/Italy         FALSE tramoseats single   
#>  south_part/southern_europe/Spain         FALSE tramoseats single   
#>  south_part/southern_europe/Portugal      FALSE tramoseats single   
#>  south_part/southern_europe/Malta         FALSE tramoseats single   
#>  south_part/south_eastern_europe          FALSE tramoseats aggregate
#>  south_part/south_eastern_europe/Greece   FALSE tramoseats single   
#>  south_part/south_eastern_europe/Cyprus   FALSE tramoseats single   
#>  south_part/south_eastern_europe/Bulgaria FALSE tramoseats single   
#>  south_part/south_eastern_europe/Romania  FALSE tramoseats single
```

Now we make the first run:

``` r

ht_europe$run()
ht_europe
#>  component                                run  method     type     
#>                                           TRUE tramoseats aggregate
#>  north_part                               TRUE tramoseats aggregate
#>  north_part/norhtern_europe               TRUE tramoseats aggregate
#>  north_part/norhtern_europe/Denmark       TRUE tramoseats single   
#>  north_part/norhtern_europe/Finland       TRUE tramoseats single   
#>  north_part/norhtern_europe/Sweden        TRUE tramoseats single   
#>  north_part/norhtern_europe/Ireland       TRUE tramoseats single   
#>  north_part/western_europe                TRUE tramoseats aggregate
#>  north_part/western_europe/France         TRUE tramoseats single   
#>  north_part/western_europe/Netherlands    TRUE tramoseats single   
#>  north_part/central_europe                TRUE tramoseats aggregate
#>  north_part/central_europe/Croatia        TRUE tramoseats single   
#>  north_part/central_europe/Germany        TRUE tramoseats single   
#>  north_part/central_europe/Czech.Republic TRUE tramoseats single   
#>  north_part/central_europe/Slovakia       TRUE tramoseats single   
#>  north_part/central_europe/Hungary        TRUE tramoseats single   
#>  north_part/central_europe/Austria        TRUE tramoseats single   
#>  north_part/central_europe/Slovenia       TRUE tramoseats single   
#>  north_part/central_europe/Luxembourg     TRUE tramoseats single   
#>  north_part/central_europe/Estonia        TRUE tramoseats single   
#>  north_part/central_europe/Lithuania      TRUE tramoseats single   
#>  north_part/central_europe/Latvia         TRUE tramoseats single   
#>  north_part/central_europe/Poland         TRUE tramoseats single   
#>  south_part                               TRUE tramoseats aggregate
#>  south_part/southern_europe               TRUE tramoseats aggregate
#>  south_part/southern_europe/Italy         TRUE tramoseats single   
#>  south_part/southern_europe/Spain         TRUE tramoseats single   
#>  south_part/southern_europe/Portugal      TRUE tramoseats single   
#>  south_part/southern_europe/Malta         TRUE tramoseats single   
#>  south_part/south_eastern_europe          TRUE tramoseats aggregate
#>  south_part/south_eastern_europe/Greece   TRUE tramoseats single   
#>  south_part/south_eastern_europe/Cyprus   TRUE tramoseats single   
#>  south_part/south_eastern_europe/Bulgaria TRUE tramoseats single   
#>  south_part/south_eastern_europe/Romania  TRUE tramoseats single   
#>  seasonality  logTransform arimaMdl       nOutliers Q  Q2 TotalQ
#>  Present       TRUE        (2 1 1)(0 1 1)  2        NA NA Severe
#>  Present       TRUE        (2 1 1)(0 1 1)  2        NA NA Severe
#>  Present       TRUE        (3 1 0)(0 1 1)  3        NA NA Severe
#>  Present       TRUE        (2 1 1)(0 1 1)  4        NA NA Severe
#>  Present       TRUE        (2 1 3)(0 1 1)  2        NA NA Severe
#>  Present       TRUE        (2 1 0)(0 1 1)  1        NA NA Severe
#>  Present       TRUE        (0 1 1)(0 1 1)  2        NA NA Good  
#>  Present       TRUE        (2 1 1)(0 1 1)  3        NA NA Severe
#>  Present       TRUE        (3 1 1)(0 1 1)  2        NA NA Severe
#>  Present       TRUE        (3 1 1)(0 1 1)  1        NA NA Severe
#>  Present       TRUE        (2 1 1)(0 1 1)  3        NA NA Severe
#>  Present       TRUE        (0 1 1)(0 1 1)  2        NA NA Severe
#>  Present       TRUE        (2 1 3)(0 1 1)  1        NA NA Severe
#>  Present       TRUE        (2 1 0)(0 1 1)  3        NA NA Severe
#>  Present      FALSE        (2 1 0)(0 1 1)  2        NA NA Severe
#>  Present       TRUE        (2 1 1)(0 1 1)  2        NA NA Severe
#>  Present       TRUE        (3 1 1)(0 1 1)  3        NA NA Severe
#>  Present       TRUE        (0 1 1)(0 1 1) NA        NA NA Severe
#>  Present       TRUE        (2 1 0)(0 1 1)  4        NA NA Severe
#>  Present       TRUE        (0 1 1)(0 1 1)  1        NA NA Severe
#>  Present       TRUE        (0 1 1)(0 1 1)  5        NA NA Severe
#>  Present       TRUE        (1 1 1)(0 1 1)  1        NA NA Severe
#>  Present       TRUE        (2 1 0)(0 1 1)  3        NA NA Severe
#>  Present       TRUE        (2 1 1)(0 1 1)  1        NA NA Severe
#>  Present       TRUE        (2 1 0)(0 1 1)  1        NA NA Severe
#>  Present       TRUE        (2 1 1)(0 1 1)  2        NA NA Severe
#>  Present       TRUE        (3 1 1)(0 1 1)  1        NA NA Severe
#>  Present       TRUE        (3 1 1)(0 1 1)  2        NA NA Severe
#>  ProbablyNone  TRUE        (0 1 1)(0 1 1)  9        NA NA Good  
#>  Present       TRUE        (0 1 1)(0 1 1)  3        NA NA Severe
#>  Present       TRUE        (0 1 1)(0 1 1)  8        NA NA Severe
#>  Present       TRUE        (2 1 1)(0 1 1)  9        NA NA Severe
#>  Present       TRUE        (0 1 1)(0 1 1)  3        NA NA Severe
#>  Present       TRUE        (1 1 2)(0 1 1)  4        NA NA Severe
```

## Weights

Weights can be used to customize the aggregation step, so the
hierachical time series is not just the sum of its components, but the
weighted sum. This can be especially helpful if the timeseries are
indices.

### Time-invariant weights

The weights parameter will be set as numeric vector with length equal to
the number of components.

Almost the same call, but with weights, one for each component.

``` r

hts_see <- perHts(list = ts_28[south_eastern_europe], weights=c(1,2,3,4))
hts_ne <- perHts(list = ts_28[northern_europe], weights = c(5,6,7,8)) # 4 countries: Denmark, Finland, Sweden, Ireland
hts_ce <- perHts(list = ts_28[central_europe], weights = rep(1,12))
hts_se <- perHts(list = ts_28[southern_europe], weights = c(8,9,10,10.5))
hts_we <- perHts(list = ts_28[western_europe], weights = c(11,12)) # 2 countries: France, Netherlands
```

In additional aggregation steps, the weight is automatically summed up.
So e.g. the weight for central europe is 12, the sum of 12 times 1.

``` r

hts_np <- perHts(northern_europe = hts_ne,
                 western_europe = hts_we,
                 central_europe = hts_ce
)
hts_sp <- perHts(southern_europe = hts_se,
                 south_eastern_europe = hts_see
)
knitr::kable(head(hts_np$weights))
```

| northern_europe | western_europe | central_europe |
|----------------:|---------------:|---------------:|
|              26 |             23 |             12 |
|              26 |             23 |             12 |
|              26 |             23 |             12 |
|              26 |             23 |             12 |
|              26 |             23 |             12 |
|              26 |             23 |             12 |

``` r

ht_europe <- perHts(north_part = hts_np,
                 south_part = hts_sp
)

knitr::kable(head(ht_europe$weights))
```

| north_part | south_part |
|-----------:|-----------:|
|         61 |       47.5 |
|         61 |       47.5 |
|         61 |       47.5 |
|         61 |       47.5 |
|         61 |       47.5 |
|         61 |       47.5 |

### Time-variant weights

In many scenarios weights are not fixed over time, but change, e.g. if
the turnover is used to aggregate a certain production index. In these
scenarios weights can be provided as lists of time series or as a mts
object.

We are starting with weights equal to 1 and then only change the weight
of Germany over time.

``` r

# Convert list to mts (multi-time series) object
weights_eu <- do.call(cbind, Exports[eu_countries]) * 0 + 1
weights_eu[is.na(weights_eu)] <- 1
weights_eu[,"Germany"] <- weights_eu[,"Germany"] + rlnorm(nrow(weights_eu))
```

We can now call the `perHts` function with the mts as input.

``` r

hts_see <- perHts(list = ts_28[south_eastern_europe],
                   weights=weights_eu[,south_eastern_europe])
hts_ne <- perHts(list = ts_28[northern_europe],
                  weights=weights_eu[,northern_europe])
hts_ce <- perHts(list = ts_28[central_europe],
                  weights=weights_eu[,central_europe])
hts_se <- perHts(list = ts_28[southern_europe],
                  weights=weights_eu[,southern_europe])
hts_we <- perHts(list = ts_28[western_europe],
                  weights=weights_eu[,western_europe])
```

And then we can proceed as before.

``` r

hts_np <- perHts(northern_europe = hts_ne,
                 western_europe = hts_we,
                 central_europe = hts_ce
)
hts_sp <- perHts(southern_europe = hts_se,
                 south_eastern_europe = hts_see
)
knitr::kable(head(hts_np$weights))
```

| northern_europe | western_europe | central_europe |
|----------------:|---------------:|---------------:|
|               4 |              2 |       12.24659 |
|               4 |              2 |       13.29087 |
|               4 |              2 |       12.08740 |
|               4 |              2 |       12.99444 |
|               4 |              2 |       13.86182 |
|               4 |              2 |       15.15318 |

``` r

ht_europe <- perHts(north_part = hts_np,
                 south_part = hts_sp
)

knitr::kable(head(ht_europe$weights))
```

| north_part | south_part |
|-----------:|-----------:|
|   18.24659 |          8 |
|   19.29087 |          8 |
|   18.08740 |          8 |
|   18.99444 |          8 |
|   19.86182 |          8 |
|   21.15318 |          8 |

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
