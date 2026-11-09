# tramoseats class

An R6 class that represents a tramoseats model.

## Usage

``` r
perTramo(
  ts,
  template = c("rsa3", "rsafull", "rsa0", "rsa1", "rsa2", "rsa4", "rsa5"),
  context = NULL,
  userdefined = NULL,
  speclist = NULL,
  ...
)
```

## Arguments

- ts:

  a time series.

- template:

  the name of the predefined specification, passed as the `name`
  argument to
  [`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html).
  Must be one of: `"rsafull"`, `"rsa0"`, `"rsa1"`, `"rsa2"`, `"rsa3"`,
  `"rsa4"`, `"rsa5"`. Defaults to `"rsa3"`.

- context:

  a list of external regressors (calendar or other) to be used for
  estimation, passed as the `context` argument of
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html).

- userdefined:

  a character vector of user-defined variables to be included in the
  output, passed as the `userdefined` argument to
  [`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html).

- speclist:

  a list of additional arguments for customizing the model
  specifications, passed to the rjd3 specification functions internally.
  As alternative input to `...`.

- ...:

  additional arguments for customizing the model specifications, passed
  to the rjd3 specification functions internally.

  These arguments allow you to set or update the specifications of the
  seasonal adjustment method by modifying one or more parameters.
  Parameters are organized into **spec-blocks**:

  - The first part of the parameter name (before the dot) indicates the
    block.

  - The second part (after the dot) specifies the parameter within that
    block.

  Supported spec-blocks and their corresponding functions:

  - `arima` →
    [`rjd3toolkit::set_arima`](https://rjdverse.github.io/rjd3toolkit/reference/set_arima.html)

  - `automdl` →
    [`rjd3toolkit::set_automodel`](https://rjdverse.github.io/rjd3toolkit/reference/set_automodel.html)

  - `basic` →
    [`rjd3toolkit::set_basic`](https://rjdverse.github.io/rjd3toolkit/reference/set_basic.html)

  - `easter` →
    [`rjd3toolkit::set_easter`](https://rjdverse.github.io/rjd3toolkit/reference/set_easter.html)

  - `estimate` →
    [`rjd3toolkit::set_estimate`](https://rjdverse.github.io/rjd3toolkit/reference/set_estimate.html)

  - `setout` →
    [`rjd3toolkit::set_outlier`](https://rjdverse.github.io/rjd3toolkit/reference/set_outlier.html)

  - `td` →
    [`rjd3toolkit::set_tradingdays`](https://rjdverse.github.io/rjd3toolkit/reference/set_tradingdays.html)

  - `transform` →
    [`rjd3toolkit::set_transform`](https://rjdverse.github.io/rjd3toolkit/reference/set_transform.html)

  - `addout` →
    [`rjd3toolkit::add_outlier`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html)

  - `remout` →
    [`rjd3toolkit::remove_outlier`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html)

  - `addramp` →
    [`rjd3toolkit::add_ramp`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html)

  - `remramp` →
    [`rjd3toolkit::remove_ramp`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html)

  - `usrdefvar` →
    [`rjd3toolkit::add_usrdefvar`](https://rjdverse.github.io/rjd3toolkit/reference/add_usrdefvar.html)

  - `x11` →
    [`rjd3x13::set_x11`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
    (x13 only)

  - `benchmarking` →
    [`rjd3toolkit::set_benchmarking`](https://rjdverse.github.io/rjd3toolkit/reference/set_benchmarking.html)

  The package offers convenient options for setting certain parameters
  to improve usability. Documentation pending.

## Value

An object of class
[`tramoseatsSingle`](https://statistikat.github.io/persephone3/reference/tramoseatsSingle.md)
representing an tramoseats seasonal adjustment model.

## Details

The function initializes an tramoseats model with the given time series
and template. Internally, it creates an
[`tramoseatsSingle`](https://statistikat.github.io/persephone3/reference/tramoseatsSingle.md)
object, stores the specification, and prepares the model for execution.
To perform the seasonal adjustment, call the `$run()` method on the
returned object.

## Inherits

The returned object is an instance of the
[`tramoseatsSingle`](https://statistikat.github.io/persephone3/reference/tramoseatsSingle.md)
class, which inherits from
[`persephoneSingle`](https://statistikat.github.io/persephone3/reference/persephoneSingle.md)
and ultimately from
[`persephone`](https://statistikat.github.io/persephone3/reference/persephone.md).

## References

For more details on tramoseats specifications and the underlying
implementation, see the rjd3tramoseats documentation:
<https://rjdverse.github.io/rjd3tramoseats/>

## See also

[`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html),
[`rjd3tramoseats::tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)

## Examples

``` r
data(AirPassengers, package = "datasets")

# create an tramoseatsSingle object with the specifications settings of the rjd3 "rsa3" template
obj <- perTramo(AirPassengers, "rsa3")
obj$run()

# access the ts object stored in the tramoseatsSingle object
obj$ts
#>      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 1949 112 118 132 129 121 135 148 148 136 119 104 118
#> 1950 115 126 141 135 125 149 170 170 158 133 114 140
#> 1951 145 150 178 163 172 178 199 199 184 162 146 166
#> 1952 171 180 193 181 183 218 230 242 209 191 172 194
#> 1953 196 196 236 235 229 243 264 272 237 211 180 201
#> 1954 204 188 235 227 234 264 302 293 259 229 203 229
#> 1955 242 233 267 269 270 315 364 347 312 274 237 278
#> 1956 284 277 317 313 318 374 413 405 355 306 271 306
#> 1957 315 301 356 348 355 422 465 467 404 347 305 336
#> 1958 340 318 362 348 363 435 491 505 404 359 310 337
#> 1959 360 342 406 396 420 472 548 559 463 407 362 405
#> 1960 417 391 419 461 472 535 622 606 508 461 390 432

# inspect the rjd3 output
obj$output
#> Model: TRAMO-SEATS
#> Log-transformation: yes 
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>   -0.4018   -0.5569 
#> 
#> No regression variables
#> 
#> For a more detailed output, use the 'summary()' function.

# set some additional parameters when initializing the object
# by using the persephone3 parameters for setting outliers
obj <- perTramo(AirPassengers, "rsa3",
              transform.fun = "Log", transform.adjust = NA,
              easter.enabled = TRUE,
              outliers = c("AO1953.7", "LS1954.6"),
              automdl.enabled = FALSE,
              arima.p = 1, arima.d = 2, arima.q = 0,
              arima.bp = 1, arima.bd = 1, arima.bq = 0)
obj$run()
obj$output
#> Model: TRAMO-SEATS
#> Log-transformation: yes 
#> SARIMA model: (1,2,0) (1,1,0)
#> 
#> SARIMA coefficients:
#>  phi(1) bphi(1) 
#>  0.7151  0.2658 
#> 
#> Regression model:
#>          easter AO (1953-07-01) LS (1954-06-01) AO (1950-11-01) 
#>        0.004435       -0.018558       -0.026008       -0.076973 
#> AO (1951-05-01) AO (1952-02-01) TC (1953-06-01) AO (1960-03-01) 
#>        0.086319        0.085779       -0.103156       -0.096129 
#> 
#> For a more detailed output, use the 'summary()' function.

# set some additional parameters when initializing the object
# by using the rjd3 parameters for setting outliers
obj2 <- perTramo(AirPassengers, "rsa3",
               transform.fun = "Log", transform.adjust = NA,
               easter.enabled = TRUE,
               addout.type = c("AO","LS"),
               addout.date = c("1953-07-01","1954-06-01"),
               automdl.enabled = FALSE,
               arima.p = 1, arima.d = 2, arima.q = 0,
               arima.bp = 1, arima.bd = 1, arima.bq = 0)
obj2$run()
obj2$output
#> Model: TRAMO-SEATS
#> Log-transformation: yes 
#> SARIMA model: (1,2,0) (1,1,0)
#> 
#> SARIMA coefficients:
#>  phi(1) bphi(1) 
#>  0.7151  0.2658 
#> 
#> Regression model:
#>          easter AO (1953-07-01) LS (1954-06-01) AO (1950-11-01) 
#>        0.004435       -0.018558       -0.026008       -0.076973 
#> AO (1951-05-01) AO (1952-02-01) TC (1953-06-01) AO (1960-03-01) 
#>        0.086319        0.085779       -0.103156       -0.096129 
#> 
#> For a more detailed output, use the 'summary()' function.
```
