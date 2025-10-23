## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "95%"
)

## ------------------------------------------------------------------------
library(persephone)
hts <- per_hts(
  a = per_x13(AirPassengers),
  b = per_tramo(AirPassengers)
)
hts

## ------------------------------------------------------------------------
display_result <- function(ts) {
  window(ts, end = c(1949, 6))
}
hts$run()
display_result(hts$adjusted)
display_result(hts$components$a$adjusted)
display_result(hts$adjusted_indirect)

## ------------------------------------------------------------------------
hts_nested <- per_hts(
  hts = hts,
  hts_child = per_hts(
    a = per_x13(AirPassengers)
  ),
  b = per_tramo(AirPassengers)
)
hts_nested

## ---- warning=FALSE------------------------------------------------------
hts_nested$run()
hts_nested$iterate(as_table = TRUE, function(obj) {
  list(mean = mean(obj$adjusted),
       var = sd(obj$adjusted))
})

## ------------------------------------------------------------------------
hts_nested$iterate(as_table = TRUE, persephone:::generateQrList)[, 1:9]

## ------------------------------------------------------------------------
hts_nested$set_options(spec = "RSA1", recursive = TRUE)

get_spec <- function(x) {
  list(spec = x$spec)
}

## check if options where applied correctly
hts_nested$iterate(as_table = TRUE, get_spec)

## modify options beginning from hts
hts$set_options(spec = "RSA0", recursive = TRUE)
hts_nested$iterate(as_table = TRUE, get_spec)


