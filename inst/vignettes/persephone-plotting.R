## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "95%"
  #fig.width=9
)

## ------------------------------------------------------------------------
library(persephone)

obj <- per_x13(AirPassengers, "RSA1")
plot(obj, drawPoints = TRUE)

## ------------------------------------------------------------------------
obj$run()
plot(obj, drawPoints=TRUE)

## ------------------------------------------------------------------------
plotSeasIrrCal(obj)

## ------------------------------------------------------------------------
plotResiduals(obj, which="acf")

