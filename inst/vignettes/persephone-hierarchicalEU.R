## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "95%"
)

## ------------------------------------------------------------------------
library(persephone)
data(ipi_c_eu, package = "RJDemetra")
ipi_eu <- ipi_c_eu[, -c(1:3, 32:37)]

## ------------------------------------------------------------------------
ts_28 <- lapply(ipi_eu, per_tramo)

## ------------------------------------------------------------------------
south_eastern_europe <- c("BG","EL","CY","RO")
northern_europe <- c("DK","FI","SE")
central_europe <- c("HR","DE","CZ","SK","HU","AT","SI","LU","EE","LT","LV","PL")
southern_europe <- c("IT","ES","PT","MT")
western_europe <- c("FR","BE","NL","IE","UK")

## ------------------------------------------------------------------------
hts_see <- per_hts(list = ts_28[south_eastern_europe])
hts_ne <- per_hts(list = ts_28[northern_europe])
hts_ce <- per_hts(list = ts_28[central_europe])
hts_se <- per_hts(list = ts_28[southern_europe])
hts_we <- per_hts(list = ts_28[western_europe])

## ------------------------------------------------------------------------
hts_np <- per_hts(norhtern_europe = hts_ne,
                 western_europe = hts_we,
                 central_europe = hts_ce
)
hts_sp <- per_hts(southern_europe = hts_se,
                 south_eastern_europe = hts_see
)

## ------------------------------------------------------------------------
ht_europe <- per_hts(north_part = hts_np,
                 south_part = hts_sp
)
ht_europe

## ------------------------------------------------------------------------
ht_europe$run()
ht_europe

## ------------------------------------------------------------------------
hts_see <- per_hts(list = ts_28[south_eastern_europe], weights=c(1,2,3,4))
hts_ne <- per_hts(list = ts_28[northern_europe], weights = c(5,6,7))
hts_ce <- per_hts(list = ts_28[central_europe], weights = rep(1,12))
hts_se <- per_hts(list = ts_28[southern_europe], weights = c(8,9,10,10.5))
hts_we <- per_hts(list = ts_28[western_europe], weights = c(11,12,13,14,15))

## ------------------------------------------------------------------------
hts_np <- per_hts(northern_europe = hts_ne,
                 western_europe = hts_we,
                 central_europe = hts_ce
)
hts_sp <- per_hts(southern_europe = hts_se,
                 south_eastern_europe = hts_see
)
knitr::kable(head(hts_np$weights))

## ------------------------------------------------------------------------
ht_europe <- per_hts(north_part = hts_np,
                 south_part = hts_sp
)

knitr::kable(head(ht_europe$weights))

## ------------------------------------------------------------------------
weights_eu <- ipi_eu * 0 + 1
weights_eu[is.na(weights_eu)] <- 1
weights_eu[,"AT"] <- weights_eu[,"AT"]+rlnorm(weights_eu[,"AT"])

## ------------------------------------------------------------------------
hts_see <- per_hts(list = ts_28[south_eastern_europe],
                   weights=weights_eu[,south_eastern_europe])
hts_ne <- per_hts(list = ts_28[northern_europe],
                  weights=weights_eu[,northern_europe])
hts_ce <- per_hts(list = ts_28[central_europe],
                  weights=weights_eu[,central_europe])
hts_se <- per_hts(list = ts_28[southern_europe],
                  weights=weights_eu[,southern_europe])
hts_we <- per_hts(list = ts_28[western_europe],
                  weights=weights_eu[,western_europe])

## ------------------------------------------------------------------------
hts_np <- per_hts(northern_europe = hts_ne,
                 western_europe = hts_we,
                 central_europe = hts_ce
)
hts_sp <- per_hts(southern_europe = hts_se,
                 south_eastern_europe = hts_see
)
knitr::kable(head(hts_np$weights))

## ------------------------------------------------------------------------
ht_europe <- per_hts(north_part = hts_np,
                 south_part = hts_sp
)

knitr::kable(head(ht_europe$weights))

