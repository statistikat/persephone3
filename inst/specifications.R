init_spec <- rjd3x13::x13_spec("RSA5c")

#estimation on sub-span between two dates (date d1 is excluded)
new_spec<-set_basic(init_spec,type = "Between",d0 = "2014-01-01",
d1 = "2019-01-01", preliminary.check = TRUE, preprocessing = TRUE)
#Estimation on the first 60 observations
new_spec <-set_basic(init_spec,Type="First", n0 = 60,
                     preliminary.check = TRUE,
                     preprocessing= TRUE)
#Estimation on the last 60 observations
new_spec <-set_basic(init_spec,Type="Last", n1 = 60,
                     preliminary.check = TRUE,
                     preprocessing= TRUE)
#Estimation excluding 60 observations at the beginning and 36 at the end of the series
new_spec <-set_basic(init_spec,Type="Excluding", n0=60, n1=36,
                     preliminary.check = TRUE,
                     preprocessing= TRUE)

blats <- AirPassengers
blats[55] <- 1000
blats[66] <- 1000

obj <- perX13(blats, template="rsa3",
              set_basic(),
              set_transform(fun = "Log", outliers = TRUE),
              add_outlier(type="LS", date="2011-07-01"),
              set_tradingdays(... ),
              set_automodel(enabled = FALSE),
              set_arima( p = 1, d = 2, q = 0, bp = 1, bd = 1, bq = 0)
              )
# obj <- perTramo(blats, "rsa3")
obj$run()

specList <- function(x, ...){

}


namesSpecFuns <- c("set_arima", "set_automodel",  "set_basic", "set_easter",  "set_estimate",
                     "set_outlier","set_tradingdays", "set_transform",
                     "add_outlier", "remove_outlier",  "add_ramp", "remove_ramp", "add_usrdefvar")

specList <- vector("list", length(namesSpecFuns))
names(specList) <- namesSpecFuns

set_transform <- function(...){rjd3toolkit::set_transform(x, ...) }


  set_transform = set_transform(fun = "Log", outliers = TRUE)
  add_outlier=add_outlier(type="LS", date="2011-07-01")
  set_automodel=set_automodel(enabled = TRUE)
  set_arima = NULL



blats <- AirPassengers
blats[55] <- 1000
blats[66] <- 1000
specList$set_transform <- list()

generateSpec <- function() {
  specList <- list(
    set_transform = rjd3toolkit::set_transform,
    add_outlier = rjd3toolkit::add_outlier,
    set_automodel = rjd3toolkit::set_automodel
  )

}


require(purrr)
funs <- list(sd=sd, mean=mean)
trees %>% map_df(~invoke_map(funs, ,.), .id="id")


rjd3toolkit::set_arima(), rjd3toolkit::set_automodel(),
rjd3toolkit::set_basic(), rjd3toolkit::set_easter(),
rjd3toolkit::set_estimate(), rjd3toolkit::set_outlier(),
rjd3toolkit::set_tradingdays(), rjd3toolkit::set_transform(),
rjd3toolkit::add_outlier(), rjd3toolkit::remove_outlier(),
rjd3toolkit::add_ramp(), rjd3toolkit::remove_ramp(), rjd3toolkit::add_usrdefvar();
# for decomposition:
rjd3x13::set_x11()



# for decomposition:
rjd3x13::set_x11()



blats <- AirPassengers
blats[55] <- 1000
blats[66] <- 1000


# jahr <- 2024
# quartal <- 4
# av <- readRDS(paste0("/data/home/meran/Saisonbereinigung/QNA/av_",jahr,"q",quartal,".Rds"))
# load("~/SeasonalAdjustmentQNA/TD5lY.Rds")
# load("~/SeasonalAdjustmentQNA/TD7lY.Rds")
# td7 <- td7lY[,1:6]
# td5 <- td5lY[,"wd5"]
# meth <- mountSTAT::mountMeth()
# testreihe_td7 <- av[, "SALxHWxW2xA"]
# saveRDS(testreihe_td7, paste0(meth,"/Projekte/TSACE/testreihe_td7.Rds"))

meth <- mountSTAT::mountMeth()
testreihe_td7 <- readRDS(paste0(meth,"/Projekte/TSACE/testreihe_td7.Rds"))
load("~/SeasonalAdjustmentQNA/TD5lY.Rds")
load("~/SeasonalAdjustmentQNA/TD7lY.Rds")
td7 <- td7lY[,1:6]
td5 <- td5lY[,"wd5"]

new_spec <- rjd3x13::x13_spec("rsa3")
new_spec <- rjd3toolkit::set_transform(new_spec, fun = "Log", outliers = TRUE)
new_spec <- rjd3toolkit::add_outlier(new_spec, type="LS", date="2011-07-01")
# new_spec <- rjd3toolkit::set_tradingdays(... )
new_spec <- rjd3toolkit::set_automodel(new_spec, enabled = FALSE)
new_spec <- rjd3toolkit::set_arima(new_spec, p = 1, d = 2, q = 0, bp = 1, bd = 1, bq = 0)
res <- rjd3x13::x13_fast(testreihe_td7, new_spec)
summary(res)


SALxHWxW2xA = perTramo(av[, "SALxHWxW2xA"], template = "RSA3",
                       # Transformation -------------------------------------------------------
                       transform.function = "Log",
                       # Outliers -------------------------------------------------------------
                       outlier.enabled = FALSE,
                       usrdef.outliersEnabled = TRUE,
                       usrdef.outliersType = c("LS"),
                       usrdef.outliersDate = c("2011-07-01"),
                       # # Trading Days ---------------------------------------------------------
                       usrdef.varEnabled = FALSE,
                       usrdef.var = td7, usrdef.varType = "Calendar",
                       # tradingdays.option = "UserDefined",
                       # Easter ---------------------------------------------------------------
                       # easter.type = NA, easter.duration = 6,
                       # Arima-Model ----------------------------------------------------------
                       automdl.enabled = FALSE,
                       arima.p  = 0, arima.d  = 0, arima.q  = 0,
                       arima.bp = 0, arima.bd = 1, arima.bq = 1, arima.mu = TRUE),
# ====================================================================================
