rm(list=ls())
# roxygen2::roxygenize()
# ht$.__enclos_env__$private$template
# ht$.__enclos_env__$private$method
#hts_child$.__enclos_env__$private$template

# context checken ob das eh funktioniert
#?? wie kommt es zu default x13???

devtools::load_all()

hts <- perHts(a = perTramo(AirPassengers), b=perX13(AirPassengers))         # Child erzeugen
hts$run()
hts


myts <- list(a = AirPassengers, b= AirPassengers)
ht <- perHts(list=myts, method = "x13")
ht$run()
ht

hts_child <- perHts(a = perX13(AirPassengers), b=perX13(AirPassengers))         # Child erzeugen
hts_nested <- perHts(hts_child = hts_child,            # Child in Parent übergeben
                     c = perX13(AirPassengers),
                     method = "x13",
                     template = "rsa0",automdl.enabled = FALSE,
                     arima.p = 1, arima.d = 2, arima.q = 0,
                     arima.bp = 1, arima.bd = 1, arima.bq = 0)
hts_nested$run()
hts_nested

comp_b <- perX13(AirPassengers)   # das ist identisch mit dem b-Argument im Aufruf
components <- list(hts_child = hts_child, b = comp_b)

obj <- perX13(AirPassengers, "rsa3")
myts <- list(a = AirPassengers, b= AirPassengers)
spec_list <- list(transform.fun = "Log", transform.adjust = NA,
                  easter.enabled = TRUE,
                  outliers = c("AO1953.7", "LS1954.6"),
                  automdl.enabled = FALSE,
                  arima.p = 1, arima.d = 2, arima.q = 0,
                  arima.bp = 1, arima.bd = 1, arima.bq = 0)
ht <- perHts(list=myts, method = "x13", template = "rsa1")
ht2 <- perHts(a=obj, b=ht, method = "x13")

ht$updateParams(speclist=spec_list)
ht$run()




myts <- list(a = AirPassengers, b= AirPassengers)
spec_list <- list(transform.fun = "Log", transform.adjust = NA,
                  easter.enabled = TRUE,
                  outliers = c("AO1953.7", "LS1954.6"),
                  automdl.enabled = FALSE,
                  arima.p = 1, arima.d = 2, arima.q = 0,
                  arima.bp = 1, arima.bd = 1, arima.bq = 0)
bt <- perBatch(list=myts, method = "x13", template = "rsa1")
bt$updateParams(speclist=spec_list)
bt$run()
bt
bt$ts
bt$adjusted




obj <- perX13(AirPassengers, "rsa3",automdl.enabled = FALSE,
              arima.p = 1, arima.d = 2, arima.q = 0,
              arima.bp = 1, arima.bd = 1, arima.bq = 0)

ht2 <- perHts()


obj$run()
obj


rm(obj)
spec_list <- list(transform.fun = "Log", transform.adjust = NA,
                  easter.enabled = TRUE,
                  outliers = c("AO1953.7", "LS1954.6"),
                  automdl.enabled = FALSE,
                  arima.p = 1, arima.d = 2, arima.q = 0,
                  arima.bp = 1, arima.bd = 1, arima.bq = 0)

obj <- perX13(AirPassengers, "rsa3", speclist = spec_list)
obj$run()
obj



myts <- list(a = AirPassengers, b= AirPassengers)
spec_list <- list(transform.fun = "Log", transform.adjust = NA,
                  easter.enabled = TRUE,
                  outliers = c("AO1953.7", "LS1954.6"),
                  automdl.enabled = FALSE,
                  arima.p = 1, arima.d = 2, arima.q = 0,
                  arima.bp = 1, arima.bd = 1, arima.bq = 0)
ht <- perHts(list=myts, method = "x13", iterate = TRUE,
             template="rsa0", speclist = spec_list)
ht$run()




myts <- list(a = AirPassengers, b= AirPassengers)
ht <- perHts(list=myts, method = "x13", iterate = FALSE,
             template="rsa0", transform.fun = "Log", transform.adjust = NA,
             easter.enabled = TRUE,
             outliers = c("AO1953.7", "LS1954.6"),
             automdl.enabled = FALSE,
             arima.p = 1, arima.d = 2, arima.q = 0,
             arima.bp = 1, arima.bd = 1, arima.bq = 0)
ht$run()

objX13 <- perX13(AirPassengers, "rsa3")
bt <- perBatch(a = objX13, b = objX13)
bt$updateParams(method = "x13",
                template="rsa0",
                automdl.enabled = FALSE,
                arima.p = 1, arima.d = 2, arima.q = 0,
                arima.bp = 1, arima.bd = 1, arima.bq = 0)
bt$run()
bt

bt <- perBatch(a = objX13, b = objX13, method = "x13",
               template="rsa0",transform.fun = "Log",
               transform.adjust = NA,
               easter.enabled = TRUE)


objX13 <- perX13(AirPassengers, "rsa3")
bt <- perBatch(a = objX13, b = objX13, method = "x13",
               template="rsa0",automdl.enabled = FALSE,
               arima.p = 1, arima.d = 2, arima.q = 0,
               arima.bp = 1, arima.bd = 1, arima.bq = 0)

bt <- perBatch(a = objX13, b = objX13)
bt$updateParams(method = "x13",
                template="rsa0",
                automdl.enabled = FALSE,
                arima.p = 1, arima.d = 2, arima.q = 0,
                arima.bp = 1, arima.bd = 1, arima.bq = 0)


myts <- list(a = AirPassengers, b= AirPassengers)
spec_list <- list(transform.fun = "Log", transform.adjust = NA,
                  easter.enabled = TRUE,
                  outliers = c("AO1953.7", "LS1954.6"),
                  automdl.enabled = FALSE,
                  arima.p = 1, arima.d = 2, arima.q = 0,
                  arima.bp = 1, arima.bd = 1, arima.bq = 0)
ht <- perHts(list=myts, method = "x13", iterate = FALSE,
             template="rsa0", bla = spec_list) # Muss noch angepasst werden.
ht$run()


ht <- perHts(list=myts, method = "x13", iterate = FALSE,
             template="rsa0",
             transform.fun = "Log", transform.adjust = NA,
             easter.enabled = TRUE,
             outliers = c("AO1953.7", "LS1954.6"),
             automdl.enabled = FALSE,
             arima.p = 1, arima.d = 2, arima.q = 0,
             arima.bp = 1, arima.bd = 1, arima.bq = 0)








mycomps <- list(a = objX13, b = objX13)

myts <- list(a = AirPassengers, b= AirPassengers)
ht <- perHts(list=myts, method = "x13", iterate = FALSE,
             template="rsa0",
             transform.fun = "Log", transform.adjust = NA,
             easter.enabled = TRUE,
             outliers = c("AO1953.7", "LS1954.6"),
             automdl.enabled = FALSE,
             arima.p = 1, arima.d = 2, arima.q = 0,
             arima.bp = 1, arima.bd = 1, arima.bq = 0)




# iterate = TRUE: neue Parameter auf alle Reihe, also alle Subreihen = components und die Aggregatreihe anwenden
# ht <- perHts(a = objX13, b = objX13, method = "x13", iterate = TRUE, template="rsa0",
#              transform.fun = "None", transform.adjust = NA, easter.enabled = TRUE)

ht <- perHts(a = objX13, b = objX13, method = "x13", iterate = FALSE,
             template="rsa0",
             transform.fun = "Log", transform.adjust = NA,
             easter.enabled = TRUE,
             outliers = c("AO1953.7", "LS1954.6"),
             automdl.enabled = FALSE,
             arima.p = 1, arima.d = 2, arima.q = 0,
             arima.bp = 1, arima.bd = 1, arima.bq = 0)
# ht <- perHts(a = objX13, b = objX13, method = "x13", iterate = TRUE, template="rsa0",
#              #usrdef.outliersEnabled = TRUE,
#              addout.type = c("AO","LS","LS"),
#              addout.date=c("1950-01-01","1955-04-01","1959-10-01"))
ht$run()
ht
# iterate = FALSE: neue Parameter nur auf die Aggregatreihe anwenden

ht$updateParams()



components <- list(a = objX13, b = objX13, transform.fun = "Log", transform.adjust = NA,
                   easter.enabled = TRUE)

components <- list(a=AirPassengers, b=AirPassengers, transform.fun = "Log", transform.adjust = NA,
                   easter.enabled = TRUE)



get_spec <- function(x) {
  list(spec = x$spec)
}
ht$iterate(asTable = TRUE, get_spec)

ht$updateParams(easter.enabled = TRUE)
ht$run()
ht$output
ht$getComponent("a")$output

blats <- AirPassengers
blats[55] <- 1000
blats[66] <- 1000
obj <- perX13(blats, "rsa3")

obj <- perTramo(AirPassengers, "rsa3",
                transform.fun = "Log", transform.adjust = NA,
                easter.enabled = TRUE,
                outliers = c("AO1953.7", "LS1954.6"),
                automdl.enabled = FALSE,
                arima.p = 1, arima.d = 2, arima.q = 0,
                arima.bp = 1, arima.bd = 1, arima.bq = 0)

obj2 <- perX13(AirPassengers, "rsa3",
               transform.fun = "Log", transform.adjust = NA,
               easter.enabled = TRUE,
               addout.type = c("AO","LS"),
               addout.date = c("1953-07-01","1954-06-01"),
               automdl.enabled = FALSE,
               arima.p = 1, arima.d = 2, arima.q = 0,
               arima.bp = 1, arima.bd = 1, arima.bq = 0)

obj$run()
obj$output
obj2$run()
obj2$output

obj$updateParams(transform.fun = "Log", transform.adjust = NA,
                 easter.enabled = TRUE,
                 outliers = c("AO1953.7", "AO1954.6"),
                 automdl.enabled = FALSE,
                 arima.p = 1, arima.d = 2, arima.q = 0,
                 arima.bp = 1, arima.bd = 1, arima.bq = 0)
obj$run()
obj

obj <- perX13(blats, "rsa3")
# obj$run()
# obj$output
obj2 <- perX13(blats, "rsa3",transform.fun = "Log", transform.adjust = NA,
               easter.enabled = TRUE, outliers = c("AO1953.7", "AO1954.6"))
ht <- perHts(a = obj, b = obj2, method = "x13", spec = "rsa1")
ht$run()
ht$updateParams(automdl.enabled = FALSE,arima.p = 1, arima.d = 2, arima.q = 0,
                arima.bp = 1, arima.bd = 1, arima.bq = 0)
ht$run()
ht

ht$updateParams(
  component = c("a"),
  automdl.enabled = FALSE,
  arima.p = 0, arima.d = 3, arima.q = 0,
  arima.bp = 1, arima.bd = 1, arima.bq = 0
)
ht$run()
ht


bt <- perBatch(a = obj, b = obj2)
bt$run()
bt$updateParams(automdl.enabled = FALSE,arima.p = 1, arima.d = 2, arima.q = 0,
                arima.bp = 1, arima.bd = 1, arima.bq = 0)
bt$run()
bt

# Entweder die Parameter nicht übernommen wurden (z.B. weil fixModel() nicht aufgerufen wurde),

# geht das nicht für alle components auf einmal, ohne fixModel()?
bt$updateParams(component = "a",
                automdl.enabled = FALSE,
                arima.p = 0, arima.d = 2, arima.q = 0,
                arima.bp = 0, arima.bd = 1, arima.bq = 0)

bt$run()
bt

#obj$params
obj$run()
obj$output
obj$updateParams(outliers = NULL)

obj <- perX13(blats, "rsa3",transform.fun = "Log", transform.adjust = NA,
              easter.enabled = TRUE, outliers = c("AO1953.7", "AO1954.6"))
obj$run()
obj <- perX13(blats, "rsa3",transform.fun = "Log", transform.adjust = NA,
              easter.enabled = TRUE, outliers = c("AO1953.7", "AO1954.6"))



init_spec <- rjd3x13::x13_spec("rsa5c")

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
obj <- perX13(blats, "rsa3")
obj$updateParams()
obj$run()
obj$plot()
obj$hchart()

# obj <- perX13(blats, template="rsa3",
#               set_basic(),
#               set_transform(fun = "Log", outliers = TRUE),
#               add_outlier(type="LS", date="2011-07-01"),
#               set_tradingdays(... ),
#               set_automodel(enabled = FALSE),
#               set_arima( p = 1, d = 2, q = 0, bp = 1, bd = 1, bq = 0)
#               )
# obj <- perTramo(blats, "rsa3")
obj$run()

# params soll x13spec klassenobjekt sein
# > class(obj$params)
# [1] "JD3_X13_SPEC"
addSpecs <- function(...) {
  namesSpecFuns <- c("set_arima", "set_automodel",  "set_basic", "set_easter",  "set_estimate",
                     "set_outlier","set_tradingdays", "set_transform",
                     "add_outlier", "remove_outlier",  "add_ramp", "remove_ramp", "add_usrdefvar")
  specfun_list <- list(transform = rjd3toolkit::set_transform, easter = rjd3toolkit::set_easter,
                       outlier = rjd3toolkit::add_outlier, estimate = rjd3toolkit::set_estimate)
  spec_list <- list( transform.fun = "Log", transform.adjust = NA,
                     easter.enabled = TRUE,
                     outlier.type="LS", outlier.date="2011-07-01")

}



namesSpecFuns <- c("set_arima", "set_automodel",  "set_basic", "set_easter",  "set_estimate",
                   "set_outlier","set_tradingdays", "set_transform",
                   "add_outlier", "remove_outlier",  "add_ramp", "remove_ramp", "add_usrdefvar")
specfun_list <- list(transform = rjd3toolkit::set_transform, easter = rjd3toolkit::set_easter,
                     outlier = rjd3toolkit::add_outlier, estimate = rjd3toolkit::set_estimate)

#https://github.com/rjdverse/rjdemetra/blob/develop/R/x13_spec.R


set_basic(
  x,
  type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
  d0 = NULL,
  d1 = NULL,
  n0 = 0,
  n1 = 0,
  preliminary.check = NA,
  preprocessing = NA
)


# x13 functions
specfun_list <- list(arima = rjd3toolkit::set_arima, automdl = rjd3toolkit::set_automodel,
                     basic = rjd3toolkit::set_basic, easter = rjd3toolkit::set_easter,
                     estimate = rjd3toolkit::set_estimate, setout = rjd3toolkit::set_outlier,
                     td = rjd3toolkit::set_tradingdays, transform = rjd3toolkit::set_transform,
                     addout = rjd3toolkit::add_outlier, remout = rjd3toolkit::remove_outlier,
                     addramp = rjd3toolkit::add_ramp, remramp = rjd3toolkit::remove_ramp,
                     usrdefvar = rjd3toolkit::add_usrdefvar,
                     x11 = rjd3x13::set_x11, benchmarking = rjd3toolkit::set_benchmarking)



spec_list <- list( transform.fun = "Log", transform.adjust = NA,
                   easter.enabled = TRUE,
                   outlier.type="LS", outlier.date="2011-07-01")

init_spec <- rjd3x13::x13_spec("rsa5c")
spec <- rjd3toolkit::set_transform(init_spec, fun = "Log", adjust = NA)
spec2 <- rjd3toolkit::set_transform(init_spec,spec_sublist)
bla <- rjd3x13::x13(AirPassengers,spec2)

#add_specs <- function(spec_list) {
# }
spec_blocks <- unique(sapply(names(spec_list), function(s) unlist(strsplit(s, ".", fixed = TRUE))[1] ))

i <- 1

#for (i in spec_blocks) {
# paste0("^",spec_blocks[i]))# prüfen ob Zeichenkette mit spec_blocks[i] beginnt:
spec_sublist <-  spec_list[grepl(paste0("^",spec_blocks[i]), names(spec_list))]
names(spec_sublist) <- gsub(paste0(spec_blocks[i],"."), "", names(spec_sublist), fixed = TRUE)

spec_new <- do.call(specfun_list[[spec_blocks[i]]], c(list(x = init_spec), spec_sublist))

spec_new <- do.call(specfun_list[grepl(paste0("^",spec_blocks[i]), names(spec_list))], spec_sublist)

paste0("transform.",formalArgs(rjd3toolkit::set_transform)[-1])
paste0("easter.",formalArgs(rjd3toolkit::set_easter)[-1])
paste0("outlier.",formalArgs(rjd3toolkit::add_outlier)[-1])
paste0("outlier.",formalArgs(rjd3toolkit::set_outlier)[-1])



# Promt:
# Ist updateFun einfach eine Art Wrapper für x13_spec und x13_spec bekommt gar nie
# direkt zusätzliche Parmater zugewiesen sondern nur über updateFun? https://github.com/statistikat/persephone
#
# Copilot said:
#   Copilot
# Gute Frage! Ich habe mir den Code von persephone angesehen, und hier ist die Funktionsweise:
#
#   x13_spec() erstellt ein Spezifikationsobjekt für die X-13ARIMA-SEATS-Analyse. Es definiert
#  die Standardparameter (z. B. für Trend, Saison, Kalenderregressionen usw.).
# → Dieses Objekt ist die Basis, die an die X-13-Engine übergeben wird.
#
# updateFun() ist tatsächlich ein Wrapper, der dazu dient, ein bestehendes Spezifikationsobjekt
# (z. B. von x13_spec()) mit zusätzlichen oder geänderten Parametern zu aktualisieren.
# → Das heißt: Du rufst nicht direkt x13_spec() mit neuen Parametern auf, sondern erzeugst
# zuerst die Spezifikation und passt sie dann mit updateFun() an.
#
# Warum so?
#   Das Design folgt dem Prinzip:
#
#   Erstelle eine Basis-Spezifikation (x13_spec()), die alle Standardwerte enthält.
# Nutze updateFun() für Änderungen, damit die Struktur konsistent bleibt und nur die gewünschten
# Teile überschrieben werden.
# Beispiel (vereinfacht):
#
# spec <- x13_spec("RSA5c")        # Basis-Spezifikation
# spec <- updateFun(spec, tradingdays = "td")  # Update nur bestimmter Parameter
#
# ✅ Merke:
#
# x13_spec() = Basis-Setup
# updateFun() = gezielte Anpassungen
# Direkte Übergabe zusätzlicher Parameter an x13_spec() ist nicht vorgesehen.


#result <- x13_fast(
# ts = my_ts,
# spec = c("rsa4", "rsa0"),
# context = list(country = "AT", source = "Statistik Austria"),
# userdefined = NULL
# )
