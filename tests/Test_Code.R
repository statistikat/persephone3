# Ensure userdefined_default is visible to R6 class methods during testing
source("R/persephone.R")
source("tests/perHtsCL.R")

# Zeitreihen für Testung
ki_unverkettet <- fread(file = paste0(mlauf, "/J2026/COSA/KI_unlinked.csv"))
linkFakt <- fread(file = paste0(mlauf, "/J2026/COSA/linkFaktoren.csv"))
gew_F <- fread(file = paste0(mlauf, "/J2026/COSA/w_F.csv"))
gew_20_21 <- fread(file = paste0(mlauf, "/J2026/COSA/w20_21.csv"))
gew20_21_ts <- ts(gew_20_21[, 2:3], start = c(2000, 1), freq = 12)
gew_F_ts <- ts(gew_F[, 2:4], start = c(2000, 1), freq = 12)

gew_mts <- cbind(gew20_21_ts, gew_F_ts)
gew_mts <- window(gew_mts, end = c(2026, 2))
colnames(gew_mts) <- c("C20", "C21", "F41", "F42", "F43")

lf_20  <- ts(linkFakt[,55], start = c(2000, 1), freq = 12)   # chain factor for component a
lf_21  <- ts(linkFakt[,56], start = c(2000, 1), freq = 12)   # chain factor for component b
lf_20_21 <- ts(linkFakt[,33], start = c(2000, 1), freq = 12)   # chain factor for aggregate ab

ts20 <- kett[, 66]
ts21 <- kett[, 74]
ts20_21 <- kett[, 73]

# ---- 1. Create a persephone object ------------------------------------------
objX13_x <- perX13(ts20, "rsa5c")
objX13_y <- perX13(ts21, "rsa5c")
objX13_x$run()
objX13 <- perX13(AirPassengers, "rsa3")

bt <- perBatch(a = objX13_x, b = objX13_y)
bt$run()
plotSiRatios(objX13_x)

htcl <- perHtsCL(
  x            = objX13_x,          # plain ts — auto-converted via as.persephone()
  y            = objX13_y,          # plain ts — auto-converted via as.persephone()
  method       = "x13",             # SA method for all components + aggregate
  template     = "rsa5c",
  cl_weights   = cl_weights_mts,
  ref_year     = 2021,
  cl_agg       = "agg",
  cal_adjusted = FALSE
)

htcl$run()

# Full quality diagnostics table
qt <- htcl$qualityTable()
qt

# ---- Plot Direkt vs. Indirekt ------------------------------------------
plot(
  cbind(
    direct   = htcl$adjustedDirect,
    indirect = htcl$adjustedIndirect
  ),
  main      = "Chain-linked SA aggregate: direct vs indirect",
  plot.type = "single",
  col       = c("steelblue", "firebrick"),
  lty       = 1,
  ylab      = "Index (2021 = 100)"
)
legend(
  "topleft",
  legend = c("Direct SA", "Indirect SA (5-step CL)"),
  col    = c("steelblue", "firebrick"),
  lty    = 1
)

# ---- 2. Create a persephone object with multiple series ---------------------
##########################################

# chain-linking Faktoren der einzelnen Reihen
lf_20  <- ts(linkFakt[,55], start = c(2000, 1), freq = 12)   
lf_21  <- ts(linkFakt[,56], start = c(2000, 1), freq = 12)   
lf_20_21 <- ts(linkFakt[,33], start = c(2000, 1), freq = 12)  

lf_41 <- ts(linkFakt[, 74], start = c(2000, 1), freq = 12)
lf_42 <- ts(linkFakt[, 75], start = c(2000, 1), freq = 12)
lf_43 <- ts(linkFakt[, 76], start = c(2000, 1), freq = 12)
lf_F <- ts(linkFakt[, 28], start = c(2000, 1), freq = 12)

lf_all <- cbind(lf_20, lf_21, lf_20_21, lf_41, lf_42, lf_43, lf_F)
colnames(lf_all) <- c("ts20", "ts21", "ts20_21", "ts41", "ts42", "ts43", "tsF")

# Originalreihen 
ts20 <- kett[, 66]
ts21 <- kett[, 74]
ts20_21 <- kett[, 73]

ts22 <- kett[, 77]
ts23 <- kett[, 80]
ts24 <- kett[, 89]
ts25 <- kett[, 96]

ts41 <- kett[, 177]
ts42 <- kett[, 181]
ts43 <- kett[, 185]
tsF <- kett[, 176]

ts_all <- cbind(ts20, ts21, ts20_21, ts22, ts23, ts24, ts25, ts41, ts42, ts43, tsF)

# Gewichte für die Aggregate
gew_mts
colnames(gew_mts) <- c("ts20", "ts21", "ts41", "ts42", "ts43")


htcl <- perHtsCL(
  series     = ts_all,          # mts: columns ts20-tsF
  weights    = gew_mts,         # mts: columns ts20, ts21, ts41, ts42, ts43
  cl_weights = lf_all,          # mts: columns ts20-tsF 
  cl_groups  = list(
    ts20_21 = c("ts20", "ts21"),
    tsF = c("ts41", "ts42", "ts43")
  ),
  method   = "x13",
  template = "rsa5c",
  ref_year = 2021
)

htcl$run()
htcl$adjustedDirect("d")
htcl$adjustedIndirect("d")
htcl$adjustedDirect("z")
htcl$adjustedIndirect("z")
htcl$qualityTable()



# ---- Single-aggregate mode (x/y API) ----------------------------------------
htcl$run()
htcl$getAdjustedDirect("tsF")       # no argument needed in single-aggregate mode
htcl$getAdjustedIndirect("tsF")     # no argument needed
htcl$qualityTable()

# ---- Batch mode (series mts API) --------------------------------------------
htcl$run()
htcl$getAdjustedDirect("ts20_21")
htcl$getAdjustedIndirect("ts20_21")
htcl$getAdjustedDirect("tsF")
htcl$getAdjustedIndirect("tsF")
htcl$qualityTable()
























printDiagnostics <- function(x) {
  #browser()
  type <- ifelse(grepl("Single",class(x)[1]),  "single" , "aggregate")

  if (is.null(x$output) | length(x$output)==0)
    return(data.frame(
      run = !is.null(x$output),
      #class = class(x)[1],
      method = x$method,
      type = type,
      seasonality = NA,
      logTransform = NA,
      arimaMdl = NA,
      nOutliers = NA,
      qStat = NA
    ))

  userdef <-  x$output$user_defined

  bpbdbq <- paste0("(", userdef$arima.p, " ",
                   userdef$arima.d, " ",
                   userdef$arima.q, ")",
                   "(", userdef$arima.bp, " ",
                   userdef$arima.bd, " ",
                   userdef$arima.bq, ")")

  qStat  <- x$output$mstats$q
  out <- getOutliers(x)
  data.frame(
    run = !is.null(x$output),
    #class = class(x)[1],
    method = x$method,
    type = type,
    # seasonality: placeholder (test for stable seasonality)
    seasonality = userdef$`diagnostics.seas-si-combined`,
    logTransform = x$output$preprocessing$description$log,
    arimaMdl = bpbdbq,
    nOutliers = ifelse(is.null(nrow(out)), NA, nrow(out)),
    qStat = ifelse(is.null(qStat), NA, round(qStat, digits = 2))
  )

}