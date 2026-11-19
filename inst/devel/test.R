##############################################################################
# test.R -- Critical Value der Ausreisser-Erkennung
#
# Ziel: klaren, welchen Critical Value JDemetra+ tatsaechlich benutzt.
#   [2] Spec-Block "outlier" in x13 und tramoseats ansehen
#   [3] die Beobachtungszahl n, die Java dafuer heranzieht
#   [4]-[5] die (aus dem jdplus-Code abgeleitete) Auto-Formel + Referenzwerte
#   [6] Default-Lauf: min |t| gegen Formelwert (Indiz fuer predcv-Reduktion)
#   [7] Einklammernde Suche: laeuft die Formel auf den realen Schwellwert?
#   [8] Was schreibt rjd3toolkit::set_outlier(critical.value = .)?
#   [9] Fall mit manuell gesetztem Ausreisser
#
# Ausfuehren:  Rscript test.R    (oder source("test.R") in der Konsole)
# Vorbedingung: Java 21+ fuer rjd3, ggf. Sys.setenv(JAVA_HOME = "...")
##############################################################################

if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressMessages(library(persephone3))
}

hr <- function(txt) cat("\n==== ", txt, " ====\n", sep = "")

# ---- Critical Value aus der Spec -------------------------------------------
spec_cv <- function(obj) {
  spec <- obj$params
  spec <- if (!is.null(spec$regarima)) spec$regarima else spec$tramo
  o <- spec$outlier
  if (length(o$outliers) > 0) {
    data.frame(
      method = "x13",
      type = vapply(o$outliers, function(z) as.character(z$type), ""),
      va = vapply(o$outliers, function(z) as.numeric(z$va), numeric(1)),
      defva = as.numeric(o$defva)
    )
  } else {
    data.frame(
      method = "tramoseats", type = "alle",
      va = as.numeric(o$va), defva = NA_real_, enabled = o$enabled
    )
  }
}

# ---- Auto-Schwelle, nachgebaut aus dem jdplus-Code -------------------------
# x13   : X13Utility.calcCv(n)   = max(CriticalValueComputer
#             ::advancedComputer(0.05)(n), 2.8)
# tramo : TramoUtility.calcCv(n) = max(CriticalValueComputer
#             ::simpleComputer()(n), 2.0)
calc_outlier_cv <- function(n, method = c("x13", "tramoseats"),
                            threshold = 0.05) {
  method <- match.arg(method)
  if (method == "tramoseats") {
    cv <- if (n <= 50) 3.3 else if (n < 450) 3.3 + 0.0025 * (n - 50) else 4.3
    return(max(cv, 2))
  }
  mincv <- 2.8
  if (n == 1) return(max(qnorm(threshold / 2, lower.tail = FALSE), mincv))
  calc_val <- function(nvals, eps) {
    acv <- sqrt(2 * log(nvals))
    bcv <- acv - (log(log(nvals)) + log(4 * pi)) / (2 * acv)
    -log(-0.5 * log(2 - sqrt(1 + eps))) / acv + bcv
  }
  x <- c(2, 100, 200)
  acv_x <- sqrt(2 * log(x))
  bcv_x <- (log(log(x)) + log(4 * pi)) / (2 * acv_x)
  y <- c(qnorm((1 + sqrt(1 - threshold)) / 2),
         calc_val(100, threshold), calc_val(200, threshold))
  b <- solve(cbind(1, bcv_x, acv_x), y)
  acv <- sqrt(2 * log(n))
  bcv <- (log(log(n)) + log(4 * pi)) / (2 * acv)
  max(b[1] + b[2] * bcv + b[3] * acv, mincv)
}

# ---- Signaturen der Erkennung ---------------------------------------------
otl <- function(obj) {
  o <- getOutliers(obj)
  if (length(o) == 1 && all(is.na(o))) o <- NULL
  o
}
otl_sig <- function(obj) {
  o <- otl(obj)
  if (is.null(o)) "<keine>" else paste(sort(o$name), collapse = " ")
}
otl_mint <- function(obj) {
  o <- otl(obj)
  if (is.null(o)) NA_real_ else min(abs(o$t), na.rm = TRUE)
}

fit_x13 <- function(ts, cv = NULL, template = "rsa3") {
  o <- if (is.null(cv)) perX13(ts, template) else
    perX13(ts, template, setout.critical.value = cv)
  o$run()
  o
}
fit_tramo <- function(ts, cv = NULL, template = "rsa3") {
  o <- if (is.null(cv)) perTramo(ts, template) else
    perTramo(ts, template, setout.critical.value = cv)
  o$run()
  o
}

## [1] Default-Laeufe (Spec-Schwelle = 0 = "auto") ---------------------------
hr("[1] Default-Laeufe")
obj_x <- fit_x13(AirPassengers)
obj_t <- fit_tramo(AirPassengers)
cat("x13        :", otl_sig(obj_x), "\n")
cat("tramoseats :", otl_sig(obj_t), "\n")
print(getOutliers(obj_x))

## [2] Spec-Block "outlier" --------------------------------------------------
hr("[2] Critical Value in der Spec")
print(rbind(spec_cv(obj_x), spec_cv(obj_t)[, names(spec_cv(obj_x))]))
cat("\nSpec x13 (Auszug):\n")
str(obj_x$params$regarima$outlier)
cat("\nSpec tramo (Auszug):\n")
str(obj_t$params$tramo$outlier)

## [3] Beobachtungszahl n ----------------------------------------------------
hr("[3] Beobachtungszahl")
y_x <- obj_x$output$preprocessing$estimation$y
y_t <- obj_t$output$preprocessing$estimation$y
n_x <- NROW(y_x)                  # RegArimaKernel: Laenge des Modelldomains
n_t <- sum(!is.na(y_t))           # TramoKernel: Zahl der tatsaechlichen Beobachtungen
print(data.frame(
  Reihe = c("x13", "tramoseats"),
  n_used = c(n_x, n_t),
  laenge_ts = c(length(AirPassengers), length(AirPassengers)),
  laenge_y = c(NROW(y_x), NROW(y_t)),
  n_effectiveobs = c(
    obj_x$output$preprocessing$estimation$likelihood$neffectiveobs,
    obj_t$output$preprocessing$estimation$likelihood$neffectiveobs
  )
))

## [4]-[5] Auto-Formel -------------------------------------------------------
hr("[4]/[5] Auto-Formel: eingesetzte Werte")
cat(sprintf(
  "n = %d -> x13 %.4f | tramoseats %.4f\n",
  n_x, calc_outlier_cv(n_x, "x13"), calc_outlier_cv(n_t, "tramoseats")
))
n_grid <- c(36, 50, 60, 100, 144, 200, 300, 449, 450, 600)
print(data.frame(
  n = n_grid,
  x13 = round(vapply(n_grid, calc_outlier_cv, 0, method = "x13"), 4),
  tramoseats = round(vapply(n_grid, calc_outlier_cv, 0, method = "tramoseats"), 4)
))

## [6] Default-Lauf gegen Formelwert ----------------------------------------
hr("[6] min |t| im Default-Lauf gegen die Formel")
lb_x <- obj_x$output$preprocessing$diagnostics
cat("verfuegbare Residualtests (x13):", paste(names(lb_x), collapse = ", "), "\n")
print(unlist(lb_x[grep("^LB", names(lb_x))], recursive = FALSE))
print(data.frame(
  reihe = c("x13", "tramoseats"),
  cv_formel = c(calc_outlier_cv(n_x, "x13"), calc_outlier_cv(n_t, "tramoseats")),
  min_abs_t = c(otl_mint(obj_x), otl_mint(obj_t)),
  cv_spec = c(spec_cv(obj_x)$va[1], spec_cv(obj_t)$va[1])
))
cat("Deutung: min |t| < cv_formel  ->  Schwelle wurde im AMI-Zyklus gesenkt\n")
cat("           (predcv bzw. pc);  min |t| >= cv_formel -> Formel passt\n")

## [7] Einklammernde Suche ---------------------------------------------------
hr("[7] Auswirkung eines fest gesetzten Critical Value")
grid_x <- sort(unique(round(c(2.8, 3.0, 3.2, 3.4, calc_outlier_cv(n_x, "x13"),
                              4.0, 4.2), 3)))
tab <- data.frame(cv = c(NA_real_, grid_x), ausreisser = NA_character_,
                  min_abs_t = NA_real_, stringsAsFactors = FALSE)
tab$ausreisser[1] <- otl_sig(obj_x)
tab$min_abs_t[1] <- otl_mint(obj_x)
for (i in seq_along(grid_x)) {
  o <- fit_x13(AirPassengers, cv = grid_x[i])
  tab$ausreisser[i + 1] <- otl_sig(o)
  tab$min_abs_t[i + 1] <- otl_mint(o)
}
cat("x13, Zeile 1 = Default (auto):\n")
print(tab, row.names = FALSE)

grid_t <- sort(unique(round(c(2.0, 3.0, calc_outlier_cv(n_t, "tramoseats"),
                              3.5, 4.0), 3)))
tab_t <- data.frame(cv = c(NA_real_, grid_t), ausreisser = NA_character_,
                    min_abs_t = NA_real_, stringsAsFactors = FALSE)
tab_t$ausreisser[1] <- otl_sig(obj_t)
tab_t$min_abs_t[1] <- otl_mint(obj_t)
for (i in seq_along(grid_t)) {
  o <- fit_tramo(AirPassengers, cv = grid_t[i])
  tab_t$ausreisser[i + 1] <- otl_sig(o)
  tab_t$min_abs_t[i + 1] <- otl_mint(o)
}
cat("\ntramoseats, Zeile 1 = Default (auto):\n")
print(tab_t, row.names = FALSE)

## [8] Was setzt set_outlier(critical.value = .)? ----------------------------
hr("[8] Geschriebene Spec-Felder bei critical.value = 4")
print(spec_cv(fit_x13(AirPassengers, cv = 4)))
print(spec_cv(fit_tramo(AirPassengers, cv = 4)))

## [9] Manuell gesetzter Ausreisser -----------------------------------------
hr("[9] Manuell gesetzter Ausreisser")
obj_m <- perX13(AirPassengers, "rsa3", outliers = "AO1960.5")
obj_m$run()
print(getOutliers(obj_m))
cat("manuell in der Spec:", vapply(
  obj_m$params$regarima$regression$outliers,
  function(z) paste0(z$code, " @ ", as.character(z$pos)), ""
), "\n")

hr("fertig")
cat("Bitte zurueckmelden: [2] Spec-Auszuege, [3] n-Tabelle,",
    "[6] min |t| vs. cv_formel, [7] beide Tabellen,\n",
    "                    [8] defva/va nach critical.value = 4\n")
