##############################################################################
# test-cv-x13.R -- Beweis, dass calc_outlier_cv(..., "x13") den Critical Value
# von JDemetra+ korrekt nachbaut
#
# Vier unabhaengige Wege:
#   [A] Java-Referenzwert: CriticalValueComputer / X13Utility direkt aufrufen
#   [B] Bisektion: bei fest gesetztem cv verschwindet AO1960.3 zwischen 3.4 und
#       3.89; die Sprungstelle T* muss unter dem Formelwert F liegen
#   [C] Flip-Test: Shock der Groesse delta an 1960-03. Das delta, bei dem der
#       Default-Lauf (auto) erstmals erkennt, klemmt F ein:
#           T*(delta_unterhalb) < F <= T*(delta_flip)
#   [D] n-Abhaengigkeit: gleicher Shock, aber der Series-Span (basic) wird
#       schrittweise verkleinert; erkannt werden muss er genau bei den
#       Spanlaengen n mit F(n) < T*
#
# Bestanden, wenn [A] (falls Java-Probe laeuft) und [C] auf denselben Wert
# innerhalb der Bisektionstoleranz zeigen.
##############################################################################

if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressMessages(library(persephone3))
}

RUN_JAVA_PROBE <- TRUE
RUN_BISECTION  <- TRUE
RUN_FLIP       <- TRUE
RUN_NDEP       <- TRUE

hr <- function(txt) cat("\n==== ", txt, " ====\n", sep = "")

# ---- Helpers aus Runde 1 ---------------------------------------------------
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

fit_x13 <- function(ts, cv = NULL, template = "rsa3", ...) {
  extra <- list(...)
  if (!is.null(cv)) extra$setout.critical.value <- cv
  o <- do.call(perX13, c(list(ts, template), extra))
  o$run()
  o
}
otl_set <- function(obj) {
  o <- getOutliers(obj)
  if (length(o) == 1 && all(is.na(o))) return(character(0))
  o$name
}
detected <- function(ts, cv = NULL, name, template = "rsa3", ...) {
  name %in% otl_set(fit_x13(ts, cv = cv, template = template, ...))
}
n_of <- function(obj) NROW(obj$output$preprocessing$estimation$y)
# Name, den die Erkennung fuer die Beobachtung i vergeben wurde (x13-Schema)
otl_name <- function(ts, i, type = "AO") {
  t <- stats::time(ts)[i]
  y <- floor(t + 1e-9)
  paste0(type, y, ".", round((t - y) * stats::frequency(ts)) + 1)
}

# Bisektion auf der cv-Achse: groesstes cv, bei dem `name` noch erkannt wird
bisect_cv <- function(ts, name, lo, hi, tol = 0.005, verbose = TRUE, ...) {
  f_lo <- detected(ts, lo, name, ...)
  f_hi <- detected(ts, hi, name, ...)
  if (f_lo == f_hi) {
    if (verbose) cat(sprintf(
      "  keine Trennung in [%g, %g] (beide %s)\n", lo, hi,
      ifelse(f_lo, "erkannt", "nicht erkannt")))
    return(invisible(NULL))
  }
  a <- lo
  b <- hi
  while (b - a > tol) {
    m <- (a + b) / 2
    if (detected(ts, m, name, ...) == f_lo) a <- m else b <- m
    if (verbose) cat(sprintf("    [%g, %g]\n", a, b))
  }
  list(lo = a, hi = b, detected_at_lo = f_lo, Tstar = (a + b) / 2)
}

# Bisektion auf der delta-Achse: kleinstes delta, bei dem auto erkennt
bisect_delta <- function(ts, i, name, lo, hi, tol = 0.5, verbose = TRUE,
                         cv = NULL, ...) {
  shock <- function(delta) {
    s <- ts
    s[i] <- s[i] + delta
    s
  }
  f_lo <- detected(shock(lo), cv, name, ...)
  f_hi <- detected(shock(hi), cv, name, ...)
  if (f_lo == f_hi) {
    if (verbose) cat(sprintf(
      "  kein Umschalten in delta [%g, %g] (beide %s)\n", lo, hi,
      ifelse(f_lo, "erkannt", "nicht erkannt")))
    return(invisible(NULL))
  }
  a <- lo
  b <- hi
  while (b - a > tol) {
    m <- (a + b) / 2
    if (detected(shock(m), cv, name, ...) == f_lo) a <- m else b <- m
    if (verbose) cat(sprintf("    delta [%g, %g]\n", a, b))
  }
  list(lo = a, hi = b, shock = shock, detected_at_hi = f_hi)
}

## [A] Java-Referenzwert ------------------------------------------------------
if (RUN_JAVA_PROBE) {
  hr("[A] Java-Referenzwert fuer calcCv")
  cat("rjd3toolkit:", as.character(utils::packageVersion("rjd3toolkit")),
      "| rJava:", as.character(utils::packageVersion("rJava")), "\n")
  java_cv <- function(n, which = c("x13", "tramo"), threshold = 0.05) {
    which <- match.arg(which)
    msgs <- character(0)
    note <- function(tag, e) {
      msgs[[length(msgs) + 1]] <<- sprintf("%s: %s", tag, conditionMessage(e))
      NA_real_
    }
    # rjd3 >= 3.8 ruft Java ueber rJava auf; aeltere Staedte hatten rjapi
    jc <- tryCatch(
      if (requireNamespace("rJava", quietly = TRUE)) {
        rJava::.jcall
      } else {
        getExportedValue("rjapi", "jcall")
      },
      error = function(e) note("kein jcall", e)
    )
    if (length(msgs) > 0) {
      return(list(direct = NA_real_, via_computer = NA_real_, msg = msgs[1]))
    }
    cls <- switch(which,
      x13 = "jdplus/x13/base/core/x13/regarima/X13Utility",
      tramo = "jdplus/tramoseats/base/core/tramo/internal/TramoUtility"
    )
    # 1) Direkt: X13Utility.calcCv(n) / TramoUtility.calcCv(n)
    direct <- tryCatch(
      jc(cls, "D", "calcCv", as.integer(n)),
      error = function(e) note("calcCv", e)
    )
    # 2) Unabhaengig davon: CriticalValueComputer, also der Pfad den Java selbst geht
    via_computer <- tryCatch({
      comp <- "jdplus/toolkit/base/core/regarima/outlier/CriticalValueComputer"
      f <- if (which == "x13") {
        jc(comp, "Ljava/util/function/IntToDoubleFunction;",
           "advancedComputer", as.double(threshold))
      } else {
        jc(comp, "Ljava/util/function/IntToDoubleFunction;", "simpleComputer")
      }
      v <- jc(f, "D", "applyAsDouble", as.integer(n))
      max(v, if (which == "x13") 2.8 else 2)
    }, error = function(e) note("CriticalValueComputer", e))
    list(direct = direct, via_computer = via_computer,
         msg = if (length(msgs) > 0) msgs[1] else "")
  }
  # Java-Backend waermt sich erst beim ersten echten Lauf auf. (Winzige Reihen
  # vermeiden: die rjd3-dev-Konversion scheitert an leeren Feldern.)
  invisible(fit_x13(AirPassengers))
  fmt <- function(x) if (length(x) == 1 && is.na(x)) "n/a" else sprintf("%.4f", x)
  report <- function(label, p, F) {
    if (nzchar(p$msg)) cat("  Java-Probe Fehler:", p$msg, "\n")
    cat(sprintf("%s   Java calcCv: %s | Java Computer: %s | R-Formel: %.4f\n",
                label, fmt(p$direct), fmt(p$via_computer), F))
    both <- c(p$direct, p$via_computer)
    both <- both[is.finite(both)]
    if (length(both) > 0) {
      cat(sprintf("  Abweichung Java vs. R-Formel: %.5f  -> %s\n",
                  max(abs(both - F)),
                  ifelse(all(abs(both - F) < 0.005),
                         "FORMEL IDENTISCH MIT JAVA", "FORMEL WEICHT AB")))
    }
  }
  report("x13  n = 144:", java_cv(144, "x13"), calc_outlier_cv(144, "x13"))
  report("TRAMO n = 144:", java_cv(144, "tramo"),
         calc_outlier_cv(144, "tramoseats"))
  for (n in c(36, 60, 300, 600)) {
    p <- java_cv(n, "x13")
    cat(sprintf("x13  n = %3d:  Java calcCv: %s | R-Formel: %.4f\n",
                n, fmt(p$direct), calc_outlier_cv(n, "x13")))
  }
}

## [B] Bisektion bei AirPassengers (n = 144) ---------------------------------
if (RUN_BISECTION) {
  hr("[B] Sprungstelle von AO1960.3, n = 144")
  F144 <- calc_outlier_cv(144, "x13")
  cat(sprintf("Formelwert F(144) = %.4f\n", F144))
  res <- bisect_cv(AirPassengers, "AO1960.3", 3.0, 4.4)
  if (!is.null(res)) {
    cat(sprintf(
      "T* in [%.4f, %.4f], Abstand zu F(144) = %.4f betraegt %.4f  ->  %s\n",
      res$lo, res$hi, F144, F144 - res$Tstar,
      ifelse(res$Tstar < F144, "konsistent", "WIDERSPRUCH"))
    )
  }
}

## [C] Flip-Test: Shock an 1960-03 -------------------------------------------
if (RUN_FLIP) {
  hr("[C] Flip-Test bei 1960-03 (Klemmt F ein)")
  F144 <- calc_outlier_cv(length(AirPassengers), "x13")
  i0 <- which(cycle(AirPassengers) == 3 & floor(stats::time(AirPassengers)) == 1960)
  target <- otl_name(AirPassengers, i0)
  cat(sprintf("Index %d, Ziel %s, F(144) = %.4f\n", i0, target, F144))
  fl <- bisect_delta(AirPassengers, i0, target, 20, 120, tol = 0.5)
  if (!is.null(fl)) {
    cat(sprintf(
      "auto erkennt ab delta ~ %.2f (nicht bei %.2f)\n", fl$hi, fl$lo))
    cat("cv-Bisektion fuer delta knapp UNTER der Schwelle:\n")
    below <- bisect_cv(fl$shock(fl$lo), target, 2.9, 4.5)
    cat("cv-Bisektion fuer delta knapp UEBER der Schwelle:\n")
    above <- bisect_cv(fl$shock(fl$hi), target, 2.9, 4.5)
    if (!is.null(below) && !is.null(above)) {
      cat(sprintf(
        "\nT*(delta=%.1f) = %.4f   T*(delta=%.1f) = %.4f   F(144) = %.4f\n",
        fl$lo, below$Tstar, fl$hi, above$Tstar, F144))
      ok <- below$hi < F144 && F144 <= above$lo
      cat(sprintf(
        "Einschliessung %s < F <= %s  ->  %s\n",
        sprintf("%.3f", below$hi), sprintf("%.3f", above$lo),
        ifelse(ok, "FORMEL BESTAETIGT",
               "Formel liegt AUSSERHALB des Intervalls"))
      )
    }
  }
}

## [D] n-Abhaengigkeit: gleicher Shock, nur der Series-Span wird verkleinert.
##     Der Shock wird so justiert, dass seine Erkennungsstatistik T* knapp ueber
##     cv_mid liegt. Erkannt werden muss er genau bei den Spanlaengen n mit
##     F(n) < T*.
if (RUN_NDEP) {
  hr("[D] n-Abhaengigkeit ueber den Series-Span")
  cv_mid <- 3.75
  # Ziel = Dez 1949 (Index 12), damit es auch beim kuerzesten Span (n0 = 50)
  # noch innerhalb des Schaetzbereichs liegt
  i0 <- which(cycle(AirPassengers) == 12 &
                floor(stats::time(AirPassengers)) == 1949)
  target <- otl_name(AirPassengers, i0)
  cat(sprintf("Ziel %s, Shock wird auf T* ~ %.2f eingestellt\n", target, cv_mid))
  fl <- bisect_delta(AirPassengers, i0, target, 10, 150, tol = 0.25, cv = cv_mid)
  if (is.null(fl)) {
    cat("Delta-Bisektion fehlgeschlagen - Grenzen anpassen\n")
  } else {
    ap <- fl$shock(fl$hi)   # T* liegt knapp ueber cv_mid
    cat(sprintf("Delta = %.2f\n\n", fl$hi))
    spans <- list(
      "alle (144)" = NULL,
      "110" = list(basic.type = "First", basic.n0 = 110),
      "95"  = list(basic.type = "First", basic.n0 = 95),
      "84"  = list(basic.type = "First", basic.n0 = 84),
      "75"  = list(basic.type = "First", basic.n0 = 75),
      "66"  = list(basic.type = "First", basic.n0 = 66),
      "50"  = list(basic.type = "First", basic.n0 = 50)
    )
    out <- do.call(rbind, lapply(names(spans), function(lab) {
      o <- do.call(fit_x13, c(list(ap, NULL), spans[[lab]]))
      n <- n_of(o)
      F <- calc_outlier_cv(n, "x13")
      data.frame(
        span = lab, n_used = n, F_formel = round(F, 4),
        F_unter_Tstar = F < cv_mid,
        erkannt = target %in% otl_set(o),
        weitere_ausreisser = paste(setdiff(otl_set(o), target), collapse = " ")
      )
    }))
    print(out, row.names = FALSE)
    ok <- all(out$erkannt[-1] == out$F_unter_Tstar[-1])
    cat(sprintf(
      "\nErkannt genau bei F < T* ~ %.2f ?  ->  %s\n", cv_mid,
      ifelse(ok, "ja, n-Abhaengigkeit der Formel bestaetigt",
             "NEIN - bitte Zeilen mit F_unter_Tstar != erkannt pruefen"))
    )
    cat("(Hinweis: bei kurzem Span aendern sich Modell und Residualstreuung,\n",
        " T* verschiebt sich leicht; Unschaerfe also nur nahe der Grenze.)\n")
  }
}

## [E] Rest: Residualtests korrekt greifen -----------------------------------
hr("[E] Residualtests (Namen sind kleingeschrieben)")
o <- fit_x13(AirPassengers)
d <- o$output$preprocessing$diagnostics
cat("verfuegbare Tests:", paste(names(d), collapse = ", "), "\n")
print(do.call(rbind, lapply(d[grep("^lb", names(d), ignore.case = TRUE)],
                            function(z) unlist(z, recursive = FALSE))))

hr("fertig")
cat("Bestanden, wenn [A] Java-Wert = R-Formel und [C] 'FORMEL BESTAETIGT'\n")
