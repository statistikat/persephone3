## show_bvar_shift.R -- vergleicht alte und neue SE-Zuordnung
library(persephone3)

build_table <- function(x) {
  variables <- x$output$preprocessing$description$variables
  vartype <- sapply(variables, function(v) v$type)
  otlind <- which(vartype %in% c("AO", "TC", "LS"))
  if (length(otlind) == 0) return(NA)
  outliers <- variables[otlind]
  outliers <- as.data.frame(do.call(rbind, lapply(outliers, unlist)))
  outliers$coef.value <- as.numeric(outliers$coef.value)
  outliers$date <- substr(outliers$name, 5, 14)
  nn <- lapply(strsplit(substr(outliers$date, 1, 7), "-"), as.numeric)
  outliers$date <- as.Date(outliers$date, origin = "1970-01-01")
  outliers$name <- paste0(outliers$type, sapply(nn, paste, collapse = "."))
  list(outliers = outliers, otlind = otlind,
       variables = variables, est = x$output$preprocessing$estimation)
}

finish <- function(b, stde) {
  o <- b$outliers
  tv <- o$coef.value / stde
  ndf <- b$est$likelihood$neffectiveobs - b$est$likelihood$nparams
  o$stde <- stde
  o$t <- tv
  o$pvalue <- 2 * pt(abs(tv), ndf, lower.tail = FALSE)
  o[, c("name", "type", "coef.value", "stde", "t", "pvalue", "date", "coef.type")]
}

# ALT: Indexierung ueber otlind (Position in ALLEN Variablen)
getOutliers_old <- function(x) {
  b <- build_table(x)
  if (length(b) == 1 && is.na(b)) return(NA)
  finish(b, sqrt(diag(b$est$bvar))[b$otlind])
}

# NEU: bvar enthaelt nur die ESTIMATED-Koeffizienten, in Variablenreihenfolge
getOutliers_new <- function(x) {
  b <- build_table(x)
  if (length(b) == 1 && is.na(b)) return(NA)
  coef_type <- vapply(b$variables, function(v) as.character(v$coef$type), "")
  stde_all <- rep(NA_real_, length(b$variables))
  if (length(b$est$bvar) > 0) {
    sel <- coef_type == "ESTIMATED"
    if (nrow(b$est$bvar) == sum(sel)) {
      stde_all[sel] <- sqrt(diag(b$est$bvar))
    } else {
      warning("bvar passt nicht zur Zahl der geschaetzten Variablen.")
    }
  }
  finish(b, stde_all[b$otlind])
}

compare <- function(o, label) {
  v <- o$output$preprocessing$description$variables
  est <- o$output$preprocessing$estimation
  typ <- vapply(v, function(z) as.character(z$coef$type), "")
  cat("\n###", label, "###\n")
  cat("length(variables) =", length(v),
      "| nrow(bvar) =", nrow(est$bvar),
      "| ncol(X) =", ncol(est$X), "\n")
  cat("coef.type ueber ALLE Variablen  :", paste(typ, collapse = " | "), "\n")
  cat("est$b (Reihenfolge der X-Spalten):",
      paste(round(as.vector(est$b), 6), collapse = ", "), "\n")
  cat("ESTIMATED-Koeff. in Var-Reihenfolge:",
      paste(round(as.numeric(vapply(
        v[typ == "ESTIMATED"], function(z) as.numeric(z$coef$value), 0
      )), 6), collapse = ", "), "\n")
  old <- getOutliers_old(o)
  new <- suppressWarnings(getOutliers_new(o))
  cat("--- ALT ---\n"); print(old, digits = 5)
  cat("--- NEU ---\n"); print(new, digits = 5)
  ref <- persephone3:::getRegressors(o)
  ref <- ref[ref$name %in% new$name, c("name", "stde", "t", "pvalue")]
  cat("--- NEU == getRegressors()?",
      isTRUE(all.equal(ref, new[, c("name", "stde", "t", "pvalue")],
                       check.attributes = FALSE)), "\n")
  if (isTRUE(all.equal(old[, c("stde", "t", "pvalue")],
                       new[, c("stde", "t", "pvalue")]))) {
    cat("ALT == NEU\n")
  } else {
    cat(">>> ALT und NEU UNTERSCHEIDEN SICH\n")
  }
}

## Fall 1: zwei manuelle AO, beide ESTIMATED -> keine Divergenz
o1 <- perX13(AirPassengers, "rsa3",
             addout.type = c("AO", "AO"),
             addout.date = as.Date(c("1958-07-01", "1960-05-01")))
o1$run()
compare(o1, "Fall 1: alle ESTIMATED")

## Fall 2: ein Koeffizient per addout.coef fixiert, in der MITTE
o2 <- perX13(AirPassengers, "rsa3",
             addout.type = rep("TC", 3),
             addout.date = as.Date(c("1958-07-01", "1959-07-01", "1960-05-01")),
             addout.coef = c(0, -0.15, 0))
o2$run()
compare(o2, "Fall 2: AO1959.7 per addout.coef FIXIERT")

## Fall 3: manueller Outlier innerhalb des Samples -> ESTIMATED
d3 <- window(AirPassengers, start = c(1955, 1), end = c(1960, 12))
o3 <- perX13(d3, "rsa3", outliers = "AO1958.5")
o3$run()
compare(o3, "Fall 3: outliers = 'AO1958.5' (innerhalb)")

## Fall 4: manueller Outlier VOR Serienbeginn -> FIXED, bvar kleiner
o4 <- perX13(d3, "rsa3", outliers = "AO1950.5")
o4$run()
compare(o4, "Fall 4: outliers = 'AO1950.5' (vor Serienbeginn -> FIXED)")
