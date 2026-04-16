##############################################

# Original unverkette Reihen
ts20_ul <- ki_unverkettet$C20
ts21_ul <- ki_unverkettet$C21
ts20_21_ul <- ki_unverkettet$C20_21

# Original verkettete Reihen
ts20 <- kett[, 66]
ts21 <- kett[, 74]
ts20_21 <- kett[, 73]

# Chainlinking
linked20 <- ts20_ul*lf_20
linked21 <- ts21_ul*lf_21

# Unlinking
unlinked20 <- ts20/lf_20
unlinked21 <- ts21/lf_21

# Saisonbereinigte verkettete Reihen
objX13_x <- perX13(ts20, "rsa3")
objX13_y <- perX13(ts21, "rsa3")
objX13_x$run()
objX13_y$run()

sa20 <- objX13_x$adjusted
sa21 <- objX13_y$adjusted

# unlinking sa-series
sa20_ul <- sa20/lf_20
sa21_ul <- sa21/lf_21

# gewichtung
gew20 <- ts20_21/(ts20+ts21)
gew21 <- 1-gew20

# Gewichtung der unverketteten SA Reihen
saAgg_ul <- sa20_ul*gew20 + sa21_ul*gew21

# Verkettung der unverketteten SA Reihen
saAgg <- saAgg_ul*lf_20_21
saAgg

# Rereference to 2021
refFak <- 2 - mean(window(saAgg, start=c(2021,1), end=c(2021,12)))/100
saAggFin <- saAgg*refFak
plot(saAggFin)

# unkorrekt aggregierte SA-Reihen
saAgg_f <- sa20*gew20 + sa21*gew21
lines(saAgg_f, col="green")
plot(saAggFin-saAgg_f)


#########################################
obj <- perX13(AirPassengers, "rsa3")
obj$run()
obj

library(rjd3x13)

dict <- x13_dictionary()

grep("spect", dict, value = TRUE, ignore.case = TRUE)
grep("peak",  dict, value = TRUE, ignore.case = TRUE)
grep("td",    dict, value = TRUE, ignore.case = TRUE)
grep("seas",  dict, value = TRUE, ignore.case = TRUE)
grep("resid", dict, value = TRUE, ignore.case = TRUE)



res <- m$result$preprocessing$estimation$res

jd_spectral_test(res, freq = 12, type = "seas", nearest = 1)

# 2te version

jd_spectral_test <- function(residuals, freq = 12,
                            type = c("td", "seas"),
                            nearest = 1,
                            tol_exact = 1e-12) {
  type <- match.arg(type)

  x <- as.numeric(na.omit(residuals))
  n <- length(x)
  if (n < 10) stop("Too few residuals")

  # JD+ standardization: variance divisor = n (not n-1)  [1](https://statistikgvat-my.sharepoint.com/personal/markus_froehlich_statistik_gv_at/_layouts/15/Doc.aspx?sourcedoc=%7BE0014539-8A66-4119-8572-A2DC68161CD6%7D&file=JD%2B_Diagnostics.docx&action=default&mobileredirect=true)
  mu <- mean(x)
  sigma <- sqrt(mean((x - mu)^2))
  z <- (x - mu) / sigma

  # Fourier frequencies λ_j = 2π j / n, j=1,...,floor(n/2) [1](https://statistikgvat-my.sharepoint.com/personal/markus_froehlich_statistik_gv_at/_layouts/15/Doc.aspx?sourcedoc=%7BE0014539-8A66-4119-8572-A2DC68161CD6%7D&file=JD%2B_Diagnostics.docx&action=default&mobileredirect=true)
  j <- 1:floor(n / 2)
  lambda <- 2 * pi * j / n

  # Periodogram at Fourier frequencies (equivalent to JD+ definition at λ_j) [1](https://statistikgvat-my.sharepoint.com/personal/markus_froehlich_statistik_gv_at/_layouts/15/Doc.aspx?sourcedoc=%7BE0014539-8A66-4119-8572-A2DC68161CD6%7D&file=JD%2B_Diagnostics.docx&action=default&mobileredirect=true)
  Fz <- fft(z)
  Ij <- (2 / n) * Mod(Fz[j + 1])^2

  # Target frequencies: seasonal or trading-day
  if (type == "seas") {
    if (freq == 12) {
      targets <- 2 * pi * (1:5) / 12  # 2π/12,...,10π/12 [1](https://statistikgvat-my.sharepoint.com/personal/markus_froehlich_statistik_gv_at/_layouts/15/Doc.aspx?sourcedoc=%7BE0014539-8A66-4119-8572-A2DC68161CD6%7D&file=JD%2B_Diagnostics.docx&action=default&mobileredirect=true)
    } else if (freq == 4) {
      targets <- 2 * pi * 1 / 4       # 2π/4 [1](https://statistikgvat-my.sharepoint.com/personal/markus_froehlich_statistik_gv_at/_layouts/15/Doc.aspx?sourcedoc=%7BE0014539-8A66-4119-8572-A2DC68161CD6%7D&file=JD%2B_Diagnostics.docx&action=default&mobileredirect=true)
    } else if (freq == 6) {
      targets <- 2 * pi * c(1, 2) / 6 # 2π/6,4π/6 [1](https://statistikgvat-my.sharepoint.com/personal/markus_froehlich_statistik_gv_at/_layouts/15/Doc.aspx?sourcedoc=%7BE0014539-8A66-4119-8572-A2DC68161CD6%7D&file=JD%2B_Diagnostics.docx&action=default&mobileredirect=true)
    } else {
      stop("Seasonal frequencies not implemented here for this frequency")
    }
  } else {
    # Monthly TD frequency ~ 0.348 cycles/month (7-day cycle vs average month) [3](https://jdemetradocumentation.github.io/JDemetra-documentation/pages/case-studies/spectralgraphs.html)[1](https://statistikgvat-my.sharepoint.com/personal/markus_froehlich_statistik_gv_at/_layouts/15/Doc.aspx?sourcedoc=%7BE0014539-8A66-4119-8572-A2DC68161CD6%7D&file=JD%2B_Diagnostics.docx&action=default&mobileredirect=true)
    if (freq != 12) stop("TD implementation here is for monthly frequency=12")
    td_cycles_per_month <- ((365.25 / freq) / 7) %% 1
    targets <- 2 * pi * td_cycles_per_month
  }

  # Select nearest Fourier frequencies to each target:
  # Use 'nearest=1' to match the JD+ p-values you quoted (0.024, 0.237).
  idx <- unique(unlist(lapply(targets, function(tg) {
    o <- order(abs(lambda - tg))
    o[seq_len(nearest)]
  })))

  stat <- max(Ij[idx])
  k <- length(idx)

  # JD+ p-value for max of k iid χ²₂ ordinates: p = 1 - (1-exp(-stat/2))^k [1](https://statistikgvat-my.sharepoint.com/personal/markus_froehlich_statistik_gv_at/_layouts/15/Doc.aspx?sourcedoc=%7BE0014539-8A66-4119-8572-A2DC68161CD6%7D&file=JD%2B_Diagnostics.docx&action=default&mobileredirect=true)
  pval <- 1 - (1 - exp(-stat / 2))^k

  list(type = type, n = n, k = k, statistic = stat, p.value = pval,
       targets = targets, selected_frequencies = lambda[idx], selected_indices = idx)
}





# Funktion für annual totals:
m <- perX13(AirPassengers, "rsa3")
m$run()
sa <- m$result$final$d11final
AirPassengers

annual_totals(original = AirPassengers, sa = sa)



