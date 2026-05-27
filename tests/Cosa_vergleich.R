library(data.table)
library(highcharter)
#library(persephone)
mlauf <- mountSTAT::mountMeth(folder = "Froehlich")
pi <- fread(paste0(mlauf, "/J2026/COSA/PI_verkettet.csv"), dec = ",")
pi_mts <- ts(as.matrix(pi[,2:ncol(pi)]), start = c(2000,1), freq = 12)

# Reihen mit 0 Varianz rauslöschen, sonst hängt sich das hts auf
vars <- apply(pi_mts, 2, var)
cols_nam <- colnames(pi_mts)[!vars == 0]
pi_clean <- pi_mts[, cols_nam]

hchart(pi_clean[,1])

#mts <- perBatch(list=pi_mts0)
hts <- perHts(list=pi_clean, method = "x13", spec = "RSA5c")
hts$run()
hts$components$AT00$output$regarima

n_ts <- ncol(pi_clean)
for(ii in 1:n_ts){
  test <- perX13(pi_clean[,1], template = "rsa5c")
  test$run()
  c(test$output$regarima$arma, )
}

test$output$mstats$q
test$output$mstats$qm2
