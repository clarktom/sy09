source("TP4/script/fonctions/anadisc.R")
source("TP4/script/fonctions/separ1.R")
source("TP4/script/fonctions/prob.ad.R")
source("TP4/script/fonctions/prob.log.R")
source("TP4/script/fonctions/prob.log2.R")
source("TP4/script/fonctions/log.R")
source("TP4/script/fonctions/erreur.R")

dataset <- read.csv("TP4/dataset/donnees/spam.csv", header=T)

X <- dataset[,2:58]
z <- dataset[,59]



err_adl <- NULL
err_adq <- NULL
err_nba <- NULL
err_log <- NULL
err_logq <- NULL
err_adl <- erreur(20, X, z, "adl")
err_adq <- erreur(20, X, z, "adq")
err_nba <- erreur(20, X, z, "nba")
err_log <- erreur(20, X, z, "log")
err_logq <- erreur(20, X, z, "logq")
taux_adl <- NULL
taux_adq <- NULL
taux_nba <- NULL
taux_log <- NULL
taux_logq <- NULL
taux_adl <- taux_intervalle(20, err_adl$err_app, err_adl$err_tst)
taux_adq <- taux_intervalle(20, err_adq$err_app, err_adq$err_tst)
taux_nba <- taux_intervalle(20, err_nba$err_app, err_nba$err_tst)
taux_log <- taux_intervalle(20, err_log$err_app, err_log$err_tst)
taux_logq <- taux_intervalle(20, err_logq$err_app, err_logq$err_tst)
taux_adl$taux_tst
taux_adq$taux_tst
taux_nba$taux_tst
taux_log$taux_tst
taux_logq$taux_tst

# prob.ad(nba,data_synth$Xtst, data_synth$ztst, 0.5)
# prob.log2(logq$beta, data_synth$Xtst, data_synth$ztst, 0.5)
