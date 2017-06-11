Synth1_40 <- read.csv("TP4/dataset/donnees/Synth1-40.csv")
Synth1_40$title <- "synth1_40"
Synth1_1000 <- read.csv("TP4/dataset/donnees/Synth1-1000.csv")
Synth1_1000$title <- "synth1_1000"
Synth2_1000 <- read.csv("TP4/dataset/donnees/Synth2-1000.csv")
Synth2_1000$title <- "synth2_1000"
Synth3_1000 <- read.csv("TP4/dataset/donnees/Synth3-1000.csv")
Synth3_1000$title <- "synth3_1000"
Pima <- read.csv("TP4/dataset/donnees/Pima.csv", header=T)

source("TP4/script/fonctions/anadisc.R")
source("TP4/script/fonctions/separ1.R")
source("TP4/script/fonctions/prob.ad.R")
source("TP4/script/fonctions/prob.log.R")
source("TP4/script/fonctions/prob.log2.R")
source("TP4/script/fonctions/logistic.R")
source("TP4/script/fonctions/log.R")
source("TP4/script/fonctions/erreur.R")

X <- Pima[,1:7]
z <- Pima[,8]

  
data_synth <- separ1(X,z)

adl <- adl.app(data_synth$Xapp,data_synth$zapp)
adq <- adq.app(data_synth$Xapp,data_synth$zapp)
nba <- nba.app(data_synth$Xapp,data_synth$zapp)
log <- log.app(data_synth$Xapp,data_synth$zapp,1,1e-5)
Xappq <- log.quadra(data_synth$Xapp)
Xtstq <- log.quadra(data_synth$Xtst)
logq <- log.app(Xappq, data_synth$zapp,1,1e-5)
# val <- ad.val(adl,data_synth$Xtst)
err_adl <- erreur(20,X,z,"adl")
err_adq <- erreur(20,X,z,"adq")
err_nba <- erreur(20,X,z,"nba")
err_log <- erreur(20,X,z,"log")
err_logq <- erreur(20,X,z,"logq")
# prob.ad(nba,data_synth$Xtst, data_synth$ztst, 0.5)
# prob.log(log$beta, data_synth$Xtst, data_synth$ztst, 0.5)
# prob.log2(logq$beta, data_synth$Xtst, data_synth$ztst, 0.5)
