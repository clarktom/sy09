source("TP4/script/fonctions/anadisc.R")
source("TP4/script/fonctions/separ1.R")
source("TP4/script/fonctions/prob.ad.R")
source("TP4/script/fonctions/prob.log.R")
source("TP4/script/fonctions/prob.log2.R")
source("TP4/script/fonctions/log.R")
source("TP4/script/fonctions/erreur.R")

Donn <- read.csv("TP4/dataset/donnees/spam.csv", header=T)

X <- Donn[,1:58]
z <- Donn[,59]
  
data_synth <- separ1(X,z)

adl <- adl.app(data_synth$Xapp,data_synth$zapp)
adq <- adq.app(data_synth$Xapp,data_synth$zapp)
nba <- nba.app(data_synth$Xapp,data_synth$zapp)
log <- log.app(data_synth$Xapp,data_synth$zapp,1,1e-5)
Xappq <- log.quadra(data_synth$Xapp)
Xtstq <- log.quadra(data_synth$Xtst)
logq <- log.app(Xappq, data_synth$zapp,1,1e-5)


# prob.ad(nba,data_synth$Xtst, data_synth$ztst, 0.5)
# prob.log2(logq$beta, data_synth$Xtst, data_synth$ztst, 0.5)
