Synth1_40 <- read.csv("TP4/dataset/donnees/Synth1-40.csv")
Synth1_40$title <- "synth1_40"
Synth1_1000 <- read.csv("TP4/dataset/donnees/Synth1-1000.csv")
Synth1_1000$title <- "synth1_1000"
Synth2_1000 <- read.csv("TP4/dataset/donnees/Synth2-1000.csv")
Synth2_1000$title <- "synth2_1000"
Synth2_1000 <- read.csv("TP4/dataset/donnees/Synth3-1000.csv")
Synth2_1000$title <- "synth3_1000"

source("TP4/script/fonctions/anadisc.R")
source("TP4/script/fonctions/separ1.R")
source("TP4/script/fonctions/prob.ad.R")
source("TP4/script/fonctions/prob.log.R")
source("TP4/script/fonctions/prob.log2.R")
source("TP4/script/fonctions/logistic.R")
source("TP4/script/fonctions/log.R")

X <- Synth1_40[,1:2]
z <- Synth1_40[,3]
  
data_synth <- separ1(X,z)

#adl <- adl.app(data_synth$Xapp,data_synth$zapp)
#adq <- adq.app(data_synth$Xapp,data_synth$zapp)
#nba <- nba.app(data_synth$Xapp,data_synth$zapp)
log <- log.app(data_synth$Xapp,data_synth$zapp,1,1e-5)
print(log)
#Xappq <- log.quadra(data_synth$Xapp)
#Xtstq <- log.quadra(data_synth$Xtst)
#logq <- log.app(Xappq, data_synth$zapp,1,1e-5)
# val <- ad.val(adl,data_synth$Xtst)

#prob.ad(nba,data_synth$Xtst, data_synth$ztst, 0.5)
#prob.log2(logq$beta, data_synth$Xtst, data_synth$ztst, 0.5)
