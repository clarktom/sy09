Synth1_1000 <- read.csv("TP4/dataset/donnees/Synth1-1000.csv")
Synth1_1000$title <- "synth1_1000"
Synth2_1000 <- read.csv("TP4/dataset/donnees/Synth2-1000.csv")
Synth2_1000$title <- "synth2_1000"
Synth2_1000 <- read.csv("TP4/dataset/donnees/Synth3-1000.csv")
Synth2_1000$title <- "synth3_1000"

source("TP4/script/fonctions/anadisc.R")
source("TP4/script/fonctions/separ1.R")

X <- Synth1_1000[,1:2]
z <- Synth1_1000[,3]
  
data_synth <- separ1(X,z)

adl <- adl.app(data_synth$Xapp,data_synth$zapp)
adq <- adq.app(data_synth$Xapp,data_synth$zapp)
nba <- nba.app(data_synth$Xapp,data_synth$zapp)

ad.val(adl,data_synth$Xtst)
