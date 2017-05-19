donn <- read.csv("TP3/dataset/donnees-tp3/Synth1-40.csv")
X <- donn[,1:2]
z <- donn[,3]
source("TP3/script/distXY.r")


ceuc.app <- function(Xapp, zapp){
  mu <-matrix(,nrow=length(unique(zapp)),ncol(Xapp))
  for (i in 1:(length(unique(zapp)))){
    mu[i,]<-apply(Xapp[which(zapp==i),],2,mean)
  }
  mu
}

ceuc.val <- function(mu, Xtst) {
  etiquette<-matrix(,nrow=nrow(Xtst),1)
  distTab <- distXY(mu, Xtst)
  for (i in 1:(nrow(Xtst))){
    etiquette[i,] <- which.min(distTab[,i])
  }
  etiquette
}