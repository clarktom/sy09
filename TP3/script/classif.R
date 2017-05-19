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
  print(distTab)
  for (i in 1:(nrow(Xtst))){
    etiquette[i,] <- which.min(distTab[,i])
  }
  etiquette
}

kppv.val<-function(Xapp,zapp,K,Xtst){
  Xapp<-as.matrix(Xapp)
  Xtst<-as.matrix(Xtst)
  zapp <- as.vector(zapp)
  napp <- nrow(Xapp) #nb indiv app
  ntst <- nrow(Xtst) #nb indiv test
  
  distance<-distXY(Xtst,Xapp) #distance indiv app-test
  distance2<-t(apply(distance,1,sort)) #distance triée
  dk <- distance2[,K] #kième distance 
  
  distk <- NULL #matrice kième distance
  for(i in 1:napp) {
    distk <- cbind(distk, dk)
  }
  comp <- distance<=distk #matrice booléenne des k indivs les + proches de chaque indiv test
  
  matzapp <- NULL #matrice des étiquettes des indiv d'apprentissage
  for(i in 1:ntst) {
    matzapp <- rbind(matzapp, zapp)
  }
  
  ztst <- comp*matzapp #Ne contient plus que les étiquettes des k plus près indiv d'apprentissage 
  z <- NULL #retient l'étiquette en plus grand nombre
  for(i in 1:ntst) {
    if(length(which(ztst[i,]==1))< length(which(ztst[i,]==2)))
      z[i] <- 2
    else
      z[i] <- 1
  }
  z
}
kppv.tune <- function(Xapp, zapp, Xval, zval, nppv)
{
  
}