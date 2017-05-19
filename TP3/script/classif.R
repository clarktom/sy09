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
  z <- matrix(data=NA, nrow=nrow(distance), 1)
  for (i in 1:nrow(distance)){
    ztmp <- zapp[as.numeric(labels(distance[i,order(distance[i,])[1:K]]))]
    print(ztmp)
    z[i] <- as.numeric(names(sort(table(ztmp), decreasing = TRUE))[1])
  }
  z
}
kppv.tune <- function(Xapp, zapp, Xval, zval, nppv)
{
  
}