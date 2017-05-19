donn <- read.csv("TP3/dataset/donnees-tp3/Synth1-40.csv")
X <- donn[,1:2]
z <- donn[,3]
source("TP3/script/distXY.r")

ceuc.app <- function(Xapp, zapp)
{
  # Xapp matrice des individus variables 
  # zapp étiquettes des individus
  X1 <- matrix(data=NA,nrow=nrow(Xapp),ncol=2)
  centroids <- matrix(data=NA,nrow=2,ncol=2)
  X1_it <- 0
  X2 <- matrix(data=NA,nrow=nrow(Xapp),ncol=2)
  X2_it <- 0
  z2 <- z == 1
  for (individu in 1:nrow(Xapp)) { 
    if(z2[individu]){
      X1_it = X1_it + 1
      X1[X1_it,1] <- X[individu,1]
      X1[X1_it,2] <- X[individu,2]
    }
    else{
      X2_it = X2_it + 1
      X2[X2_it,1] <- X[individu,1]
      X2[X2_it,2] <- X[individu,2]
    }
  }
 X1 <- na.omit(as.data.frame(X1))
 X1_centroid <- c(mean(X1[,1]), mean(X1[,2]))
 X2 <- na.omit(as.data.frame(X2))
 X2_centroid <- c(mean(X2[,1]), mean(X2[,2]))
 centroids[1,] <- X1_centroid
 centroids[2,] <- X2_centroid
 plot(X2)
 # centroids = barycentres
 centroids
 
}
ceuc.val <- function(mu, Xtst)
{
  # mu les barycentres
  # Xtst la matrice de tests
  for(individu in 1:nrow(Xtst)){
    print("distance vers premier barycentre : " ,distXY(Xtst[individu,],mu[1,] ))
    print("distance vers second barycentre : " ,distXY(Xtst[individu,], mu[2,]))
  }
}