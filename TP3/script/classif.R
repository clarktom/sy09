donn <- read.csv("TP3/dataset/donnees-tp3/Synth1-40.csv")
Synth1_40 <- read.csv("TP3/dataset/donnees-tp3/Synth1-40.csv")
Synth1_40$title <- "synth1_40"
Synth1_100 <- read.csv("TP3/dataset/donnees-tp3/Synth1-100.csv")
Synth1_100$title <- "synth1_100"
Synth1_500 <- read.csv("TP3/dataset/donnees-tp3/Synth1-500.csv")
Synth1_500$title <- "synth1_500"
Synth1_1000 <- read.csv("TP3/dataset/donnees-tp3/Synth1-1000.csv")
Synth1_1000$title <- "synth1_1000"
Synth2_1000 <- read.csv("TP3/dataset/donnees-tp3/Synth2-1000.csv")
Synth2_1000$title <- "synth2_1000"
X <- as.matrix(Synth1_40[, 1 : 2])
z <- Synth1_40[, 3]
X <- donn[,1:2]
z <- donn[,3]
source("TP3/script/distXY.r")
library(xtable)

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

kppv.val<-function(Xapp,zapp,K,Xtst){
  Xapp<-as.matrix(Xapp)
  Xtst<-as.matrix(Xtst)
  zapp <- as.vector(zapp)
  napp <- nrow(Xapp) #nb indiv app
  ntst <- nrow(Xtst) #nb indiv test
  
  distance<-distXY(Xtst,Xapp) #distance indiv app-test
  distance2<-t(apply(distance,1,sort))
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

kppv.tune <- function(Xapp, zapp, Xval, zval, nppv) {
  Xapp <- as.matrix(Xapp)
  Xval <- as.matrix(Xval)
  zapp <- as.vector(zapp)
  zval <- as.vector(zval)
  res = c(1:length(nppv)) #contiendra le taux d'erreur pour chaque valeur de ppv
  for(i in 1:length(nppv)) { #Pour chaque valeur de ppv
    kppv_v <- kppv.val(Xapp, zapp, i, Xval) #On cherche le vecteur des étiquettes
    res[i] <- sum(zval == kppv_v)
  }
  res
}

estimation_params <- function(X,z){
  # fonction qui renvoie le mu, sigma et pi (c.f cours page 94)
  # mu = centre de gravité 
  # sigma = matrice de covariance
  # pi = nombre nk/n (avec nk nombre d'individu de classe k)
  classes <- levels(factor(z))
  nb_classes <- length(classes)
  mu <- matrix(, nb_classes, dim(X)[2])
  sigma <- list()
  pi <- c()
  params <- NA
  i <- 0
  for (k in classes){#pour chaque classes
    i <- i+1
    X_k <- X[z==k,]
    mu_k <- apply(X_k, MARGIN=2, mean)
    mu[i,] <- mu_k
    sigma[[i]] <- cov(X_k)
    piK <- length(z[z == k]) / length(z)
    pi <- c(pi, piK)
  }
  params$mu <- mu
  params$sigma <- sigma
  params$pi <- pi
  params
}

all_dataset <- list(Synth1_40, Synth1_100, Synth1_500, Synth1_1000)

# for (dataset in all_dataset) {
#   X <- dataset[, 1 : 2]
#   z <- dataset[, 3]
#   res <- estimation_params(X,z)
#   print(dataset$title)
#   print(res)
# }

erreur_ceuc <- function(N,X,z){
  err_app <- c()
  err_tst <- c()
  for(i in 1:N){
    splitted <- separ1(X,z)
    Xapp <- splitted$Xapp
    zapp <- splitted$zapp
    Xtst <- splitted$Xtst
    ztst <- splitted$ztst
    mu <- ceuc.app(Xapp,zapp)
    res_zapp <- ceuc.val(mu, Xapp)
    res_ztst <- ceuc.val(mu, Xtst)
    err_app[i] <- 1 - (sum(zapp == res_zapp) / length(zapp))
    err_tst[i] <- 1 - (sum(ztst == res_ztst) / length(zapp))
  }
  res <- NULL
  res$err_app <- err_app
  res$err_tst <- err_tst
  res
}

taux_intervalle <- function(N,err_app, err_tst) {
  res <- NULL
  # calcul moyenne, variance et écart type induit
  epsilon_app <- mean(err_app)
  res$taux_app <- epsilon_app
  variance = sum((err_app - epsilon_app)^2)/(N - 1)
  ecart_type <- sqrt(variance) # sd
  # 95% => 1.960
  # calcul intervalle de confiance pour app
  app_intervalle_gauche <- err_app - 1.96 * ecart_type / sqrt(N)
  app_intervalle_droite <- err_app + 1.96 * ecart_type / sqrt(N)
  res$app_inter_gauche <- mean(app_intervalle_gauche)
  res$app_inter_droite <- mean(app_intervalle_droite)
  
  epsilon_tst <- mean(err_tst)
  res$taux_tst <- epsilon_tst
  variance = sum((err_tst - epsilon_tst)^2)/(N - 1)
  ecart_type <- sqrt(variance) # sd
  
  tst_intervalle_gauche <- err_tst - 1.96 * ecart_type / sqrt(N)
  tst_intervalle_droite <- err_tst + 1.96 * ecart_type / sqrt(N)
  
  res$tst_inter_gauche <- mean(tst_intervalle_gauche)
  res$tst_inter_droite <- mean(tst_intervalle_droite)
  res
}



# for (dataset in all_dataset) {
#   X <- dataset[, 1 : 2]
#   z <- dataset[, 3]
#   print(dataset$title)
#   X2 = separ1(X, z)
#   Xapp <- as.matrix(X2$Xapp)
#   zapp <- X2$zapp
#   Xtst <- as.matrix(X2$Xtst)
#   ztst <- X2$ztst
#   l2 <- kppv.tune(Xapp, zapp, Xapp, zapp, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#   print(l2)
# }

# errorCeuc <- erreur_ceuc(20,X,z)
# taux_ceuc <- taux_intervalle(20,errorCeuc$err_app, errorCeuc$err_tst)

erreur_kppv <- function(N,X,z){
  err_app <- c()
  err_tst <- c()
  for(i in 1:N){
    splitted <- separ2(X,z)
    Xapp <- splitted$Xapp
    zapp <- splitted$zapp
    Xval <- splitted$Xval
    zval <- splitted$zval
    Xtst <- splitted$Xtst
    ztst <- splitted$ztst
    K <- which.max(kppv.tune(Xapp, zapp, Xval, zval, c(2, 3, 4, 5, 6, 7, 8, 9, 10)))
    res_zapp <- kppv.val(Xapp, zapp, K, Xapp)
    res_ztst <- kppv.val(Xapp, zapp, K, Xtst)
    err_app[i] <- 1 - sum(zapp == res_zapp) / length(zapp)
    err_tst[i] <- 1 - sum(ztst == res_ztst) / length(zapp)
  }
  res <- NULL
  res$err_app <- err_app
  res$err_tst <- err_tst
  res
}

# for (dataset in all_dataset) {
#   X <- dataset[, 1 : 2]
#   z <- dataset[, 3]
#   errorKppv = erreur_kppv(20, X, z)
#   taux_kppv = taux_intervalle(20, errorKppv$err_app, errorKppv$err_tst)
#   print(dataset$title)
#   print(taux_kppv)
# }


# X <- Synth2_1000[, 1 : 2]
# z <- Synth2_1000[, 3]
# params_synth2_1000 <- estimation_params(X,z)
# errorCeuc <- erreur_ceuc(20,X,z)
# taux_ceuc <- taux_intervalle(20,errorCeuc$err_app, errorCeuc$err_tst)
# errorKppv = erreur_kppv(20, X, z)
# taux_kppv = taux_intervalle(20, errorKppv$err_app, errorKppv$err_tst)

# Pima <- read.csv("TP3/dataset/donnees-tp3/Pima.csv")
# X <- Pima[, 1 : 7]
# z <- Pima[, 8]
# params_pima <- estimation_params(X,z)
# errorCeuc <- erreur_ceuc(20,X,z)
# taux_ceuc <- taux_intervalle(20,errorCeuc$err_app, errorCeuc$err_tst)
# errorKppv = erreur_kppv(20, X, z)
# taux_kppv = taux_intervalle(20, errorKppv$err_app, errorKppv$err_tst)

Breastcancer <- read.csv("TP3/dataset/donnees-tp3/Breastcancer.csv")
X <- Breastcancer[, 1 : 9]
z <- Breastcancer[, 10]
params_breastcancer <- estimation_params(X,z)
errorCeuc <- erreur_ceuc(20,X,z)
taux_ceuc <- taux_intervalle(20,errorCeuc$err_app, errorCeuc$err_tst)
errorKppv = erreur_kppv(20, X, z)
taux_kppv = taux_intervalle(20, errorKppv$err_app, errorKppv$err_tst)