donn <- read.csv("TP3/dataset/donnees-tp3/Synth1-40.csv")
Synth1_40 <- read.csv("TP3/dataset/donnees-tp3/Synth1-40.csv")
Synth1_100 <- read.csv("TP3/dataset/donnees-tp3/Synth1-100.csv")
Synth1_1000 <- read.csv("TP3/dataset/donnees-tp3/Synth1-1000.csv")
Synth2_1000 <- read.csv("TP3/dataset/donnees-tp3/Synth2-1000.csv")
X <- as.matrix(Synth1_40[, 1 : 2])
z <- Synth1_40[, 3]
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
    ztst <- kppv.val(Xapp, zapp, i, Xval) #On cherche le vecteur des étiquettes
    res[i] <- mean(abs(zval-ztst)) #On soustrait les deux vecteurs et on fait la moyenne de la val abs
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
  i <- 1
  for (k in classes){#pour chaque classes
    Xk <- X[z==k,]
    mu_k <- apply(X_k, MARGIN=2, mean)
    mu[i,] <- mu_k
    sigma[[i]] <- cov(Xk)
    piK <- length(z[z == k]) / length(z)
    pi <- c(pi, piK)
  }
  params$mu <- mu
  params$sigma <- sigma
  params$pi <- pi
  params
}

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
    err_app[i] <- 1 - sum(zapp == res_zapp) / length(zapp)
    err_tst[i] <- 1 - sum(ztst == res_ztst) / length(zapp)
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
  res$app_inter_gauche <- app_intervalle_gauche
  res$app_inter_droite <- app_intervalle_droite
  
  epsilon_tst <- mean(err_tst)
  res$taux_tst <- epsilon_tst
  variance = sum((err_tst - epsilon_tst)^2)/(N - 1)
  ecart_type <- sqrt(variance) # sd
  
  tst_intervalle_gauche <- err_tst - 1.96 * ecart_type / sqrt(N)
  tst_intervalle_droite <- err_tst + 1.96 * ecart_type / sqrt(N)
  
  res$tst_inter_gauche <- tst_intervalle_gauche
  res$tst_inter_droite <- tst_intervalle_droite
  res
}
