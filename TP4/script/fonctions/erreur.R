source("TP4/script/fonctions/anadisc.R")
source("TP4/script/fonctions/separ1.R")
source("TP4/script/fonctions/prob.ad.R")
source("TP4/script/fonctions/prob.log.R")
source("TP4/script/fonctions/prob.log2.R")
source("TP4/script/fonctions/log.R")
#source("TP4/script/fonctions/tree.R")

erreur <- function(N,X,z,method){
  err_app <- c()
  err_tst <- c()
  res_zapp <- NULL
  res_ztst <- NULL
  ad <- NULL
  for(i in 1:N){
    splitted <- separ1(X,z)
    Xapp <- splitted$Xapp
    zapp <- splitted$zapp
    Xtst <- splitted$Xtst
    ztst <- splitted$ztst
    switch(method, 
           adl =  ad <- adl.app(Xapp,zapp),
           adq = ad <- adq.app(Xapp,zapp),
           nba = ad <- nba.app(Xapp,zapp),
           log = ad <- log.app(Xapp,zapp,1,1e-5),
           tree = ad <- tree.app(Xapp,zapp),
           rforest = ad <- rforest.app(Xapp,zapp),
           logq = {
             Xappq <- log.quadra(Xapp)
             Xtstq <- log.quadra(Xtst)
             ad <- log.app(Xappq, zapp,1,1e-5)
           }
    )
    if ((method == "log") || (method == "logq")){
      res_zapp <- log.val(ad$beta, Xapp)$pred
      res_ztst <- log.val(ad$beta, Xtst)$pred
    }
    else if (method == "tree"){
      res_zapp <- tree.val(ad, Xapp)
      res_ztst <- tree.val(ad, Xtst)
    }
    else if (method == "rforest"){
      res_zapp <- rforest.val(ad, Xapp)
      res_ztst <- rforest.val(ad, Xtst)
    }
    else{
      res_zapp <- ad.val(ad, Xapp)$pred
      res_ztst <- ad.val(ad, Xtst)$pred
    }
    err_app[i] <- 1 - (sum(zapp == res_zapp) / length(zapp))
    err_tst[i] <- 1 - (sum(ztst == res_ztst) / length(ztst))
  }
  res <- NULL
  res$err_app <- err_app
  res$err_tst <- err_tst
  res
}

taux_intervalle <- function(N, err_app, err_tst) {
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
