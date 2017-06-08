source("TP4/script/fonctions/anadisc.R")
source("TP4/script/fonctions/separ1.R")
source("TP4/script/fonctions/prob.ad.R")
source("TP4/script/fonctions/prob.log.R")
source("TP4/script/fonctions/prob.log2.R")
source("TP4/script/fonctions/log.R")

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
           logq = {
             Xappq <- log.quadra(Xapp)
             Xtstq <- log.quadra(Xtst)
             ad <- log.app(Xappq, zapp,1,1e-5)
           }
    )
    if ((method == "log") || (method == "logq")){
      res_zapp  <- log.val(ad$beta, Xapp)$pred
      res_ztst  <- log.val(ad$beta, Xtst)$pred
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