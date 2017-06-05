# Régression logistique
library(matlab)
library(MASS) # pour ginv
log.app <- function(Xapp,zapp,intr,epsi){
  # intr = valeur intercept 
  # epsi scalaire convergence 
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  ti <- as.numeric(zapp==1)
  prob_priori <- 0
  res <- NULL
  if(intr==1){
    # si intr == 1 on rajoute une colonne de 1 à Xapp
    Xapp <- cbind(ones(n,1), Xapp) 
    beta <- vector('numeric', length=p+1)
  }
  else{
    beta <- vector('numeric', length=p)
  }
  ecart <- epsi + 0.5 #ou plus? à définir
  nb_iter <- 0
  Xapp <- as.matrix(Xapp)
  while(ecart > epsi){ # Itération de newton-raphson
    # trouver pro à posteriori
    beta_matrix <- matrix(rep(t(beta),n),nrow=n,byrow=T)
    # print(beta_matrix)
    
    numerator <- exp(rowSums(beta_matrix*Xapp))
    prob_priori <- numerator/(1+numerator)
    # Matrice W 
    W <- diag(prob_priori * (1 - prob_priori))
    # Matrice Hessienne H
    H <- -t(Xapp)%*%W%*%Xapp
    # Calcul du beta en q+1, le dernier élément est le grad log_vraissemblance
    
    beta_qplus1 <- beta - ginv(H)%*%(t(Xapp)%*%(ti-prob_priori)) 
    ecart <- sum((beta-beta_qplus1)^2)
    beta <-beta_qplus1
    nb_iter <- nb_iter + 1
  }
  res$logL <- sum(ti*log(prob_priori) + (1 - ti)*log(1 - prob_priori))
  res$beta <- beta
  res$nb_iter <- nb_iter
  res
}

# log.app(data_synth$Xapp,data_synth$zapp,1,1e-5)

log.val <- function(beta, Xtst){
  beta <- t(beta)
  n <- nrow(Xtst)
  res <-  NULL
  if(ncol(Xtst)<ncol(beta)){
    Xtst <- cbind(ones(n,1), Xtst) 
  }
  
  res$prob_prio <- array(0, c(n,2))
  beta_matrix <- matrix(rep(t(beta),n),nrow=n,byrow=T)
  print(beta_matrix)
  print(Xtst)
  numerator <- exp(rowSums(beta_matrix*Xtst))
  prob_prio <- numerator/(1+numerator)
  res$prob_prio[,1] <- prob_prio
  res$prob_prio[,2] <- 1 - prob_prio
  res$found <- max.col(res$prob_prio)
  res
}

log.quadra <- function(Xapp){
  Xapp_quadra <- Xapp
  pow_quadra <- NULL
  for(dim in 1:(ncol(Xapp)-1)){
    for(dim_mult in (dim+1):ncol(Xapp)){
      Xapp_quadra <- cbind(Xapp_quadra, Xapp[,dim]*Xapp[,dim_mult])
    }
    pow_quadra <- cbind(pow_quadra, Xapp[,dim]*Xapp[,dim])
  }
  pow_quadra <- cbind(pow_quadra, Xapp[ncol(Xapp)]*Xapp[ncol(Xapp)])
  Xapp_quadra <- cbind(Xapp_quadra, pow_quadra)
  Xapp_quadra
}