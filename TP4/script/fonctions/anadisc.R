# Analyse discriminante 

source("TP4/script/fonctions/mvdnorm.r")
# Analyse quadratique soit mu_{k}, sigma_{k} différents

adq.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		X_k <- Xapp[zapp==k,]
		param$MCov[,,k] <- cov(X_k)
		param$mean[k,] <-  apply(X_k, MARGIN=2, mean)
		param$prop[k] <- length(zapp[zapp == k]) / length(zapp)
	}

	param
}

# Analyse linéaire , soit avec matrice égales
adl.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	MCov <- array(0, c(p,p))
	sum_MCov <- 0 # Store le produit des nk*Vk
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)
  
	for (k in 1:g)
	{
		nk <- length(which(zapp==k)) # Nombre d'individus de la classe k
		X_k <- Xapp[zapp==k,] # Individus de la classe k
		Vk <- cov(X_k) #Matrice de covariance de la classe k
		sum_MCov <- sum_MCov + (nk - 1) * Vk
		print(apply(X_k, MARGIN=2, mean))
		param$mean[k,] <- apply(X_k, MARGIN=2, mean)
		param$prop[k] <- length(zapp[zapp == k]) / length(zapp)
	}
	MCov <- 1/(n - g) * sum_MCov # Matrice commune dans toutes les classes
	for (k in 1:g)
	{
		param$MCov[,,k] <- MCov
	}

	param
}
# 
nba.app <- function(Xapp, zapp)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  print(p)
  g <- max(unique(zapp))
  
  param <- NULL
  param$MCov <- array(0, c(p,p,g)) # Matrice de covariance, une diagonale de Vk
  param$mean <- array(0, c(g,p))
  param$prop <- rep(0, g)
  
  for (k in 1:g)
  {
    nk <- length(which(zapp==k)) # Nombre d'individus de la classe k
    X_k <- Xapp[zapp==k,]
    Vk <- cov(X_k) #Matrice de covariance de la classe k
    print(Vk)
    param$MCov[,,k] <- diag(diag(Vk))
    param$mean[k,] <-  apply(X_k, MARGIN=2, mean)
    param$prop[k] <- length(zapp[zapp == k]) / length(zapp)
  }
  
  param
}
# Probabilités à posteriori
# page 88
ad.val <- function(param, Xtst)
{
	n <- dim(Xtst)[1]
	p <- dim(Xtst)[2]
	g <- length(param$prop)

	out <- NULL

	prob <- matrix(0, nrow=n, ncol=g)
	deno <- 0
	for (k in 1:g){
	  dens <- mvdnorm(Xtst, param$mean[k,], param$MCov[,,k])  # densité conditionnelle f1(x) => prob[1] ou f2(x) => prob[2]
		prob[,k] <- param$prop[k] * dens
		deno <- deno + prob[,k]
	}
	for (k in 1:g){
	  prob[,k] <- prob[,k] / deno
	}
	pred <- max.col(prob)

	out$prob <- prob
	out$pred <- pred

	out
}
