# Analyse discriminante 
Synth1_1000 <- read.csv("TP4/dataset/donnees/Synth1-1000.csv")
Synth1_1000$title <- "synth1_1000"
Synth2_1000 <- read.csv("TP4/dataset/donnees/Synth2-1000.csv")
Synth2_1000$title <- "synth2_1000"
Synth2_1000 <- read.csv("TP4/dataset/donnees/Synth3-1000.csv")
Synth2_1000$title <- "synth3_1000"
source("TP4/script/mvdnorm.r")
# Analyse quadratique soit mu_{k}, sigma_{k} différents

adq.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	print(p)
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		X_k <- Xapp[z==k,]
		print(X_k)
		print(cov(X_k))
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
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		X_k <- Xapp[z==k,]
		MCov <- cov(X_k)
		param$mean[k,] <- apply(X_k, MARGIN=2, mean)
		param$prop[k] <- length(zapp[zapp == k]) / length(zapp)
	}
	MCov <- cov(X_k)
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
  param$MCov <- array(0, c(p,p,g))
  param$mean <- array(0, c(g,p))
  param$prop <- rep(0, g)
  
  for (k in 1:g)
  {
    indk <- which(zapp==k)
    X_k <- Xapp[z==k,]
    print(X_k)
    print(cov(X_k))
    param$MCov[,,k] <- cov(X_k)
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
  # calculer densités conditionnelles
	for (k in 1:g)
	{
		prob[,k] <-  # p88 - potentiellement faux mais potentiellement vrai
	}
	# prob  = résoudre système d'équations avec prob[1]/prob[2]=cste et prob[1] + prob[2] = 1
	prob <- 
	pred <- max.col(prob)

	out$prob <- prob
	out$pred <- pred

	out
}
