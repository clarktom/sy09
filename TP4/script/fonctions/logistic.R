library(MASS)

log.app <- function(Xapp, zapp, intr, epsi)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]

	Xapp <- as.matrix(Xapp)

	if (intr == T)
	{
		Xapp <- cbind(rep(1,n),Xapp)
		p <- p + 1
	}

	targ <- as.numeric(zapp==1)
	tXap <- t(Xapp)

	beta <- matrix(0,nrow=p,ncol=1)

	conv <- F
	iter <- 0
	while (conv == F)
	{
		iter <- iter + 1
		bold <- beta

		prob <- postprob(beta, Xapp)
		MatW <- diag(prob*(1-prob)) #
    MatH <- -tXap %*% MatW %*% Xapp
    print(targ)
    print(prob)
    print(zapp)
		beta <- bold - ginv(MatH) %*% (tXap %*% (targ - prob))

		if (norm(beta-bold)<epsi)
		{
			conv <- T
		}
	}

	prob <- postprob(beta, Xapp)
	out <- NULL
	out$beta <- beta
	out$iter <- iter
	out$logL <- sum(targ * log(prob) + (1-targ) * log(1-prob))

	out
}

log.val <- function(beta, Xtst)
{
	m <- dim(Xtst)[1]
	p <- dim(beta)[1]
	pX <- dim(Xtst)[2]

	Xtst <- as.matrix(Xtst)

	if (pX == (p-1))
	{
		Xtst  <- cbind(rep(1,m),Xtst)
	}

	prob <- postprob(beta, Xtst)
	pred <- max.col(prob)

	out <- NULL
	out$prob <- prob
	out$pred <- pred

	return(out)
}

postprob <- function(beta, X)
{
	X <- as.matrix(X)
	n <- dim(X)[1]
	beta <- as.matrix(beta)
	MatB <- matrix(rep(t(beta),n),nrow=n,byrow=T)
  
	tmp <- exp(rowSums(MatB * X))
	prob <- tmp / (1 + tmp)
}
