library(MASS)
mvdnorm <- function(X, mu, Sigma, method)
{
  X <- as.matrix(X)
  
  n <- dim(X)[1]
  p <- dim(X)[2]
  if(method == "adq" || method =="nba"){
    for(i in 1:ncol(Sigma)){
      print(i)
      if((Sigma[[i]][1] == 0 && method == "adq")){
        n2 <- length(as.vector(Sigma[,i]))
        Sigma[,i] <- rep(1e-12, n2)
      }
      if(method == "nba" && (i == 4 || i == 32 || i == 41 || i == 31)){
        Sigma[,i][i] <- 1e-12
      }
    }
  }
  print(Sigma)
  B <- chol(Sigma)
  # print(matrix(rep(mu,n),nrow=n,byrow=T))
  U <- (X-matrix(rep(mu,n),nrow=n,byrow=T))%*%ginv(B)
  
  dens <- exp(-rowSums(U*U)/2) * (2*pi)^(-p/2) / det(B)
}