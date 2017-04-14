mut <- read.csv("TP2/dataset/mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut, diag=T, upper=T)

qualite <- function(eig_val, k){  
  sum(eig_val[1:k])/ sum(eig_val) *100 
}

plot_aftd <- function(nombre_pre){
  AFTD <- cmdscale(mut, eig=TRUE, nombre_pre) 
  plot(Shepard(mut, AFTD$points), xlab="Proximités", ylab="Distances", main=paste("Diagramme de Sphepard des données crabs2 k =",toString(nombre_pre)))
  abline(a=0,b=1,col="blue") 
  qualite(AFTD$eig,nombre_pre) 
}

plot_aftd_ACP <- function(){
  # Représentation de l'AFTD dans le premier plan factoriel
  AFTD <- cmdscale(mut, eig=TRUE, 2)
  plot(AFTD$points[,1], AFTD$points[,2], xlab="Comp1", ylab="Comp2", main = "Représentation des individus dans le premier plan factoriel") 
  text(x = AFTD$points[,1], y = AFTD$points[,2], labels=labels(mut)) 
}

plot_hclust <- function(){
  plot(hclust(mut))
}
