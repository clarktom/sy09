library(ggdendro)
library(dplyr)
library(ggplot2)
library(MASS)
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
  clusters <- hclust(mut)
  clusterCut <- cutree(clusters, 3)
  clusterCut2 <- rect.hclust(clusters,5)
  plot(clusters)
  #identify(clusters)
  clusterCut2 
  #plot(clusterCut2)
}

mds_aftd <- function(){
  hc <- hclust(mut, method="ward.D2")
  cut <- as.data.frame(cutree(hc, k=7))
  names(cut) <- "cut"
  cut$names <- rownames(cut)
  
  hcdata <- dendro_data(hc, type="triangle")
  hcdata$labels <- left_join(hcdata$labels, cut, by=c("label"="names"))
  
  ggplot(hcdata$segments) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
    geom_text(data = hcdata$labels, aes(x, y, label = label, colour=factor(cut)), 
              hjust = 1, size = 2.9) + scale_colour_discrete(name = "clusters") +
    labs(x="", y="") +  coord_flip() + ylim(-1, 100) + xlim(0,20)  + theme_bw()
  
  mds.cmdscale <- as.data.frame(cmdscale(as.matrix()))
  mds.cmdscale$names <- rownames(mds.cmdscale)
  mds.cmdscale$cut <- cut$cut
  ggplot(mds.cmdscale, aes(V1, V2, label=names)) + 
    geom_point(aes(colour=factor(cut)), size=2.3) +
    geom_text(aes(colour=factor(cut)), check_overlap = TRUE, size=2.2, 
              hjust = "center", vjust = "bottom", nudge_x = 0, nudge_y = 0.005) + 
    scale_colour_discrete(name = "clusters") +
    labs(x="", y="", title="MDS by Jaccard and cmdscale()") + theme_bw()
}

