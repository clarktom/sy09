library(ggdendro)
library(dplyr)
library(ggplot2)
library(MASS)
mut <- read.csv("TP2/dataset/mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut, diag=T, upper=T)
library(broom)
library(cluster)

iner_kmeans <- function(){
  iner_mut_k3 <- matrix(0, nrow=300, 1)
  for(k in 1:1)
  {
    for (i in 1:300){
      iner_mut_k3[i,k] <-kmeans(mut2$points,3)$tot.withinss
    }
  }
  # mut K=1
  min(iner_mut_k3[,1])
  median(iner_mut_k3[,1])
  iner_mut_k3$c1 = iner_mut_k3[,1]
  ggplot(as.data.frame(iner_mut_k3), aes(x=factor(0),c1)) + geom_boxplot() + xlab("X") + ylab("Inertie interclasse")
}

plot_kmeans <- function(){
  mut2<-cmdscale(mut,k=5, eig=TRUE)
  k=kmeans(mut2$points,3)
  plot(mut2$points[,1], mut2$points[,2], col=k$cluster, xlab="Comp1", ylab="Comp2", main = "Représentation des individus dans le premier plan factoriel")
  mut2$pc1 <- mut2$points[,1]
  mut2$pc2 <- mut2$points[,2]
  mut2$grp <- k$cluster
  ggplot(as.data.frame(mut2$points)) + geom_point(aes(x = mut2$pc1, y = mut2$pc2), color=k$cluster)
  
}

qualite <- function(eig_val, k){  
  sum(eig_val[1:k])/ sum(eig_val) *100 
}

plot_aftd <- function(nombre_pre){
  AFTD <- cmdscale(mut, eig=TRUE, nombre_pre) 
  plot(Shepard(mut, AFTD$points), xlab="Proximités", ylab="Distances", main=paste("Shepard à ",toString(nombre_pre), "dimensions"))
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
  # A number of different clustering methods are provided. 
  # Ward's minimum variance method aims at finding compact, spherical clusters. 
  # The complete linkage method finds similar clusters. 
  # The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 
  # The other methods can be regarded as aiming for clusters with characteristics somewhere between the single and complete link methods.
  # Note however, that methods "median" and "centroid" are not leading to a monotone distance measure,
  # or equivalently the resulting dendrograms can have so called inversions or reversals which are hard to interpret, 
  # but note the trichotomies in Legendre and Legendre (2012). 
  dendo<-hclust(mut,method="ward.D2") #classification ascendante
  dendo2<-hclust(mut,method="single") 
  dendo3<-hclust(mut,method="complete") 
  dendo4<-hclust(mut,method="average") 
  dendo5<-hclust(mut,method="mcquitty") 
  dendo6<-hclust(mut,method="median") 
  dendo7<-hclust(mut,method="centroid") 
  p1 <- ggdendrogram(dendo, rotate = FALSE, size = 2) + ggtitle("method : ward.D2")
  p2 <- ggdendrogram(dendo2, rotate = FALSE, size = 2) + ggtitle("method : single")
  p3 <- ggdendrogram(dendo3, rotate = FALSE, size = 2) + ggtitle("method : complete")
  p4 <- ggdendrogram(dendo4, rotate = FALSE, size = 2) + ggtitle("method : average")
  p5 <- ggdendrogram(dendo5, rotate = FALSE, size = 2) + ggtitle("method : mcquitty")
  p6 <- ggdendrogram(dendo6, rotate = FALSE, size = 2) + ggtitle("method : median")
  p7 <- ggdendrogram(dendo7, rotate = FALSE, size = 2) + ggtitle("method : centroid")
  #multiplot(p1, p2, p3, p4, p5, p6, p7, cols = 2)
  p7
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

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


