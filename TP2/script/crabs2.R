library(ggfortify)
library(RColorBrewer)
crabs <- read.csv("TP2/dataset/crabs2.csv", header=T)
summary(crabs)
crabsquant <- crabs[1:4]
crabs$grp <- rep(c("Blue Male", "Blue Female", "Orange Male", "Orange Female"), each = 50)

# Change colors 
myColors <- brewer.pal(4,"Set1")
names(myColors) <- levels(crabs$grp)
colScale <- scale_colour_manual(name = "grp",values = myColors)
visualisation_crabs <- function(){
  pca_crabs <- prcomp(crabsquant)
  # plot without colors
  p1 <- ggplot(pca_crabs,aes(PC1,PC2)) + geom_point()
  # plot with colors
  p2 <- ggplot(pca_crabs,aes(PC1,PC2,colour = crabs$grp)) + geom_point() + colScale
  p2
}