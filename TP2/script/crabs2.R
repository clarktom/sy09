library(ggfortify)
crabs <- read.csv("TP2/dataset/crabs2.csv", header=T)
summary(crabs)
crabsquant <- crabs2[1:4]
crabs$grp <- rep(1:4, each = 50)
visualisation_crabs <- function(){
  pca_crabs <- prcomp(crabsquant)
  autoplot(pca_crabs, data=crabs,loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.size = 3)
}