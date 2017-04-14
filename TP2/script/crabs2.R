library(ggfortify)
crabs2 <- read.csv("TP2/dataset/crabs2.csv", header=T)
summary(crabs2)
crabs2quant <- crabs2[1:4]
pca_crabs <- prcomp(crabs2quant)
autoplot(pca_crabs, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
