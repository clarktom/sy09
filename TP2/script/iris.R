### IRIS ###
library(ggfortify)
data(iris)
irisquant <- iris[1:4]
#summary(iris)
visualisation <- function(){
  pca_iris <- prcomp(irisquant)
  autoplot(pca_iris, loadings = TRUE, data=iris, colour='Species', loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.size = 3)
}
