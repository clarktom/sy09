### IRIS ###
library(ggplot2)
library(ggfortify)
library(dendextend)
library(colorspace)
library(cluster)
data(iris)
irisquant <- iris[1:4]
#summary(iris)
visualisation <- function(){
  pca_iris <- prcomp(irisquant)
  autoplot(pca_iris, loadings = TRUE, data=iris, colour='Species', loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.size = 3)
}

hclust_iris_dist <- function(){
  d_iris <- dist(irisquant)
  hc_iris <- hclust(d_iris, method = "complete")
  iris_species <- rev(c("virginica", "versicolor", "setosa"))
  
  dend <- as.dendrogram(hc_iris)
  # order it the closest we can to the order of the observations:
  dend <- rotate(dend, 1:150)
  
  # Color the branches based on the clusters:
  dend <- color_branches(dend, k=3, groupLabels = iris_species) #, groupLabels=iris_species)
  
  # Manually match the labels, as much as possible, to the real classification of the flowers:
   labels_colors(dend) <-
    rainbow_hcl(3)[sort_levels_values(
      as.numeric(iris[,5])[order.dendrogram(dend)]
    )]
  
  # We shall add the flower type to the labels:
   # labels(dend) <- paste(as.character(iris[,5])[order.dendrogram(dend)],
   #                       "(",labels(dend),")", 
   #                       sep = "")
  # We hang the dendrogram a bit:
  dend <- hang.dendrogram(dend,hang_height=0.1)
  # reduce the size of the labels:
   dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
  dend <- set(dend, "labels_cex", 0.5)
  # And plot:
  par(mar = c(3,3,3,7))
  plot(dend, 
       main = "Ascending Clustered Iris data set", 
       horiz =  TRUE,  nodePar = list(cex = .006))
  legend("topleft", legend = iris_species, fill = rainbow_hcl(3))
}

hclust_iris_diana <- function(){
  hc_iris <- diana(irisquant) 
  iris_species <- rev(c("virginica", "versicolor", "setosa"))
  
  dend <- as.dendrogram(hc_iris)
  # order it the closest we can to the order of the observations:
  dend <- rotate(dend, 1:150)
  
  # Color the branches based on the clusters:
  dend <- color_branches(dend, k=3, groupLabels = iris_species) #, groupLabels=iris_species)
  
  # Manually match the labels, as much as possible, to the real classification of the flowers:
   labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(iris[,5])[order.dendrogram(dend)]
  )]
  
  # We shall add the flower type to the labels:
   # labels(dend) <- paste(as.character(iris[,5])[order.dendrogram(dend)],
   #                       "(",labels(dend),")", 
   #                       sep = "")
  # We hang the dendrogram a bit:
  dend <- hang.dendrogram(dend,hang_height=0.1)
  # reduce the size of the labels:
  dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
  dend <- set(dend, "labels_cex", 0.5)
  # And plot:
  par(mar = c(3,3,3,7))
  plot(dend, 
       main = "Descending Clustered Iris data set", 
       horiz =  TRUE,  nodePar = list(cex = .007))
  legend("topleft", legend = iris_species, fill = rainbow_hcl(3))
}
