library(grid)
library(gridExtra)
library(reshape2)
library(ggplot2)
library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
library(ggfortify)

notes <- read.csv("sy09/script/dataset/sy02-p2016.csv")
moy.median <- aggregate(note.median~correcteur.median, data=notes, FUN=mean)
names(moy.median) <- c("correcteur","moy.median")
std.median <- aggregate(note.median~correcteur.median, data=notes, FUN=sd)
names(std.median) <- c("correcteur","std.median")
median <- merge(moy.median, std.median)
moy.final <- aggregate(note.final~correcteur.final, data=notes, FUN=mean)
names(moy.final) <- c("correcteur","moy.final")
std.final <- aggregate(note.final~correcteur.final, data=notes, FUN=sd)
names(std.final) <- c("correcteur","std.final")
final <- merge(moy.final, std.final)
correcteurs <- merge(median, final, all=T)
corr.acp <- correcteurs[-c(2,8),]
#  Centrage et réduction éventuelle du tableau
Y <- as.matrix(corr.acp[2:5])
n <- dim(Y)[1]
X <- Y-matrix(1,n,1) %*% apply(Y,2,mean)
# X <- X/matrix(1,n,1) %*% apply(X,2,sd)
# Calcul de la matrice de covariance ou de corrélation
V <- (1/n)*t(X) %*% X
# Calcul des valeurs propres et des axes d’inertie
tmp <- eigen(V, symmetric=TRUE)
L <- diag(tmp$values)
U <- tmp$vectors
# Calcul des composantes principales des individus
C <- X %*% U
# Calcul des contributions
COR <- diag(1/apply(X^2,1,sum)) %*% C^2
CTR <- (1/n)*C^2 %*% diag(1/diag(L))
# Représentation des variables
D <- diag(1/(sqrt((n-1)/n)*apply(X, 2, sd))) %*% U %*% sqrt(L)
plot_1_2 <- function(){
  pca_a <- princomp(corr.acp[2:5])
  autoplot(pca_a, loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.size = 3)
}
plot_1_3 <- function(){
    plot(-1.5:1.5,-1.5:1.5,type="n",xlab="Axe 1",ylab="Axe 3")
    text(D[,1],D[,3],colnames(corr.acp));abline(h=0);abline(v=0)
    curve(sqrt(1-x^2),-1,1,add=TRUE)
    curve(-sqrt(1-x^2),-1,1,add=TRUE)
}
plot_corr <- function(){
    plot(C[,1],C[,2],type="n");text(C[,1],C[,2],rownames(corr.acp))
    abline(h=0);abline(v=0)
    # plot(C[,1],C[,3],type="n");text(C[,1],C[,3],rownames(corr.acp))
    # abline(h=0);abline(v=0)
}
sum_derivated <- function(){
    sumi <- C[,1] %*% t(U[,1]) + C[,2] %*% t(U[,2]) + C[,3] %*% t(U[,3]) + C[,4] %*% t(U[,4])
    # We find the difference between the value and the average
    # Equal to X because C is X %*% U and we "cut" the U
    xtable(sumi)
}
princomp_notes <- function(){
    pca <- prcomp(corr.acp[2:5])
    # Standard derivation
    pca$sdev
    # Loadings :
    pca$rotation
    # Loadings = eigen vectors * eigen value
    # loadings are the covariances/correlations between the original variables and the unit-scaled components.
    summary(pca)
    # Porportion of variance : show which PC is the best to describe the data
    biplot(pca)
    # Plot the data in the new PC plan
    plot(pca)
    # Variance
}
pca_notes_ <- function()
{
    pca_notes <- prcomp(corr.acp[2:5])
    g <- ggbiplot(pca_notes, obs.scale = 1, var.scale = 1)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal',
                legend.position = 'top')
    print(g)
}
pca_notes_corrected <- function(){
    # Recreate the matrix using mean values from the caracteristics
    correcteurs[8,2] <- mean(corr.acp$moy.median)
    correcteurs[8,3] <- mean(corr.acp$std.median)
    correcteurs[2,4] <- mean(corr.acp$moy.final)
    correcteurs[2,5] <- mean(corr.acp$std.final)
    pca_notes <- prcomp(correcteurs[2:5])
    g <- ggbiplot(pca_notes, obs.scale = 1, var.scale = 1, choice=c(1,2))
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal',
                legend.position = 'top')
    print(g)
}
