notes <- read.csv("dataset/sy02-p2016.csv")
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
    # Tracé des graphiques
    plot(-1:1,-1:1,type="n",xlab="Axe 1",ylab="Axe 2")
    text(D[,1],D[,2],colnames(corr.acp));abline(h=0);abline(v=0)
    curve(sqrt(1-x^2),-1,1,add=TRUE)
    curve(-sqrt(1-x^2),-1,1,add=TRUE)
}
plot_1_3 <- function(){
    plot(-1:1,-1:1,type="n",xlab="Axe 1",ylab="Axe 3")
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
    sumi
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
pca_notes <- function()
{
    pca_notes <- prcomp(corr.acp[2:5])
    g <- ggbiplot(pca_notes, obs.scale = 1, var.scale = 1)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal',
                legend.position = 'top')
    print(g)
}
