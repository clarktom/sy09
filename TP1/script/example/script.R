notes <- read.csv("C:/Users/tompu/Downloads/donnees/sy02-p2016.csv", na.strings="", header=T)

boxplot(note.totale~specialite, data=notes)

newMedian = cut(notes$note.median, c(0,5,10,15,20))
newFinal = cut(notes$note.final, c(0,5,10,15,20))
chisq.test(newMedian, newFinal)
# Donc on peut affirmer sans risque que les variables sont dépendantes

boxplot(notes$note.totale~notes$correcteur.median)
boxplot(notes$note.totale~notes$correcteur.final)

scatter.smooth(notes$note.median~notes$note.final)

hist(notes$note.totale)
shapiro.test(notes$note.totale)

# Ex2
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
corr.acp.values = corr.acp[,2:5]

# Centre de gravité
g = c(mean(corr.acp.values[,1]), mean(corr.acp.values[,2]), mean(corr.acp.values[,3]), mean(corr.acp.values[,4]))

inertie_totale = 0
for(n in 1:dim(corr.acp.values)[1]) {
  for(p in 1:dim(corr.acp.values)[2]) {
    inertie_totale = inertie_totale + (corr.acp.values[n,p] - g[p])^2
  }
}
inertie_totale = inertie_totale/n
inertie_totale

# Centré
corr.acp.values.c = scale(corr.acp.values, scale=FALSE)
corr.acp.values.c

# Covariance
corr.acp.cov = cov(corr.acp.values.c)
corr.acp.cov

# Valeurs et vecteurs propres
u = eigen(corr.acp.cov)
u

# Nouvelles données transformées par ACP
corr.acp.new = u$values * corr.acp.values
corr.acp.new

# plot(corr.acp.new[,2]~corr.acp.new[,1])
# plot(corr.acp.values[,2]~corr.acp.values[,1])
plot(corr.acp.new[,1:2], pch=16, col="red")
par(new=TRUE)
plot(corr.acp.values[,1:2], pch=16, col="green")



# La matrice de corrélation = matrice de covariances des données centrées, réduites.
# Centrage = soustraction moyenne des valeurs
# Reduction = 

corr.acp.values = read.csv("exemple.csv", sep=";")
corr.acp.values = corr.acp.values[,2:6]

#Les données
corr.acp.values

#Centrage des données
corr.acp.values.c = scale(corr.acp.values, scale=FALSE)
corr.acp.values.c = round(corr.acp.values.c, digits=2)
corr.acp.values.c

#Matrice de variance
matriceVar = (1.0/9) * (t(corr.acp.values.c) %*% corr.acp.values.c)
matriceVar

#Axes principaux d'inertie
# Valeurs et Vecteurs propres
res = eigen(matriceVar)
valuesP = res$values
vectorsP = res$vectors
valuesP
vectorsP

#plotInertia()


#Composantes principales
acp = corr.acp.values * valuesP
acp




