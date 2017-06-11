library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

library(tree)

source("TP4/script/fonctions/separ1.R")

Synth1_40 <- read.csv("TP4/dataset/donnees/Synth1-40.csv")
Synth1_40$title <- "synth1_40"
Synth1_1000 <- read.csv("TP4/dataset/donnees/Synth1-1000.csv")
Synth1_1000$title <- "synth1_1000"
Synth2_1000 <- read.csv("TP4/dataset/donnees/Synth2-1000.csv")
Synth2_1000$title <- "synth2_1000"
Synth2_1000 <- read.csv("TP4/dataset/donnees/Synth3-1000.csv")
Synth2_1000$title <- "synth3_1000"

X <- Synth1_1000[,1:2]
z <- Synth1_1000[,3]

data <- separ1(X,z)

df = cbind(data$zapp, data$Xapp)
names(df)[1] = "label" 
# print(df)

tree = rpart(label ~ ., data=df, method="class", control=rpart.control(minsplit=0.0001, cp=0, xval=20))
summary(tree)
# plot(fit)
fancyRpartPlot(tree)
# prp(fit, extra=1)

plotcp(fit)

# treeSimple = prune(tree, cp=0.0053)

treeOptimal = prune(tree,cp=tree$cptable[which.min(tree$cptable[,4]),1])
fancyRpartPlot(treeOptimal)

print("Prediction...")
predicted = predict(treeOptimal, data$Xtst, type="class")
print(predicted)
# fancyRpartPlot(predicted)

table(df$V1, predict(treeOptimal, data$Xtst, type="class"))

# tree = tree(label ~ ., data=df)
# plot(tree)

#Donn <- read.csv("TP4/dataset/donnees/spam.csv", header=T)
#X <- Donn[,1:57]
#z <- Donn[,58]

source("TP4/script/fonctions/erreur.R")
err_tree <- erreur(20, X, z, "tree")
taux_intervalle <- taux_intervalle(20, err_tree$err_app, err_tree$err_tst)
taux_intervalle

source("TP4/script/fonctions/erreur.R")
err_tree <- erreur(20, X, z, "tree")
taux_intervalle <- taux_intervalle(20, err_tree$err_app, err_tree$err_tst)
taux_intervalle

source("TP4/script/fonctions/erreur.R")
err_tree <- erreur(20, X, z, "rforest")
taux_intervalle <- taux_intervalle(20, err_tree$err_app, err_tree$err_tst)
taux_intervalle





dataset <- read.csv("TP4/dataset/donnees/spam.csv", header=T)
X <- dataset[,2:58]
z <- dataset[,59]
data <- separ1(X,z)
df = cbind(data$zapp, data$Xapp)
names(df)[1] = "label"
df$label = as.factor(df$label)

# print(df)
fit = randomForest(label ~ ., data=df, method="class", ntree=10000, mtry = 2, na.action = na.roughfix)
plot(fit$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")

set.seed(123)
library(caret)
mod <- train(label ~ ., data=df, method="rf")
print(mod)
# Random Forest 
# 
# 3068 samples
# 57 predictor
# 2 classes: '1', '2' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 3068, 3068, 3068, 3068, 3068, 3068, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.9401865  0.8731960
# 29    0.9396693  0.8728184
# 57    0.9324187  0.8576674
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2.

# > print(mod$finalModel)
# 
# Call:
#   randomForest(x = x, y = y, mtry = param$mtry) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 6.16%
# Confusion matrix:
#   1    2 class.error
# 1 1077  132  0.10918114
# 2   57 1802  0.03066165




