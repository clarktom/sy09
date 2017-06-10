library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

tree.app <- function(Xapp, zapp) {
  df = cbind(zapp, Xapp)
  names(df)[1] = "label"
  df$label = as.factor(df$label)
  tree = rpart(label ~ ., data=df, method="class", control=rpart.control(minsplit=0.0001, cp=0, xval=20))
  treeOptimal = prune(tree, cp=tree$cptable[which.min(tree$cptable[,4]),1])
  treeOptimal
}

tree.val <- function(ad, Xtst) {
  predicted = predict(ad, Xtst, type="class")
  predicted
}

rforest.app <- function(Xapp, zapp) {
  df = cbind(zapp, Xapp)
  names(df)[1] = "label"
  df$label = as.factor(df$label)
  rforest = randomForest(label ~ ., data=df)
  rforest
}

rforest.val <- function(ad, Xtst) {
  predicted = predict(ad, Xtst, type="response")
  print(predicted)
  predicted
}

