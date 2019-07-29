source("dTree.R")
source("knn.R")
source("nBayes.R")
source("rForest.R")
source("svm.R")

dtree_accuracy = dTree() * 100
knn_accuracy = Knn() * 100
nBayes_accuracy = nBayes() * 100
rForest_accuracy = rForest() *100
svm_accuracy = svm1() *100

accuracy = c(dtree_accuracy , knn_accuracy , nBayes_accuracy , rForest_accuracy , svm_accuracy)

n = c("dtree" , "knn" , "nBayes" , "rForest" , "svm")

barplot(accuracy , names.arg = n , ylim = c(0,100), xlab = "Model Name" , ylab = "Accuracy(In %)" , col = "green" , border = "red" , main = "Bar graph of classification Models")
