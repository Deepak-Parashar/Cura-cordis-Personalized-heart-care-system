dataset = read.csv("data.csv")

# Preprocessing the dataset

dataset$trestbps = as.numeric(dataset$trestbps)
dataset$trestbps[dataset$trestbps == 1] = NA
na_index_trestbps = which(is.na(dataset$trestbps))
dataset$trestbps[na_index_trestbps] = mean(dataset$trestbps ,na.rm = TRUE)

dataset$cholestrol = as.numeric(dataset$cholestrol)
dataset$cholestrol[dataset$cholestrol == 1] = NA
dataset$cholestrol[dataset$cholestrol == 0] = NA
na_index_cholestrol = which(is.na(dataset$cholestrol))
dataset$cholestrol[na_index_cholestrol] = mean(dataset$cholestrol ,na.rm = TRUE)

dataset$thalach = as.numeric(dataset$thalach)
dataset$thalach[dataset$thalach == 1] = NA
na_index_thalach = which(is.na(dataset$thalach))
dataset$thalach[na_index_trestbps] = mean(dataset$thalach ,na.rm = TRUE)

dataset$fasting_blood_sugar[dataset$fasting_blood_sugar == '?'] = 0
dataset$fasting_blood_sugar[dataset$fasting_blood_sugar == '0.0'] = 0
dataset$fasting_blood_sugar[dataset$fasting_blood_sugar == '1.0'] = 1
dataset$fasting_blood_sugar = factor(dataset$fasting_blood_sugar , levels = c(0,1))

dataset$restecg[dataset$restecg == '?'] = 0
dataset$restecg[dataset$restecg == '0.0'] = 0
dataset$restecg[dataset$restecg == '1.0'] = 1
dataset$restecg[dataset$restecg == '2.0'] = 2
dataset$restecg = factor(dataset$restecg , levels = c(0,1,2))

dataset$exang[dataset$exang == '?'] = 0
dataset$exang[dataset$exang == '0.0'] = 0
dataset$exang[dataset$exang == '1.0'] = 1
dataset$exang = factor(dataset$exang , levels = c(0,1))


dataset$Sex = factor(dataset$Sex , levels = c(0,1))
dataset$chest_pain_type = factor(dataset$chest_pain_type , levels = c(1,2,3,4))

dataset = dataset[c(1,2,3,4,5,6,7,8,9,14)]

dataset$num = factor(dataset$num , levels = c(0,1,2,3,4))


library(caTools)

set.seed(123)

split = sample.split(dataset$num , SplitRatio = 0.75)

training_set = subset(dataset , split == TRUE)
test_set = subset(dataset , split == FALSE)

# feature_scaling

training_set[c(1,4,5)] = scale(training_set[c(1,4,5)])
test_set[c(1,4,5)] = scale(test_set[c(1,4,5)])

a = length(levels(dataset$Sex))
b = length(levels(dataset$num))

l = 0


Observed = c()


for(i in levels(training_set$num)){
  for(j in levels(dataset$Sex)){
    l = l + 1
    Observed[l] = nrow(subset(training_set ,  Sex== j & num == i))
  }
}

mat = matrix(Observed, nrow = 5, byrow = T)

df = (a - 1) * (b - 1)

sumCol = c()
sumRow = c()


for( v in 1:b){
  sumRow[v] = sum(mat[v,])
}

for( u in 1:a){
  sumCol[u] = sum(mat[,u])
}


d = nrow(training_set)

Expected = c()

i1 = 0


for(i in 1:b){
  for(j in 1:a){
    i1 = i1 + 1
    Expected[i1] = (sumRow[i] *  sumCol[j])/d
  }
}

mat1 = matrix(Expected, nrow = 5, byrow = T)

v1 = Observed - Expected

v2 = v1 * v1

v3 = v2/Expected

chiSquare_Sex = sum(v3)

#####################################################################

a = length(levels(dataset$chest_pain_type))
b = length(levels(dataset$num))

l = 0


Observed = c()


for(i in levels(training_set$num)){
  for(j in levels(dataset$chest_pain_type)){
    l = l + 1
    Observed[l] = nrow(subset(training_set ,  chest_pain_type== j & num == i))
  }
}

mat = matrix(Observed, nrow = 5, byrow = T)

df = (a - 1) * (b - 1)

sumCol = c()
sumRow = c()


for( v in 1:b){
  sumRow[v] = sum(mat[v,])
}

for( u in 1:a){
  sumCol[u] = sum(mat[,u])
}


d = nrow(training_set)

Expected = c()

i1 = 0


for(i in 1:b){
  for(j in 1:a){
    i1 = i1 + 1
    Expected[i1] = (sumRow[i] *  sumCol[j])/d
  }
}

mat1 = matrix(Expected, nrow = 5, byrow = T)

v1 = Observed - Expected

v2 = v1 * v1

v3 = v2/Expected

chiSquare_chest_pain_type = sum(v3)






#################################################################

a = length(levels(dataset$fasting_blood_sugar))
b = length(levels(dataset$num))

l = 0


Observed = c()


for(i in levels(training_set$num)){
  for(j in levels(dataset$fasting_blood_sugar)){
    l = l + 1
    Observed[l] = nrow(subset(training_set , fasting_blood_sugar  == j & num == i))
  }
}

mat = matrix(Observed, nrow = 5, byrow = T)

df = (a - 1) * (b - 1)

sumCol = c()
sumRow = c()


for( v in 1:b){
  sumRow[v] = sum(mat[v,])
}

for( u in 1:a){
  sumCol[u] = sum(mat[,u])
}


d = nrow(training_set)

Expected = c()

i1 = 0


for(i in 1:b){
  for(j in 1:a){
    i1 = i1 + 1
    Expected[i1] = (sumRow[i] *  sumCol[j])/d
  }
}

mat1 = matrix(Expected, nrow = 5, byrow = T)

v1 = Observed - Expected

v2 = v1 * v1

v3 = v2/Expected

chiSquare_Fbs = sum(v3)


###################################################################

a = length(levels(dataset$restecg))
b = length(levels(dataset$num))

l = 0


Observed = c()


for(i in levels(training_set$num)){
  for(j in levels(dataset$restecg)){
    l = l + 1
    Observed[l] = nrow(subset(training_set , restecg  == j & num == i))
  }
}

mat = matrix(Observed, nrow = 5, byrow = T)

df = (a - 1) * (b - 1)

sumCol = c()
sumRow = c()


for( v in 1:b){
  sumRow[v] = sum(mat[v,])
}

for( u in 1:a){
  sumCol[u] = sum(mat[,u])
}


d = nrow(training_set)

Expected = c()

i1 = 0


for(i in 1:b){
  for(j in 1:a){
    i1 = i1 + 1
    Expected[i1] = (sumRow[i] *  sumCol[j])/d
  }
}

mat1 = matrix(Expected, nrow = 5, byrow = T)

v1 = Observed - Expected

v2 = v1 * v1

v3 = v2/Expected

chiSquare_restecg = sum(v3)




##############################################################




a = length(levels(dataset$exang))
b = length(levels(dataset$num))

l = 0


Observed = c()


for(i in levels(training_set$num)){
  for(j in levels(dataset$exang)){
    l = l + 1
    Observed[l] = nrow(subset(training_set , exang  == j & num == i))
  }
}

mat = matrix(Observed, nrow = 5, byrow = T)

df = (a - 1) * (b - 1)

sumCol = c()
sumRow = c()


for( v in 1:b){
  sumRow[v] = sum(mat[v,])
}

for( u in 1:a){
  sumCol[u] = sum(mat[,u])
}


d = nrow(training_set)

Expected = c()

i1 = 0


for(i in 1:b){
  for(j in 1:a){
    i1 = i1 + 1
    Expected[i1] = (sumRow[i] *  sumCol[j])/d
  }
}

chiSquare

barplot(accuracy , names.arg = n , ylim = c(0,100), xlab = "Model Name" , ylab = "Accuracy(In %)" , col = "green" , border = "red" , main = "Bar graph of classification Models")


mat1 = matrix(Expected, nrow = 5, byrow = T)

v1 = Observed - Expected

v2 = v1 * v1

v3 = v2/Expected

chiSquare_exang = sum(v3)


chiSquare <- c(chiSquare_chest_pain_type , chiSquare_Sex , chiSquare_Fbs , chiSquare_exang , chiSquare_restecg)

name <- c("chest_pain_type" , "Sex" , "Fbs" , "exang" , "restecg")

barplot(chiSquare , names.arg = name , xlab = "Feature Name" , ylab = "Chi Square Value" , col = "green" , border = "red" , main = "Bar graph Chi Square values of Features")
