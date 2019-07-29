nBayes <- function() {
#Importing the Data

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
dataset[c(1,4,5)] = scale(dataset[c(1,4,5)])

#install.packages('e1071')
# naive bayes

library(e1071)

classifier = naiveBayes( x=training_set[-10], 
                         y=training_set$num )


y_pred = predict(classifier , newdata = test_set[-10])

cm = table(test_set[, 10], y_pred)

print(cm)

correctprediction = sum(diag(cm))

total_test_set_rows = nrow(test_set)


accuracy = correctprediction / total_test_set_rows 

return (accuracy)

}


