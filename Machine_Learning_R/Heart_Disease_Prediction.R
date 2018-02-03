#Read the heart file
Heart <- read.csv("D:\\Heart_data.csv")

head(Heart)

#Test and Train
Heart_train <- Heart[1:270, ]
Heart_test <- Heart[271:360, ]

#Fit the model with train data
library(C50)
C5.0_model <- C5.0(Heart_train[,1:7], Heart_train[,8])
summary(C5.0_model)

#Tree Plot
plot(C5.0_model)

#Evaluate the model
library(caret)
library(ROCR)
C5.0_Predict <- predict(C5.0_model, Heart_test[,1:7])
confusionMatrix(C5.0_Predict, Heart_test[,8])

Predicted_prob = predict(C5.0_model, Heart_test[,1:7],type="prob")
mult_measures = prediction(Predicted_prob[,2], Heart_test[,8])
ROC = performance(mult_measures, measure="tpr",x.measure="fpr")
plot(ROC)
