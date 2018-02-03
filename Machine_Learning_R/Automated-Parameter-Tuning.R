                              #Improving Model Performance

#Automated Parameter Tuning

library(caret)
data(GermanCredit)
set.seed(300)
tree_model <- train(Class ~ ., data=GermanCredit, method="C5.0")
tree_model
tree_pred <-predict(tree_model, GermanCredit)
table(tree_pred, GermanCredit$Class)

#Bootstrap aggregation - Bagging
library(ipred)
set.seed(300)
trybag <- bagging(Class~., data=GermanCredit, nbagg =25)
predClass <- predict(trybag, GermanCredit)
table(predClass, GermanCredit$Class)
trybag$mtrees

#Boosting:
tryboost <- C5.0(Class~., data=GermanCredit, trials=10)
tryboost

#Random Forest - will sample the features along with observations
library(randomForest)
tryforest <- randomForest(Class~., data=GermanCredit)
tryforest
