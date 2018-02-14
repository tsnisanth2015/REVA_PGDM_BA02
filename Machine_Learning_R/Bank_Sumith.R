###########################   DATA EXPLORATION AND PREPARATION  ############################################

#Read the file
mydata <- read.csv("D:\\Bank-full Dataset.csv", sep=';',header=T)

#Check the no of rows
NROW(mydata)

#Check the structure of the dataset
str(mydata)

#Explore the dataset
summary(mydata)

#Check the frequency of y variable
table(mydata$y)

#Check the type of mydata
class(mydata)

#edit(mydata)

#Find the unique values in job variable
unique(mydata$job)

#Remove unknowns from job
table(mydata$job)
mydata1 <- subset(mydata,subset=(job!="unknown"))
table(mydata1$job)

#Remove unknowns from education
table(mydata1$education)
mydata2 <- subset(mydata1,subset=(education!="unknown"))
table(mydata2$education)

#Remove unknowns from contact - too many unknown records - one option is to remove the variable
#table(mydata2$contact)
#mydata3 <- subset(mydata2,subset=(contact!="unknown"))
#table(mydata3$contact)

#Remove unknowns from poutcome - too many unknown records - one option is to remove the variable
#table(mydata3$poutcome)
#mydata4 <- subset(mydata3,subset=(poutcome!="unknown"))
#table(mydata4$poutcome)

mydata3 <- subset(mydata2,select = -c(default,duration,poutcome,contact))

table(mydata3$y)

#Variable y recoding and creating a new variable yvar
mydata3$yvar <- ifelse(mydata3$y == "yes",1,2)

#Check the frequencies - whether it is showing the numeric values
table(mydata3$yvar)

#Convert yvar to factor variable
mydata3$yvar <- as.factor(mydata3$yvar)

#Check the structure again
str(mydata3)

#Remove the original output variable y
mydata4 <- subset(mydata3,select = -c(y))

#Check the structure again
str(mydata4)

#Find the unique values in pdays variable
unique(mydata4$pdays)

#Check the frequencies in pdays variable
table(mydata4$pdays)

#Suggestion : Remove the variable pdays since it has 35281 occurences of -1. Its ok to remove here
#since there is a data imbalance.

#Plot the distribution of yvar
barplot(table(mydata4$yvar))

#Histogram of age distribution - better representation would be using ggplot2
hist(mydata4$age,col=terrain.colors(10))

library(ggplot2)

#Proportion of No's and Yes's (yvar values) in various age groups
ggplot()+geom_bar(data = mydata4, aes(x=(mydata4$age),fill=factor(mydata4$yvar)),position = "fill")

barplot(table(mydata4$job))



###################################   MODELLING  ###########################################################

#Shuffle the pack - random uniform
set.seed(123)
datamixed <- mydata4[order(runif(43193)),]

#Split into train and test data
traindata <- datamixed[1:30235,]
testdata <- datamixed[30236:43193,]

##DECISION TREE - C5.0

#Import the C50 package
library(C50)

#Fitting the model with train data
modelC5 <- C5.0(traindata$yvar~.,data = traindata)
plot(modelC5)

#Predict the yvar based on test data
predicted_C5 <- predict(modelC5, testdata[,1:12])

#PRUNING - Cut the tree into small parts since the plot is very complicated for C5

#Create a confusion matrix
confusionmatrix <- confusionMatrix(predicted_C5,testdata[,13])
confusionmatrix

#Managing data to tackle class imbalance
#Undersampling - reducing the majority class, might loose important info
#Oversampling - increase the minority class
#Overfitting - Algorithm has learnt only things specific to the data and not generic

#SMOTE algorithm: does undersampling and oversampling in a more sophisticated way
#Natalie Japkowic, Chawla - read on

#Managing algorithm to tackle class imbalance
#Bias the algorithm towards the minority class,biases decision boundary

#Reduce the 'No' classes from training data and test using test data - Undersampling

table(traindata$yvar)

#3471:26764
#11.5%:88.5%

#We need to make it 30%:70%
#3471:8099

#40%:60%
#3471:5206

#For 30:70
#26764 - 8099 = 18,655 need to be removed

#Order the No's and YES's in descending order - xtfrm is used since yvar is a factor variable
ordereddata <- traindata[order(-xtfrm(traindata$yvar)),]

barplot(table(ordereddata$yvar))

#Removing the first 18655 records after ordering
sampletraindata1 <- ordereddata[18656:30235,]
barplot(table(sampletraindata1$yvar))

#Fitting the model on the new data
modelC5_U1 <- C5.0(sampletraindata1$yvar~.,data = sampletraindata1)

#Predict the yvar based on test data
predicted_C5_U1 <- predict(modelC5_U1, testdata[,1:12])

plot(modelC5_U1)

#Create a confusion matrix
library(caret)
confusionmatrix_U1 <- confusionMatrix(predicted_C5_U1,testdata[,13])
confusionmatrix_U1


#For 40:60
#26764 - 5206 = 21,558 need to be removed

#Removing the first 21,558 records
sampletraindata2 <- ordereddata[21559:30235,]
barplot(table(sampletraindata2$yvar))

#Fitting the model on the new data
modelC5_U2 <- C5.0(sampletraindata2$yvar~.,data = sampletraindata2)
modelC5_U2_boost <- C5.0(sampletraindata2$yvar~.,data = sampletraindata2,trials=100,rules=TRUE)

#Predict the yvar based on test data
predicted_C5_U2 <- predict(modelC5_U2, testdata[,1:12])
predicted_C5_boost <- predict(modelC5_U2_boost, testdata[,1:12])

#Create a confusion matrix
confusionmatrix_U2 <- confusionMatrix(predicted_C5_U2,testdata[,13])
confusionmatrix_U2_boost <- confusionMatrix(predicted_C5_boost,testdata[,13])
confusionmatrix_U2
confusionmatrix_U2_boost

#Sensitivity has not improved much (52~53), should be ideally around 80%


##NEURAL NETWORKS : NNET

library(nnet)

modelnnet <- nnet(sampletraindata1$yvar~.,size=7,data = sampletraindata1)

library(devtools)
library(NeuralNetTools)

NeuralNetTools::plotnet(modelnnet)

predictednnet <- predict(modelnnet, testdata[,1:12], type = "class")
confusionmatrixnnet <- confusionMatrix(predictednnet,testdata[,13])
confusionmatrixnnet

#m <- train(sampletraindata1$yvar~.,data=sampletraindata1,method="C5.0")


library(caret)
library(kernlab)
svm_train = ksvm(sampletraindata1$yvar ~ ., data = sampletraindata1, kernel = "anovadot")
svm_predicted = predict(svm_train, testdata[,1:12])
confusionmatrixsvm <- confusionMatrix(svm_predicted,testdata[,13])
confusionmatrixsvm


#m <- train(x=sampletraindata1[,1:12],y=sampletraindata1$yvar,method="C5.0")
