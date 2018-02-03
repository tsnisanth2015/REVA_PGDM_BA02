#Home Loan Prediction  Project

# Read the home loan csv file and split it into Train and Test sets
Home_Loan_Raw <- read.csv(file="D:\\REVA\\Reva Assignments\\Home Loan Assignment\\Home_Loan_Raw.csv",header=TRUE,na.strings = c(""," ",NA))
dim(Home_Loan_Raw)

# Find a summary of all columns in the raw file
library(mlr)
summary(Home_Loan_Raw)
summarizeColumns(Home_Loan_Raw)

#Insights:
# 1.More than 25% of the applicants do not have co-applicants
# 2.There could be outliers in applicant Income
# 3.There are missing values in data


# Identify the target variable and check the proportions of loan approval and rejection
library(ggplot2)
summary(Home_Loan_Raw$Loan_Status)
prop.table(table(Home_Loan_Raw$Loan_Status))

ggplot(Home_Loan_Raw, aes(x = Home_Loan_Raw$Loan_Status)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rates")

# Explore the predictor variables
# 1.Gender:
barplot(table(Home_Loan_Raw$Gender),main="Gender")
summary(Home_Loan_Raw$Gender)
prop.table(table(Home_Loan_Raw$Gender))

# 2.Married:
summary(Home_Loan_Raw$Married)
table(Home_Loan_Raw$Married)
prop.table(table(Home_Loan_Raw$Married))
barplot(table(Home_Loan_Raw$Married))

# 3.Dependents:
summary(Home_Loan_Raw$Dependents)
table(Home_Loan_Raw$Dependents)
prop.table(table(Home_Loan_Raw$Dependents))
barplot(table(Home_Loan_Raw$Dependents))

# 4.Education:
summary(Home_Loan_Raw$Education)
table(Home_Loan_Raw$Education)
prop.table(table(Home_Loan_Raw$Education))
barplot(table(Home_Loan_Raw$Education))


# 5.Self Employed:
summary(Home_Loan_Raw$Self_Employed)
table(Home_Loan_Raw$Self_Employed)
prop.table(table(Home_Loan_Raw$Self_Employed))
barplot(table(Home_Loan_Raw$Self_Employed))


# 6.Applicant Income & 7.CoapplicantIncome:
summary(Home_Loan_Raw$ApplicantIncome)
summary(Home_Loan_Raw$CoapplicantIncome)

par(mfrow=c(1,2))
boxplot(Home_Loan_Raw$ApplicantIncome,Home_Loan_Raw$CoapplicantIncome,names=c("Applicant","Coapplicant"),main="Income comparison")


# 8.Loan Amount:
summary(Home_Loan_Raw$LoanAmount)
boxplot(Home_Loan_Raw$LoanAmount,main="Loan Amount")

#Insights:
#LoanAmount has missing and well as extreme values, which needs further analysis.


# 9.Loan Amount Term:
summary(Home_Loan_Raw$Loan_Amount_Term)
hist(Home_Loan_Raw$Loan_Amount_Term,breaks=500,main="Home Loan Term")

# 10.Credit History
# This is in integer format. Let's convert this to factor variable (2 levels - 0 & 1).

Home_Loan_Raw$Credit_History <- as.factor(Home_Loan_Raw$Credit_History)
barplot(table(Home_Loan_Raw$Credit_History),main="Credit History")
summary(Home_Loan_Raw$Credit_History)
prop.table(table(Home_Loan_Raw$Credit_History))

# 11.Property Area:
summary(Home_Loan_Raw$Property_Area)
table(Home_Loan_Raw$Property_Area)
prop.table(table(Home_Loan_Raw$Property_Area))
barplot(table(Home_Loan_Raw$Property_Area))

#TARGET VARIABLE WHEN PLOTTED AGAINST THE VARIOUS PREDICTOR VARIABLES:

# Loan status by Gender ?
ggplot(Home_Loan_Raw, aes(x = Gender, fill = Loan_Status)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rate by Gender")

# Loan status by Marital Status ?
ggplot(Home_Loan_Raw, aes(x = Married, fill = Loan_Status)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rate by Marital Status")

# Loan status by Gender and Marital Status ?
ggplot(Home_Loan_Raw, aes(x = Gender, fill = Loan_Status)) +
  theme_bw() +
  facet_wrap(~ Married) +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rate by Gender & Marital Status") 


# Loan status by number of dependents ?
library(ggplot2)
ggplot(Home_Loan_Raw, aes(x = Loan_Status)) +
  theme_bw() +
  facet_grid(.~Dependents) +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rate by number of Dependents")

# Loan status by number of Education ?
ggplot(Home_Loan_Raw, aes(x = Loan_Status)) +
  theme_bw() +
  facet_grid(.~Education) +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rate by Education")

# Loan status by 'self employed' applicants ?
ggplot(Home_Loan_Raw, aes(x = Loan_Status)) +
  theme_bw() +
  facet_grid(.~Self_Employed) +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rate of Self Employed Applicants")

# Loan status by Loan Term?
ggplot(Home_Loan_Raw, aes(x = Loan_Status)) +
  theme_bw() +
  facet_grid(.~Loan_Amount_Term) +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rate vs Loan Term")

# Loan status by Credit History ?
ggplot(Home_Loan_Raw, aes(x = Loan_Status)) +
  theme_bw() +
  facet_grid(.~Credit_History) +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rate vs Credit History")

# Loan status by Credit History and Gender ?
ggplot(Home_Loan_Raw, aes(x = Credit_History, fill = Loan_Status)) +
  theme_bw() +
  facet_grid(.~Gender) +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rate vs Credit History and Gender")


# Loan status by area of property?
ggplot(Home_Loan_Raw, aes(x = Loan_Status)) +
  theme_bw() +
  facet_grid(.~Property_Area) +
  geom_bar() +
  labs(y = "Loan Accept/Reject Count",
       title = "Loan Approval Rate by Area of Property")

# Loan status by applicant income ?
ggplot(Home_Loan_Raw, aes(x = Loan_Status, y = ApplicantIncome)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Applicant Income",
       title = "Loan Approval Rate by Applicant Income")

# Loan status by Coapplicant income ?
ggplot(Home_Loan_Raw, aes(x = Loan_Status, y = CoapplicantIncome)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Coapplicant Income",
       title = "Loan Approval Rate by Coapplicant Income")

# Loan status by Loan Amount ?
ggplot(Home_Loan_Raw, aes(x = Loan_Status, y = LoanAmount)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Loan Amount",
       title = "Loan Approval Rate by Loan Amount")


#DATA ANALYSIS:

# Applicant Income Distribution:
ggplot(Home_Loan_Raw, aes(x = ApplicantIncome)) +
  theme_bw() +
  geom_histogram(binwidth = 100) +
  labs(y = "Applicant Count",
       x = "Income (binwidth = 5000)",
       title = "Applicant Income distribution")

# Applicant Income mapped against Marital and grouped by Gender:
ggplot(Home_Loan_Raw, aes(x = ApplicantIncome, fill = Married)) +
  theme_bw() +
  facet_wrap(~ Gender) +
  geom_histogram(binwidth = 5000,position = "dodge") +
  labs(y = "Applicant Count",
       x = "Income (binwidth = 5000)",
       title = "Applicant Income distribution")

# Applicant Income mapped against Education:
ggplot(Home_Loan_Raw, aes(x = Education, y = ApplicantIncome)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Income",
       title = "Applicant Income by Education")

#Insights:
#We can see that there is no substantial different between the mean income of graduate and non-graduates. 
#But there are a higher number of graduates with very high incomes, which are appearing to be the outliers.


# Coapplicant Income mapped against Marital and grouped by Gender:
ggplot(Home_Loan_Raw, aes(x = CoapplicantIncome, fill = Married)) +
  theme_bw() +
  facet_wrap(~ Gender) +
  geom_histogram(binwidth = 5000,position = "dodge") +
  labs(y = "Applicant Count",
       x = "Income (binwidth = 5000)",
       title = "Coapplicant Income distribution")

# Lets look at the combined income
library(plyr)
Home_Loan_Raw2<-mutate(Home_Loan_Raw,TotalIncome=ApplicantIncome+CoapplicantIncome)
ggplot(Home_Loan_Raw2, aes(x = TotalIncome, fill = Married)) +
  theme_bw() +
  facet_wrap(~ Gender) +
  geom_histogram(binwidth = 5000,position = "dodge") +
  labs(y = "Applicant Count",
       x = "Income (binwidth = 5000)",
       title = "Total Income distribution")

## MISSING VALUE IMPUTATION AND DATA CLEANING

#Report the missing values in the dataframe.
sapply(Home_Loan_Raw2, function(x) sum(is.na(x)))

# Lets impute the missing values of 'Married' variable
# If the co-applicant income is 0, the  marital status - 'UN-MARRIED'
# If the co-applicant income is not 0, then marital status - 'MARRIED'
Home_Loan_Raw2$Married[is.na(Home_Loan_Raw2$Married) & Home_Loan_Raw2$CoapplicantIncome==0] <- "No"
Home_Loan_Raw2$Married[is.na(Home_Loan_Raw2$Married)] <- "Yes"
summary(Home_Loan_Raw2$Married)
summary(Home_Loan_Raw$Married)


# Lets impute the missing values of dependents
# Plot no of dependents vs Gender and group by marital status
ggplot(Home_Loan_Raw2,aes(x=Dependents, fill=Gender)) + geom_bar() + facet_grid(.~Married)
# For unmarried applicants, the number of dependents can be imputed as 0
Home_Loan_Raw2$Dependents[is.na(Home_Loan_Raw2$Dependents) & Home_Loan_Raw2$Married=="No"]<- "0"
summary(Home_Loan_Raw$Dependents)
summary(Home_Loan_Raw2$Dependents)

# Use rpart to predict the missing dependent values based on applicant income,coapplicant income, 
# loan amount, loan term and property area as predictors
Dep_Filter <- Home_Loan_Raw2[(Home_Loan_Raw2$Gender=="Male" & Home_Loan_Raw2$Married=="Yes"),c(4,7:10,12)]
Dep_Train <- Dep_Filter[!is.na(Dep_Filter$Dependents),]
Dep_Test <- Dep_Filter[is.na(Dep_Filter$Dependents),]
library(rpart)
library(rattle)
Dep_Fit <- rpart(data=Dep_Train,Dependents~.,method = "class")
fancyRpartPlot(Dep_Fit)

Dep_Pred<-predict(Dep_Fit,Dep_Train,type="class")
Dep_Acc=sum(Dep_Pred==Dep_Train[,1])/length(Dep_Pred)
Dep_Acc

caret::confusionMatrix(Dep_Pred, Dep_Train$Dependents)

# Impute the missing dependents with predicted values 
Home_Loan_Raw2$Dependents[is.na(Home_Loan_Raw2$Dependents) & Home_Loan_Raw2$Gender=="Male" & Home_Loan_Raw2$Married == "Yes"]<- predict(Dep_Fit,newdata=Dep_Test,type="class")
#Home_Loan_Raw2$Dependents <- recode(Home_Loan_Raw2$Dependents,"'3+'='3' ")


# Use rpart to predict the missing Gender values based on applicant income,coapplicant income, 
# loan amount, loan term and property area as predictors
Gender_Train <- Home_Loan_Raw2[!is.na(Home_Loan_Raw2$Gender),2:8]
Gender_Test <- Home_Loan_Raw2[is.na(Home_Loan_Raw2$Gender),2:8]
Gender_Fit <- rpart(data=Gender_Train,Gender~.,xval=3)
fancyRpartPlot(Gender_Fit)

# Predict Accuracy
Gender_Pred <- predict(Gender_Fit,Gender_Train,type="class")
Gender_Acc <- sum(Gender_Pred==Gender_Train[,1])/length(Gender_Pred)
Gender_Acc

# Impute the missing Gender Values
Home_Loan_Raw2$Gender[is.na(Home_Loan_Raw2$Gender)] <- predict(Gender_Fit,Gender_Test,type="class")

# Impute missing values for 'Self Employed' variable.
# Majority are not self employed (500/582 = 86%)
Home_Loan_Raw2$Self_Employed[is.na(Home_Loan_Raw2$Self_Employed)] <- "No"


# Impute the missing values for 'Credit History' variable.
# There are quite a few approved cases in the missing credit history category of applicants, although its low.
# So we will treat this as a separate category
library(car)
levels <- levels(Home_Loan_Raw2$Credit_History)
levels
levels[length(levels) + 1] <- "2"
Home_Loan_Raw2$Credit_History <- factor(Home_Loan_Raw2$Credit_History, levels = levels)
Home_Loan_Raw2$Credit_History[is.na(Home_Loan_Raw2$Credit_History)] <- "2"


# Impute the missing values for 'Loan Amount'
# We will be using the Generalized Linear Models for imputing the missing values here.
# Outliers greater than 500 has been ignored for training the set.
LoanAmount_Train<-Home_Loan_Raw2[!is.na(Home_Loan_Raw2$LoanAmount) & Home_Loan_Raw2$LoanAmount<500,c(2:9,10)]
LoanAmount_Test <- Home_Loan_Raw2[is.na(Home_Loan_Raw2$LoanAmount),c(2:9,10)]
LoanAmount_Fit <- glm(data=LoanAmount_Train,LoanAmount~.,na.action=na.exclude)
# Impute the values
Home_Loan_Raw2$LoanAmount[is.na(Home_Loan_Raw2$LoanAmount)] <- predict(LoanAmount_Fit,newdata=LoanAmount_Test)


# Impute the missing values for 'Loan Amount Terms'
# Vast majority has gone for a loan term of 360 months. So it will be reasonable to impute 360 for the missing ones
Home_Loan_Raw2$Loan_Amount_Term <- as.factor(Home_Loan_Raw2$Loan_Amount_Term)
Home_Loan_Raw2$Loan_Amount_Term[is.na(Home_Loan_Raw2$Loan_Amount_Term)] <- "360"

# Check and confirm there are no more missing values.
sapply(Home_Loan_Raw2, function(x) sum(is.na(x)))

# REMOVING OUTLIERS FROM LOANAMOUNT AND APPLICANT INCOME

Home_Loan_Raw2$LogLoanAmount <- log(Home_Loan_Raw2$LoanAmount)
Home_Loan_Raw2$LogApplicantIncome <- log(Home_Loan_Raw2$ApplicantIncome)
Home_Loan_Raw2$LogCoapplicantIncome <- log(Home_Loan_Raw2$CoapplicantIncome)
Home_Loan_Raw2$LogTotalIncome <- log(Home_Loan_Raw2$TotalIncome)

# Select the train and test sets
# 75% of the sample size
smp_size <- floor(0.70 * nrow(Home_Loan_Raw2))
Home_Loan_Train <- sample(seq_len(nrow(Home_Loan_Raw2)), size = smp_size)
train <- Home_Loan_Raw2[Home_Loan_Train, ]
test <- Home_Loan_Raw2[-Home_Loan_Train, ]

# LOGISTIC REGRESSION MODEL

logr1 = glm(formula = Home_Loan_Raw2$Loan_Status ~ Home_Loan_Raw2$Credit_History + Home_Loan_Raw2$LogLoanAmount, family = binomial(link = "logit"))
summary(logr1)

logr2 = glm(formula = Home_Loan_Raw2$Loan_Status ~ Home_Loan_Raw2$Credit_History, family = "binomial")
summary(logr2)

attach(Home_Loan_Raw2)
logr3 = glm(Loan_Status ~ Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area+LogLoanAmount, data = Home_Loan_Raw2, family = "binomial")
summary(logr3)

