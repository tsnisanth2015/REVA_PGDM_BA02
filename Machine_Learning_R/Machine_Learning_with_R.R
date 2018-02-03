car.test.frame

summary(car.test.frame)

#Remove the reliability variable
car <- car.test.frame[,-3]

head(car)

#Maintain uniformity and select the same set of values
set.seed(123)

car_mixed = car[order(runif(60)), ]

head(car_mixed)

#Split into train and test
car_train = car_mixed[1:45, ]
car_test = car_mixed[46:60,]

#Fit the model with train data
CART_model = rpart(Price~., data = car_train)

#Use the model to predict the test data
CART_pred = predict(CART_model, car_test[,-1])

#Will show the error
mae(car_test[,1], CART_pred)

z <- cbind(CART_pred, car_test[,1])
head(z)

#Tree Plot
library(rpart.plot)
rpart.plot(CART_model)






