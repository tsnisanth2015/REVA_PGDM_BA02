library("kernlab")

data(musk)

str(musk,list.len=200)

set.seed(123)
musk_mixed=musk[order(runif(476)), ]

musk_train = musk_mixed[1:357, ]
musk_test = musk_mixed[358:476, ]

#vanilladot is the default value. No kernel used.
musk_svm = ksvm(Class ~ ., data = musk_train, kernel = "vanilladot")

musk_svm_pred = predict(musk_svm, musk_test[,1:166])

library(caret)
confusionMatrix(musk_svm_pred, musk_test$Class)

musk_svm_imp = ksvm(Class ~ ., data = musk_train, kernel = "rbfdot")

musk_svm_imp_pred = predict(musk_svm_imp, musk_test[,1:166])

confusionMatrix(musk_svm_imp_pred, musk_test$Class)
