## Neural Networks
install.packages("xlsx")

library(xlsx)
cc_default <- default_of_credit_card_clients

colnames(cc_default) <- cc_default[1,]
cc_default <- cc_default[-1,]
colnames(cc_default)[7] <- "PAY_1"
summary(cc_default)
sum(is.na(cc_default))

sum(unique(cc_default))
nrow(cc_default)
ncol(cc_default)
str(cc_default)

cc_default$LIMIT_BAL <-as.numeric(cc_default$LIMIT_BAL)
cc_default$SEX <- as.factor(cc_default$SEX)
cc_default$EDUCATION <- as.factor(cc_default$EDUCATION)
cc_default$MARRIAGE <- as.factor(cc_default$MARRIAGE)
cc_default$AGE <- as.numeric(cc_default$AGE)
cc_default$PAY_1 <- as.factor(cc_default$PAY_1)



cc_default$PAY_2 <- as.factor(cc_default$PAY_2)
cc_default$PAY_3 <- as.factor(cc_default$PAY_3)
cc_default$PAY_4 <- as.factor(cc_default$PAY_4)
cc_default$PAY_5 <- as.factor(cc_default$PAY_5)
cc_default$PAY_6 <- as.factor(cc_default$PAY_6)
cc_default$BILL_AMT1 <- as.numeric(cc_default$BILL_AMT1)
cc_default$BILL_AMT2 <- as.numeric(cc_default$BILL_AMT2)
cc_default$BILL_AMT3 <- as.numeric(cc_default$BILL_AMT3)
cc_default$BILL_AMT4 <- as.numeric(cc_default$BILL_AMT4)
cc_default$BILL_AMT5 <- as.numeric(cc_default$BILL_AMT5)
cc_default$BILL_AMT6 <- as.numeric(cc_default$BILL_AMT6)
cc_default$PAY_AMT1 <- as.numeric(cc_default$PAY_AMT1)
cc_default$PAY_AMT2 <- as.numeric(cc_default$PAY_AMT2)
cc_default$PAY_AMT3 <- as.numeric(cc_default$PAY_AMT3)
cc_default$PAY_AMT4 <- as.numeric(cc_default$PAY_AMT4)
cc_default$PAY_AMT5 <- as.numeric(cc_default$PAY_AMT5)
cc_default$PAY_AMT6 <- as.numeric(cc_default$PAY_AMT6)
cc_default$default <- as.factor(cc_default$`default payment next month`)
cc_default$`default payment next month` <- NULL
cc_default$ID <- NULL

cc_default$MARRIAGE  <- ifelse(cc_default$MARRIAGE==0,NA,cc_default$MARRIAGE)
cc_default$EDUCATION <- ifelse(cc_default$EDUCATION>4,NA,cc_default$EDUCATION)
cc_default$EDUCATION <- ifelse(cc_default$EDUCATION==0,NA,cc_default$EDUCATION)
for (name in c('PAY_0', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6')){
  cc_default[cc_default[,name]==-2, name] <- NA
}

library(caret)
set.seed(4365677)
cc_sampling_vector <- createDataPartition(cc_default$default, p = 0.80, list = FALSE)
cc_train <- cc_default[cc_sampling_vector,]
cc_test <- cc_default[-cc_sampling_vector,]

cc_pp <- preProcess(cc_train[c(2:24)], method = c("range"))
cc_train <- cbind(predict(cc_pp, cc_train[2:24]), default = cc_train$default)
cc_test <- cbind(predict(cc_pp, cc_test[2:24]), default = cc_test$default)

library(nnet)
cc_model <- nnet(default ~ ., data = cc_train, size = 10, maxit = 1000, MaxNWts = 10000000)
cc_train[,2:11]
train_predictions <- predict(cc_model, cc_train[,1:24], type = "class")
mean(train_predictions == cc_train$default)

test_predictions <- predict(cc_model, cc_test[,1:24], type = 'class')
mean(test_predictions == cc_test$default)
summary(cc_model)
plot(cc_model)


cc_imp <- varImp(cc_model)
cc_imp
