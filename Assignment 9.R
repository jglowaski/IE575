## Bayesian Networks

library(tm)
library(caret)
library(e1071)
library(dplyr)

colnames(cc_default) <- cc_default[1,]
cc_default <- cc_default[-1,]

summary(cc_default)

unique(cc_default$EDUCATION)
unique(cc_default$MARRIAGE)
unique(cc_default$PAY_0)
unique(cc_default$PAY_2)
unique(cc_default$PAY_3)
unique(cc_default$PAY_4)
unique(cc_default$PAY_5)
unique(cc_default$PAY_6)
sum(unique(cc_default))
nrow(cc_default)
ncol(cc_default)
str(cc_default)

cc_default$MARRIAGE  <- ifelse(cc_default$MARRIAGE==0,NA,cc_default$MARRIAGE)
cc_default$EDUCATION <- ifelse(cc_default$EDUCATION>4,NA,cc_default$EDUCATION)
cc_default$EDUCATION <- ifelse(cc_default$EDUCATION==0,NA,cc_default$EDUCATION)
for (name in c('PAY_0', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6')){
  cc_default[cc_default[,name]==-2, name] <- NA
}

summary(cc_default)
apply(is.na(cc_default),2,sum)
ncol(cc_default)
nrow(cc_default)
cc_default <- cc_default[complete.cases(cc_default), ]
cc_default$ID <- NULL
colnames(cc_default)[24] <- "DEFAULT"
cc_default$DEFAULT <- as.factor(cc_default$DEFAULT)

set.seed(443452342)
default <- cc_default$DEFAULT
cc_sampling_vector <- createDataPartition(cc_default$DEFAULT, p = 0.80, list = FALSE)

cc_train <- cc_default[cc_sampling_vector,]
cc_test <- cc_default[-cc_sampling_vector,]
default_train <- default[cc_sampling_vector]
default_test <- default[-cc_sampling_vector]

cc_model <- naiveBayes(cc_train[, -24], default_train)

cc_train_pred <- predict(cc_model, cc_train)
mean(cc_train_pred == default_train)
table(actual = default_train, predictions = cc_train_pred)

cc_test_pred <- predict(cc_model, cc_test)
mean(cc_test_pred == default_test)
table(actual = default_test, predictions = cc_test_pred)
