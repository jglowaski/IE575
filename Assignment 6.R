library(dplyr)
library(nnet)

##C5.0 model

patients <- Patient_Data

summary(patients)
sum(is.na(patients))
nrow(patients)
nrow(unique(patients))
head(patients, 5)
unique(patients$bp)
unique(patients$cholesteral)

install.packages("C50")
library(C50)

patients$heartattack_s <- factor(patients$heartattack_s)

HApatients_c5 <- C5.0(heartattack_s ~., data = patients)
summary(HApatients_c5)
plot(HApatients_c5, main = "C5.0 tree for patient data", 
     type = "simple",
     gp = gpar(fontsize = 10), 
     tp_args= list(width = 4, id = FALSE)
)

p_pred <- predict(HApatients_c5,patients)
mean(p_pred == patients$heartattack_s)

(confusionMatrix <- table(predicted = p_pred, actual = patients$heartattack_s))
(precision <- confusionMatrix[2,2]/sum(confusionMatrix[2,]))
(recall <- confusionMatrix[2,2]/sum(confusionMatrix[,2]))
(f1 = 2 * precision * recall/ (precision + recall))


#Q2
library(rpart)
patient_cart <- rpart(heartattack_s ~., method = "class", data = patients)
pruneptree <- prune(patient_cart, cp=patient_cart$cptable[which.min(patient_cart$cptable[,"xerror"]),"CP"])
plot(pruneptree, uniform=TRUE,
     
     main="Pruned Classification Tree for Patient Heartattacks")
text(pruneptree, use.n=TRUE, all=TRUE, cex=.6)
printcp(prunep tree)

cart_pred <- predict(pruneptree, patients, type = "class")
mean(cart_pred == patients$heartattack_s)
(confusionMatrix <- table(predicted = cart_pred, actual = patients$heartattack_s))
(precision <- confusionMatrix[2,2]/sum(confusionMatrix[2,]))
(recall <- confusionMatrix[2,2]/sum(confusionMatrix[,2]))
(f1 = 2 * precision * recall/ (precision + recall))


#Q3
library(caret)
set.seed(266)
patients_sampling_vector <- createDataPartition(patients$heartattack_s, p=0.80, list=FALSE)
patients_train <- patients[patients_sampling_vector,]
patients_test <- patients[-patients_sampling_vector,]

HApatients_c5 <- C5.0(heartattack_s ~., data = patients_train)
summary(HApatients_c5)
plot(HApatients_c5)

train_pred <- predict(HApatients_c5,patients_train)
mean(train_pred == patients_train$heartattack_s)

test_pred <- predict(HApatients_c5, patients_test)
mean(test_pred == patients_test$heartattack_s)

(confusionMatrix <- table(predicted = train_pred, actual = patients_train$heartattack_s))
(precision <- confusionMatrix[2,2]/sum(confusionMatrix[2,]))
(recall <- confusionMatrix[2,2]/sum(confusionMatrix[,2]))
(f1 = 2 * precision * recall/ (precision + recall))

(confusionMatrix <- table(predicted = test_pred, actual = patients_test$heartattack_s))
(precision <- confusionMatrix[2,2]/sum(confusionMatrix[2,]))
(recall <- confusionMatrix[2,2]/sum(confusionMatrix[,2]))
(f1 = 2 * precision * recall/ (precision + recall))
