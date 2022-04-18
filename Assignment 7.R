## Neural Networks

summary(Patient_Data_1_)
sum(is.na(Patient_Data_1_))

sum(unique(Patient_Data_1_))
nrow(Patient_Data_1_)
ncol(Patient_Data_1_)
str(Patient_Data_1_)
install.packages('ISLR')
library(ISLR)

str(Patient_Data_1_)
Patient_Data_1_$gender <- factor(as.numeric(Patient_Data_1_$gender))
Patient_Data_1_$diabetes <- factor(Patient_Data_1_$diabetes)
Patient_Data_1_$smoker <- factor(Patient_Data_1_$smoker)
Patient_Data_1_$active <- factor(Patient_Data_1_$active)
Patient_Data_1_$obesity <- factor(Patient_Data_1_$obesity)
Patient_Data_1_$heartattack_s <- factor(Patient_Data_1_$heartattack_s)
Patient_Data_1_$bp <- factor(Patient_Data_1_$bp)
Patient_Data_1_$cholesteral <- factor(Patient_Data_1_$cholesteral)
Patient_Data_1_$age <- as.numeric(Patient_Data_1_$age)

#convert to numeric
gender <- as.numeric(Patient_Data_1_$gender) - 1
diabetes <- as.numeric(Patient_Data_1_$diabetes) - 1
smoker <- as.numeric(Patient_Data_1_$smoker) - 1
active <- as.numeric(Patient_Data_1_$active) - 1
obesity <- as.numeric(Patient_Data_1_$obesity) - 1
heartattack_s = as.numeric(Patient_Data_1_$heartattack_s)-1
bp <- as.numeric(Patient_Data_1_$bp) - 2
cholesteral <- as.numeric(Patient_Data_1_$cholesteral) - 1


data = cbind(Patient_Data_1_[,1], gender, diabetes, smoker, active, obesity, heartattack_s, bp, cholesteral)
is.na(Patient_Data_1_)
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
as.numeric(Patient_Data_1_)
scaled.data <- as.data.frame(scale(data,center = mins, scale = maxs - mins))
data <- as.data.frame(Patient_Data_1_)

install.packages("neuralnet")
install.packages("caTools")
library(neuralnet)
data <- Patient_Data_1_[,c(1:6, 8:9)]
feats <- names(Patient_Data_1_[,-7])
f <- paste(feats, collapse = ' + ')
f <- paste('heartattack_s ~ ', f)
f <- as.formula(f)

nn <- neuralnet(f, scaled.data, hidden =  c(10),linear.output = FALSE, err.fct = "ce", threshold = 0.1)

summary(Patient_Data_1_)
typeof(Patient_Data_1_$age)
f
f_data <- model.matrix(f, data = data)
str(data)
names(data)
names(Patient_Data_1_)
f

predicted.nn.values <- compute(nn, scaled.data[,-7])
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(data$heartattack_s, predicted.nn.values$net.result)
mean(scaled.data$heartattack_s == predicted.nn.values$net.result)
head(data[,-7])
head(data)

library(caret)
library(caTools)
set.seed(101)
split = sample.split(scaled.data$heartattack_s, SplitRatio = 0.80)
train = subset(scaled.data, split == TRUE)
test = subset(scaled.data, split == FALSE)
nrow(train)
nrow(test)

nn <- neuralnet(f, train, hidden =  c(10),linear.output = FALSE, err.fct = "ce", threshold = 0.1)
predicted.nn.values <- compute(nn, train[,-7])
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(train$heartattack_s, predicted.nn.values$net.result)
mean(train$heartattack_s == predicted.nn.values$net.result)

predicted.nn.values <- compute(nn, test[,-7])
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(test$heartattack_s, predicted.nn.values$net.result)
mean(test$heartattack_s == predicted.nn.values$net.result)
