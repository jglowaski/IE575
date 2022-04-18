install.packages("caret")
library('caret')
set.seed(100)
data_loan1app <- read.csv(url('https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv'))
str(data_loan1app)

summary(data_loan1app)
apply(is.na(data_loan1app), 2, which)

sum(is.na(data_loan1app$Credit_History))
data_loan1app$aggregatedIncome <- data_loan1app$ApplicantIncome + data_loan1app$CoapplicantIncome
data_loan1app$aggregatedIncome
head(data_loan1app$aggregatedIncome, 10)
summary(data_loan1app$aggregatedIncome)
grouped <- cut(data_loan1app$aggregatedIncome, c(-Inf, 4166, 7522, Inf))
print(factordata)
head(grouped)
cat <- factor(c('High', 'Medium', 'Low'))
data_loan1app$IncomeLevel <- as.factor(ifelse(data_loan1app$aggregatedIncome <= 4166, 'Low',
                                              ifelse(data_loan1app$aggregatedIncome <= 7522, 'Medium',
                                                     'High')))
summary(data_loan1app$IncomeLevel)
library(dplyr)


data <- data_loan1app %>%
  group_by(IncomeLevel) %>%
  mutate(LoanAmount = ifelse(is.na(LoanAmount),
                             median(LoanAmount, na.rm = TRUE),
                             LoanAmount))

x <- data %>%
  group_by(IncomeLevel, Education ,Loan_Status) %>%
  summarise(mean(LoanAmount)) %>%
  filter(Loan_Status == 'Y')
  
x

mean(data_loan1app$LoanAmount, na.rm = TRUE)
median(data_loan1app$LoanAmount, na.rm = TRUE)

