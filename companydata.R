install.packages("C50")
library(rpart)
library(C50)
library(tree)
library(gmodels)
li
CompanyData <- read.csv("Company_Data.csv")
attach(CompanyData)
hist(CompanyData$Sales)
High = ifelse(CompanyData$Sales<=8, "No", "Yes")
CD = data.frame(CompanyData, High)
head(CD)
CD_train <- CD[1:200,]
CD_test <- CD[201:400,]
op_tree <- tree(CD_train$High ~., data=CD_train)

summary(op_tree)
plot(op_tree)
pred_tree <- as.data.frame(predict(op_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=CD_test)


mean(pred_test_df==CD$High)
