library(party)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
set.seed(123)
Fraudata<-read.csv("Fraud_check.csv")
hist(Fraudata$Taxable.Income)
hist(Fraudata$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","violet"))
Risky_Good = ifelse(Fraudata$Taxable.Income<= 30000, "Risky", "Good") # if Taxable Income <= 30000 then Risky else Good.
FCtemp= data.frame(Fraudata,Risky_Good)
FC = FCtemp[,c(1:7)]
FC$Undergrad<-as.factor(FC$Undergrad)
FC$Marital.Status<- as.factor(FC$Marital.Status)
FC$Urban<- as.factor(FC$Urban)
FC$Risky_Good <- as.factor(FC$Risky_Good)

str(FC)
FC_train <- FC[1:300,]
FC_test <- FC[301:600,]
png(file = "decision_tree.png")
partyfun_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(partyfun_tree)
plot(partyfun_tree)
##### party function on training data###
png(file = "decision_tree.png")
pfun_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(pfun_tree)
plot(pfun_tree)
  ###### Prediction on Test data####

pred_tree <- as.data.frame(predict(pfun_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(pfun_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good) #82%

CrossTable(FC_test$Risky_Good,pred_test_df)

confusionMatrix(FC_test$Risky_Good,pred_test_df) ### accuracy 82%
