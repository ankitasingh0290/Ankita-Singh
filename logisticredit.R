getwd()
#install.packages("corrplot")
library(readr)
library(corrplot)
credit<-read.csv("creditcard.csv",sep=",")
View(credit)
str(credit)
sum(is.na(credit))
corrplot(cor(factor(credit)), method = "circle")

logit<-glm(factor(card)~.,family=binomial,data=credit)
summary(logit)


exp(coef(logit))
table(credit$card)

#confusion matrix
prob1234<-predict(logit,type="response",credit)
plot(prob1234, 
     main = "Scatterplot of Probabilities of Default (test data)", 
     xlab = "Customer ID", ylab = "Predicted Probability of Default")

confusion<-table(prob1234>0.5,credit$card)
probab<-prob1234>0.5
table(probab)
confusion

#Model Accuracy
Accuracy_1<-sum(diag(confusion)/sum(confusion))
Accuracy_1
Error_1<-1-Accuracy_1
Error_1
#ROC curve
#install.packages("ROCR")
library(ROCR)
rocrpred_1<-prediction(prob1234,credit$card)
rocrpred_1
rocrpred_1<-performance(rocrpred_1,"tpr","fpr")
rocrpred_1
plot(rocrpred_1,colorize=T,text.adj=c(-0.2,1.7))
