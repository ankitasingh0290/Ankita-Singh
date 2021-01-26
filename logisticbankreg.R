      
getwd()

library(readr)

banks<-read.csv("bank-full.csv",sep=";")
View(banks)
str(banks)
sum(is.na(banks))
mod_lm <- lm(factor(y)~.,data=banks)
pred1 <- predict(mod_lm,banks)
plot(banks$age,pred1)
plot(pred1)
boxplot(banks$age,y)

logit<-glm(factor(y)~.,family=binomial,data=banks)
summary(logit)
exp(coef(logit))
table(banks$y)

#confusion matrix
prob123<-predict(logit,type=c("response"),banks)
prob123
confusion<-table(prob123>0.5,banks$y)
probo<-prob123>0.5
table(probo)
confusion

#Model Accuracy4
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error<-1-Accuracy
Error
#ROC curve
#install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob123,banks$y)
rocrpred
rocrpred<-performance(rocrpred,"tpr","fpr")
rocrpred
plot(rocrpred,colorize=T,text.adj=c(-0.2,1.7))
