## INTUITIVE EXPLANATION MLE IN BINARY LOGISTIC REGRESSION ##

smoke01 <- read.csv(file.choose()) #load smoke01.csv   
# smoke01 <- read.csv("E:/Classes/Trainer Tools/Final/04 Logistic Regression/Data Sets/smoke01.csv")
summary(smoke01)
plot(smoke01)
plot(smoke01$Age,smoke01$Smoker)
# Logistic Regression
logit01=glm(smoke01$Smoker ~ smoke01$Age ,family=binomial,data=smoke01)
summary(logit01)
prob01=predict(logit01,type=c("response"),smoke01)
# Confusion Matrix Table
confusion01<-table(prob01>0.5,smoke01$Smoker)
confusion01
# Model Accuracy
Accuracy01<-sum(diag(confusion01))/sum(confusion01)
Accuracy01


smoke02 <- read.csv(file.choose()) #load smoke02.csv   
#smoke02 <- read.csv("E:/Classes/Trainer Tools/Final/04 Logistic Regression/Data Sets/smoke02.csv")
summary(smoke02)
plot(smoke02$Age,smoke02$Smoker)
# Logistic Regression
logit02=glm(smoke02$Smoker ~ smoke02$Age ,family= "binomial",data=smoke02)

summary(logit02)
prob02=predict(logit02,type=c("response"),smoke02)
prob02# Confusion Matrix Table
confusion02<-table(prob02>0.5,smoke02$Smoker)
confusion02
# Model Accuracy
Accuracy02<-sum(diag(confusion02))/sum(confusion02)
Accuracy02


smoke03 <- read.csv(file.choose()) #load smoke03.csv
#smoke03 <- read.csv("E:/Classes/Trainer Tools/Final/04 Logistic Regression/Data Sets/smoke03.csv")
summary(smoke03)
plot(smoke03$Age,smoke03$Smoker)
# Logistic Regression
logit03=glm(smoke03$Smoker ~ smoke03$Age ,family= "binomial",data=smoke03)
summary(logit03)
prob03=predict(logit03,type=c("response"),smoke03)
# Confusion Matrix Table
confusion03<-table(prob03>0.5,smoke03$Smoker)
confusion03
# Model Accuracy
Accuracy03<-sum(diag(confusion03))/sum(confusion03)
Accuracy03

smoke04 <- read.csv(file.choose()) #load smoke04.csv
#smoke04 <- read.csv("E:/Classes/Trainer Tools/Final/04 Logistic Regression/Data Sets/smoke04.csv")
summary(smoke04)
plot(smoke04$Age,smoke04$Smoker)
# Logistic Regression
logit04=glm(smoke04$Smoker ~ smoke04$Age ,family= "binomial",data=smoke04)
summary(logit04)
# Confusion Matrix Table
prob04=predict(logit04,type=c("response"),smoke04)
confusion04<-table(prob04>0.499999999999,smoke04$Smoker)
confusion04
confusion04<-table(prob04>0.500000000001,smoke04$Smoker)
confusion04
# Model Accuracy
Accuracy04<-sum(diag(confusion04))/sum(confusion04)
Accuracy04
