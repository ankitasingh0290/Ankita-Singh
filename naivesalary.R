#install.packages("caret")
#install.packages("kernlab")
#install.packages("psych")
#install.packages("naivebayes")
library("e1071")
library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(naivebayes)

# TEST DATA

salarytest<-read.csv("SalaryData_Test(1).csv")
#View(salarytest)
class(salarytest)
str(salarytest)
salarytest$educationno <- as.factor(salarytest$educationno)
class(salarytest)

# Data train
salarytrain <- read.csv("SalaryData_Train(1).csv")
str(salarytrain)

View(salarytrain)
salarytrain$educationno <- as.factor(salarytrain$educationno)
class(salarytrain)
ggplot(data=salarytrain,aes(x=salarytrain$Salary, y = salarytrain$age, fill = salarytrain$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
plot(salarytrain$workclass,salarytrain$Salary)
plot(salarytrain$education,salarytrain$Salary)
ggplot(data=salarytrain,aes(x = salarytrain$age, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


#####Naive Model#####

Model <- naiveBayes(factor(salarytrain$Salary )~ ., data = salarytrain)
Model
Model_pred <- predict(Model,salarytest)
mean(Model_pred==salarytest$Salary)
# 81.87%
confusionMatrix(Model_pred,factor(salarytest$Salary)) ##3 accuracy 82%

  ######RESULT########
#Confusion Matrix and Statistics
#Reference
#Prediction  <=50K  >50K
#<=50K  10549  1919
#>50K     811  1781

#Accuracy : 0.8187          
#95% CI : (0.8125, 0.8248)
#No Information Rate : 0.7543          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.456           

#Mcnemar's Test P-Value : < 2.2e-16       
                                          
#            Sensitivity : 0.9286          
#            Specificity : 0.4814          
#         Pos Pred Value : 0.8461          
#         Neg Pred Value : 0.6871          
#             Prevalence : 0.7543          
#         Detection Rate : 0.7005          
#   Detection Prevalence : 0.8279          
#      Balanced Accuracy : 0.7050          
                                          
 #      'Positive' Class :  <=50K          
                             
