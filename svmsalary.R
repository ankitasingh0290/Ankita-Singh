#install.packages("caret")
#install.packages("kernlab")
#install.packages("psych")
library("e1071")
library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)

# TEST DATA

salarytest<-read.csv("SalaryData_Test(1).csv")
#View(salarytest)
class(salarytest)
str(salarytest)
salarytest$educationno <- as.factor(salarytest$educationno)
salarytest$workclass<-as.factor(salarytest$workclass)
salarytest$maritalstatus<-as.factor(salarytest$maritalstatus)
salarytest$occupation<-as.factor(salarytest$occupation)
salarytest$relationship<-as.factor(salarytest$relationship)
salarytest$race <- as.factor(salarytest$race)
salarytest$sex <-as.factor(salarytest$sex)
salarytest$native <-as.factor(salarytest$native)
salarytest$Salary <-as.factor(salarytest$Salary)
class(salarytest)

# Data train
salarytrain <- read.csv("SalaryData_Train(1).csv")
str(salarytrain)

#View(salarytrain)
salarytrain$educationno <- as.factor(salarytrain$educationno)
salarytrain$workclass<-as.factor(salarytrain$workclass)
salarytrain$maritalstatus<-as.factor(salarytrain$maritalstatus)
salarytrain$occupation<-as.factor(salarytrain$occupation)
salarytrain$relationship<-as.factor(salarytrain$relationship)
salarytrain$race <- as.factor(salarytrain$race)
salarytrain$sex <-as.factor(salarytrain$sex)
salarytrain$native <-as.factor(salarytrain$native)
salarytrain$Salary <-as.factor(salarytrain$Salary)
class(salarytrain)
######  Visualization 
# Plot and ggplot 
ggplot(data=salarytrain,aes(x=salarytrain$Salary, y = salarytrain$age, fill = salarytrain$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
plot(salarytrain$workclass,salarytrain$Salary)
plot(salarytrain$education,salarytrain$Salary)
plot(salarytrain$educationno,salarytrain$Salary)
plot(salarytrain$maritalstatus,salarytrain$Salary)
plot(salarytrain$occupation,salarytrain$Salary)
plot(salarytrain$relationship,salarytrain$Salary)
plot(salarytrain$race,salarytrain$Salary)
plot(salarytrain$sex,salarytrain$Salary)
plot(salarytrain$native,salarytrain$Salary)
ggplot(data=salarytrain,aes(x=salarytrain$Salary, y = salarytrain$capitalgain, fill =salarytrain$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=salarytrain,aes(x=salarytrain$Salary, y = salarytrain$capitalloss, fill = salarytrain$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=salarytrain,aes(x=salarytrain$Salary, y = salarytrain$hoursperweek, fill = salarytrain$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

######   Density Plot 

ggplot(data=salarytrain,aes(x = salarytrain$age, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Age - Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$workclass, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Workclass Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$education, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("education Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$educationno, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("educationno Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$maritalstatus, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("maritalstatus Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$occupation, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("occupation Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$sex, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("sex Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$relationship, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Relationship Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$race, fill =salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Race Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$capitalgain, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Capitalgain Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$capitalloss, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Capitalloss Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$hoursperweek, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Hoursperweek Density Plot")
ggplot(data=salarytrain,aes(x = salarytrain$native, fill = salarytrain$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("native Density Plot")

# Building different model 

mo_van<-ksvm(factor(salarytrain$Salary)~., 
             data= salarytrain, kernel = "vanilladot")
mo_van

#Evaluating model
Salary_prediction <- predict(mo_van, salarytest)

table(Salary_prediction,salarytest$Salary)

agreement <- Salary_prediction == salarytest$Salary

table(agreement)
#2313 12747

prop.table(table(agreement))
#FALSE      TRUE 
#0.1535857 0.8464143
# kernel = rfdot 
mo_rfdot<-ksvm(factor(salarytrain$Salary)~., 
                  data= salarytrain,kernel = "rbfdot")
pred_rfdot<-predict(mo_rfdot,newdata=salarytest)
mean(pred_rfdot==salarytest$Salary) # 85.19

# kernel = vanilladot
mo_van<-ksvm(factor(salarytrain$Salary)~., 
                    data= salarytrain,kernel = "vanilladot")
pred_vanilla<-predict(mo_van,newdata=salarytest)
mean(pred_vanilla==salarytest$Salary) # 84.64
#0.8464143
