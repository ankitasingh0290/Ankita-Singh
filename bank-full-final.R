#y=whether the client has subscribed term deposit or not

#install.packages("PerformanceAnalytics")
#install.packages("RColorBrewer")
#install.packages("InformationValue")
#install.packages("pscl")
#install.packages("Amelia")
#install.packages("VIM")
library(PerformanceAnalytics)
library(corrplot)                                 
library(RColorBrewer)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(psych)
library(corrplot)
library(InformationValue)
library(caTools)
library(pscl)
library(pROC)
library(ROCR)
library(VIM)
library(Amelia)


banks <- read.csv("bank-full.csv",sep=";")
#View(banks)
class(banks)
summary(banks)
head(banks)
str(banks)

attach(banks)

###### EDA ######

##find mode to get an idea of highest number of occurrences

mode <- function(y){
  uv <- unique(y)
  uv[which.max(tabulate(match(y,uv)))]
}
head(banks,4)
colnames(banks)

mode(job)
mode(age)
mode(balance)
hist(balance)

######Age distribution vs marital status that subscribes term deposit
plot1 <- ggplot(banks, aes(x=age, fill=marital)) +  geom_histogram(binwidth = 2, alpha=0.7) +
  facet_grid(cols = vars(y)) +expand_limits(x=c(0,100)) +scale_x_continuous(breaks = seq(0,100,10)) +
  ggtitle("Age Distribution by Marital Status")

plot1    #term deposit are mostly married

plotgg=ggplot(banks)
plot2 <-plotgg + geom_histogram(aes(x=age),fill="red",colour="snow2",binwidth= 5) + ggtitle("age distribution") 
        + ylab("count") +  xlab("age")
plot2
plotwithmean1 <- plot2 + geom_vline(aes(xintercept=mean(age),color = "pink"))
plotwithmean1

plotwithmean2 <- plotwithmean1 + scale_x_continuous(breaks= seq(5,100,10))+theme(legend.position = "none")

plotwithmean2

#hence age between 35-45 are the most for term deposits.

boxplot <- plotgg + geom_boxplot(aes(x="",y=age))#RIGHT skewed
grid.arrange(plot2,boxplot,ncol=2)


# #The bulk of clients are between the ages of 33 (1st Quartile) and 48 (3rd Quartile) with mean lying on 41 
#visualized on the histogram with red vertical line.
#Boxplot of age describes essentially the same statistics but we can see outliers above the age of 65


#data cleaning we'll use dplyr and arranging

bankcl <- filter(banks,2e-16>=0.5)
bankcl
###to jumble the data and see the pattern of samples we'll use sample
sample <- sample(1:nrow(banks),20,replace=FALSE)
sample(banks)

levels(banks$job)
plot(banks$job)
#we can see that admin are most.

pairs.panels(banks[,1:4,17])
sum(duplicated(banks))  ####check for duplicate rows
sum(!complete.cases(banks))   #### #check for missing data in rows
all.empty=rowSums(is.na(banks))==ncol(banks) ####rows with completely missing values in all columns
sum(all.empty)
sapply(banks,function(x) sum(is.na(x))) ###columns with missing values

#instead of seeing each column for missing value,we can see using graph also (VIM)
summary(aggr(banks))
missmap(banks,main='missing values',col=c('snow2','pink')) ##another one is missmap to create a map of the missing value in data

# now data has been  cleaned

#we need to check prop. of events with non events
table(banks$y)  ##no   yes 
                #39922  5289 
banks$y <- ifelse(banks$y=='yes',1,0)

for (i in 1:nrow(banks)){
  if (age[i]<20){
    print='Teenagers'
  } else if (age[i]<35 & age[i] > 19){
    print="Young adults"
  } else if (age[i] < 60 & age[i] > 34){
    print="aDults"
  } else if (age[i] > 70){
    print="Senior"
  }
}

#splitting data set into training and test data
set.seed(123)
split <- sample.split(banks$y,SplitRatio=0.8)
trainset <- subset(banks,split=="TRUE")
testset <- subset(banks,split=="FALSE")

table(trainset$y)  #  0     1 
                   #31938  4231 

#scaling numeric columns
trainsetscale <- scale(trainset[c(1,6,10,12,13)])
testsetscale <- scale(testset[c(1,6,10,12,13)])

# logistic FITTING MODEL
logit <- glm( y ~.,family= binomial,data = trainset)
summary(logit)

#with significant variables
final <- trainset[,c(1,2,3,4,5,6,7,8,12,15,16,17)]
logit1 <- glm(formula=y~.,family= binomial, data = final)
summary(logit1)
anova(logit1,test="Chisq")
plot(logit)
exp(coef(logit))
table(trainset$y)
anova(logit, test="Chisq")

pR2(logit)#pseudo r2 in logistics.closer to zero is good
pR2(logit1)


#confusion matrix table
predict <- predict(logit,type=c("response"),newdata=testset[-17])
predict

#to get prediction probability bound between 0 nd 1,we'll use plogis
predict <- plogis(predict(logit1,testset[-17]))
predict


predict2 <- predict(logit,type=c("response"),newdata=trainset[-17])
predict2
plogis <- plogis(predict(logit,testset[-17]))
plogis

#computing the optimal score that minimizes the misclassification error for the above model

optimal <- optimalCutoff(testset$y,plogis)
optimal
optimal2 <- optimalCutoff(testset$y,predict)
optimal2
probo <- predict>0.5
table(probo)
probo2 <- predict2>0.5
table(probo2)
confusion<-table(predict>0.5,testset$y)
#misclassification error
misClassError(testset$y,plogis,threshold="optimal")
#roc-auc curve

plotROC(testset$y,plogis)

#concordance=the higher the better-true>false
Concordance(testset$y,plogis)

#model accuracy

accuracy=sum(diag(confusion)/sum(confusion))
accuracy
Error <- 1-accuracy
Error

#confusion matrix
confusionMatrix(testset$y,plogis)

#plogis has good results that means taking all variables best fits  the model.









