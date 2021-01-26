library(randomForest)
library(MASS)
library(caret)
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
table(FC$Risky_Good) 
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
set.seed(213)
rforest<- randomForest(Risky_Good~., data=train)
rforest   #### OOB error rate 0.47%
attributes(rforest)
pred1 <- predict(rforest, train)
head(pred1)
head(train$Risky_Good)
confusionMatrix(pred1, train$Risky_Good) # accuracy 100 %
pred2 <- predict(rforest, test)
confusionMatrix(pred2, test$Risky_Good) ## accuracy 100%
plot(rforest)
tune_model <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
                     trace = TRUE, improve = 0.05)
randf1 <- randomForest(Risky_Good~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                       proximity = TRUE)
randf1
pred1 <- predict(randf1, train)
confusionMatrix(pred1, train$Risky_Good)  ##accuracy 100%
pred2 <- predict(randf1, test)
confusionMatrix(pred2, test$Risky_Good)    ###accuracy 100%
hist(treesize(randf1), main = "Number of Nodes for the trees", col = "brown")
varImpPlot(randf1)
varImpPlot(randf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
importance(randf1)
varUsed(rforest)   
partialPlot(randf1, train, Taxable.Income, "Good")
onetr1 <- getTree(randf1, 2, labelVar = TRUE)
MDSplot(randf1, FC$Risky_Good)
