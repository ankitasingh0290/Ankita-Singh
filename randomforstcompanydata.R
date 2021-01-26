library(randomForest)
library(MASS)
library(caret)
set.seed(123)
companydata<-read.csv("Company_Data.csv")
hist(companydata$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))
highsales = ifelse(companydata$Sales<9, "No", "Yes")  # if > 8 then high sales otherwise Low
CD = data.frame(companydata[2:11], highsales)
CD$ShelveLoc<-as.factor(CD$ShelveLoc)
CD$Urban<-as.factor(CD$Urban)
CD$US<- as.factor(CD$US)
CD$highsales<- as.factor(CD$highsales)
str(CD)
table(CD$highsales)
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]
set.seed(213)
rfmodel <- randomForest(highsales~., data=train)
rfmodel    ##### OOB error rate 18.25%
attributes(rfmodel)
pred1 <- predict(rfmodel, train)
head(pred1)
head(train$highsales)
confusionMatrix(pred1, train$highsales)   # accuracy 100%
# Prediction with test data - Test Data 
pred2 <- predict(rfmodel, test)
confusionMatrix(pred2, test$highsales) ###acuracy 84%
plot(rfmodel)
tune1 <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
plot(tune1)
rforest1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rforest1
pred1 <- predict(rforest1, train)
confusionMatrix(pred1, train$highsales) ##accuracy 100%
pred2 <- predict(rforest1, test)
confusionMatrix(pred2, test$highsales)  ### accuracy 83%
hist(treesize(rf1), main = "No of Nodes for the trees", col = "yellow")
varImpPlot(rforest1)
varImpPlot(rforest1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
partialPlot(rforest1, train, Price, "Yes")
getTree(rforest1,1, labelVar = TRUE)
MDSplot(rforest1, CD$highsales)
