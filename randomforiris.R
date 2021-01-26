library(randomForest)
library(MASS)
library(caret)
set.seed(123)
data(iris)
View(iris)   #### total 150 
iris_setosa<-iris[iris$Species=="setosa",] 
#iris_setosa
iris_versicolor <- iris[iris$Species=="versicolor",]
iris_virginica <- iris[iris$Species=="virginica",] 

#### dividing in training and testing data ##########
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

######### Calling Randomforest to model ######
rf1 <- randomForest(Species~., data=iris_train)
rf1        #### OOB error rate 4%
attributes(rf1)
###3Prediction and confusion matrix on test and train data  ######

pred1 <- predict(rf1, iris_train)
head(pred1)
head(iris_train$Species)
confusionMatrix(pred1, iris_train$Species) #accuracy 100% on train data
pred2 <- predict(rf1, iris_test)
confusionMatrix(pred2, iris_test$Species) #acuracy 94.6% on test data
plot(rf1)   ## Error rate view
#### Tune random forest model m try####

tuneRanFor <- tuneRF(iris_train[,-5], iris_train[,5], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)

## mtry = 2  OOB error = 4% 
## mtry = 4     OOB error = 4% 
## mtry = 1     OOB error = 4% 
plot(tuneRanFor)

####After tuning  mtry=2 and oob 4%  ##

rf2 <- randomForest(Species~., data=iris_train, ntree = 140, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf2 
### Using tune rf2 model for predicting data#
pred3 <- predict(rf2, iris_train)
confusionMatrix(pred3, iris_train$Species) # accuracy 100% for train data
pred4 <- predict(rf2, iris_test)
confusionMatrix(pred4, iris_test$Species)  # accuracy 94.6% for test data
hist(treesize(rf2), main = "No of Nodes for the trees", col = "pink")

#####  variable importance and its plot used in random forest ####
varImpPlot(rf2)
varImpPlot(rf2 ,Sort = T, n.var = 4, main = "Top 4 -Variable Importance")
importance(rf2)

#### partial dependence plot###

partialPlot(rf2, iris_train, Petal.Length, "versicolor")
partialPlot(rf2, iris_train, Petal.Length, "setosa")
partialPlot(rf2, iris_train, Petal.Length, "virginica")

###### Extracting single tree from forest  #######
onetree <- getTree(rf2, 1, labelVar = TRUE)
onetree

### Multidimension scaling plot####
MDSplot(rf2, iris$Species)
