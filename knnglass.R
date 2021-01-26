install.packages("pROC")
install.packages("mlbench")
library(caret)
library(pROC)
library(mlbench)
library(lattice)
glass <- read.csv("glass.csv")
glass$Type[glass$Type==1] <- 'Type1'
glass$Type[glass$Type==2] <- 'Type2'
glass$Type[glass$Type==3] <- 'Type3'
glass$Type[glass$Type==4] <- 'Type4'
glass$Type[glass$Type==5] <- 'Type5'
glass$Type[glass$Type==6] <- 'Type6'
glass$Type[glass$Type==7] <- 'Type7'
str(glass)
glass$Type <- as.factor(glass$Type) 
View(glass)
set.seed(123)
ind <- sample(2,nrow(glass), replace = T, prob = c(0.7,0.3))
train <- glass[ind==1,]
test <- glass[ind==2,]
#### KNN model######


trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3)

set.seed(222)

fit <- train(Type ~., data = train, method = 'knn', tuneLength = 20,
             trControl = trcontrol, preProc = c("center","scale"))
fit # optimum value of k is 5
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = test )
confusionMatrix(pred, test$Type)# 68% accuracy
