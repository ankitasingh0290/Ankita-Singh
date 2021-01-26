
library(neuralnet)

forestfire<-read.csv("forestfires.csv")
View(forestfire)
str(forestfire)

#  Normalising  the data
Normal <- function(x){return((x-min(x))/(max(x)-min(x)))}
Normal(c(1,5,6,8,-9))
View(forestfire)
str(forestfire)
forestfire$month <- as.numeric(as.factor(forestfire$month))
forestfire$day<- as.numeric(as.factor(forestfire$day))
forestfire$size_category <- as.numeric(as.factor(forestfire$size_category))


View(forestfire)
table(forestfire$size)
summary(forestfire)

#applying  Norm function to the "forestfire" data .
forest_Norm <- as.data.frame(lapply(forestfire,Normal))
View(forest_Norm)
str(forest_Norm)
summary(forest_Norm)

#   data partition for area column

library(caret)
Trainlocal <- createDataPartition(forest_Norm$area, p=.70, list=F)
Train <- forest_Norm[Trainlocal,]
Test <- forest_Norm[-Trainlocal,]
View(Train)
View(Test)

library(nnet)
library(neuralnet)

#### modeling neural network #####

Model <- neuralnet(area~., data = Test)
str(Model)
plot(Model)
Predict <- compute(Model, Test[1:30])
Area_burnt <- Predict$net.result
Predict$neurons
cor(Area_burnt, Test$area)
