#install.packages("caret")
#install.packages("kernlab")
library("e1071")
library(kernlab)
library(caret)
library(plyr)
forestfire<-read.csv("forestfires.csv")
#View(forestfire)
#class(forestfire)
str(forestfire)
hist(forestfire$area)
rug(forestfire$area)
forestfire1 <- mutate(forestfire, y = log(area + 1))  # default is to the base e, y is lower case
hist(forestfire1$y)
summary(forestfire)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfire$temp = normalize(forestfire$temp)
forestfire$RH   = normalize(forestfire$RH)
forestfire$wind = normalize(forestfire$wind)
forestfire$rain = normalize(forestfire$rain)
attach(forestfire)
sum(forestfire$area < 5) 
sum(forestfire$area >= 5)
forestfire$size <- NULL
forestfire$size <- factor(ifelse(forestfire$area < 5, 1, 0),
                          labels = c("small", "large"))
train <- sample(x = nrow(forestfire), size = 400, replace = FALSE)                           labels = c("small", "large")
#set.seed(123)             
ind <- sample(2, nrow(forestfire), replace = TRUE, prob = c(0.7,0.3))

# MODEL VANILLADOT
m.van<-ksvm(size~temp+rain+wind+RH, 
                    data = forestfire[train, ],kernel = "vanilladot")  
m.van


#MODEL POLYDOT


m.poly <- ksvm(size ~ temp + RH + wind + rain,
               data = forestfire[train, ],
               kernel = "polydot", C = 1)
m.poly


# model RBDOT

m.rad <- ksvm(size ~ temp + RH + wind + rain,
              data = forestfire[train, ],
              kernel = "rbfdot", C = 1)
m.rad

m.tan <- ksvm(size ~ temp + RH + wind + rain,
              data = forestfire[train, ],
              kernel = "tanhdot", C = 1)
m.tan


#system.time(predict(m.rad,x))

pred <- predict(m.rad, newdata = forestfire[-train, ], type = "response")

table(pred, forestfire[-train,"size"])
confusionMatrix (table(pred, forestfire[-train, "size"]), positive = "small")
#76% accuracy
