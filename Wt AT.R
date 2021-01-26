wc.at<- read.csv("wc-at.csv")

View(wc.at)
install.packages("lattice")
library("lattice")
?lattice

# Exploratory data analysis
summary(wc.at)
#aggregate(wc.at)

?dotplot


# Graphical exploration
dotplot(wc.at$Waist, main="Dot Plot of Waist Circumferences")
dotplot(wc.at$AT, main="Dot Plot of Adipose Tissue Areas")
boxplot(wc.at$Waist,col="dodgerblue4")
boxplot(wc.at$AT,col="red", horizontal = T)
?boxplot

hist(wc.at$Waist)
hist(wc.at$AT)

qqnorm(wc.at$Waist)
qqline(wc.at$Waist)

qqnorm(wc.at$AT)
qqline(wc.at$AT)

hist(wc.at$Waist, prob=TRUE)            # prob=TRUE for probabilities not counts
lines(density(wc.at$Waist))             # add a density estimate with defaults
lines(density(wc.at$Waist, adjust=2), lty="dotted")   # add another "smoother" density

hist(wc.at$AT, prob=TRUE)            # prob=TRUE for probabilities not counts
lines(density(wc.at$AT))             # add a density estimate with defaults
lines(density(wc.at$AT, adjust=2), lty="dotted")   # add another "smoother" density

#Scatter plot
plot(wc.at$Waist,wc.at$AT,main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="Waist Ciscumference", 
     ylab="Adipose Tissue area", pch=20)  # plot(x,y)

?plot
## alternate simple command
plot(wc.at$Waist,wc.at$AT)

attach(wc.at)

cor(Waist, AT)



reg <- lm(AT~Waist, data=wc.at) # Y ~ X
summary(reg)


confint(reg,level=0.95)
?confint

pred <- predict(reg,interval="predict")

pred <- as.data.frame(pred)

pred
View(pred)
??predict
 
cor(pred$fit, wc.at$AT)

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(AT~sqrt(Waist), data=wc.at)
summary(reg_sqrt)

confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

reg_log<-lm(AT~log(Waist), data=wc.at)
summary(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")



reg1<-lm(log(AT)~Waist + I(Waist*Waist), data=wc.at)
summary(reg1)
confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred<-predict(reg1,interval="predict")

pred<-as.data.frame(pred)
View(pred)




exp(pred$fit)

cor(exp(pred$fit),wc.at$AT)





reg_sqrt1<-lm(sqrt(AT)~Waist, data=wc.at)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
predict(reg_sqrt1,interval="predict")

reg_log1<-lm(log(AT)~Waist, data=wc.at)
summary(reg_log1)
confint(reg_log1,level=0.95)
predict(reg_log1,interval="predict")

