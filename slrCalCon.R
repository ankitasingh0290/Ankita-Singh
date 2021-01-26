library(readr)
calcon <- read_csv("calories_consumed.csv")
View(calcon)

# Exploratory data analysis
summary(calcon)

#Scatter plot
plot(calcon$`Weight gained (grams)`, calcon$`Calories Consumed`)  # plot(X,Y)

attach(calcon)


#Correlation Coefficient (r)
cor(`Weight gained (grams)`, `Calories Consumed`)             # cor(X,Y)

# Simple Linear Regression model
reg_1<- lm(`Calories Consumed` ~ `Weight gained (grams)`) # lm(Y ~ X)

summary(reg_1)

pred <- predict(reg_1)

reg_1$residuals
sum(reg_1$residuals)

mean(reg_1$residuals)
sqrt(sum(reg_1$residuals^2)/nrow(calcon))  #RMSE

sqrt(mean(reg_1$residuals^2))

confint(reg_1,level=0.95)
predict(reg_1,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = calcon, aes(x = `Weight gained (grams)`, y = `Calories Consumed`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calcon, aes(x=`Weight gained (grams)`, y=pred))



########################
####################

# Logrithamic Model

# x = log(Weight gained ); y = calorie consumed

plot(log(`Weight gained (grams)`), `Calories Consumed`)
cor(log(`Weight gained (grams)`), `Calories Consumed`)

reg_log_1 <- lm(`Calories Consumed` ~ log(`Weight gained (grams)`))   # lm(Y ~ X)

summary(reg_log_1)
predict(reg_log_1)

reg_log_1$residuals
sqrt(sum(reg_log_1$residuals^2)/nrow(calcon))  #RMSE

confint(reg_log_1,level=0.95)
predict(reg_log_1,interval="confidence")

######################

# Exponential Model


plot(`Weight gained (grams)`, log(`Calories Consumed`))

cor(`Weight gained (grams)`, log(`Calories Consumed`))

reg_exp_1 <- lm(log(`Calories Consumed`) ~ `Weight gained (grams)`)  #lm(log(Y) ~ X)

summary(reg_exp_1)

reg_exp_1$residuals

sqrt(mean(reg_exp_1$residuals^2))

logcon <- predict(reg_exp_1)
con <- exp(logcon)

error_1 = calcon$`Calories Consumed` - con
error_1

sqrt(sum(error_1^2)/nrow(calcon))  #RMSE

confint(reg_exp_1,level=0.95)
predict(reg_exp_1,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(`Weight gained (grams)`, `Calories Consumed`)
plot(`Weight gained (grams)`*`Weight gained (grams)`, `Calories Consumed`)

cor(`Weight gained (grams)`*`Weight gained (grams)`, `Calories Consumed`)

plot(`Weight gained (grams)`*`Weight gained (grams)`, log(`Calories Consumed`))

cor(`Weight gained (grams)`, log(`Calories Consumed`))


cor(`Weight gained (grams)`*`Weight gained (grams)`, log(`Calories Consumed`))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree_1<- lm(log(`Calories Consumed`) ~ `Weight gained (grams)` + I(`Weight gained (grams)`*`Weight gained (grams)`))

summary(reg2degree_1)

logpol_1 <- predict(reg2degree_1)
expy_1 <- exp(logpol_1)

err_1 = calcon$`Calories Consumed` - expy_1

sqrt(sum(err_1^2)/nrow(calcon))  #RMSE

confint(reg2degree_1,level=0.95)
predict(reg2degree_1,interval="confidence")

# visualization
ggplot(data = calcon, aes(x = `Weight gained (grams)` + I(`Weight gained (grams)`^2), y = log(`Calories Consumed`))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calcon, aes(x=`Weight gained (grams)`+I(`Weight gained (grams)`^2), y=logpol_1))


##############################
#  Polynomial model with 3 degree

reg3degree_1<-lm(log(`Calories Consumed`)~`Weight gained (grams)` + I(`Weight gained (grams)`*`Weight gained (grams)`) + I(`Weight gained (grams)`*`Weight gained (grams)`*`Weight gained (grams)`))

summary(reg3degree_1)
logpol_3 <- predict(reg3degree_1)
expy_3 <- exp(logpol_3)


# visualization
ggplot(data = calcon, aes(x = `Weight gained (grams)` + I(`Weight gained (grams)`^2) + I(`Weight gained (grams)`^3), y = `Calories Consumed`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calcon, aes(x=`Weight gained (grams)`+I(`Weight gained (grams)`^2)+I(`Weight gained (grams)`^3), y=expy_3))

################################
