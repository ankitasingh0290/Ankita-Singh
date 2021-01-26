library(readr)
deltime<- read_csv("delivery_time.csv")
View(deltime)

# Exploratory data analysis
summary(deltime)

#Scatter plot
plot(deltime$`Delivery Time`, deltime$`Sorting Time`)  # plot(X,Y)

attach(deltime)


#Correlation Coefficient (r)
cor(`Delivery Time`, `Sorting Time`)             # cor(X,Y)

# Simple Linear Regression model
reg_2<- lm(`Sorting Time` ~ `Delivery Time`) # lm(Y ~ X)

summary(reg_2)

pred <- predict(reg_2)

reg_2$residuals
sum(reg_2$residuals)

mean(reg_2$residuals)
sqrt(sum(reg_2$residuals^2)/nrow(deltime))  #RMSE

sqrt(mean(reg_2$residuals^2))

confint(reg_2,level=0.95)
predict(reg_2,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = deltime, aes(x = `Delivery Time`, y = `Sorting Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = deltime, aes(x=`Delivery Time`, y=pred))



########################
####################

# Logrithamic Model

# x = log(Weight gained ); y = calorie consumed

plot(log(`Delivery Time`), `Sorting Time`)
cor(log(`Delivery Time`), `Sorting Time`)

reg_log_2 <- lm(`Sorting Time` ~ log(`Delivery Time`))   # lm(Y ~ X)

summary(reg_log_2)
predict(reg_log_2)

reg_log_2$residuals
sqrt(sum(reg_log_2$residuals^2)/nrow(deltime))  #RMSE

confint(reg_log_2,level=0.95)
predict(reg_log_2,interval="confidence")

######################

# Exponential Model


plot(`Delivery Time`, log(`Sorting Time`))

cor(`Delivery Time`, log(`Sorting Time`))

reg_exp_2 <- lm(log(`Sorting Time`) ~ `Delivery Time`)  #lm(log(Y) ~ X)

summary(reg_exp_2)

reg_exp_2$residuals

sqrt(mean(reg_exp_2$residuals^2))

logdel <- predict(reg_exp_2)
del <- exp(logdel)

error_2 = deltime$`Sorting Time` - del
error_2

sqrt(sum(error_2^2)/nrow(deltime))  #RMSE

confint(reg_exp_2,level=0.95)
predict(reg_exp_2,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(`Delivery Time`, `Sorting Time`)
plot(`Delivery Time`*`Delivery Time`, `Sorting Time`)

cor(`Delivery Time`*`Delivery Time`, `Sorting Time`)

plot(`Delivery Time`*`Delivery Time`, log(`Sorting Time`))

cor(`Delivery Time`, log(`Sorting Time`))


cor(`Delivery Time`*`Delivery Time`, log(`Sorting Time`))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree_2<- lm(log(`Sorting Time`) ~ `Delivery Time`+ I(`Delivery Time`*`Delivery Time`))

summary(reg2degree_2)

logpol_2 <- predict(reg2degree_2)
expy_2 <- exp(logpol_2)

err_2 = deltime$`Sorting Time` - expy_2

sqrt(sum(err_2^2)/nrow(deltime))  #RMSE

confint(reg2degree_2,level=0.95)
predict(reg2degree_2,interval="confidence")

# visualization
ggplot(data = deltime, aes(x = `Delivery Time` + I(`Delivery Time`^2), y = log(`Sorting Time`))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = deltime, aes(x=`Delivery Time`+I(`Delivery Time`^2), y=logpol_2))


##############################
#  Polynomial model with 3 degree

reg3degree_2<-lm(log(`Sorting Time`)~`Delivery Time` + I(`Delivery Time`*`Delivery Time`) + I(`Delivery Time`*`Delivery Time`*`Delivery Time`))

summary(reg3degree_2)
logpol_4 <- predict(reg3degree_2)
expy_4 <- exp(logpol_4)


# visualization
ggplot(data = deltime, aes(x = `Delivery Time` + I(`Delivery Time`^2) + I(`Delivery Time`^3), y = `Sorting Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = deltime, aes(x=`Delivery Time`+I(`Delivery Time`^2)+I(`Delivery Time`^3), y=expy_4))

################################
