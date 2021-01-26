#install.packages("forecast")
library(readr)
AirlineData <- read.csv("Airlines+Data.csv")
View(AirlineData) # Seasonality 12 months

# Pre Processing

# So creating 12 dummy variables 
X1 <- data.frame(outer(rep(month.abb,length =96), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X1) <- month.abb # Assigning month names 
#View(X1)
AirlineDataNew <- cbind(AirlineData,X1)
#View(AirlineDataNew)
colnames(AirlineDataNew)

# input t
AirlineDataNew["t"] <- c(1:96)
View(AirlineDataNew)

AirlineDataNew["log_Passengers"] <- log(AirlineDataNew["Passengers"])
AirlineDataNew["t_square"] <- AirlineDataNew["t"]*AirlineDataNew["t"]
View(AirlineDataNew)
## Preprocesing completed

attach(AirlineDataNew)
# partitioning
train <- AirlineDataNew[1:86,]
test <- AirlineDataNew[87:96,]

########################### LINEAR MODEL #############################

linear_model_l <- lm(Passengers ~ t, data = train)
summary(linear_model_l)      ###  Adjusted R-squared:  0.7987 
linear_pred_1 <- data.frame (predict (linear_model_l, interval='predict', newdata =test))
View(linear_pred_1)
rmse_linear_1 <- sqrt(mean((test$Passengers-linear_pred_1$fit)^2, na.rm = T))
rmse_linear_1    ###### 58.64315

######################### Exponential model #################################

expo_model_1 <- lm(log_Passengers~ t, data = train)
summary(expo_model_1)
expo_pred_1 <- data.frame(predict(expo_model_1, interval='predict', newdata = test))
rmse_expo_1 <- sqrt(mean((test$Passengers-exp(expo_pred_1$fit))^2, na.rm = T))
rmse_expo_1     ###### 49.90312

######################### Quadratic ####################################

Quad_model_1 <- lm(Passengers ~ t + t_square, data = train)
summary(Quad_model_1)
Quad_pred_1 <- data.frame(predict(Quad_model_1, interval='predict', newdata=test))
rmse_Quad_1 <- sqrt(mean((test$Passengers-Quad_pred_1$fit)^2, na.rm=T))
rmse_Quad_1   #### 53.91436

######################### Additive Seasonality #########################

sea_add_model_1 <- lm(Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea_add_model_1)
sea_add_pred_1 <- data.frame(predict(sea_add_model_1, newdata=test, interval = 'predict'))
rmse_sea_add_1 <- sqrt(mean((test$Passengers-sea_add_pred_1$fit)^2, na.rm = T))
rmse_sea_add_1      ###### 136.7901

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model_1 <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_Quad_model_1)
Add_sea_Quad_pred_1 <- data.frame(predict(Add_sea_Quad_model_1, interval='predict', newdata=test))
rmse_Add_sea_Quad_1 <- sqrt(mean((test$Passengers - Add_sea_Quad_pred_1$fit)^2, na.rm=T))
rmse_Add_sea_Quad_1   ##### 29.10455

######################## Multiplicative Seasonality #########################

multi_sea_model_1 <- lm(log_Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_model_1)
multi_sea_pred_1 <- data.frame(predict(multi_sea_model_1, newdata=test, interval='predict'))
rmse_multi_sea_1 <- sqrt(mean((test$Passengers-exp(multi_sea_pred_1$fit))^2, na.rm = T))
rmse_multi_sea_1    #####  144.3849


# Preparing table on model and it's RMSE values 

table_rmse_1 <- data.frame(c("rmse_linear_1","rmse_expo_1","rmse_Quad_1","rmse_sea_add_1","rmse_Add_sea_Quad_1","rmse_multi_sea_1"),c(rmse_linear_1,rmse_expo_1,rmse_Quad_1,rmse_sea_add_1,rmse_Add_sea_Quad_1,rmse_multi_sea_1))
colnames(table_rmse_1) <- c("model","RMSE")
View(table_rmse_1)

# Additive seasonality with Quadratic has least RMSE value

write.csv(AirlineDataNew, file="AirlineData_New.csv", row.names = F)

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final_1 <- lm(log_Passengers ~ t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = AirlineDataNew)
summary(Add_sea_Quad_model_final_1)   ### Adjusted R-squared:  95.88% 
new_model_pred<-data.frame(predict(Add_sea_Quad_model_1,newdata=AirlineDataNew,interval='predict'))
View(new_model_pred)
new_model_fin <- exp(Add_sea_Quad_model_final_1$fit)
View(new_model_fin)

plot(Add_sea_Quad_model_final_1)
pred_res<- predict(arima(log_Passengers,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(`Airlines+Data`$Month)
View(Month)

Final <- as.data.frame(cbind(Month,AirlineDataNew$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
#Final <- as.data.frame(Final)
plot(Final$Passengers,main = "ActualGraph", xlab="Passengers(Actual)", ylab="Months",
     col.axis="blue",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Passengers(Predicted)", ylab="Months",
     col.axis="red",type="s")
View(Final)

