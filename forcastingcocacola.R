library(forecast)
library(readxl)
C.cola <- read_excel ("CocaCola_Sales_Rawdata.xlsx") 
View(C.cola) # Quarterly 4 months 
plot(C.cola$Sales,type = "line")
plot(C.cola$Sales,type="o")
Q1 <-  ifelse(grepl("Q1",C.cola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",C.cola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",C.cola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",C.cola$Quarter),'1','0') # Creating 12 dummy variables 

Cocacola<-cbind(C.cola,Q1,Q2,Q3,Q4)
View(Cocacola)
colnames(Cocacola)
Cocacola["t"]<- 1:42
View(Cocacola)
Cocacola["log_Sales"]<-log(Cocacola["Sales"])
Cocacola["t_square"]<-Cocacola["t"]*Cocacola["t"]
attach(Cocacola)

train<-Cocacola[1:36,]

test<-Cocacola[37:42,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)  ###Adjusted R-squared:  0.7922 
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 644.0188

######################### Exponential model #################################

expo_model<-lm(log_Sales ~ t,data=train)
summary(expo_model)  ###Adjusted R-squared:  0.8017 
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 526.7673

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)  ### Adjusted R-squared:  0.8596 
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad   ### 485.1407

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)  ##Adjusted R-squared:  0.03346 
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add   ### 1895.559

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)  ###Adjusted R-squared:  0.8761
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear   ###555.3404

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)  ###Adjusted R-squared:  0.9549 
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 283.062

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)  ####Adjusted R-squared:  0.05006 
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea   #### 1980.534

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model)  ###Adjusted R-squared:  0.8986 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea  #### 323.2128

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive Seasonality with Quadratic trend  has least RMSE value 283.0620

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=Cocacola)
new_model_pred<-data.frame(predict(new_model,newdata=Cocacola,interval='predict'))
new_model_fin <- new_model$fit
View(new_model_fin)
Quarter <- as.data.frame(Cocacola$Quarter)

Final <- as.data.frame(cbind(Quarter,Cocacola$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
View(Final)
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 
