library(plyr)

Startup_50 <- read.csv ("50_Startups.csv")
View(Startup_50)
class(Startup_50)   #### data.frame

Startup_50$State <-as.factor(Startup_50$State)
attach(Startup_50)
Startup <- cbind(R.D.Spend,Administration,Marketing.Spend,State,Profit)

class(Startup)   #### matrix and array
View(Startup)
Startup <- as.data.frame(Startup)

summary(Startup)     # Explore the data

qqnorm(R.D.Spend)
qqline(R.D.Spend)
plot(R.D.Spend, Profit)# Plot relation ships between each X with Y
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)
## Or make a combined plot
pairs(Startup)   # Scatter plot for all pairs of variables

cor(Startup) # correlation matrix

#######The Linear Model
Model.Startup <- lm(Profit~R.D.Spend+Marketing.Spend+Administration+State)
summary(Model.Startup)

Model.Startups1 <- lm(Profit~R.D.Spend+log(Administration))
summary(Model.Startups1)

#######   Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Startup, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

###    Partial Correlation matrix
#install.packages("corpcor")
library(corpcor)

cor2pcor(cor(Startup))


# Diagnostic Plots
#install.packages("CARS")
library(CARS)
plot(Model.Startup)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance

qqPlot(Model.Startup, id.n=5) # QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential variable
#install.packages("mvinfluence")
library(mvinfluence)
influence.measures(Model.Startup)
influenceIndexPlot(Model.Startup, id.n=3) # Index Plots of the influence measures
influencePlot(Model.Startup, id.n=3) # A user friendly representation of the above

## Regression after deleting the 49th and 50th observation
Model.Startup_log<-lm(Profit~R.D.Spend+log(Administration)+Marketing.Spend+log(State),data=Startup[-c(49,50),]) 

summary(Model.Startup_log)    #Adjusted R2 Value = 0.9591  
confint(Model.Startup_log,level=0.95)
predict(Model.Startup_log,interval="predict")


Model.Startup_fin1<-lm(Profit~R.D.Spend+Administration+Marketing.Spend+State,data=Startup[-c(49,50),])
summary(Model.Startup_fin1) # Adjusted R2 Value is 0.9593

##########Exponential model 
Model.Startup_exp<-lm(log(Profit)~R.D.Spend+Administration+Marketing.Spend+State,data=Startup[-c(49,50),])
summary(Model.Startup_exp)  #Adjusted R2 Value is 0.9182

Model.Startup_exp1<-lm(log(Profit)~R.D.Spend+Marketing.Spend,data=Startup[-c(49,50),])
summary(Model.Startup_exp1)    ##Adjusted R-squared:  0.9187

##########Quad Model
Model.Startup_Quad <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+Administration+I(Administration^2)
                          +Marketing.Spend+I(Marketing.Spend^2)+State+I(State^2),data=Startup[-c(49,50),])
summary(Model.Startup_Quad)  #Adjusted R2 value is 0.9567
confint(Model.Startup_Quad,level=0.95)
predict(Model.Startup_Quad,interval="predict")

Model.Startup_Quad1 <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+Marketing.Spend+I(Marketing.Spend^2)
                           ,data=Startup[-c(49,50),])
summary(Model.Startup_Quad1)  #Adjusted R2 value is 0.9585

####### Polynomial Model
Model.Startup_Poly <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+I(R.D.Spend^3)+
                            Administration+I(Administration^2)+I(Administration^3)+
                            Marketing.Spend+I(Marketing.Spend^2)+I(Marketing.Spend^3)+
                            State+I(State^2)+I(State^3),data=Startup[-c(49,50),])
summary(Model.Startup_Poly) #Adjusted R Square Value is 0.9569

Model.Startup_Poly1 <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+I(R.D.Spend^3)+
                             Marketing.Spend+I(Marketing.Spend^2)+I(Marketing.Spend^3)
                           ,data=Startup[-c(49,50),])
summary(Model.Startup_Poly1) #Adjusted R Square Value is 0.9601
### Variance Inflation Factors
vif(Model.Startup_log)  # VIF is > 10 => collinearity

#### Added Variable Plots ######
avPlots(Model.Startup_log, id.n=2, id.cex=0.7 ,col="green")

####### Final Model
Model.final<-lm(Profit~R.D.Spend+log(Administration)+Marketing.Spend+
                 log(State),data=Startup[-c(49,50),])

summary(Model.final) #Adjusted R2 Value = 0.9591 
Predict_profit <- predict(Model.final,interval="predict")

Final <- cbind(Startup$R.D.Spend,Startup$Administration,Startup$Marketing.Spend,
               Startup$State,Startup$Profit,Predict_profit)
View(Final)
plot(Model.final)
qqPlot(Model.final, id.n=5) 

#install.packages("MASS")

library("MASS")
stepAIC(Model.final) # backward

plot(model.car)

model.final <- lm(MPG~VOL+HP+SP, data=Cars)
summary(model.final)


model.final1 <- lm(MPG~VOL+HP+SP, data=Cars[-77,])
summary(model.final1)

avPlots(model.final1, id.n=2, id.cex=0.8, col="red")

vif(model.final1)


# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.