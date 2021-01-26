library(plyr)

ToyotaCorolla <- read.csv ("ToyotaCorolla.csv")
View(ToyotaCorolla)
class(ToyotaCorolla)   #### data.frame
attach(ToyotaCorolla) 
Corolla<- cbind(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
Corolla<- as.data.frame(Corolla)
class(Corolla)
View(Corolla)
attach(Corolla)
summary(Corolla)

 #### Plots  ####
plot(Age_08_04, Price)
plot(KM, Price)
plot(HP, Price)
plot(cc, Price)
plot(Doors, Price)
plot(Gears, Price)
plot(Quarterly_Tax, Price)
plot(Weight, Price)
pairs(Corolla)
cor(Corolla)

# The Linear Model 
model.corolla<- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(model.corolla)   ## Adjusted R-squared:  0.863 

model.corolla_1 <-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight)
summary(model.corolla_1)   #### Adjusted R-squared:  0.863 

### Scatter plot matrix with Correlations inserted in graph
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
pairs(Corolla, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

### Partial Correlation matrix - Pure correlation between the variables
# install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Corolla))

# install.packages("mvinfluence")
library(mvinfluence)
library(CARS)

# Deletion Diagnostics for identifying influential variable
influence.measures(model.corolla)
influenceIndexPlot(model.corolla, id.n=3) # Index Plots of the influence measures
influencePlot(model.corolla, id.n=3)


## Regression after deleting the 81st observation, which is influential observation
model.corolla_1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla[-81,])
summary(model.corolla_1)   ### Adjusted R-squared:  0.8686

### Variance Inflation Factors is a formal way to check for collinearity
vif(model.corolla_1)  # VIF is > 10 => collinearity
plot(Age_08_04,KM, col="red",pch=20)
plot(HP,cc, col="red",pch=20)

layout(matrix(c(1,2,3,4),2,2))
plot(model.corolla_1)
avPlots(model.corolla_1, id.n=2, id.cex=0.7) # Added Variable Plots

# VIF & avPlots has given us an indication to delete 'WT' variable
model.final <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(model.final)   ###Adjusted R-squared:  0.863 
Price.Predict <- predict(model.corolla,interval="predict")

Pred_final <- predict(model.corolla)
Final <- cbind(Price,Pred_final,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
View(Final)


# Evaluate model LINE assumptions
plot(model.final)
qqPlot(model.final, id.n=5)

library("MASS")
stepAIC(model.final) # backward
