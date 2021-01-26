# CUTLET DATA
cutlet<-read.csv("Cutlets.csv")
View(cutlet)
attach(cutlet)
s1 <- c(`Unit.A`)
s2 <- c(`Unit.B`)
shapiro.test(s1)
shapiro.test(s2)

var.test(s1,s2)
t.test(Unit.A,Unit.B,conf.level = 0.95,var.equal = T)

#####Result######
#Unit.A and Unit.B
#t = 0.72287, df = 68, p-value = 0.4722
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # -0.09646454  0.20605311
#sample estimates:
#  mean of x mean of y 
#7.019091  6.964297 

# FALTOONS DATA
faltoon <- read.csv("Faltoons.csv")
View(faltoon)
attach(faltoon)
toon <- table(c(Weekdays,Weekend))
prop.test(toon)

######RESULT####
#toon, null probability 0.5
#X-squared = 71.401, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
# 0.6156672 0.6828721
#sample estimates:
 # p 
#0.65 


######   BUYERRATIO DATA############
library(MASS)
BuyerRatio <- read.csv("BuyerRatio.csv")
tab<- data.frame(East  = c(50,435), 
                West  = c(142,1523), 
                North=c(131,1356),
                South=c(70,750),
           row.names = c("males","Females"))
tab

chisq.test(tab)

#########RESULT##########


#LabTAT data
labtat<-read.csv("LabTAT.csv")
View(labtat)

attach(labtat)
s_1 <- c(`Laboratory.1`)
s_2 <- c(`Laboratory.2`)
s_3 <- c(`Laboratory.3`)
s_4 <- c(`Laboratory.4`)
shapiro.test(s_1)
shapiro.test(s_2)
shapiro.test(s_3)
shapiro.test(s_4)
var.test(s_1,s_2)
var.test(s_1,s_3)
var.test(s_1,s_4)
var.test(s_2,s_3)
var.test(s_2,s_4)
var.test(s_3,s_4)
data123 <- data.frame(labtat)
stacked123<- stack(data123)
stacked123
annova123 <- aov(values~ind,data = stacked123)
summary(annova)

######RESULT############
#Df Sum Sq Mean Sq F value Pr(>F)    
#ind           3  79979   26660   118.7 <2e-16 ***
#  Residuals   476 106905     225                   
#---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# costomer form

costomer<-read.csv("Costomer+OrderForm.csv")
View(costomer)
attach(costomer)
costomer$Phillippines[costomer$Phillippines=="Defective"] <- '0'
costomer$Phillippines[costomer$Phillippines=="Error Free"] <- '1'
costomer$Indonesia[costomer$Indonesia=="Defective"] <- '0'
costomer$Indonesia[costomer$Indonesia=="Error Free"] <- '1'
costomer$Malta[costomer$Malta=="Defective"] <- '0'
costomer$Malta[costomer$Malta=="Error Free"] <- '1'
costomer$India[costomer$India=="Defective"] <- '0'
costomer$India[costomer$India=="Error Free"] <- '1'
d1<-data.frame(costomer)
#d1
cos<-table(stack(d1))
cos
chisq.test(cos)

#data:  cos
#X-squared = 3.859, df = 3, p-value = 0.2771

