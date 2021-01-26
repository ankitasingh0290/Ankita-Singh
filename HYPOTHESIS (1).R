# CUTLET DATA
cutlet<-read.csv("Cutlets.csv")

attach(cutlet)
s1 <- c(`Unit.A`)
s2 <- c(`Unit.B`)
shapiro.test(s1)
shapiro.test(s2)

var.test(s1,s2)
t.test(Unit.A,Unit.B,conf.level = 0.95,var.equal = T)

# FALTOONS DATA
faltoon <- read.csv("Faltoons.csv")
View(faltoon)
attach(faltoon)
toon <- table(c(Weekdays,Weekend))
prop.test(toon)


library(MASS)
BuyerRatio <- read.csv("BuyerRatio.csv")
tab<- data.frame(East  = c(50,435), 
                West  = c(142,1523), 
                North=c(131,1356),
                South=c(70,750),
           row.names = c("males","Females"))
tab

chisq.test(tab)


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

# costomer form

costomer<-read.csv("Costomer+OrderForm.csv")
View(costomer)
attach(costomer)
cos<-data.frame(costomer)
cos
t_1<- table(Phillippines,Indonesia)
t_1
t_2<-table(Malta,India)
t_2
chisq.test(t_1)

