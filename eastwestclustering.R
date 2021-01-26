install.packages("factoextra")
install.packages("fpc")
install.packages("NbClust")

library(cluster)
library(factoextra)
library(fpc)
library(NbClust)

Eastwest <- read.csv("EastWestAirlines.csv")
str(Eastwest)
Eastwest$cc1_miles = ifelse(Eastwest$cc1_miles==1,2500,
                              ifelse(Eastwest$cc1_miles==2,7500,
                                     ifelse(Eastwest$cc1_miles==3,17500,
                                            ifelse(Eastwest$cc1_miles==4,32500,
                                                   ifelse(Eastwest$cc1_miles==5,50000,0)))))

Eastwest$cc2_miles = ifelse(Eastwest$cc2_miles==1,2500,
                              ifelse(Eastwest$cc2_miles==2,7500,
                                     ifelse(Eastwest$cc2_miles==3,17500,
                                            ifelse(Eastwest$cc2_miles==4,32500,
                                                   ifelse(Eastwest$cc2_miles==5,50000,0)))))
Eastwest$cc3_miles = ifelse(Eastwest$cc3_miles==1,2500,
                              ifelse(Eastwest$cc3_miles==2,7500,
                                     ifelse(Eastwest$cc3_miles==3,17500,
                                            ifelse(Eastwest$cc3_miles==4,32500,
                                                   ifelse(Eastwest$cc3_miles==5,50000,0)))))

mydatawd  <-Eastwest[,2:11]
mydata    <- data.frame(mydatawd, names = "cc1_miles", omit.constants=FALSE )
mydata    <- data.frame(mydatawd, names = "cc2_miles", omit.constants=FALSE )
mydata    <- data.frame(mydatawd, names = "cc3_miles", omit.constants=FALSE )

my_data   <- scale(mydatawd)
d         <- dist(my_data, method = "euclidean")
res.hc    <- hclust(d, method = "ward.D2" )
Dend1   <- as.dendrogram(res.hc)

data123 = scale(Eastwest)
fit123 <- hclust(data123, method="complete")
fit123 <- as.dendrogram(fit123)
cd = color_branches(fit,k=3)

d123 <- dist(data123[,2:11], method = "euclidean") 

fit123 <- hclust(d123, method="complete")
fit123 <- as.dendrogram(fit123)
fit123 <- hclust(d123,)
fit123 <- as.dendrogram(fit123)

plot(fit123)
groups <- cutree(fit123, k=3)
groups
table(groups)
rect.hclust(fit, k=3, border="red")
membership123<-as.matrix(groups)
membership123
finallewd <- data.frame(Eastwest,membership123)

finallewd
write.csv(finallewd, file="finallewd.csv")

getwd()





# k means
data123 = scale(Eastwest)
km.3 <- hclust(data123[,2:11], "kmeans", k = 3, nstart = 25, graph = TRUE)
?eclust






