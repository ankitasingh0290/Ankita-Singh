#Perform Clustering for the crime data and identify the number of clusters 
#formed and draw inferences.

#Data Description:
 # Murder -- Muder rates in different places of United States
#Assualt- Assualt rate in different places of United States
#UrbanPop - urban population in different places of United States
#Rape - Rape rate in different places of United States

crimedata1 <- read.csv("crime_data.csv")
View(crimedata1)
normalized_crimedata <- scale(crimedata1[,2:4])
d0 <- dist(normalized_crimedata, method = "euclidean") ## Euclidean distance method
fit <- hclust(d, method="complete")
plot(fit)
plot(fit, hang=-1)
groups <- cutree(fit, k=3)
rect.hclust(fit, k=3, border="red")
membership_1<-as.matrix(groups)
finally <- data.frame(crimedata1, membership_1)
finally1 <- finally[,c(ncol(finally),1:(ncol(finally)-1))]
finally1
write.csv(finally1, file="finally1.csv")

getwd()

#### Another method Manhattan

d1 <- dist(normalized_crimedata, method = "manhattan") 
fit1 <- hclust(d, method="single")
plot(fit1)
plot(fit1, hang=+1)
groups <- cutree(fit1, k=3)
rect.hclust(fit1, k=3, border="red")
membership_2<-as.matrix(groups)
fin <- data.frame(crimedata1, membership_2)
fin1 <- fin[,c(ncol(fin),1:(ncol(fin)-1))]
fin1

 #### K Means and elbow curve

crime0 <- na.omit(USArrests)
View(crime0)
crime <- data.matrix (crime0)
View(crime)
str(crime)
cl <- kmeans(crime, 5)
class(cl)
str(cl)
kmeans.k <- function(crime, k){
  km = kmeans(crime, k)
  return (km$tot.withinss)
}
kmeans.k(crime,5) ###  28240.23
kmeans.k(crime,10)  ###  12022.66
kmeans.dis <- function(crime, maxk){
   dis=(nrow(crime)-1)*sum(apply(crime,2,var))
   dis[2:maxk]=sapply (2:maxk, kmeans.k, crime=crime)
   return(dis)
}
maxk = 10
dis = kmeans.dis(crime, maxk);
plot(1:maxk, dis, type='b', xlab="Number of Clusters",
        ylab="Distortion",
       col="blue")
library(animation)
cl.ani<- kmeans.ani(crime, 4)
cl.ani$centers
