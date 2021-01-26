#Perform clustering (Both hierarchical and K means clustering)
#for the airlines data to obtain optimum number of clusters. 
#Draw the inferences from the clusters obtained.

#Data Description:
  
  #The file EastWestAirlinescontains information on passengers who belong to an airline’s frequent flier program. For each passenger the data include information on their mileage history and on different ways they accrued or spent miles in the last year. The goal is to try to identify clusters of passengers that have similar characteristics for the purpose of targeting different segments for different types of mileage offers

#ID --Unique ID

#Balance--Number of miles eligible for award travel

#Qual_mile--Number of miles counted as qualifying for Topflight status

#cc1_miles -- Number of miles earned with freq. flyer credit card in the past 12 months:
 # cc2_miles -- Number of miles earned with Rewards credit card in the past 12 months:
#  cc3_miles -- Number of miles earned with Small Business credit card in the past 12 months:
  
  #1 = under 5,000
#2 = 5,000 - 10,000
#3 = 10,001 - 25,000
#4 = 25,001 - 50,000
#5 = over 50,000

#Bonus_miles--Number of miles earned from non-flight bonus transactions in the past 12 months

#Bonus_trans--Number of non-flight bonus transactions in the past 12 months

#Flight_miles_12mo--Number of flight miles in the past 12 months

#Flight_trans_12--Number of flight transactions in the past 12 months

#Days_since_enrolled--Number of days since enrolled in flier program

#Award--whether that person had award flight (free flight) or not

airlinedata <-read.csv("EastWestAirlines.csv")
View(airlinedata)
normalized_airdata <- scale(airlinedata[,2:11]) 
done<- dist(normalized_airdata, method = "euclidean") # distance matrix

fitair <- hclust(done, method="ward.D2")

plot(fitair) # display dendrogram
plot(fitair, hang=-1)
groups <- cutree(fitair, k=5) 
rect.hclust(fitair, k=5, border="red")
membership_airline<-as.matrix(groups)
#View(membership_airline)

final2 <- data.frame(airlinedata, membership_airline)
final_2 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final_2)
write.csv(final_2, file="final_2.csv")

#### K means and elbow curve 
kmean.wss = (nrow(normalized_airdata)-1)*sum(apply(normalized_airdata, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:12) kmean.wss[i] = sum(kmeans(normalized_airdata, centers=i)$withinss)

###elbow curve
plot(1:12,kmean.wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
fitair1 <- kmeans(normalized_airdata, 3) # 3 cluster solution
fina<- data.frame(airlinedata, fitair1$cluster) # append cluster membership
fina
aggregate(airlinedata[,2:12], by=list(fitair1$cluster), FUN=mean)
table(fitair1$cluster)
