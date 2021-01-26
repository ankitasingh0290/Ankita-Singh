#Association Rule Problem Statement for "My_movies" Data sets

#Prepare rules for all the data sets 
#1) Try different values of support and confidence. Observe the change in number
#of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots

library(arules)
library(arulesViz)
library(readr)

#Data Set : my_movies 

my_movies<-read.csv(file.choose())
View(my_movies)

class(my_movies)
str(my_movies)
summary(my_movies)


range(my_movies[,6:15])


movies_trans<-as(as.matrix(my_movies[,6:15]),"transactions")
inspect(movies_trans[1:10])


rules_A<-apriori(movies_trans,parameter = list(support=0.002,confidence=0.07,minlen=3))
rules_A  ### tell us total no. of rules
inspect(rules_A[1:10]) ## higher the lift ration best is the model 

plot(rules_A,method = "scatterplot", jitter=0)
plot(rules_A,method = "grouped")
plot(rules_A,method = "graph")  
plot(rules_A,method = "matrix3D")  
head(quality(rules_A)) 
head(sort(rules_A, by="lift"))
inspect(head(sort(rules_A, by="lift"))) # sorted out best six rule 


rules_B<-apriori(movies_trans,parameter = list(support=0.003,confidence=0.6,minlen=4))
rules_B
inspect(rules_B[1:10])

plot(rules_B,method = "scatterplot", jitter=0)
plot(rules_B,method = "grouped")
plot(rules_B,method = "graph")  
plot(rules_B,method = "matrix3D")  ## for this method is best to interpret which  
## item we have to put  right side and left side.

head(quality(rules_B)) 
head(sort(rules_B, by="lift"))
inspect(head(sort(rules_B, by="lift"))) ## sorted out best six rule 


rules_C<-apriori(movies_trans,parameter = list(support=0.004,confidence=0.8,minlen=2))
inspect(rules_C[1:10])

plot(rules_C,method = "scatterplot", jitter=0)
plot(rules_C,method = "grouped")
plot(rules_C,method = "graph")  
plot(rules_C,method = "matrix3D")  ## for this method is best to interpret which  
## item we have to put  right side and left side.

head(quality(rules_C)) 
head(sort(rules_C, by="lift"))
inspect(head(sort(rules_C, by="lift"))) ## sorted out best six rule 
