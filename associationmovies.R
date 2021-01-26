#install.packages("arulesViz")
library(arules)
library(arulesViz)
movie<-read.csv("my_movies.csv")
View(movie)
class(movie)
str(movie)
summary(movie)

rules1 <- apriori(as.matrix(movie[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))
rules1  # set of 77 rules
inspect(head(sort(rules1, by = "lift")))
head(quality(rules1))
plot(rules1,method = "scatterplot")
plot(rules1, method = "grouped")
plot(rules1,method = "graph")
write(rules1, file="a_rulesmovie.csv",sep=",")

getwd()

movie2<-as(as.matrix(movie[,6:15]),"transactions")
inspect(movie2[1:10])
rules2<-apriori(movie2,parameter = list(support=0.002,confidence=0.07,minlen=3))
rules2  ### tell us total no. 77  rules
inspect(rules2[1:10]) ## higher the lift  best is the model 

plot(rules2,method = "scatterplot", jitter=0)
plot(rules2,method = "grouped")
plot(rules2,method = "graph")  
plot(rules2,method = "matrix3D")  
head(quality(rules2)) 
head(sort(rules2, by="lift")) #  set of 6 rules 
inspect(head(sort(rules2, by="lift")))


rules3<-apriori(movie2,parameter = list(support=0.003,confidence=0.6,minlen=4))
rules3  ### SET OF 29 RULES
inspect(rules3[1:10])

plot(rules3,method = "scatterplot", jitter=0)
plot(rules3,method = "grouped")
plot(rules3,method = "graph")  
plot(rules3,method = "matrix3D")  ## for this method is best to interpret which  
## item we have to put  right side and left side.

head(quality(rules3)) 
head(sort(rules3, by="lift"))
inspect(head(sort(rules3, by="lift"))) ## sorted out best six rule 


rules4<-apriori(movie2,parameter = list(support=0.004,confidence=0.8,minlen=2))
# set of 77 rules 
inspect(rules4[1:10])

plot(rules4,method = "scatterplot", jitter=0)
plot(rules4,method = "grouped")
plot(rules4,method = "graph")  
plot(rules4,method = "matrix3D")  ## for this method is best to interpret which  
## item we have to put  right side and left side.

head(quality(rules4)) 
head(sort(rules4, by="lift"))
inspect(head(sort(rules4, by="lift"))) ## sorted out best six rule 
