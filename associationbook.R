library(arules)
library(arulesViz)
book<-read.csv(file.choose())
View(book)
rulesA <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=5))

rulesA   # set of 186 rules
inspect(head(sort(rulesA, by = "lift")))
head(quality(rulesA))
plot(rulesA,method = "scatterplot")
plot(rulesA, method = "grouped")
plot(rulesA,method = "graph")
write(rulesA, file="a_rulesbook.csv",sep=",")

getwd()

rulesB<-apriori(book,parameter = list(support=0.003,confidence=0.6,minlen=4))
rulesB  ## set of 10637 rules 
inspect(rulesB[1:10])

plot(rulesB,method = "scatterplot", jitter=0)
plot(rulesB,method = "grouped")
plot(rulesB,method = "graph")  
plot(rulesB,method = "matrix3D")  ## for this method is best to interpret which  
## item we have to put  right side and left side.

head(quality(rulesB)) 
head(sort(rulesB, by="lift"))
inspect(head(sort(rulesB, by="lift"))) ## sorted out best six rule 


rulesC<-apriori(book,parameter = list(support=0.04,confidence=0.9,minlen=9))
#605 rule
inspect(rulesC[1:10])

plot(rulesC,method = "scatterplot", jitter=0)
plot(rulesC,method = "grouped")
plot(rulesC,method = "graph")  
plot(rulesC,method = "matrix3D")  ## for this method is best to interpret which  
## item we have to put  right side and left side.

head(quality(rulesC)) 
head(sort(rulesC, by="lift"))
inspect(head(sort(rulesC, by="lift"))) ## sorted out best six rule 
