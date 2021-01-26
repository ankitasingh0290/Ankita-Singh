library(cluster)
library(fpc)
library(NbClust)
library(factoextra)
wine<-read.csv("wine.csv")
View(wine)

### Building PCA 

W.pca <- princomp(wine[,-1], cor = TRUE, scores = TRUE, covmat = NULL)

summary(W.pca)
plot(W.pca)
biplot(W.pca)

 #### finding number of clusters

no.of.Clusters = NbClust(wine, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")
###   According to the majority rule, the best number of clusters is  7 

# Plot bar chart for the clusters
fviz_nbclust(no.of.Clusters) + theme_minimal()
### According to the majority rule, the best number of clusters is  7 

### Hierarchical clustering - All Variables
hclust.complete = eclust(wine, "hclust", k = 7, method = "complete", graph = FALSE) 
fviz_dend(hclust.complete, rect = TRUE, show_labels = FALSE) 

### K-Means clustering - All Variables
km.7 = eclust(wine, "kmeans", k = 5, nstart = 25, graph = FALSE)
fviz_cluster(km.7, geom = "point", frame.type = "norm")

##### Cluster Analysis - PCA Suggested Components

wine.pca = wine[,2:14]

no.of.Clusters_1 = NbClust(wine.pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")
##  According to the majority rule, the best number of clusters is  7 

# Plot bar chart for the clusters
fviz_nbclust(no.of.Clusters_1) + theme_minimal()
###  According to the majority rule, the best number of clusters is  7 .

### Hierarchical clustering - PCA Suggested Components
hclust.complete = eclust(wine.pca, "hclust", k = 7, method = "complete", graph = FALSE) 
fviz_dend(hclust.complete, rect = TRUE, show_labels = FALSE) 

###K-Means clustering - PCA Suggested Components
km.7 = eclust(wine.pca, "kmeans", k = 5, nstart = 25, graph = FALSE)
fviz_cluster(km.7, geom = "point", frame.type = "norm")

##When PCA was applied on the entire set of varibles (13); 
##PCA suggested that 90% of the information can be inferred from the first 7 variables.
#We then plotted dendrogram for both 13 variables and 7 variables data and found 
#that the number of clustered required are 7 and the dendrogram seem identical.