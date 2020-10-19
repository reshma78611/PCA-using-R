library(readr)
wine<-read.csv('C:/Users/HP/Desktop/assignments submission/PCA/wine.csv')
View(wine)
sum(is.na(wine))
#normalize data
norm_data=scale(wine[,2:14])
View(norm_data)

##########k-means########

library(factoextra)
fviz_nbclust(norm_data,kmeans,method='wss')
#k=4
km<-kmeans(norm_data,4)
km$cluster
km$centers
cluster_k_means<-data.frame('cluster_k'=km$cluster)

##########hierarchy#########

d<-dist(norm_data,method='euclidean')
h_clust<-hclust(d,method='complete')
cluster_hierarchy<-cutree(h_clust,4)

#######################################################################

################### PCA ########################

#PCA data with 3 Principal component scores
pca<-princomp(norm_data,scores=TRUE)
summary(pca)
pca$scores
pca_data<-data.frame(pca$scores)
pca_data<-pca_data[,1:3]

######### K-Means with PCA #############

library(factoextra)
fviz_nbclust(pca_data,kmeans,method='wss')
#k=4,with PC1,PC2,PC3(3 columns) also we got same k value as for with original data(13 columns)
km_pca<-kmeans(pca_data,4)
km_pca$cluster
km_pca$centers
cluster_k_means_pca<-data.frame('cluster_k_pca'=km_pca$cluster)

############ Hierarchy with PCA ############

d1<-dist(pca_data,method='euclidean')
h_clust_pca<-hclust(d1,method='complete')
cluster_hierarchy_pca<-cutree(h_clust_pca,4)


