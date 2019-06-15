
## install packages
install.packages("cluster")



## load packages
library(cluster)

## Hierarchical Clustering




## Pre-processing for clustering
## scale
## scale all your columns for clustering 



## Hierarchical Clustering algorithm 
## types of clustering:
## agglomerative clustering: starts with n clusters
## divisive clustering: starts with one cluster


## Output Dendograms

hierarchical_clustering <- function(data, features ){
  
  ## scale 
  
}


########################



data(agriculture)
## Example 1 in ref:
## Dissimilarities using Euclidean metric and without standardization
d.agr <- daisy(agriculture, metric = "euclidean", stand = FALSE)
d.agr
as.matrix(d.agr)[,"DK"] # via as.matrix.dist(.)
## compare with
as.matrix(daisy(agriculture, metric = "gower"))

hc4 <- diana(agriculture)

summary(d.agr)

grp <- cutree(hc4, k = 4)
grp <- as.data.frame(grp)


agriculture[ ,agriculture$cluster] <- grp
agriculture

