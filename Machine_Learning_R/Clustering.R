                                    ## HIERARCHICAL CLUSTERING ##


# STEPS:
# 1.Create a hierarchical cluster model
# 2.Save the cluster memberships
# 3.Create a C5.0 tree where cluster membership is the DV.


install.packages("C50")
library(C50)
str(mtcars)

#Bring to same scale
mtcars_scaled = scale(mtcars)

# centroid Method to calculate distance
library(cluster)
Cluster_avg <- agnes(mtcars_scaled, metric = "euclidean", method = "average")
Cluster_avg
Cluster_avg$method

plot(Cluster_avg)
plot(Cluster_avg, which=2, main="Dendrogram of 'mtcars' (Average)")
rect.hclust(Cluster_avg, k=4)

coef(Cluster_avg)

# Ward Method
Cluster_Ward <- agnes(mtcars_scaled, metric="euclidean", method = "ward")
plot(Cluster_Ward, main="Dendrogram of 'mtcars' (Ward)", which=2)
coef(Cluster_Ward)

# Interpreting clusters - assign cluster names to each observation
mtcars$cluster= cutree(Cluster_Ward, k=4)
c=C5.0(mtcars[1:11],as.factor(mtcars[,12]))
plot(c)

table(mtcars$cluster)
barplot(table(mtcars$cluster))

aggregate(mtcars[, 3:4], list(mtcars$cluster), mean)
aggregate(mtcars[, 1:10], list(mtcars$cluster), mean)



