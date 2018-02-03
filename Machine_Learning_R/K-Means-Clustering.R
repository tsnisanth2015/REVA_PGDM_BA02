str(mtcars)

cars_sc=scale(mtcars)

set.seed(345)

k3=kmeans(cars_sc, centers=3)

clusplot(x=cars_sc, clus=k3$cluster,labels=2)

twss <- kmeans(cars_sc,centers=1)$tot.withinss

for (i in 2:(nrow(cars_sc)-1)) twss[i] <- kmeans(cars_sc,centers=i)$tot.withinss

plot(twss, type="b", xlab="Number of Clusters",ylab="Total Within groups SS")

set.seed(345)

k4=kmeans(cars_sc, centers=4)
k4

clusplot(x=cars_sc,clus=k4$cluster)

c=C5.0(mtcars[1:11],as.factor(mtcars[,12]))

plot(c)
