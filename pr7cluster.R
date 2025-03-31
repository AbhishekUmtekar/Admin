#Q1
data("iris")
new_data <- subset(iris, select = c(-Species))
cl <- kmeans(new_data, 3)
wss <- sapply(1:15, function(k) kmeans(new_data, k)$tot.withinss)
plot(wss, type = "b")
clusplot(new_data, cl$cluster,lines = 0)
cl$cluster
cl$centers

#Q2
# Load the iris dataset
data("iris")
names(iris)
#To performs hierarchical clustering
clusters <- hclust(dist(iris))
plot(clusters)
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)
library(ggplot2)
#To create a scatter plot of Petal.Length vs Petal.Width
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green'))

clusters <- hclust(dist(iris), method = 'average')
clusterCut1 <- cutree(clusters, 3)
table(clusterCut1, iris$Species)

plot(clusters)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut1) + 
  scale_color_manual(values = c('black', 'red', 'green'))

#Q3
# Load the iris dataset
data(iris)
iris_1 <- iris[, -5]
kmeans_re <- kmeans(iris_1, centers = 3)
kmeans_re$cluster
kmeans_re$centers
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],kmeans_re$cluster,lines=0,color = TRUE)
plot(iris_1[, c("Sepal.Length", "Sepal.Width")],col=kmeans_re$cluster,lines=0,color = TRUE,pch=17)
points(kmeans_re$centers[, c("Sepal.Length", "Sepal.Width")])  # Centroids are marked with larger points
