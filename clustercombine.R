library(cluster)
data("iris")
ir<-iris[,-5]
wss<-sapply(1:15,function(k) kmeans(ir,k)$tot.withinss)
cl<-kmeans(ir,centers = 3)
clusplot(ir[,c("Sepal.Length","Sepal.Width")],cl$cluster,color = TRUE,lines = 0)
plot(wss,type="b")
library(ggplot2)
clust<-hclust(dist(ir))
plot(clust)
clm<-cutree(clust,3)
ggplot(ir, aes(Sepal.Length, Sepal.Width)) + 
  geom_point(alpha = 5, size = 3)+
  geom_point(col=clm)+
  scale_colour_manual(values = c("red", "blue", "green"))
