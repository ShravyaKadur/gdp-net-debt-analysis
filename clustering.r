#CLUSTERING

#Model based

library(mclust)
fit <- Mclust(fvmat)
plot(fit)
summary(fit)


#K means

km <- kmeans(fvmat, 3, nstart = 25)
km$cluster
plot(fvmat,col=km$cluster)

#Elbow method

k.max <- 15 # Maximum number of clusters
wss <- sapply(1:k.max, 
              function(k){kmeans(fvmat, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)


#Dendrogram

dend <- hclust(dist(fvmat))
plot(dend)

