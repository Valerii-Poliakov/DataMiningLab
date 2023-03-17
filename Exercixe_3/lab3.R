d<-read.csv(file="f1.csv", header = FALSE, sep=' ')
d
with(d, plot(V1, V2))

# iter 1
km<-kmeans(d, centers=5)
km
# iter 2
km<-kmeans(d, centers=5)
km
with(d, plot(V1, V2, col=km$cluster))
# iter 3
km<-kmeans(d, centers=5)
km
with(d, plot(V1, V2, col=km$cluster))
# iter 4
km<-kmeans(d, centers=5)
km
with(d, plot(V1, V2, col=km$cluster))
# iter 5
km<-kmeans(d, centers=5)
km
with(d, plot(V1, V2, col=km$cluster))
points(km$centers, col=1,pch=16,cex=1)

# Use Elbow Method to identify number of clusters. 
N <- 15 # Number of iterations
K <- 5 # Number of algorithm runs in each iteration
results <- list()
for (i in 1:N) {
  km <- kmeans(d, centers = i, nstart = K)
  results[[i]] <- km
}

wss <- sapply(results, function(x) x$tot.withinss)
wss
plot(wss, type = "b", xlab = "Iteration", ylab = "WSS")

