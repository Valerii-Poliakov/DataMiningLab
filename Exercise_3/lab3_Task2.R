d <- read.csv("zoo.csv", header = TRUE, sep = ",")
d

km <- kmeans(d, centers=7, nstart=10)
km

d_clustered <- cbind(d, cluster=km$cluster)
write.csv(d_clustered, "zoo_clustered.csv")
