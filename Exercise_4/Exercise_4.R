library(class)

d<-read.csv("seeds.csv", header=TRUE, sep=',')
d

with(d, plot(length, width, col =as.factor(class)))

d<-d[sample(nrow(d)),]   #randomly shuffle the data

# Task 1
# creating test and training sets
test_indices<-c(1:20)  # first 20 records is the test dataset
d.test <- d[test_indices,1:7]       #without wheat type (class)
d.test.class<- d[test_indices,8]    #only wheat type (class)
d.train <- d[-test_indices, 1:7]    
d.train.class <- d[-test_indices, 8]

#knn algorithm
knn.result <- knn(d.train, d.test, d.train.class, k = 3)
knn.result 

t<-table(d.test.class,knn.result)
err_rate<-mean(knn.result!=d.test.class)

cat("\n\nerror rate = ",err_rate,"\n\n")
print(t)

# Task 2
nbreaks<-10
folds <- cut(seq(1,nrow(d)),breaks=nbreaks,labels=FALSE)
av_err_rate<-0

for(i in seq(1:nbreaks)){
  test_indices <- which(folds==i,arr.ind=TRUE)
  d.test <- d[test_indices,1:7]  #without class
  d.test.class<- d[test_indices,8]    #only class
  d.train <- d[-test_indices, 1:7]
  d.train.class <- d[-test_indices, 8]
  
  #Use the test and train data partitions in knn algorith
  knn.result <- knn(d.train, d.test, d.train.class, k = 7)
  t<-table(d.test.class,knn.result)
  err_rate<-(nrow(d.test)-sum(diag(t)))/nrow(d.test)
  av_err_rate = av_err_rate + err_rate 
  
  cat("\n\nFold = ",i,"\t error rate = ",err_rate,"\n")
  print(t)
}
av_err_rate = av_err_rate/nbreaks
av_err_rate