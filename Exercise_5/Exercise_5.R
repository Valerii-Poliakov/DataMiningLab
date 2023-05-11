#install.packages(c("rpart","rpart.plot","C50"))
library("rpart");library("rpart.plot");library("C50"); library(class)
d<-read.csv(file="1_heart_disease.csv", stringsAsFactors=TRUE)

# Task 1. Make research how the size of a decision tree influences the classification error rate.

# Create folds for k-fold cross validation
nbreaks<-10
folds <- cut(seq(1,nrow(d)), breaks=nbreaks, labels=FALSE)

# randomly shuffle the data
d<-d[sample(nrow(d)),]

# data holders for the results plot
taer = traer = ti = list()

# minCases from 1 to 20 with step 2
for(i in seq(1, 20, by=2)){
  test_av_err_rate<-train_av_err_rate<-0
  
  # k-fold cross validation with k=10
  for(k in seq(1:nbreaks)){
    test_indices <- which(folds==k,arr.ind=TRUE)
    d.train <- d[-test_indices, 1:13]     #train data without class
    d.train.class <- d[-test_indices, 14] #train data with class only
    d.test <- d[test_indices,1:13]        #test data without class
    d.test.class<- d[test_indices, 14]    #test data with class only
    
    model<-C5.0(x=d.train, y=d.train.class, control=C5.0Control(CF=1.0, minCases = i))
    
    # count test error rate
    pred <- predict(model, d.test, type="class") 
    test_av_err_rate = test_av_err_rate + mean(pred != d.test.class)
    
    # count train error rate
    pred <- predict(model, d.train, type="class") 
    train_av_err_rate = train_av_err_rate + mean(pred != d.train.class)
  }
  
  # output results with mean error rate
  cat("minCases: ", i, " | Tree size: ", model$size)
  cat(" | test_error_rate: ", test_av_err_rate/nbreaks)
  cat(" | train_error_rate: ", train_av_err_rate/nbreaks, "\r\n")
  
  # save data for the results plot
  taer<-append(taer, test_av_err_rate/nbreaks)
  traer<-append(traer, train_av_err_rate/nbreaks)
  ti<-append(ti, i)
}

#results plot
plot(ti, taer, type="l", ylim=c(0, 0.3), ylab="Test error rate", xlab="Train error rate", axes = FALSE, col="red")
lines(ti, traer, col="green")
xlabel <- seq(1, 20, by = 2)
axis(1, at = xlabel) #set more informative x label
axis(2)


# Task 2. Classify new records
n<-read.csv(file="new records.csv", header=TRUE)
n

# CART
model<-rpart(class ~ ., data = d, method = "class", control=rpart.control(minsplit=2, cp=0.0))
rpart.plot(model)

# results of the CART algorithm for our new records
pred <- predict(model, n, type="class")
pred


# C4.5
d.train <- d[, 1:13]     #train data without class
d.train.class <- d[, 14] #train data with class only

model<-C5.0(x=d.train, y=d.train.class, control=C5.0Control(CF=1.0, minCases = 2))
plot(model)

# results of the C4.5 algorithm for our new records
pred <- predict(model, n, type="class")
pred
