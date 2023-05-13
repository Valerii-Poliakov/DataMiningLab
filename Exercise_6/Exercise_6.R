d<-read.csv(file="ConcreteData.csv")
make.names(names(d))
d

x<-d$Cement 
y<-d$Concrete.compressive.strength
model<-lm(y ~ x)  # lm() creates a linear model

plot(x,y)
abline(model, col='red')  # adds the model line to the plot

summary(model)

new <- data.frame((x = c(198.6,412.8, 616.4)))  # creating new values for Cement
pred.conf <- predict(model, new, interval="confidence", level=0.95)  #making predictions
pred.conf

pred.pred <- predict(model, new, interval="prediction", level=0.95)
pred.pred

p <- predict(model, new)  # estimated  values of Y for new values of X
q <- c(24.89, 47.13, 59)  # true values of Y for new values of X

sse <- sum((q - p)^2)
sst <- sum((y-mean(y))^2)
pseudo_r2 <- 1 - sse/sst
pseudo_r2

# multiple linear regression
x1<-d$Cement
x2<-d$Blast.Furnace.Slag
mult_model <- lm(y ~ x1+x2)
summary(mult_model)