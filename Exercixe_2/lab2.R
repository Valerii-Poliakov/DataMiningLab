d <- read.csv(file="cars 1.csv", header=TRUE, sep=",")
d

#change the names
e <- d$engine.displacement
a <- d$acceleration

#1. Identifying possible outliers

#draw histograms
hist(e, breaks=15)
hist(e, breaks=50)
hist(a, breaks=15)
hist(a, breaks=50)

#draw a scatter plots
plot(e,a)
plot(a,e)


#2. Verifying possible outliers

#Z-score standardization  
zs.e <- (e-mean(e))/sd(e)
zs.e
#histogram after standardization
hist(zs.e)
#determine points whose values after standardization are less than -3 or greater than 3
zs.e.outliers <- e[(zs.e < (-3)) | (zs.e > 3)]
zs.e.outliers

zs.a <- (a-mean(a))/sd(a)
zs.a
#histogram after standardization
hist(zs.a)
#determine points whose values after standardization are less than -3 or greater than 3
zs.a.outliers <- a[(zs.a < (-3)) | (zs.a > 3)]
zs.a.outliers


#caclulate quartiles and outliers using the IQR method
iqr.e.outliers <- e[(e < quantile(e,0.25) - 1.5*IQR(e)) | (e > quantile(e,0.75) + 1.5*IQR(e))]
iqr.e.outliers

iqr.a.outliers <- a[(a < quantile(a,0.25) - 1.5*IQR(a)) | (a > quantile(a,0.75) + 1.5*IQR(a))]
iqr.a.outliers

#4. Checking mean and sd after removing outliers

mean( e[-which(e %in% c('740','850') )] )
sd( e[-which(e %in% c('740','850') )] )
mean( a[-which(a %in% c('3','1.4','24.8','7','8') )] )
sd( a[-which(a %in% c('3','1.4','24.8','7','8') )] )

