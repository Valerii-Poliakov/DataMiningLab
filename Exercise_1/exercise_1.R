data<-read.csv(file="data1.csv", header=FALSE)
print(is.data.frame(data))
print(ncol(data))
print(nrow(data))

col1<-data[,1]

# 1. Calculate the mean and the standard deviation for the variable 
# after excluding NA values. Draw the histogram.
clean_x<-na.omit(col1)
mean(clean_x)
sd(clean_x)
hist(clean_x)

# 2. Replace the missing values with the mean calculated in Task 1. 
# Draw the histogram. 
# Calculate the mean and the standard deviation after replacement.
clean_y<-ifelse(is.na(col1), mean(col1, na.rm = TRUE), col1)
mean(clean_y)
sd(clean_y)
hist(clean_y)

# 3. Replace the missing values with the values generated at random 
# from the observed variable distribution. Draw the histogram. 
# Calculate the mean and the standard deviation after replacement.
col_temp<-col1
for(i in seq_along(col_temp)) {
  col_temp[i]<-ifelse(is.na(col1[i]), sample(na.omit(col1), 1), col1[i])
}
mean(col_temp)
sd(col_temp)
hist(col_temp)