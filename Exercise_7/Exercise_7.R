#install.packages("arules")
library("arules")
library("arulesViz")

#Read the whole data set
d<-read.csv("vote1.csv")
d

#Generate association rules with no more than 5 antecedents, the confidence of 80% and the support of 30%
rules_democrat<-apriori(d, parameter=list(supp=0.3, conf = 0.8, maxlen=5), 
                        appearance = list(default="lhs", rhs="class=democrat"))
inspect(sort(rules_democrat[1:10], by="confidence", decreasing=TRUE))
plot(rules_democrat)


#Generate association rules with no more than 5 antecedents, the confidence of 95% and the support of 30%
rules_democrat95<-apriori(d, parameter=list(supp=0.3, conf = 0.95, maxlen=5), 
                          appearance = list(default="lhs", rhs="class=democrat"))
inspect(sort(rules_democrat95[1:10], by="confidence", decreasing=TRUE))
plot(rules_democrat95)
