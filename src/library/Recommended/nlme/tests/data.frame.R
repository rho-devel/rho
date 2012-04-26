library(nlme)
subs<-rep(LETTERS[1:10],rep(3,10))
resp<-rnorm(30)
groupedData(resp~1|subs)
