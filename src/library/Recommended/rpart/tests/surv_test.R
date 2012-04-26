## example from coxph: gave error in rpart 3.1-31
## due to missing drop = FALSE
test1 <- list(time=  c(4, 3,1,1,2,2,3),
               status=c(1,NA,1,0,1,1,0),
               x=     c(0, 2,1,1,1,0,0),
               sex=   c(0, 0,0,0,1,1,1))
library(survival)
library(rpart)
rpart(Surv(time, status) ~ x + sex, test1)
