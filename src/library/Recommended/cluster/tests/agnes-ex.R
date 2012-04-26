library(cluster)
options(digits = 6)
data(votes.repub)

.proctime00 <- proc.time()

agn1 <- agnes(votes.repub, metric = "manhattan", stand = TRUE)
summary(agn1)
Dvr <- daisy(votes.repub)
agn2 <- agnes(Dvr, method = "complete")
summary(agn2)
## almost same:
(ag2. <- agnes(Dvr, method= "complete", keep.diss=FALSE))
ag22  <- agnes(votes.repub, method= "complete", keep.diss=FALSE,keep.data=FALSE)
stopifnot(identical(agn2[-5:-6], ag2.[-5:-6]),
          identical(Dvr, daisy(votes.repub)), # DUP=FALSE (!)
          identical(ag2.[-6], ag22[-6])
         )

data(agriculture)
summary(agnes(agriculture))

data(ruspini)
summary(ar0 <- agnes(ruspini, keep.diss=FALSE, keep.data=FALSE))
summary(ar1 <- agnes(ruspini, metric = "manhattan"))
str(ar1)

cat('Time elapsed: ', proc.time() - .proctime00,'\n')

summary(ar2 <- agnes(ruspini, metric="manhattan", method = "weighted"))
print  (ar3 <- agnes(ruspini, metric="manhattan", method = "flexible",
                     par.meth = 0.5))
stopifnot(all.equal(ar2[1:4], ar3[1:4], tol=1e-12))

cat('Time elapsed: ', proc.time() - .proctime00,'\n')
