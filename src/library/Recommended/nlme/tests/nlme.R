library(nlme)
data(Loblolly)
fm1 <- nlsList(SSasymp, Loblolly)
fm1
fm2 <- nlme(fm1, random = Asym ~ 1)
fm2
q()
