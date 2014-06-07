## tests of fix for PR#9831
library(nlme)
val <- c("10"=1.10,"14"=1.14)
vf <- varIdent(value=val, form=~1|age, fixed=c("12"=1.12))
vfi <- Initialize(vf,Orthodont)
str(vfi)
coef(vfi)
coef(vfi, unconstrained = FALSE, allCoef = TRUE)
vfiCopy <- vfi        # copy of an initialized object
length(vfiCopy)             # length is 2
coef(vfiCopy) <- c(11,12)   # error in 3.1-84

## error in 3.1-84
gls.error  <- gls(distance ~ age, weights = vfi, data=Orthodont)
