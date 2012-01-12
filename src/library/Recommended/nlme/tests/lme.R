library(nlme)
data(Oxboys)
fm1 <- lmList(Oxboys)
fm1
fm2 <- lme(fm1)
fm2

# bug report from Arne.Mueller@sanofi-aventis.com
mod <- distance ~ age + Sex
fm3 <- lme(mod, Orthodont, random = ~ 1)
predict(fm3, Orthodont)

## bug report and fix from Dimitris Rizopoulos and Spencer Graves:
## when 'returnObject = TRUE', do not stop() but give warning() on non-convergence:
fm1 <- lme(distance ~ age, data = Orthodont,
	   control = lmeControl(msMaxIter = 1, returnObject = TRUE))

## based on bug report on R-help
predict(fm3, Orthodont[1,])
# failed in 3.1-88
