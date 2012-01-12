# Any necessary setup
library(rpart)
library(survival)
options(na.action="na.omit")
options(digits=4) # to match earlier output
RNGversion("1.6.2")
set.seed(1234)
#
# Read the data
#
#   Time to progression in years
#   status  1=progressed, 0= censored
#   age
#   early endocrine therapy   1=no 2=yes
#   % of cells in g2 phase, from flow cytometry
#   tumor grade (Farrow) 1,2,3,4
#   Gleason score (competing grading system)
#   ploidy

stagec <- read.table('data.stagec',  col.names=c("pgtime", "pgstat", "age",
			"eet", "g2", "grade", "gleason", "ploidy"))
stagec$ploidy <- factor(stagec$ploidy, levels=1:3,
				labels=c("diploid", "tetraploid", "aneuploid"))

cox0 <- coxph(Surv(pgtime, pgstat) ~ 1, stagec)
cox1 <- coxph(Surv(pgtime, pgstat) ~ age + eet + g2 + grade + ploidy, stagec)
cox1

fit1 <- rpart(Surv(pgtime, pgstat) ~ age + eet + g2+grade+gleason +ploidy,
		stagec, control=rpart.control(usesurrogate=0, cp=0),
		method='poisson')
fit1
summary(fit1)

fit2 <- rpart(cox0$residual ~ age + eet + g2+grade+gleason +ploidy,
		stagec)
fit2
summary(fit2)

#
# In 6/2001 rpart.exp changed to fix a bug.  A side effect was reexamination
#  of one definition: the old version didn't count person-years after the
#  last death in computing the rescaling, the new one does.  One could argue
#  about which is better.  To match the old output, we modify the data.
tdata <- stagec
tdata$pgtime <- pmin(tdata$pgtime, max(tdata$pgtime[tdata$pgstat==1]))

fit3 <- rpart(Surv(pgtime, pgstat) ~ age + eet + g2+grade+gleason +ploidy,
		tdata, control=rpart.control(usesurrogate=1, cp=.001))

summary(fit3)

#
# In order to compare the x-vals estimates of the mainline and S versions,
#  it is necessary that we use stratified xval sets (like the mainline
#  does).

mystate <- data.frame(state.x77, region=factor(state.region))
names(mystate) <- c("population","income" , "illiteracy","life" ,
       "murder", "hs.grad", "frost",     "area",      "region")

xvals <- 1:nrow(mystate)
xvals[order(mystate$income)] <- rep(1:10, length=nrow(mystate))

fit4 <- rpart(income ~ population + region + illiteracy +life + murder +
			hs.grad + frost , mystate,
		   control=rpart.control(minsplit=10, xval=xvals))

summary(fit4)


#
# Check out xpred.rpart
#
meany <- mean(mystate$income)
xpr <- xpred.rpart(fit4, xval=xvals)
xpr2 <- (xpr - mystate$income)^2
risk0 <- mean((mystate$income - meany)^2)
xpmean <- as.vector(apply(xpr2, 2, mean))   #kill the names
all.equal(xpmean/risk0, as.vector(fit4$cptable[,'xerror']))

xpstd <- as.vector(apply((sweep(xpr2, 2, xpmean))^2, 2, sum))
xpstd <- sqrt(xpstd)/(50*risk0)
all.equal(xpstd, as.vector(fit4$cptable[,'xstd']))

#
# recreate subset #3 of the xval
#
tfit4 <- rpart(income ~ population + region + illiteracy +life + murder +
			hs.grad + frost , mystate,  subset=(xvals!=3),
		   control=rpart.control(minsplit=10, xval=0))
tpred <- predict(tfit4, mystate[xvals==3,])
all.equal(tpred, xpr[xvals==3,ncol(xpr)])

# How much does this differ from the "real" formula, more complex,
#   found on page 309 of Breiman et al. ?
#xtemp <- (xpr2/outer(rep(1,50),xpmean)) -  ((mystate$income - meany)^2)/risk0
#real.se<- xpmean* sqrt(apply(xtemp^2,2,sum))/(risk0*50)

fit5 <- rpart(factor(pgstat) ~  age + eet + g2+grade+gleason +ploidy,
	  stagec)

fit5

fit6 <- rpart(factor(pgstat) ~  age + eet + g2+grade+gleason +ploidy,
		stagec, parm=list(prior=c(.5,.5)))
summary(fit6)
#
# Fit a classification model to the car data.
#  Now, since Reliability is an ordered category, this model doesn't
# make a lot of statistical sense, but it does test out some
# areas of the code that nothing else does
#

carfit <- rpart(Reliability ~ Price + Country + Mileage + Type,
		   method='class', data=cu.summary)

summary(carfit)
#
# Simplest weight test: treble the weights
#
#  By using the unshrunken estimates the weights will nearly cancel
#   out:  frame$wt, frame$dev, frame$yval2, and improvement will all
#   be threefold larger, other things will be the same.
# The improvement is the splits matrix, column 3, rows with n>0.  Other
#   rows are surrogate splits.

tempc <- rpart.control(maxsurrogate=0, cp=0, xval=0)
fit1 <- rpart(Surv(pgtime, pgstat) ~ age + eet + g2+grade+gleason +ploidy,
                stagec, control=tempc,
                method='poisson', parms=list(shrink=0))
wts <- rep(3, nrow(stagec))
fit1b <- rpart(Surv(pgtime, pgstat) ~ age + eet + g2+grade+gleason +ploidy,
                stagec, control= tempc, parms=list(shrink=0),
                method='poisson', weights=wts)
fit1b$frame$wt   <- fit1b$frame$wt/3
fit1b$frame$dev  <- fit1b$frame$dev/3
fit1b$frame$yval2[,2] <- fit1b$frame$yval2[,2]/3
fit1b$splits[,3] <- fit1b$splits[,3]/3
all.equal(fit1[-3], fit1b[-3])   #all but the "call"

#
# Compare a pair of multiply weighted fits
#  In this one, the lengths of where and y won't match
# I have to set minsplit to the smallest possible, because otherwise
#  the replicated data set will sometimes have enough "n" to split, but
#  the weighted one won't.  Use of CP keeps the degenerate splits
#  (n=2, several covariates with exactly the same improvement) at bay.
# For larger trees, the weighted split will sometimes have fewer
#  surrogates, because of the "at least two obs" rule.
#
set.seed(1234)
wts <- sample(1:5, nrow(stagec), replace=TRUE)
temp <- rep(1:nrow(stagec), wts)             #row replicates
xgrp <- rep(1:10, length=146)[order(runif(146))]
xgrp2<- rep(xgrp, wts)
#  Direct: replicate rows in the data set, and use unweighted
fit2 <- rpart(Surv(pgtime, pgstat) ~ age + eet + g2+grade+gleason +ploidy,
	      control=rpart.control(minsplit=2, xval=xgrp2, cp=.025),
	      data=stagec[temp,], method='poisson')
#  Weighted
fit2b<- rpart(Surv(pgtime, pgstat) ~ age + eet + g2+grade+gleason +ploidy,
	      control=rpart.control(minsplit=2, xval=xgrp, cp=.025),
	      data=stagec, method='poisson', weight=wts)

all.equal(fit2$frame[-2],  fit2b$frame[-2])  # the "n" component won't match
all.equal(fit2$cptable,    fit2b$cptable)
# next two depend on how ties are resolved, so platform-dependent
# all.equal(fit2$splits[,-1],fit2b$splits[,-1]) #fails
# all.equal(fit2$splits[-24,-1],fit2b$splits[-24,-1]) #ok
all.equal(fit2$csplit,    fit2b$csplit)
# Line 24 is a surrogate split in a group whose 2 smallest ages are
#  47 and 48.  The weighted fit won't split there because it wants to
#  send at least 2 obs to the left; the replicate fit thinks that there
#  are several 47's.





#
# Test weights in a regression problem
#

xgrp <- rep(1:10,5)
fit4 <- rpart(income ~ population + region + illiteracy +life + murder +
                        hs.grad + frost , mystate,
                   control=rpart.control(minsplit=10, xval=xgrp))
wts <- rep(3, nrow(mystate))
fit4b <-  rpart(income ~ population + region + illiteracy +life + murder +
                        hs.grad + frost , mystate,
                   control=rpart.control(minsplit=10, xval=xgrp), weights=wts)
fit4b$frame$wt   <- fit4b$frame$wt/3
fit4b$frame$dev  <- fit4b$frame$dev/3
fit4b$cptable[,5] <- fit4b$cptable[,5] * sqrt(3)
temp <- c('frame', 'where', 'splits', 'csplit', 'cptable')
all.equal(fit4[temp], fit4b[temp])


# Next is a very simple case, but worth keeping
dummy <- data.frame(y=1:10, x1=c(10:4, 1:3), x2=c(1,3,5,7,9,2,4,6,8,0))

xx1 <- rpart(y ~ x1 + x2, dummy, minsplit=4, xval=0)
xx2 <- rpart(y ~ x1 + x2, dummy, weights=rep(2,10), minsplit=4, xval=0)

all.equal(xx1$frame$dev, c(82.5, 10, 2, .5, 10, .5, 2))
all.equal(xx2$frame$dev, c(82.5, 10, 2, .5, 10, .5, 2)*2)
summary(xx2)


# Now for a set of non-equal weights
#  We need to set maxcompete=3 because there just happens to be, in one
#  of the lower nodes, an exact tie between variables "life" and "murder".
#  Round off error causes fit5 to choose one and fit5b the other.
# Later -- cut it back to maxdepth=3 for the same reason (a tie).
#
nn <- nrow(mystate)
wts <- sample(1:5, nn, replace=TRUE)
temp <- rep(1:nn, wts)             #row replicates
xgrp <- rep(1:10, length=nn)[order(runif(nn))]
xgrp2<- rep(xgrp, wts)
tempc <- rpart.control(minsplit=2, xval=xgrp2, maxsurrogate=0,
		       maxcompete=3, maxdepth=3)
#  Direct: replicate rows in the data set, and use unweighted
fit5 <-  rpart(income ~ population + region + illiteracy +life + murder +
                        hs.grad + frost , data=mystate[temp,], control=tempc)
#  Weighted
tempc <- rpart.control(minsplit=2, xval=xgrp, maxsurrogate=0,
		       maxcompete=3, maxdepth=3)
fit5b <-  rpart(income ~ population + region + illiteracy +life + murder +
                        hs.grad + frost , data=mystate, control=tempc,
                        weights=wts)
all.equal(fit5$frame[-2],  fit5b$frame[-2])  # the "n" component won't match
all.equal(fit5$cptable,    fit5b$cptable)
all.equal(fit5$splits[,-1],fit5b$splits[,-1])
all.equal(fit5$csplit,    fit5b$csplit)
#
# The treble test for classification trees
#
#
xgrp <- rep(1:10,length=nrow(cu.summary))
carfit <- rpart(Country ~ Reliability + Price + Mileage + Type,
		 method='class', data=cu.summary,
		 control=rpart.control(xval=xgrp))

carfit2 <- rpart(Country ~ Reliability + Price + Mileage + Type,
		 method='class', data=cu.summary,
		 weight=rep(3,nrow(cu.summary)),
		 control=rpart.control(xval=xgrp))

all.equal(carfit$frame$wt,    carfit2$frame$wt/3)
all.equal(carfit$frame$dev,   carfit2$frame$dev/3)
all.equal(carfit$frame[,5:7], carfit2$frame[,5:7])
all.equal(carfit$frame$yval2[,12:21], carfit2$frame$yval2[,12:21])
all.equal(carfit[c('where', 'csplit')],
	  carfit2[c('where', 'csplit')])
xx <- carfit2$splits
xx[,'improve'] <- xx[,'improve'] / ifelse(xx[,5]> 0,1,3) # surrogate?
all.equal(xx, carfit$splits)
all.equal(as.vector(carfit$cptable),
	  as.vector(carfit2$cptable%*% diag(c(1,1,1,1,sqrt(3)))))

summary(carfit2)

#
# Treble test for class trees with 2 outcomes
#
fit1 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
               control=rpart.control(maxsurrogate=0, cp=0, xval=0),
               parms=list(prior=c(.7,.3),
                          loss=matrix(c(0,1,2,0),nrow=2,ncol=2)))
wts <- rep(3, nrow(kyphosis))
fit1b <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
                control=rpart.control(maxsurrogate=0, cp=0, xval=0),
	       weights=wts,
               parms=list(prior=c(.7,.3),
                          loss=matrix(c(0,1,2,0),nrow=2,ncol=2)))
fit1b$frame$wt   <- fit1b$frame$wt/3
fit1b$frame$dev  <- fit1b$frame$dev/3
fit1b$frame$yval2[,2:3] <- fit1b$frame$yval2[,2:3]/3
fit1b$splits[,3] <- fit1b$splits[,3]/3
all.equal(fit1[-3], fit1b[-3])   #all but the "call"

# Now for a set of non-equal weights
nn <- nrow(kyphosis)
wts <- sample(1:5, nn, replace=TRUE)
temp <- rep(1:nn, wts)             #row replicates
xgrp <- rep(1:10, length=nn)[order(runif(nn))]
xgrp2<- rep(xgrp, wts)
tempc <- rpart.control(minsplit=2, xval=xgrp2, maxsurrogate=0)
#  Direct: replicate rows in the data set, and use unweighted
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis[temp,],
               control=tempc,
               parms=list(prior=c(.7,.3),
                          loss=matrix(c(0,1,2,0),nrow=2,ncol=2)))
#  Weighted
tempc <- rpart.control(minsplit=2, xval=xgrp, maxsurrogate=0)
fit2b <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
               control=tempc, weights=wts,
               parms=list(prior=c(.7,.3),
                          loss=matrix(c(0,1,2,0),nrow=2,ncol=2)))

# all.equal(fit2$frame[-2],  fit2b$frame[-2])  # the "n" component won't match
all.equal(fit2$cptable,    fit2b$cptable)
# all.equal(fit2$splits[,-1],fit2b$splits[,-1]) #fails
all.equal(fit2$csplit,    fit2b$csplit)


#
# Check out using costs
#
fit1 <- rpart(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno
	      + meal.cal + wt.loss, data=lung,
	      maxdepth=1, maxcompete=6, xval=0)

fit2 <- rpart(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno
	      + meal.cal + wt.loss, data=lung,
	      maxdepth=1, maxcompete=6, xval=0, cost=(1+ 1:7/10))

temp1 <- fit1$splits[1:7,]
temp2 <- fit2$splits[1:7,]
temp3 <- c('age', 'sex', 'ph.ecog', 'ph.karno', 'pat.karno', 'meal.cal',
	   'wt.loss')
indx1 <- match(temp3, dimnames(temp1)[[1]])
indx2 <- match(temp3, dimnames(temp2)[[1]])
all.equal(temp1[indx1,1], temp2[indx2,1])             #same n's ?
all.equal(temp1[indx1,3], temp2[indx2,3]*(1+ 1:7/10)) #scaled importance


q()
