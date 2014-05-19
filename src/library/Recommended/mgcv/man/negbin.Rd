\name{negbin}
\alias{negbin}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{GAM negative binomial family}
\description{The \code{gam} modelling function is designed to be able to use 
the \code{\link{negbin}} family (a modification of MASS library \code{negative.binomial} family 
by Venables and Ripley), with or without a known \eqn{\theta}{theta} parameter. 
Two approaches to estimating the \code{theta} parameter are available:

\itemize{
\item If `performance iteration' is used for smoothing parameter estimation 
(see \code{\link{gam}}), then smoothing parameters are chosen by GCV and 
\code{theta} is chosen in order to ensure that the Pearson estimate of the scale 
parameter is as close as possible to 1, the value that the scale parameter should have.

\item If `outer iteration' is used for smoothing parameter selection, and smoothing parameters 
are chosen by UBRE/AIC (with scale parameter set to 1) then a value of \code{theta} is  
searched for which minimizes the AIC of the model. Alternatively If (RE)ML is used for smoothing 
parameter estimation then a value of \code{theta} is searched for which maximizes the (restricted) 
likelihood. 
}
The second option is much slower than the first, but the first can sometimes fail to converge. 
To use the first option, set the \code{optimizer} argument of \code{\link{gam}} to \code{"perf"}.
}

\usage{
negbin(theta = stop("'theta' must be specified"), link = "log")
}
\arguments{
\item{theta}{Either i) a single value known value of theta, ii) two values of theta specifying the 
endpoints of an interval over which to search for theta or iii) an array of values of theta, specifying the set of theta values to search. (iii) is only available with AIC based theta estimation. }

\item{link}{The link function: one of \code{"log"}, \code{"identity"} or \code{"sqrt"}}
}
\value{
 An object inheriting from class \code{family}, with additional elements
\item{dvar}{the function giving the first derivative of the variance function w.r.t. \code{mu}.}
\item{d2var}{the function giving the second derivative of the variance function w.r.t. \code{mu}.}
\item{getTheta}{A function for retrieving the value(s) of theta. This also useful for retriving the 
estimate of \code{theta} after fitting (see example).}
}

\details{ If a single value of \code{theta} is supplied then it is always taken as the known fixed value, 
and estimation of smoothing paramaters is then by UBRE/AIC. If \code{theta} is two numbers (\code{theta[2]>theta[1]}) 
then they are taken as specifying the range of values over which to search for 
the optimal theta. If \code{theta} is any other array of numbers then they are
taken as the discrete set of values of theta over which to search for \code{theta}. The latter 
option only works with AIC based outer iteration, if performance iteration is used then an array will only
be used to define a search range.

If performance iteration is used (see \code{\link{gam}} argument \code{optimizer}) then the method 
of estimation is to choose \eqn{\hat \theta}{theta}  so that the GCV (Pearson) estimate 
of the scale parameter is one (since the scale parameter 
is one for the negative binomial). In this case \eqn{\theta}{theta} estimation is nested within the IRLS loop 
used for GAM fitting. After each call to fit an iteratively weighted additive model to the IRLS pseudodata, 
the \eqn{\theta}{theta} estimate is updated. This is done by conditioning on all components of the current GCV/Pearson 
estimator of the scale parameter except \eqn{\theta}{theta} and then searching for the 
\eqn{\hat \theta}{theta} which equates this conditional  estimator to one. The search is 
a simple bisection search after an initial crude line search to bracket one. The search will 
terminate at the upper boundary of the search region is a Poisson fit would have yielded an estimated 
scale parameter <1.

If outer iteration is used then \eqn{\theta}{theta} is estimated by searching for the value yielding the lowest AIC.
The search is either over the supplied array of values, or is a grid search over the supplied range, followed by a 
golden section search. A full fit is required for each trial \eqn{\theta}{theta}, so the process is slow, but speed
is enhanced by making the changes in \eqn{\theta}{theta} as small as possible, from one step to the next, and 
using the previous smothing parameter and fitted values to start the new fit. 

In a simulation test based on 800 replicates of
the first example data, given below, the GCV based (performance iteration) method yielded models with, 
on avergage 6\% better MSE performance than the AIC based (outer iteration) method. 
\code{theta} had a 0.86 correlation coefficient between the two methods.
\code{theta} estimates averaged 3.36 with a standard deviation of 0.44 for the AIC based method and
3.22 with a standard deviation of 0.43 for the GCV based method. However the GCV based method is 
less computationally reliable, failing in around 4\% of replicates. 

}

%- maybe also `usage' for other objects documented here.

\author{ Simon N. Wood \email{simon.wood@r-project.org}
modified from Venables and Ripley's \code{negative.binomial} family.
}

\references{
Venables, B. and B.R. Ripley (2002) Modern Applied Statistics in S, Springer.
}

\section{WARNINGS}{
\code{\link{gamm}} does not support \code{theta} estimation

The negative binomial functions from the MASS library are no longer supported.
}

\examples{
library(mgcv)
set.seed(3)
n<-400
dat <- gamSim(1,n=n)
g <- exp(dat$f/5)

# negative binomial data  
dat$y <- rnbinom(g,size=3,mu=g)
# known theta ...
b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=negbin(3),data=dat)
plot(b,pages=1)
print(b)

## unknown theta via performance iteration...
b1 <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=negbin(c(1,10)),
          optimizer="perf",data=dat)
plot(b1,pages=1)
print(b1)

## unknown theta via outer iteration and AIC search
## (quite slow, which is why it's commented out for 
## checking)...
\dontrun{
b2<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=negbin(c(1,10)),
        data=dat)
plot(b2,pages=1)
print(b2)

## Same again all by  REML...
b2a <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=negbin(c(1,10)),
        data=dat,method="REML")
plot(b2a,pages=1)
print(b2a)

## how to retrieve Theta...
b2a$family$getTheta()
}

## unknown theta via outer iteration and AIC search
## over a discrete set of values...
\dontrun{
b3<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=negbin(2:10/2),
        data=dat)
plot(b3,pages=1)
print(b3)}

## another example...
set.seed(1)
f <- dat$f
f <- f - min(f);g <- f^2
dat$y <- rnbinom(g,size=3,mu=g)
b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=negbin(1:10,link="sqrt"),
         data=dat) 
plot(b,pages=1)
print(b)
rm(dat)
}
\keyword{models} \keyword{regression}%-- one or more ..


