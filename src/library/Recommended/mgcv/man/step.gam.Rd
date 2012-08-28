\name{step.gam}
\alias{step.gam}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Alternatives to step.gam}
\description{ There is no \code{step.gam} in package \code{mgcv}. The
\code{mgcv} default for model selection is to use either prediction error criteria such as 
GCV, GACV, Mallows' Cp/AIC/UBRE or the likelihood based methods of REML or ML.  Since the 
smoothness estimation part of model
selection is done in this way it is logically most consistent to perform the rest of model
selection in the same way. i.e. to decide which terms to include
or omit by looking at changes in GCV, AIC, REML etc. 

To facilitate fully automatic model selection the package implements two smooth
modification techniques which can be used to allow smooths to be shrunk to zero as 
part of smoothness selection.

\describe{
\item{Shrinkage smoothers}{are smoothers in which a small multiple of the identity matrix
is added to the smoothing penalty, so that strong enough penalization will shrink all the 
coefficients of the smooth to zero. Such smoothers can effectively be penalized out of the 
model altogether, as part of smoothing parameter estimation. 2 classes
of these shrinkage smoothers are implemented: \code{"cs"} and \code{"ts"}, based on 
cubic regression spline and thin plate regression spline smoothers (see \code{\link{s}})  }

\item{Null space penalization}{An alternative is to construct an extra penalty for each 
smooth which penalizes the space of functions of zero wiggliness according to its existing penalties.
If all the smoothing parameters for such a term tend to infinity then the term is penalized to zero, 
and is effectively dropped from the model. The advantage of this approach is that it can be 
implemented automatically for any smooth. The \code{select} argument to \code{\link{gam}} causes 
this latter approach to be used.}
}

REML and ML smoothness selection are equivalent under this approach, and simulation evidence suggests 
that they tend to perform a little better than prediction error criteria, for model selection.

}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\references{
Marra, G. and S.N. Wood (2011) Practical variable selection for generalized additive models
Computational Statistics and Data Analysis 55,2372-2387

}

\seealso{\code{\link{gam.selection}}}

\examples{
## an example of GCV based model selection as
## an alternative to stepwise selection, using
## shrinkage smoothers...
library(mgcv)
set.seed(0);n <- 400
dat <- gamSim(1,n=n,scale=2)
dat$x4 <- runif(n, 0, 1)
dat$x5 <- runif(n, 0, 1)
attach(dat)
## Note the increased gamma parameter below to favour
## slightly smoother models...
b<-gam(y~s(x0,bs="ts")+s(x1,bs="ts")+s(x2,bs="ts")+
   s(x3,bs="ts")+s(x4,bs="ts")+s(x5,bs="ts"),gamma=1.4)
summary(b)
plot(b,pages=1)

## Same again using REML/ML
b<-gam(y~s(x0,bs="ts")+s(x1,bs="ts")+s(x2,bs="ts")+
   s(x3,bs="ts")+s(x4,bs="ts")+s(x5,bs="ts"),method="REML")
summary(b)
plot(b,pages=1)

## And once more, but using the null space penalization
b<-gam(y~s(x0,bs="cr")+s(x1,bs="cr")+s(x2,bs="cr")+
   s(x3,bs="cr")+s(x4,bs="cr")+s(x5,bs="cr"),
   method="REML",select=TRUE)
summary(b)
plot(b,pages=1)


detach(dat);rm(dat)
}

\keyword{models} \keyword{regression}%-- one or more ..


