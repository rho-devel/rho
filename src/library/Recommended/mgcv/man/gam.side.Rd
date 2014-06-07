\name{gam.side}
\alias{gam.side}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Identifiability side conditions for a GAM}
\description{ GAM formulae with repeated variables may only correspond to
  identifiable models given some side conditions. This routine works 
  out appropriate side conditions, based on zeroing redundant parameters.
  It is called from \code{mgcv:::gam.setup} and is not intended to be called by users. 

  The method identifies nested and repeated variables by their names, but
  numerically evaluates which constraints need to be imposed. Constraints are always
  applied to smooths of more variables in preference to smooths of fewer
  variables. The numerical approach allows appropriate constraints to be
  applied to models constructed using any smooths, including user defined smooths.
}
\usage{
gam.side(sm,Xp,tol=.Machine$double.eps^.5,with.pen=FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{sm}{ A list of smooth objects as returned by
  \code{\link{smooth.construct}}.}
  \item{Xp}{The model matrix for the strictly parametric model components.}
  \item{tol}{The tolerance to use when assessing linear dependence of smooths.}
  \item{with.pen}{Should the computation of dependence consider the penalties or not. 
   Doing so will lead to fewer constraints.}
}
\details{ Models such as  \code{y~s(x)+s(z)+s(x,z)} can be estimated by
  \code{\link{gam}}, but require identifiability constraints to be applied, to
  make them identifiable. This routine does this, effectively setting redundant parameters
  to zero. When the redundancy is between smooths of lower and higher numbers
  of variables, the constraint is always applied to the smooth of the higher
  number of variables. 

  Dependent smooths are identified symbolically, but which constraints are 
  needed to ensure identifiability of these smooths is determined numerically, using
  \code{\link{fixDependence}}. This makes the routine rather general, and not
  dependent on any particular basis.

  \code{Xp} is used to check whether there is a constant term in the model (or 
  columns that can be linearly combined to give a constant). This is because 
  centred smooths can appear independent, when they would be dependent if there 
  is a constant in the model, so dependence testing needs to take account of this.

}
\value{ A list of smooths, with model matrices and penalty matrices adjusted
to automatically impose the required constraints. Any smooth that has been
modified will have an attribute \code{"del.index"}, listing the columns of its
model matrix that were deleted. This index is used in the creation of
prediction matrices for the term.
}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\examples{
## The first two examples here iluustrate models that cause
## gam.side to impose constraints, but both are a bad way 
## of estimating such models. The 3rd example is the right
## way.... 
set.seed(7)
require(mgcv)
dat <- gamSim(n=400,scale=2) ## simulate data
## estimate model with redundant smooth interaction (bad idea).
b<-gam(y~s(x0)+s(x1)+s(x0,x1)+s(x2),data=dat)
plot(b,pages=1)

## Simulate data with real interation...
dat <- gamSim(2,n=500,scale=.1)
old.par<-par(mfrow=c(2,2))

## a fully nested tensor product example (bad idea)
b <- gam(y~s(x,bs="cr",k=6)+s(z,bs="cr",k=6)+te(x,z,k=6),
       data=dat$data)
plot(b)

old.par<-par(mfrow=c(2,2))
## A fully nested tensor product example, done properly,
## so that gam.side is not needed to ensure identifiability.
## ti terms are designed to produce interaction smooths
## suitable for adding to main effects (we could also have
## used s(x) and s(z) without a problem, but not s(z,x) 
## or te(z,x)).
b <- gam(y ~ ti(x,k=6) + ti(z,k=6) + ti(x,z,k=6),
       data=dat$data)
plot(b)

par(old.par)
rm(dat)
}
\keyword{models} \keyword{regression}%-- one or more ..


