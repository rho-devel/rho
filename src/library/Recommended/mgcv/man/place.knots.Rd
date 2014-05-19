\name{place.knots}
\alias{place.knots}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Automatically place a set of knots evenly through covariate values}
\description{
Given a univariate array of covariate values, places a set of knots for a regression spline evenly through the 
covariate values.
}
\usage{ place.knots(x,nk)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{array of covariate values (need not be sorted).} 
 \item{nk}{integer indicating the required number of knots.}
}
\details{ 
Places knots evenly throughout a set of covariates. For example, if you had 11 covariate values and wanted 6 knots 
then a knot would be placed at the first (sorted) covariate value and every second (sorted) value thereafter. With 
less convenient numbers of data and knots the knots are placed within intervals between data in order to achieve 
even coverage, where even means having approximately the same number of data between each pair of knots.}

\value{ An array of knot locations.
}
\references{
\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}} 

\seealso{ \code{\link{smooth.construct.cc.smooth.spec}}  }

\examples{
require(mgcv)
x<-runif(30)
place.knots(x,7)
rm(x)
}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..




