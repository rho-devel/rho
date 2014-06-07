\name{fs.test}
\alias{fs.test}
\alias{fs.boundary}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{FELSPLINE test function}
\description{Implements a finite area test function based on one proposed by Tim Ramsay (2002).
}
\usage{

fs.test(x,y,r0=.1,r=.5,l=3,b=1,exclude=TRUE)
fs.boundary(r0=.1,r=.5,l=3,n.theta=20)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x,y}{Points at which to evaluate the test function.}
  \item{r0}{The test domain is a sort of bent sausage. This is the radius of
  the inner bend}
  \item{r}{The radius of the curve at the centre of the sausage.}
 \item{l}{The length of an arm of the sausage.}
  \item{b}{The rate at which the function increases per unit increase in
  distance along the centre line of the sausage.}
\item{exclude}{Should exterior points be set to \code{NA}?}
\item{n.theta}{How many points to use in a piecewise linear representation of
a quarter of a circle, when generating the boundary curve.}
}

\details{
 The function details are not given in the source article: but this is pretty
 close. The function is modified from Ramsay (2002), in that it bulges, rather
 than being flat: this makes a better test of the smoother.

}


\value{ \code{fs.test} returns function evaluations, or \code{NA}s for points
outside the boundary. \code{fs.boundary} returns a list of \code{x,y} points
to be jointed up in order to define/draw the boundary.

}
 
\references{
Tim Ramsay (2002) "Spline smoothing over difficult regions" J.R.Statist. Soc. B  64(2):307-319 

}
\author{ Simon N. Wood \email{simon.wood@r-project.org}} 

\examples{
require(mgcv)
## plot the function, and its boundary...
fsb <- fs.boundary()
m<-300;n<-150 
xm <- seq(-1,4,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
tru <- matrix(fs.test(xx,yy),m,n) ## truth
image(xm,yn,tru,col=heat.colors(100),xlab="x",ylab="y")
lines(fsb$x,fsb$y,lwd=3)
contour(xm,yn,tru,levels=seq(-5,5,by=.25),add=TRUE)
}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..




