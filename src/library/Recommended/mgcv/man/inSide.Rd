\name{inSide}
\alias{inSide}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Are points inside boundary?}
\description{
Assesses whether points are inside a boundary. The boundary must enclose the
domain, but may include islands. 
}
\usage{
inSide(bnd,x,y)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{bnd}{This should have two equal length columns with names matching whatever is 
  supplied in \code{x} and \code{y}. This may contain several sections of boundary separated by
  \code{NA}. Alternatively \code{bnd} may be a list, each element of which 
  contains 2 columns named as above.
  See below for details.}
  \item{x}{x co-ordinates of points to be tested.}
  \item{y}{y co-ordinates of points to be tested.}
}

\details{
  Segments of boundary are separated by \code{NA}s, or are in separate list elements.
   The boundary co-ordinates are taken to define nodes which are joined by straight line segments in
  order to create the boundary. Each segment is assumed to
  define a closed loop, and the last point in a segment will be assumed to be
  joined to the first. Loops must not intersect (no test is made for
  this). 

  The method used is to count how many times a line, in the y-direction from a
  point, crosses a boundary segment. An odd number of crossings defines an
  interior point. Hence in geographic applications it would be usual to have
  an outer boundary loop, possibly with some inner `islands' completely
  enclosed in the outer loop. 
 
  The routine calls compiled C code and operates by an exhaustive search for
  each point in \code{x, y}.

}


\value{ The function returns a logical array of the same dimension as \code{x} and
\code{y}. \code{TRUE} indicates that the corresponding \code{x, y} point lies
inside the boundary.

}
   
\references{

\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}} 


\examples{
require(mgcv)
m <- 300;n <- 150
xm <- seq(-1,4,length=m);yn<-seq(-1,1,length=n)
x <- rep(xm,n);y<-rep(yn,rep(m,n))
er <- matrix(fs.test(x,y),m,n)
bnd <- fs.boundary()
in.bnd <- inSide(bnd,x,y)
plot(x,y,col=as.numeric(in.bnd)+1,pch=".")
lines(bnd$x,bnd$y,col=3)
points(x,y,col=as.numeric(in.bnd)+1,pch=".")
## check boundary details ...
plot(x,y,col=as.numeric(in.bnd)+1,pch=".",ylim=c(-1,0),xlim=c(3,3.5))
lines(bnd$x,bnd$y,col=3)
points(x,y,col=as.numeric(in.bnd)+1,pch=".")

}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..




